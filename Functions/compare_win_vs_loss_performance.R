#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Read in Data
#===============================================================================

combined_stats <-
  read_rds("Data/afl_fantasy_2015_2025_data.rds") |>
  bind_rows(read_rds("Data/afl_fantasy_2026_data.rds")) |>
  mutate(home_away = if_else(home_team == player_team, "home", "away")) |>
  mutate(player_team_score = if_else(player_team == home_team, home_team_score, away_team_score),
         opposition_team_score = if_else(player_team == home_team, away_team_score, home_team_score),
         result = case_when(
           player_team_score > opposition_team_score ~ "win",
           player_team_score < opposition_team_score ~ "loss",
           TRUE ~ "draw"
         ))

combined_stats_2026 <-
  combined_stats |>
  tidytable::filter(season_name %in% c("2025", "2026"))

#===============================================================================
# Function to compare win vs loss performance
#===============================================================================

compare_win_vs_loss_performance <- function(player, stat) {
  # Get all of player's games last season (drawn games are excluded)
  player_stats <-
    combined_stats_2026 |>
    tidytable::filter(player_full_name == player) |>
    tidytable::filter(result %in% c("win", "loss")) |>
    tidytable::arrange(tidytable::desc(start_time_utc)) |>
    tidytable::select(match_name, round, season_name, start_time_utc, result, player_name = player_full_name, player_stat = !!sym(stat), home_team, away_team)

  # Get median, mean and game count of player's stat grouped by win / loss status
  win_loss_summary <-
  player_stats |>
    tidytable::group_by(player_name, result) |>
    tidytable::summarise(median_stat = median(player_stat, na.rm = TRUE),
                         mean_stat = mean(player_stat, na.rm = TRUE),
                         games = tidytable::n())

  # Get difference between win and loss median and mean
  win_loss_wide <-
    win_loss_summary |>
      tidytable::pivot_wider(names_from = result,
                             values_from = c(median_stat, mean_stat, games))

  # A player may have only wins or only losses in the window; ensure both sides
  # exist so the diff columns can always be computed (missing side -> NA / 0 games)
  for (col in c("median_stat_win", "median_stat_loss", "mean_stat_win", "mean_stat_loss")) {
    if (col %notin% names(win_loss_wide)) win_loss_wide[[col]] <- NA_real_
  }
  for (col in c("games_win", "games_loss")) {
    if (col %notin% names(win_loss_wide)) win_loss_wide[[col]] <- 0L
  }

  win_loss_wide |>
    tidytable::mutate(median_diff = median_stat_win - median_stat_loss,
                      mean_diff = mean_stat_win - mean_stat_loss) |>
    tidytable::mutate(stat = stat)
}
