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
  mutate(home_away = if_else(home_team == player_team, "home", "away"))

combined_stats_2026 <-
  combined_stats |> 
  tidytable::filter(season_name %in% c("2025", "2026"))

#===============================================================================
# Function to compare home vs away performance
#===============================================================================

compare_home_vs_away_performance <- function(player, stat) {
  # Get all of player's games last season
  player_stats <-
    combined_stats_2026 |> 
    tidytable::filter(player_full_name == player) |> 
    tidytable::arrange(tidytable::desc(start_time_utc)) |> 
    tidytable::select(match_name, round, season_name, start_time_utc, home_away, player_name = player_full_name, player_stat = !!sym(stat), home_team, away_team)

  # Get median, mean and game count of player's stat grouped by home / away status
  home_away_summary <-
  player_stats |>
    tidytable::group_by(player_name, home_away) |>
    tidytable::summarise(median_stat = median(player_stat, na.rm = TRUE),
                         mean_stat = mean(player_stat, na.rm = TRUE),
                         games = tidytable::n())

  # Get difference between home and away median and mean
  home_away_wide <-
    home_away_summary |>
      tidytable::pivot_wider(names_from = home_away,
                             values_from = c(median_stat, mean_stat, games))

  # A player may have only home or only away games in the window; keep the row
  # importable with NA diffs and explicit zero counts for the missing side.
  for (col in c("median_stat_home", "median_stat_away", "mean_stat_home", "mean_stat_away")) {
    if (col %notin% names(home_away_wide)) home_away_wide[[col]] <- NA_real_
  }
  for (col in c("games_home", "games_away")) {
    if (col %notin% names(home_away_wide)) home_away_wide[[col]] <- 0L
  }

  home_away_wide |>
    tidytable::mutate(median_diff = median_stat_home - median_stat_away,
                      mean_diff = mean_stat_home - mean_stat_away) |>
    tidytable::mutate(stat = stat)
}
