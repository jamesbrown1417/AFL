#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(future)
library(furrr)
`%notin%` <- Negate(`%in%`)

# Set up parallel processing
plan(multisession)

#===============================================================================
# Load in function
#===============================================================================

source("Functions/compare_win_vs_loss_performance.R")

#===============================================================================
# Get all players who played at least 16 games last season
#===============================================================================

all_players <-
combined_stats |>
  filter(season_name %in% c("2025", "2026")) |>
  group_by(player_full_name, player_id) |>
  summarise(games_played = n()) |>
  filter(games_played >= 16) |>
  select(player = player_full_name) |>
  ungroup()

#===============================================================================
# Apply function to all players
#===============================================================================

# Disposals---------------------------------------------------------------------
disposals_win_vs_loss <-
future_pmap(all_players, compare_win_vs_loss_performance, stat = "disposals", .progress = TRUE) |>
  bind_rows() |>
  arrange(desc(median_diff))

# Fantasy Points----------------------------------------------------------------
fantasy_win_vs_loss <-
  future_pmap(all_players, compare_win_vs_loss_performance, stat = "fantasy_points", .progress = TRUE) |>
  bind_rows() |>
  arrange(desc(median_diff))

# Goals-------------------------------------------------------------------------
goals_win_vs_loss <-
  future_pmap(all_players, compare_win_vs_loss_performance, stat = "goals", .progress = TRUE) |>
  bind_rows() |>
  arrange(desc(median_diff))

# Marks-------------------------------------------------------------------------
marks_win_vs_loss <-
  future_pmap(all_players, compare_win_vs_loss_performance, stat = "marks", .progress = TRUE) |>
  bind_rows() |>
  arrange(desc(median_diff))

# Tackles-----------------------------------------------------------------------
tackles_win_vs_loss <-
  future_pmap(all_players, compare_win_vs_loss_performance, stat = "tackles", .progress = TRUE) |>
  bind_rows() |>
  arrange(desc(median_diff))

#===============================================================================
# Write median differences to disk (one tidy row per player + stat)
#===============================================================================

win_vs_loss_median_diff <-
  bind_rows(disposals_win_vs_loss,
            fantasy_win_vs_loss,
            goals_win_vs_loss,
            marks_win_vs_loss,
            tackles_win_vs_loss) |>
  transmute(player_name, stat, median_diff, games_win, games_loss)

write_csv(win_vs_loss_median_diff, "Data/player_win_vs_loss_diff.csv")
