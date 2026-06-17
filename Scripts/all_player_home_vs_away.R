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

source("Functions/compare_home_vs_away_performance.R")

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
disposals_home_vs_away <-
future_pmap(all_players, compare_home_vs_away_performance, stat = "disposals", .progress = TRUE) |> 
  bind_rows() |> 
  arrange(desc(median_diff))

# Fantasy Points----------------------------------------------------------------
fantasy_home_vs_away <-
  future_pmap(all_players, compare_home_vs_away_performance, stat = "fantasy_points", .progress = TRUE) |> 
  bind_rows() |> 
  arrange(desc(median_diff))

# Goals-------------------------------------------------------------------------
goals_home_vs_away <-
  future_pmap(all_players, compare_home_vs_away_performance, stat = "goals", .progress = TRUE) |> 
  bind_rows() |> 
  arrange(desc(median_diff))

# Marks-------------------------------------------------------------------------
marks_home_vs_away <-
  future_pmap(all_players, compare_home_vs_away_performance, stat = "marks", .progress = TRUE) |> 
  bind_rows() |> 
  arrange(desc(median_diff))

# Tackles-----------------------------------------------------------------------
tackles_home_vs_away <-
  future_pmap(all_players, compare_home_vs_away_performance, stat = "tackles", .progress = TRUE) |>
  bind_rows() |>
  arrange(desc(median_diff))

#===============================================================================
# Write median differences to disk (one tidy row per player + stat)
#===============================================================================

home_vs_away_median_diff <-
  bind_rows(disposals_home_vs_away,
            fantasy_home_vs_away,
            goals_home_vs_away,
            marks_home_vs_away,
            tackles_home_vs_away) |>
  transmute(player_name, stat, median_diff, games_home, games_away)

write_csv(home_vs_away_median_diff, "Data/player_home_vs_away_diff.csv")
