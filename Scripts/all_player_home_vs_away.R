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
# Get players with enough recent games to produce a useful split
#===============================================================================

all_players <-
combined_stats |> 
  filter(season_name %in% c("2025", "2026")) |> 
  group_by(player_full_name, player_id) |> 
  summarise(games_played = n()) |>
  filter(games_played >= 4) |>
  select(player = player_full_name) |> 
  ungroup()

#===============================================================================
# Apply function to all players
#===============================================================================

stats_to_compare <-
  tribble(
    ~stat, ~source_stat,
    "disposals", "disposals",
    "fantasy_points", "fantasy_points",
    "goals", "goals",
    "marks", "marks",
    "tackles", "tackles",
    "kicks", "kicks",
    "handballs", "handballs",
    "hitouts", "hitouts",
    "clearances", "total_clearances"
  )

compare_home_vs_away_stat <- function(market_stat, source_stat) {
  future_pmap(all_players, compare_home_vs_away_performance, stat = source_stat, .progress = TRUE) |>
    bind_rows() |>
    mutate(stat = market_stat) |>
    arrange(desc(median_diff))
}

#===============================================================================
# Write median differences to disk (one tidy row per player + stat)
#===============================================================================

home_vs_away_median_diff <-
  pmap(rename(stats_to_compare, market_stat = stat), compare_home_vs_away_stat) |>
  bind_rows() |>
  transmute(player_name, stat, median_diff, games_home, games_away)

write_csv(home_vs_away_median_diff, "Data/player_home_vs_away_diff.csv")
