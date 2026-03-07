#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(future)
library(furrr)
`%notin%` <- Negate(`%in%`)

# Set up parallel processing
plan(multisession)

# Read in function
source("Functions/get_empirical_probabilities_last_season.R")

# Get 2025 players
players_2025 <- combined_stats |> filter(season_name == "2025")

# Get list of all players this year
players_this_year <-
  read_rds("Data/2026_start_positions_and_prices.rds") |> 
  filter(player_full_name %in% players_2025$player_full_name) |>
  pull(player_full_name) |> 
  unique()

#===============================================================================
# Hardcoded market lines
#===============================================================================

disposal_lines <- seq(9.5, 39.5, by = 1)
fantasy_lines <- seq(64.5, 129.5, by = 5)
goal_lines <- seq(0.5, 6.5, by = 1)
kick_lines <- seq(9.5, 27.5, by = 2)
handball_lines <- seq(7.5, 25.5, by = 2)
mark_lines <- seq(1.5, 11.5, by = 1)
tackle_lines <- seq(0.5, 12.5, by = 1)
hitout_lines <- seq(24.5, 49.5, by = 5)
clearance_lines <- seq(2.5, 9.5, by = 1)

market_specs <-
  tibble(
    market_key = c(
      "disposals",
      "fantasy_points",
      "goals",
      "kicks",
      "handballs",
      "marks",
      "tackles",
      "hitouts",
      "clearances"
    ),
    stat_column = c(
      "disposals",
      "fantasy_points",
      "goals",
      "kicks",
      "handballs",
      "marks",
      "tackles",
      "hitouts",
      "total_clearances"
    ),
    lines = list(
      disposal_lines,
      fantasy_lines,
      goal_lines,
      kick_lines,
      handball_lines,
      mark_lines,
      tackle_lines,
      hitout_lines,
      clearance_lines
    )
  )

get_season_probs <- function(market_key, stat_column, lines) {
  player_stat_lines <-
    expand_grid(player_full_name = players_this_year, line = lines) |>
    mutate(stat = stat_column)

  season_results <-
    future_pmap(player_stat_lines, get_empirical_prob_season, .progress = TRUE) |>
    bind_rows()

  player_stat_lines |>
    select(player_full_name, line) |>
    bind_cols(season_results) |>
    mutate(
      stat = market_key,
      across(where(is.numeric), ~ round(., 3))
    ) |>
    relocate(stat, .after = line)
}

#===============================================================================
# Combine and save as RDS
#===============================================================================

# Combine
combined_table <-
  pmap_dfr(market_specs, get_season_probs, .progress = TRUE)

# Save as RDS
write_rds(combined_table, "Data/empirical_probabilities_2025.rds")
