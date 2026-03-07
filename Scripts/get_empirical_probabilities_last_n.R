# Libraries and functions-------------------------------------------------------
library(tidyverse)
library(future)
library(furrr)

# Set up parallel processing
plan(multisession)

# Get empirical probability function
source("Functions/get_empirical_probabilities_last_n.R")

# All players for current season setup (including players with zero games)
players_this_year <-
  read_rds("Data/2026_start_positions_and_prices.rds") |>
  pull(player_full_name) |>
  unique() |>
  na.omit() |>
  as.character()

# Shared runner for all stats
get_last_n_probs <- function(market_key, stat_column, lines) {
  combos <-
    expand_grid(player_full_name = players_this_year, line = lines) |>
    mutate(stat = stat_column)

  future_pmap(combos, get_empirical_prob, .progress = TRUE) |>
    bind_rows() |>
    select(player_name = player_full_name, line, contains("emp_prob")) |>
    write_csv(glue::glue("Data/empirical_probabilities_{market_key}_last_n.csv"))
}

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

#===============================================================================
# Run and write outputs
#===============================================================================

pwalk(market_specs, get_last_n_probs)
