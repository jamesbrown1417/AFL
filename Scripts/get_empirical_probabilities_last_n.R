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
get_last_n_probs <- function(stat, lines, output_path) {
  combos <-
    expand_grid(player_full_name = players_this_year, line = lines) |>
    mutate(stat = stat)

  future_pmap(combos, get_empirical_prob, .progress = TRUE) |>
    bind_rows() |>
    select(player_name = player_full_name, line, contains("emp_prob")) |>
    write_csv(output_path)
}

#===============================================================================
# Line definitions (same as Scripts/get_empirical_probabilties_2025.R)
#===============================================================================

disposal_lines <- seq(9.5, 39.5, by = 1)
fantasy_lines <- c(69.5, 74.5, 79.5, 84.5, 89.5, 94.5, 99.5, 104.5, 109.5, 114.5, 119.5)
goal_lines <- c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5)
kick_lines <- c(9.5, 11.5, 13.5, 15.5, 17.5, 19.5, 21.5, 23.5, 25.5, 27.5)
handball_lines <- c(7.5, 9.5, 11.5, 13.5, 15.5, 17.5, 19.5, 21.5, 23.5)

#===============================================================================
# Run and write outputs
#===============================================================================

get_last_n_probs("disposals", disposal_lines, "Data/empirical_probabilities_disposals_last_n.csv")
get_last_n_probs("fantasy_points", fantasy_lines, "Data/empirical_probabilities_fantasy_points_last_n.csv")
get_last_n_probs("goals", goal_lines, "Data/empirical_probabilities_goals_last_n.csv")
get_last_n_probs("kicks", kick_lines, "Data/empirical_probabilities_kicks_last_n.csv")
get_last_n_probs("handballs", handball_lines, "Data/empirical_probabilities_handballs_last_n.csv")
