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
# Apply function to all player, disposal combinations
#===============================================================================

# Get Disposal Lines
disposal_lines <- seq(9.5, 39.5, by = 1)

# Get table of all player, disposal combinations
disposals_table <- 
  expand_grid(player_full_name = players_this_year, line = disposal_lines) |> 
  mutate(stat = "disposals")

# Apply function to all player, disposal combinations
disposals_results <-
  future_pmap(disposals_table, get_empirical_prob_season, .progress = TRUE) |> 
  bind_rows()

# Join with initial table
disposals_table <-
  disposals_table |> 
  bind_cols(disposals_results) |> 
  mutate(across(where(is.numeric), ~round(., 3)))

#===============================================================================
# Apply function to all player, fantasy combinations
#===============================================================================

# Get fantasy Lines
fantasy_lines <- c(69.5, 74.5, 79.5, 84.5, 89.5, 94.5, 99.5, 104.5, 109.5, 114.5, 119.5)

# Get table of all player, fantasy combinations
fantasy_table <- 
  expand_grid(player_full_name = players_this_year, line = fantasy_lines) |> 
  mutate(stat = "fantasy_points")

# Apply function to all player, fantasy combinations
fantasy_results <-
  future_pmap(fantasy_table, get_empirical_prob_season, .progress = TRUE) |> 
  bind_rows()

# Join with initial table
fantasy_table <-
  fantasy_table |> 
  bind_cols(fantasy_results) |> 
  mutate(across(where(is.numeric), ~round(., 3)))

#===============================================================================
# Apply function to all player, goal combinations
#===============================================================================

# Get goal Lines
goal_lines <- c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5)

# Get table of all player, goal combinations
goal_table <- 
  expand_grid(player_full_name = players_this_year, line = goal_lines) |> 
  mutate(stat = "goals")

# Apply function to all player, goal combinations
goal_results <-
  future_pmap(goal_table, get_empirical_prob_season, .progress = TRUE) |> 
  bind_rows()

# Join with initial table
goal_table <-
  goal_table |> 
  bind_cols(goal_results) |> 
  mutate(across(where(is.numeric), ~round(., 3)))

#===============================================================================
# Apply function to all player, kick combinations
#===============================================================================

# Get kick Lines
kick_lines <- c(9.5, 11.5, 13.5, 15.5, 17.5, 19.5, 21.5, 23.5, 25.5, 27.5)

# Get table of all player, kick combinations
kick_table <- 
  expand_grid(player_full_name = players_this_year, line = kick_lines) |> 
  mutate(stat = "kicks")

# Apply function to all player, kick combinations
kick_results <-
  future_pmap(kick_table, get_empirical_prob_season, .progress = TRUE) |> 
  bind_rows()

# Join with initial table
kick_table <-
  kick_table |> 
  bind_cols(kick_results) |> 
  mutate(across(where(is.numeric), ~round(., 3)))

#===============================================================================
# Apply function to all player, handball combinations
#===============================================================================

# Get handball Lines
handball_lines <- c(7.5, 9.5, 11.5, 13.5, 15.5, 17.5, 19.5, 21.5, 23.5)

# Get table of all player, handball combinations
handball_table <- 
  expand_grid(player_full_name = players_this_year, line = handball_lines) |> 
  mutate(stat = "handballs")

# Apply function to all player, handball combinations
handball_results <-
  future_pmap(handball_table, get_empirical_prob_season, .progress = TRUE) |> 
  bind_rows()

# Join with initial table
handball_table <-
  handball_table |> 
  bind_cols(handball_results) |> 
  mutate(across(where(is.numeric), ~round(., 3)))

#===============================================================================
# Combine and save as RDS
#===============================================================================

# Combine
combined_table <-
  bind_rows(disposals_table, fantasy_table, goal_table, kick_table, handball_table)

# Save as RDS
write_rds(combined_table, "Data/empirical_probabilities_2025.rds")
