# Libraries and functions-------------------------------------------------------
library(tidyverse)
library(googlesheets4)
library(googledrive)
library(future)
library(furrr)

# Set up parallel processing
plan(multisession)

# Get empirical probability function
source("Functions/get_empirical_probabilities_last_n.R")

#===============================================================================
# Disposals
#===============================================================================

# Get all scraped odds files and combine
all_player_disposals <-
  list.files("Data/scraped_odds", full.names = TRUE, pattern = "disposals") |>
  map(read_csv) |>
  # Ignore null elements
  keep(~nrow(.x) > 0) |>
  # de-select event_id from each if it exists
  map(~select(.x, -matches("id"))) |>
  reduce(bind_rows) |>
  arrange(player_name, line, desc(over_price)) |>
  select(-matches("id"))

# Disposals
distinct_disposal_combos <-
  all_player_disposals |>
  distinct(player_name, line) |> 
  rename(player_full_name = player_name) |> 
  mutate(stat = "disposals")

player_emp_probs_last_n <-
  future_pmap(distinct_disposal_combos, get_empirical_prob, .progress = TRUE) |>
  bind_rows() |>
  select(player_name = player_full_name, line, contains("emp_prob"))

# Save disposals output
player_emp_probs_last_n |>
  write_csv("Data/empirical_probabilities_disposals_last_n.csv")

#===============================================================================
# Goals
#===============================================================================

# Get all scraped odds files and combine
all_player_goals <-
  list.files("Data/scraped_odds", full.names = TRUE, pattern = "goals") |>
  map(read_csv) |>
  # Ignore null elements
  keep(~nrow(.x) > 0) |>
  # de-select event_id from each if it exists
  map(~select(.x, -matches("id"))) |>
  reduce(bind_rows) |>
  arrange(player_name, line, desc(over_price)) |>
  select(-matches("id"))

# Goals
distinct_goal_combos <-
  all_player_goals |>
  distinct(player_name, line) |> 
  rename(player_full_name = player_name) |> 
  mutate(stat = "goals")

player_emp_probs_goals_last_n <-
  future_pmap(distinct_goal_combos, get_empirical_prob, .progress = TRUE) |>
  bind_rows() |>
  select(player_name = player_full_name, line, contains("emp_prob"))

# Save goals output
player_emp_probs_goals_last_n |>
  write_csv("Data/empirical_probabilities_goals_last_n.csv")

#===============================================================================
# Fantasy Points
#===============================================================================

# Get all scraped odds files and combine
all_player_fantasy_points <-
  list.files("Data/scraped_odds", full.names = TRUE, pattern = "fantasy_points") |>
  map(read_csv) |>
  # Ignore null elements
  keep(~nrow(.x) > 0) |>
  # de-select event_id from each if it exists
  map(~select(.x, -matches("id"))) |>
  reduce(bind_rows) |>
  arrange(player_name, line, desc(over_price)) |>
  select(-matches("id"))

# Fantasy Points
distinct_fantasy_point_combos <-
  all_player_fantasy_points |>
  distinct(player_name, line) |>
  rename(player_full_name = player_name) |>
  mutate(stat = "fantasy_points")

player_emp_probs_fantasy_points_last_n <-
  future_pmap(distinct_fantasy_point_combos, get_empirical_prob, .progress = TRUE) |>
  bind_rows() |>
  select(player_name = player_full_name, line, contains("emp_prob"))

# Save fantasy points output
player_emp_probs_fantasy_points_last_n |>
  write_csv("Data/empirical_probabilities_fantasy_points_last_n.csv")

#===============================================================================
# Marks
#===============================================================================

# Get all scraped odds files and combine
all_player_marks <-
  list.files("Data/scraped_odds", full.names = TRUE, pattern = "marks") |>
  map(read_csv) |>
  # Ignore null elements
  keep(~nrow(.x) > 0) |>
  # de-select event_id from each if it exists
  map(~select(.x, -matches("id"))) |>
  reduce(bind_rows) |>
  arrange(player_name, line, desc(over_price)) |>
  select(-matches("id"))

# Marks
distinct_mark_combos <-
  all_player_marks |>
  distinct(player_name, line) |> 
  rename(player_full_name = player_name) |> 
  mutate(stat = "marks")

player_emp_probs_marks_last_n <-
  future_pmap(distinct_mark_combos, get_empirical_prob, .progress = TRUE) |>
  bind_rows() |>
  select(player_name = player_full_name, line, contains("emp_prob"))

# Save marks output
player_emp_probs_marks_last_n |>
  write_csv("Data/empirical_probabilities_marks_last_n.csv")

#===============================================================================
# Tackles
#===============================================================================

# Get all scraped odds files and combine
all_player_tackles <-
  list.files("Data/scraped_odds", full.names = TRUE, pattern = "tackles") |>
  map(read_csv) |>
  # Ignore null elements
  keep(~nrow(.x) > 0) |>
  # de-select event_id from each if it exists
  map(~select(.x, -matches("id"))) |>
  reduce(bind_rows) |>
  arrange(player_name, line, desc(over_price)) |>
  select(-matches("id"))

# Tackles
distinct_tackle_combos <-
  all_player_tackles |>
  distinct(player_name, line) |> 
  rename(player_full_name = player_name) |> 
  mutate(stat = "tackles")

player_emp_probs_tackles_last_n <-
  future_pmap(distinct_tackle_combos, get_empirical_prob, .progress = TRUE) |>
  bind_rows() |>
  select(player_name = player_full_name, line, contains("emp_prob"))

# Save tackles output
player_emp_probs_tackles_last_n |>
  write_csv("Data/empirical_probabilities_tackles_last_n.csv")

#===============================================================================
# Kicks
#===============================================================================

# Get all scraped odds files and combine
all_player_kicks <-
  list.files("Data/scraped_odds", full.names = TRUE, pattern = "kicks") |>
  map(read_csv) |>
  # Ignore null elements
  keep(~nrow(.x) > 0) |>
  # de-select event_id from each if it exists
  map(~select(.x, -matches("id"))) |>
  reduce(bind_rows) |>
  arrange(player_name, line, desc(over_price)) |>
  select(-matches("id"))

# Kicks
distinct_kick_combos <-
  all_player_kicks |>
  distinct(player_name, line) |> 
  rename(player_full_name = player_name) |> 
  mutate(stat = "kicks")

player_emp_probs_kicks_last_n <-
  future_pmap(distinct_kick_combos, get_empirical_prob, .progress = TRUE) |>
  bind_rows() |>
  select(player_name = player_full_name, line, contains("emp_prob"))

# Save kicks output
player_emp_probs_kicks_last_n |>
  write_csv("Data/empirical_probabilities_kicks_last_n.csv")

#===============================================================================
# Handballs
#===============================================================================

# Get all scraped odds files and combine
all_player_handballs <-
  list.files("Data/scraped_odds", full.names = TRUE, pattern = "handballs") |>
  map(read_csv) |>
  # Ignore null elements
  keep(~nrow(.x) > 0) |>
  # de-select event_id from each if it exists
  map(~select(.x, -matches("id"))) |>
  reduce(bind_rows) |>
  arrange(player_name, line, desc(over_price)) |>
  select(-matches("id"))

# Handballs
distinct_handball_combos <-
  all_player_handballs |>
  distinct(player_name, line) |> 
  rename(player_full_name = player_name) |> 
  mutate(stat = "handballs")

player_emp_probs_handballs_last_n <-
  future_pmap(distinct_handball_combos, get_empirical_prob, .progress = TRUE) |>
  bind_rows() |>
  select(player_name = player_full_name, line, contains("emp_prob"))

# Save handballs output
player_emp_probs_handballs_last_n |>
  write_csv("Data/empirical_probabilities_handballs_last_n.csv")


