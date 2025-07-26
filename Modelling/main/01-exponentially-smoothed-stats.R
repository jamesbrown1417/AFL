library(tidyverse)

# Read in data
afl_fantasy_2015_2024_data <- readRDS("../../Data/afl_fantasy_2015_2023_data.rds")
afl_fantasy_2025_data <- readRDS("../../Data/afl_fantasy_2024_data.rds")

# Combine data
combined_stats <-
  bind_rows(afl_fantasy_2015_2023_data, afl_fantasy_2024_data) |> 
  mutate(total_points = home_team_score + away_team_score)