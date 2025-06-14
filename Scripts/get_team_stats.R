library(tidyverse)

# Read in current fantasy data table
afl_fantasy_2025_data <- read_rds("Data/afl_fantasy_2025_data.rds")
afl_fantasy_2015_2024_data <- read_rds("Data/afl_fantasy_2015_2024_data.rds")
afl_fantasy_data <- bind_rows(afl_fantasy_2015_2024_data, afl_fantasy_2025_data)

# Get just since 2021
afl_fantasy_data <-
  afl_fantasy_data |>
  filter(season_name %in% c("2021", "2022", "2023", "2024", "2025"))

# Get team variables
afl_team_data <-
  afl_fantasy_data |> 
  select(
    match_name,
    home_team,
    away_team,
    venue,
    start_time_utc,
    round,
    season_name,
    temperature,
    weather_category,
    home_team_goals,
    home_team_behinds,
    home_team_score,
    away_team_goals,
    away_team_behinds,
    away_team_score,
    margin,
    player_team,
    disposals,
    tackles,
    marks,
    fantasy_points
  )

# Get home team stats
home_data <-
  afl_team_data |>
  filter(home_team == player_team) |>
  group_by(
    match_name,
    home_team,
    away_team,
    venue,
    start_time_utc,
    round,
    season_name,
    temperature,
    weather_category,
    home_team_goals,
    home_team_behinds,
    home_team_score,
    away_team_goals,
    away_team_behinds,
    away_team_score,
    margin
  ) |>
  summarise(
    home_team_disposals = sum(disposals, na.rm = TRUE),
    home_team_tackles = sum(tackles, na.rm = TRUE),
    home_team_marks = sum(marks, na.rm = TRUE),
    home_team_fantasy_points = sum(fantasy_points, na.rm = TRUE)
  ) |>
  ungroup()

# Get away team stats
away_data <-
  afl_team_data |>
  filter(away_team == player_team) |>
  group_by(
    match_name,
    home_team,
    away_team,
    venue,
    start_time_utc,
    round,
    season_name,
    temperature,
    weather_category,
    home_team_goals,
    home_team_behinds,
    home_team_score,
    away_team_goals,
    away_team_behinds,
    away_team_score,
    margin
  ) |>
  summarise(
    away_team_disposals = sum(disposals, na.rm = TRUE),
    away_team_tackles = sum(tackles, na.rm = TRUE),
    away_team_marks = sum(marks, na.rm = TRUE),
    away_team_fantasy_points = sum(fantasy_points, na.rm = TRUE)
  ) |>
  ungroup()

# Combine home and away stats
all_team_stats <-
  home_data |>
  left_join(away_data, by = c("match_name", "home_team", "away_team", "venue", "start_time_utc", "round", "season_name", "temperature", "weather_category", "home_team_goals", "home_team_behinds", "home_team_score", "away_team_goals", "away_team_behinds", "away_team_score", "margin")) |>
  mutate(
    match_name,
    home_team,
    away_team,
    venue,
    start_time_utc,
    round,
    season_name,
    temperature,
    weather_category,
    home_team_goals,
    home_team_behinds,
    home_team_score,
    home_team_disposals,
    home_team_tackles,
    home_team_marks,
    home_team_fantasy_points,
    away_team_goals,
    away_team_behinds,
    away_team_score,
    away_team_disposals,
    away_team_tackles,
    away_team_marks,
    away_team_fantasy_points,
    margin,
    disposals = home_team_disposals + away_team_disposals,
    tackles = home_team_tackles + away_team_tackles,
    marks = home_team_marks + away_team_marks,
    fantasy_points = home_team_fantasy_points + away_team_fantasy_points,
    .keep = "none"
  ) |> 
  arrange(desc(start_time_utc))
