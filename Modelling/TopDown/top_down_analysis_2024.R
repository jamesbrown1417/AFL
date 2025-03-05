#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(arrow)
library(fitzRoy)
`%notin%` <- Negate(`%in%`)

source("Functions/fix_team_names.R")

#===============================================================================
# Get fixture for current season
#===============================================================================

# Get fixtures
fixture_2024 <- fetch_fixture_afl(season = 2024)

# Tidy dataset
fixture_2024 <-
  fixture_2024 |>
  mutate(
    home.team.name = fix_team_names(home.team.name),
    away.team.name = fix_team_names(away.team.name)
  ) |>
  transmute(
    match = paste(home.team.name, "v", away.team.name),
    round = round.name,
    home_team = home.team.name,
    away_team = away.team.name,
    start_time = ymd_hms(utcStartTime),
    venue = venue.name
  )

# Convert start_time to adelaide time
fixture_2024$start_time <- with_tz(fixture_2024$start_time, tzone = "Australia/Adelaide")

# Select columns
fixture_2024 <-
  fixture_2024 |> 
  select(match, round, start_time, venue)

#===============================================================================
#Read in historical odds file
#===============================================================================

# Read in file
historical_odds <-
  read_parquet("/Users/jamesbrown/Projects/OddsHistory/AFL/Season2024/output_datasets/combined.parquet") |> 
  filter(!is.na(home_team)) |> 
  # Give timestamp adelaide timezone
  mutate(timestamp = force_tz(timestamp, tzone = "Australia/Adelaide"))

#===============================================================================
# Add match info to dataset
#===============================================================================

# Add match info
historical_odds <-
  historical_odds |>
  left_join(fixture_2024, by = c("match" = "match")) |> 
  # Create a variable which is the time difference between the start of the match and the time the odds were scraped
  mutate(time_diff = as.numeric(difftime(start_time, timestamp, units = "hours"))) |>
  arrange(match, player_name, line, agency, start_time) |> 
  # Filter less than zero
  filter(time_diff >= 0) |>
  # Filter so that the time diff is smaller than 2 weeks in hours
  filter(time_diff <= 336)

#===============================================================================
# Get Results
#===============================================================================

# Get Disposals Results
disposals_results <-
  read_rds("Data/afl_fantasy_2024_data.rds") |>
  select(match = match_name,
         venue,
         round,
         player_name = player_full_name,
         player_team,
         disposals) |> 
  mutate(match = str_replace(match, "Vs", "v"))

#===============================================================================
# Get all opening and closing lines
#===============================================================================

# Opening Odds
opening_odds <-
  historical_odds |>
  group_by(match, home_team, away_team, round, line, market_name, agency) |> 
  filter(time_diff == max(time_diff)) |>
  ungroup() |> 
  left_join(disposals_results) |>
  filter(!is.na(player_team)) |> 
  filter(!is.na(disposals))

# Opening Lines
opening_lines <-
  opening_odds |> 
  filter(!is.na(under_price)) |>
  group_by(match, player_name, player_team, round, venue) |> 
  filter(time_diff == min(time_diff)) |> 
  ungroup() |> 
  arrange(timestamp) |>
  mutate(over_won = if_else(disposals >= line, TRUE, FALSE)) |> 
  filter(agency != "Neds")

# Closing Odds
closing_odds <-
  historical_odds |>
  group_by(match, home_team, away_team, round, line, market_name, agency) |> 
  filter(time_diff == min(time_diff)) |>
  ungroup() |> 
  left_join(disposals_results) |>
  filter(!is.na(player_team)) |> 
  filter(!is.na(disposals))

# Closing Lines
closing_lines <-
  closing_odds |> 
  filter(!is.na(under_price)) |>
  group_by(match, player_name, player_team, round, venue) |> 
  filter(time_diff == min(time_diff)) |> 
  ungroup() |> 
  arrange(timestamp) |>
  mutate(over_won = if_else(disposals >= line, TRUE, FALSE)) |> 
  filter(agency != "Neds")

#===============================================================================
# Get Sportsbet Devigged Lines
#===============================================================================

# Source devig function
source("Scripts/devig_function.R")

# Get SB Odds
sb_odds <-
  closing_lines |> 
  filter(agency == "Sportsbet")

# Get devigged prices
devigged_prices <- 
  de_vig_log(over_price = sb_odds$over_price, under_price = sb_odds$under_price)

sb_odds_devigged <-
  sb_odds |> 
  bind_cols(devigged_prices) |> 
  relocate(over_price_devigged, .after = over_price) |> 
  relocate(under_price_devigged, .after = under_price) |> 
  select(-over_price, -under_price, -diff_over_2023, -diff_under_2023, -agency)

#===============================================================================
# Analyse SB Devigged Lines
#===============================================================================

# Calculate ROI
sb_odds_devigged |> 
  mutate(profit_overs = if_else(over_won, 100*over_price_devigged - 100, -100),
         profit_unders = if_else(!over_won, 100*under_price_devigged - 100, -100)) |>
  summarise(total_bets = n(),
            profit_overs = sum(profit_overs),
            profit_unders = sum(profit_unders),
            total_staked = 100*total_bets,
            roi_overs = profit_overs/total_staked,
            roi_unders = profit_unders/total_staked)

#===============================================================================
# Analyse Results When Always Taking Best Price
#===============================================================================

# Best Overs
closing_lines |> 
  group_by(match, player_name, player_team, round, venue, line) |>
  filter(over_price == max(over_price)) |> 
  ungroup() |> 
  mutate(profit_overs = if_else(over_won, 100*over_price - 100, -100),
         profit_unders = if_else(!over_won, 100*under_price - 100, -100)) |>
  summarise(total_bets = n(),
            profit_overs = sum(profit_overs),
            profit_unders = sum(profit_unders),
            total_staked = 100*total_bets,
            roi_overs = profit_overs/total_staked,
            roi_unders = profit_unders/total_staked)

# Best Unders
closing_lines |> 
  group_by(match, player_name, player_team, round, venue, line) |>
  filter(under_price == max(under_price)) |> 
  ungroup() |> 
  mutate(profit_overs = if_else(over_won, 100*over_price - 100, -100),
         profit_unders = if_else(!over_won, 100*under_price - 100, -100)) |>
  summarise(total_bets = n(),
            profit_overs = sum(profit_overs),
            profit_unders = sum(profit_unders),
            total_staked = 100*total_bets,
            roi_overs = profit_overs/total_staked,
            roi_unders = profit_unders/total_staked)

#===============================================================================
# Compare Performance For Alt Lines when Better than second best price
#===============================================================================

beat_by_five <-
closing_odds |> 
  filter(is.na(under_price)) |> 
  group_by(match,venue, player_name, player_team, round, line) |>
  arrange(match, venue, player_name, player_team, round, line, desc(over_price)) |>
  filter(n() > 5) |>
  filter(!is.na(line)) |> 
  # Get implied probability difference between best and second best price
  mutate(diff_over = 1/lead(over_price, 1) - 1/over_price) |> 
  slice_head(n = 1) |>
  ungroup() |>
  filter(diff_over > 0.1) |>
  # Calculate stake to win 100 based on over price
  mutate(stake = 500/(over_price-1)) |>
  mutate(over_won = if_else(disposals >= line, TRUE, FALSE)) |> 
  mutate(profit = if_else(over_won, stake*over_price - stake, -stake))

beat_by_five |>
  group_by(agency) |> 
  summarise(total_bets = n(),
            profit = sum(profit),
            total_staked = sum(stake),
            roi = profit/total_staked)
