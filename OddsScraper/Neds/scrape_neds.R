# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)
library(tidyjson)

# Get Squads
epl_squads <- read_rds("Data/epl_squads.rds")

# Get Fix Team Names Function
source("Scripts/fix_team_names.r")

# Get Fix Player Names Function
source("Scripts/fix_player_names.r")

#===============================================================================
# Get JSON for each match
#===============================================================================

# Read in df
df <- read_csv("OddsScraper/EPL/Neds/neds_epl_match_urls.csv")

# Get match json files
json_match_files <- list.files("OddsScraper/EPL/Neds/", pattern = "^data_.*.json", full.names = TRUE)

event_json_list <- map(json_match_files, ~fromJSON(.x))

#===============================================================================
# Get the market information for each match
#===============================================================================

# Initialize empty vectors to store the market names and IDs for mapping
market_lookup_name <- character()
market_lookup_id <- character()

# Initialize empty vectors to store data
event_ids <- character()
entrants <- character()
entrant_ids <- character()
market_id <- character()
match_names <- character()
handicaps <- numeric()
prices <- numeric()

# Loop through the entrants
for (i in seq_along(event_json_list)) {
  match <- event_json_list[[i]] 
  
  for (entrant in match$entrants) {
    entrants <- c(entrants, entrant$name)
    market_id <- c(market_id, entrant$market_id)
    event_ids <- c(event_ids,  event_json_list[[i]]$events[[1]]$id)
    entrant_ids <- c(entrant_ids, entrant$id)
  } 
  
  
  # Loop through the markets
  for (market in match$markets) {
    market_lookup_name <- c(market_lookup_name, market$name)
    market_lookup_id <- c(market_lookup_id, market$id)
    
    if (is.null(market$handicap)) {
      handicaps <- c(handicaps, NA)
    } else {
      handicaps <- c(handicaps, market$handicap)
    }
  }
  
  # Loop through the prices
  for (price in match$prices) {
    fractional_odds <- price$odds$numerator / price$odds$denominator
    decimal_odds <- fractional_odds + 1
    prices <- c(prices, decimal_odds)
  }
}

# Create market lookup dataframe
market_lookup_df <- data.frame(market_id = market_lookup_id, market_name = market_lookup_name, handicaps = handicaps)

# Create market dataframe
market_df <- data.frame(event_id = event_ids, market_id = market_id, entrants = entrants, entrant_id = entrant_ids, price = prices)

# Merge market lookup dataframe with market dataframe
market_df <- merge(market_df, market_lookup_df, by = 'market_id', all.x = TRUE)

# Reorder columns in market_df
market_df <- market_df |> select(event_id, market_id, market_name, entrants, entrant_id, handicaps, price)

# Add match names
market_df <-
  market_df |> 
  left_join(df[,c("event_name", "event_id")], by = c("event_id" = "event_id")) |> 
  relocate(event_name, .before = event_id) |> 
  rename(match_name = event_name) |> 
  select(-event_id)

# Create event ID df
event_ids_df <-
  df |>
  select(event_name, event_id) |> 
  rename(match_name = event_name) |> 
  separate(match_name, c("home_team", "away_team"), sep = " vs ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |> 
  mutate(match_name = paste(home_team, "v", away_team, sep = " ")) |> 
  select(match = match_name, event_id)

##%######################################################%##
#                                                          #
####               Get Head to Head Data                ####
#                                                          #
##%######################################################%##

# Filter to only include head to head markets
h2h_data <-
market_df |> 
    filter(market_name == "Match Result") |> 
    select(-market_name)

# Home teams
home_teams <-
    h2h_data |> 
    separate(match_name, c("home_team", "away_team"), sep = " vs ", remove = FALSE) |> 
    filter(entrants == home_team) |> 
    select(match = match_name, home_team, home_win = price) |> 
    mutate(home_team = fix_team_names(home_team))

# Draw
draws <-
    h2h_data |> 
    separate(match_name, c("home_team", "away_team"), sep = " vs ", remove = FALSE) |> 
    filter(entrants == "Draw") |> 
    select(match = match_name, draw = price)

# Away teams
away_teams <-
    h2h_data |> 
    separate(match_name, c("home_team", "away_team"), sep = " vs ", remove = FALSE) |> 
    filter(entrants == away_team) |> 
    select(match = match_name, away_team, away_win = price) |> 
    mutate(away_team = fix_team_names(away_team))

# Merge home and away teams
h2h_data <-
    home_teams |> 
    left_join(draws, by = "match") |>
    left_join(away_teams, by = c("match")) |> 
    mutate(match = paste(home_team, "v", away_team, sep = " ")) |>
    mutate(margin = round(1 / home_win + 1/draw + 1 / away_win, digits = 2)) |>
    mutate(agency = "Neds") |>
    mutate(market_name = "Head To Head") |> 
    select(match, market_name, home_team, away_team, home_win, draw, away_win, margin, agency)

# Get Total Goals Data

##%######################################################%##
#                                                          #
####                  Write out as CSV                  ####
#                                                          #
##%######################################################%##

h2h_data |> write_csv("Data/scraped_odds/neds_h2h.csv")
player_disposals_data |> write_csv("Data/scraped_odds/neds_player_disposals.csv")
player_goals_data |> write_csv("Data/scraped_odds/neds_player_goals.csv")
player_fantasy_data |> write_csv("Data/scraped_odds/neds_player_fantasy_points.csv")
