# Libraries
library(tidyverse)
library(rvest)
library(httr2)

# URL to get responses
neds_url = "https://api.neds.com.au/v2/sport/event-request?category_ids=%5B%2223d497e6-8aab-4309-905b-9421f42c9bc5%22%5D&competition_id=ccff2e9a-5347-41aa-902a-bb6b1886d817&include_any_team_vs_any_team_events=true"

# Make request and get response
neds_response <-
    request(neds_url) |>
    req_perform() |> 
    resp_body_json()

# Function to fix team names
source("Functions/fix_team_names.R")

# Player names file
player_names <- read_rds("Data/2024_start_positions_and_prices.rds")
player_names <- player_names |> select(player_full_name, team_name)

# Initialize empty lists to store data
event_name <- character()
event_id <- character()
competition_name <- character()

# Extract event IDs and names from JSON response
for (value in neds_response$events) {
    event_name <- c(event_name, value$name)
    event_id <- c(event_id, value$id)
    competition_name <- c(competition_name, value$competition$name)
}

# Create a data frame from the vectors
df <- data.frame(event_name, event_id, competition_name)

# Filter the data frame to only include matches with ' vs ' in the event name
df <- df |> filter(str_detect(event_name, ' vs '))

# Only get AFL Games
df <- df |> filter(str_detect(competition_name, 'AFL'))

# Event IDs list
event_ids_df <-
  df |>
  separate(event_name, into = c("home_team", "away_team"), sep = " vs ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team), away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |> 
  select(match, event_id)

#===============================================================================
# Get event card data for each match
#===============================================================================

# Base URL for event card
event_url <- "https://api.neds.com.au/v2/sport/event-card?id="

# List of event URLs
event_json <- paste0(event_url, df$event_id)

# Initialize an empty list to store event JSON data
event_json_list <- list()

# Loop through each event URL and get the event card JSON data
for (url in event_json) {
  tryCatch({
    response2 <-
      request(url) |>
      req_headers(
        accept = "*/*",
        `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/88.0.4324.150 Safari/537.36",
        `Accept-Language` = "en-US,en;q=0.9"
      ) |>
      req_perform() |>
      resp_body_json()
    event_json_list <- append(event_json_list, list(response2))
  }, error = function(e) {
    cat("Error:", url, "\n", e$message, "\n")
  })
}

#===============================================================================
# Get the market information for each match
#===============================================================================

# Initialize empty vectors to store the market names and IDs for mapping
market_lookup_name <- character()
market_lookup_id <- character()

# Initialize empty vectors to store data
entrants <- character()
entrant_id <- character()
market_id <- character()
match_names <- character()
handicaps <- numeric()
prices <- numeric()

# Loop through the entrants
for (i in seq_along(event_json_list)) {
    match_name <- df$event_name[i]
    match <- event_json_list[[i]]
    
    for (entrant in match$entrants) {
        entrants <- c(entrants, entrant$name)
        entrant_id <- c(entrant_id, entrant$id)
        market_id <- c(market_id, entrant$market_id)
        match_names <- c(match_names, match_name)
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
market_df <- data.frame(match_name = match_names, market_id = market_id, entrants = entrants, price = prices, entrant_id = entrant_id)

# Merge market lookup dataframe with market dataframe
market_df <- merge(market_df, market_lookup_df, by = 'market_id', all.x = TRUE)

# Reorder columns in market_df
market_df <- market_df |> select(match_name, market_name, market_id, entrants, entrant_id, handicaps, price)

##%######################################################%##
#                                                          #
####               Get Head to Head Data                ####
#                                                          #
##%######################################################%##

# Filter to only include head to head markets
h2h_data <-
market_df |> 
    filter(market_name == "Match Betting") |> 
    select(-market_name)

# Home teams
home_teams <-
    h2h_data |> 
    separate(match_name, c("home_team", "away_team"), sep = " vs ", remove = FALSE) |> 
    filter(entrants == home_team) |> 
    select(match = match_name, home_team, home_win = price) |> 
    mutate(home_team = fix_team_names(home_team))

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
    left_join(away_teams, by = c("match")) |> 
    mutate(match = paste(home_team, "v", away_team, sep = " ")) |>
    mutate(margin = round(1 / home_win + 1 / away_win, digits = 2)) |>
    mutate(agency = "Neds") |>
    mutate(market_name = "Head To Head") |> 
    select(match, market_name, home_team, away_team, home_win, away_win, margin, agency)

##%######################################################%##
#                                                          #
####               Player Disposals Data                ####
#                                                          #
##%######################################################%##

# Filter to only include player disposals markets
player_disposals_data <-
market_df |> 
    filter(str_detect(market_name, "(Player Disposals O/U)|(To Have \\d+\\+ Disposals)")) |> 
  filter(str_detect(market_name, "(Duos)|(Trios)", negate = TRUE))
         
# Overs
disposals_overs <-
    player_disposals_data |>
    filter(str_detect(entrants, "Over") |
               str_detect(market_name, "To Have")) |>
    mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+")) - 0.5) |>
    mutate(handicap = coalesce(handicaps, handicap_1)) |>
    mutate(player_name_1 = str_extract(entrants, pattern <-
                                           ".*(?= \\()")) |>
    mutate(player_name_2 = str_extract(market_name, "(?<= \\- ).*")) |> 
    mutate(player_name = coalesce(player_name_1, player_name_2)) |>
    transmute(
        match = match_name,
        market_name = "Player Disposals",
        market_id,
        player_name,
        entrant_id,
        line = handicap,
        over_price = price,
        agency = "Neds"
    )

# Unders
disposals_unders <-
    player_disposals_data |>
    filter(str_detect(entrants, "Under")) |>
    mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+")) - 0.5) |>
    mutate(handicap = coalesce(handicaps, handicap_1)) |>
    mutate(player_name_1 = str_extract(entrants, pattern <-
                                           ".*(?= \\()")) |>
    mutate(player_name_2 = str_extract(market_name, "(?<= \\- ).*")) |>
    mutate(player_name = coalesce(player_name_1, player_name_2)) |>
    transmute(
        match = match_name,
        market_name = "Player Disposals",
        market_id,
        player_name,
        entrant_id,
        line = handicap,
        under_price = price,
        agency = "Neds"
    )

# Merge overs and unders
player_disposals_data <-
  disposals_overs |>
  full_join(disposals_unders,
            by = c("match", "player_name", "line", "agency", "market_name", "market_id", "entrant_id")) |>
  select(
    match,
    market_name,
    market_id,
    player_name,
    entrant_id,
    line,
    over_price,
    under_price,
    agency
  ) |>
  separate(
    match,
    into = c("home_team", "away_team"),
    sep = " vs ",
    remove = FALSE
  ) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team, sep = " ")) |>
  mutate(
    player_name = case_when(
      player_name == "PJ Washington" ~ "P.J. Washington",
      player_name == "Kelly Oubre" ~ "Kelly Oubre Jr.",
      player_name == "Derrick Jones" ~ "Derrick Jones Jr.",
      player_name == "Jabari Smith Jr" ~ "Jabari Smith Jr.",
      .default = player_name
    )
  ) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  rename(player_team = team_name) |>
  mutate(
    opposition_team = case_when(
      player_team == home_team ~ away_team,
      player_team == away_team ~ home_team
    )
  ) |>
  relocate(player_team, opposition_team, .after = player_name)

##%######################################################%##
#                                                          #
####               Player Goals Data                ####
#                                                          #
##%######################################################%##

# Filter to only include player goals markets
player_goals_data <-
  market_df |> 
  mutate(market_name = if_else(market_name == "Player Goals - Anytime Goal Kicker", "Player Goals - To Kick 1+ Goals", market_name)) |> 
  filter(str_detect(market_name, "(Player Goals O/U)|(To Kick)")) |> 
  filter(str_detect(market_name, "(Duos)|(Trios)", negate = TRUE)) |> 
  filter(str_detect(market_name, "(Quarter)|(Halves)", negate = TRUE))

# Overs
goals_overs <-
  player_goals_data |>
  filter(str_detect(entrants, "Over") |
           str_detect(market_name, "To Kick")) |>
  mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+")) - 0.5) |>
  mutate(handicap = coalesce(handicaps, handicap_1)) |>
  mutate(player_name_1 = str_extract(entrants, pattern <-
                                       ".*(?= \\()")) |>
  mutate(player_name_2 = str_extract(market_name, "(?<= \\- ).*")) |> 
  mutate(player_name = coalesce(player_name_1, player_name_2)) |>
  transmute(
    match = match_name,
    market_name = "Player Goals",
    market_id,
    player_name,
    entrant_id,
    line = handicap,
    over_price = price,
    agency = "Neds"
  )

# Unders
goals_unders <-
  player_goals_data |>
  filter(str_detect(entrants, "Under")) |>
  mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+")) - 0.5) |>
  mutate(handicap = coalesce(handicaps, handicap_1)) |>
  mutate(player_name_1 = str_extract(entrants, pattern <-
                                       ".*(?= \\()")) |>
  mutate(player_name_2 = str_extract(market_name, "(?<= \\- ).*")) |>
  mutate(player_name = coalesce(player_name_1, player_name_2)) |>
  transmute(
    match = match_name,
    market_name = "Player Goals",
    market_id,
    player_name,
    entrant_id,
    line = handicap,
    under_price = price,
    agency = "Neds"
  )

# Merge overs and unders
player_goals_data <-
  goals_overs |> 
  full_join(goals_unders, by = c("match", "player_name", "line", "agency", "market_name", "market_id", "entrant_id")) |>
  select(
    match,
    market_name,
    market_id,
    player_name,
    entrant_id,
    line,
    over_price,
    under_price,
    agency
  ) |> 
  separate(match, into = c("home_team", "away_team"), sep = " vs ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team, sep = " ")) |>
  mutate(player_name = case_when(player_name == "Harry Petty" ~ "Harrison Petty",
                                 player_name == "Tom Berry" ~ "Thomas Berry",
                                 .default = player_name)) |> 
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |> 
  rename(player_team = team_name) |>
  mutate(opposition_team = case_when(player_team == home_team ~ away_team,
                                     player_team == away_team ~ home_team)) |>
  relocate(player_team, opposition_team, .after = player_name)

##%######################################################%##
#                                                          #
####               Player Fantasy Data                ####
#                                                          #
##%######################################################%##

# Filter to only include player fantasy markets
player_fantasy_data <-
  market_df |> 
  filter(str_detect(market_name, "(Player Fantasy O/U)|(To Have \\d+\\+ AFL Fantasy Points)"))

# Overs
fantasy_overs <-
  player_fantasy_data |>
  filter(str_detect(entrants, "Over") |
           str_detect(market_name, "To Have")) |>
  mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+")) - 0.5) |>
  mutate(handicap = coalesce(handicaps, handicap_1)) |>
  mutate(player_name_1 = str_extract(entrants, pattern <-
                                       ".*(?= \\()")) |>
  mutate(player_name_2 = str_extract(market_name, "(?<= \\- ).*")) |> 
  mutate(player_name = coalesce(player_name_1, player_name_2)) |>
  transmute(
    match = match_name,
    market_name = "Player Fantasy",
    market_id,
    player_name,
    entrant_id,
    line = handicap,
    over_price = price,
    agency = "Neds"
  )

# Unders
fantasy_unders <-
  player_fantasy_data |>
  filter(str_detect(entrants, "Under")) |>
  mutate(handicap_1 = as.numeric(str_extract(market_name, "\\d+")) - 0.5) |>
  mutate(handicap = coalesce(handicaps, handicap_1)) |>
  mutate(player_name_1 = str_extract(entrants, pattern <-
                                       ".*(?= \\()")) |>
  mutate(player_name_2 = str_extract(market_name, "(?<= \\- ).*")) |>
  mutate(player_name = coalesce(player_name_1, player_name_2)) |>
  transmute(
    match = match_name,
    market_name = "Player Fantasy",
    market_id,
    player_name,
    entrant_id,
    line = handicap,
    under_price = price,
    agency = "Neds"
  )

# Merge overs and unders
player_fantasy_data <-
  fantasy_overs |>
  full_join(fantasy_unders,
            by = c("match", "player_name", "line", "agency", "market_name", "market_id", "entrant_id")) |>
  select(
    match,
    market_name,
    market_id,
    player_name,
    entrant_id,
    line,
    over_price,
    under_price,
    agency
  ) |>
  separate(
    match,
    into = c("home_team", "away_team"),
    sep = " vs ",
    remove = FALSE
  ) |>
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team, sep = " ")) |>
  mutate(
    player_name = case_when(
      player_name == "PJ Washington" ~ "P.J. Washington",
      player_name == "Kelly Oubre" ~ "Kelly Oubre Jr.",
      player_name == "Derrick Jones" ~ "Derrick Jones Jr.",
      player_name == "Jabari Smith Jr" ~ "Jabari Smith Jr.",
      .default = player_name
    )
  ) |>
  left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
  rename(player_team = team_name) |>
  mutate(
    opposition_team = case_when(
      player_team == home_team ~ away_team,
      player_team == away_team ~ home_team)) |>
  relocate(player_team, opposition_team, .after = player_name)

# Add event ID to each
player_fantasy_data <-
  player_fantasy_data |>
  left_join(event_ids_df, by = c("match")) |>
  relocate(event_id, .after = match)

player_disposals_data <-
  player_disposals_data |>
  left_join(event_ids_df, by = c("match")) |>
  relocate(event_id, .after = match)

player_goals_data <-
  player_goals_data |>
  left_join(event_ids_df, by = c("match")) |>
  relocate(event_id, .after = match)

##%######################################################%##
#                                                          #
####                  Write out as CSV                  ####
#                                                          #
##%######################################################%##

h2h_data |> write_csv("Data/scraped_odds/neds_h2h.csv")
player_disposals_data |> write_csv("Data/scraped_odds/neds_player_disposals.csv")
player_goals_data |> write_csv("Data/scraped_odds/neds_player_goals.csv")
player_fantasy_data |> write_csv("Data/scraped_odds/neds_player_fantasy_points.csv")
