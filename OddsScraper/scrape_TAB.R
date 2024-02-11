# Libraries
library(tidyverse)
library(rvest)
library(httr2)

# URL to get responses
tab_url = "https://api.beta.tab.com.au/v1/recommendation-service/AFL%20Football/featured?jurisdiction=SA"

# Function to fix team names
source("Functions/fix_team_names.R")

main_tab <- function() {

# Make request and get response
tab_response <-
    request(tab_url) |>
    req_perform() |> 
    resp_body_json()

tab_response <- tab_response$competitions[[1]]

# Function to extract market info from response---------------------------------
get_market_info <- function(markets) {
    
    # Market info
    markets_name = markets$betOption
    market_propositions = markets$propositions
    
    # Output Tibble
    tibble(market = markets_name,
           propositions = market_propositions)
}

# Function to extract match info from response----------------------------------
get_match_info <- function(matches) {
    # Match info
    match_name = matches$name
    match_start_time = matches$startTime
    
    # Market info
    market_info = map(matches$markets, get_market_info) |> bind_rows()
    
    # Output Tibble
    tibble(
        match = match_name,
        start_time = match_start_time,
        market_name = market_info$market,
        propositions = market_info$propositions
    )
}

# Map functions to data
all_tab_markets <-
    map(tab_response$matches, get_match_info) |> bind_rows()

# Expand list col into multiple cols
all_tab_markets <-
all_tab_markets |>
    unnest_wider(col = propositions, names_sep = "_") |>
    select(any_of(c("match",
           "start_time",
           "market_name")),
           prop_name = propositions_name,
           prop_id = propositions_id,
           price = propositions_returnWin)

#===============================================================================
# Head to head markets
#===============================================================================

# Home teams
home_teams <-
    all_tab_markets |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    filter(market_name == "Head To Head") |> 
    group_by(match) |> 
    filter(row_number() == 1) |> 
    rename(home_win = price) |> 
    select(-prop_name, -prop_id)

# Away teams
away_teams <-
  all_tab_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(market_name == "Head To Head") |> 
  group_by(match) |> 
  filter(row_number() == 2) |> 
  rename(away_win = price) |> 
  select(-prop_name, -prop_id)

# Combine
tab_head_to_head_markets <-
    home_teams |>
    left_join(away_teams) |> 
    select(match, start_time, market_name, home_team, home_win, away_team, away_win) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "TAB")

# Fix team names
tab_head_to_head_markets <-
    tab_head_to_head_markets |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team))

# Write to csv
write_csv(tab_head_to_head_markets, "Data/scraped_odds/tab_h2h.csv")

#===============================================================================
# Line markets
#===============================================================================

# Home teams
home_teams <-
  all_tab_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(market_name == "Line") |> 
  group_by(match) |> 
  filter(row_number() == 1) |> 
  rename(home_win = price) |> 
  mutate(home_line = as.numeric(str_extract(prop_name, "-?\\d+\\.?\\d*"))) |>
  select(-prop_name, -prop_id)

# Away teams
away_teams <-
  all_tab_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(market_name == "Line") |> 
  group_by(match) |> 
  filter(row_number() == 2) |> 
  rename(away_win = price) |> 
  mutate(away_line = as.numeric(str_extract(prop_name, "-?\\d+\\.?\\d*"))) |>
  select(-prop_name, -prop_id)

# Combine
tab_line_markets <-
  home_teams |>
  left_join(away_teams) |> 
  select(match, start_time, market_name, home_team, home_line, home_win, away_team, away_line, away_win) |> 
  mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
  mutate(agency = "TAB")

# Fix team names
tab_line_markets <-
  tab_line_markets |> 
  mutate(home_team = fix_team_names(home_team)) |>
  mutate(away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team))

# Write to csv
write_csv(tab_line_markets, "Data/scraped_odds/tab_line.csv")

#===============================================================================
# Player Disposals
#===============================================================================

# Filter to player disposals markets
player_disposals_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "Player Disposals$"))

# Alternate Player Disposals
alternate_player_disposals_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "\\d+\\+ Disposals$"))

# Extract player names
player_disposals_markets <-
    player_disposals_markets |> 
    filter(str_detect(prop_name, "Over|Under")) |>
    mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
    mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
    mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
    mutate(line = as.numeric(line)) |>
    mutate(type = str_detect(prop_name, "Over|\\+")) |> 
    mutate(type = ifelse(type, "Over", "Under"))

alternate_player_disposals_markets <-
    alternate_player_disposals_markets |>
    mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
    mutate(line = str_extract(market_name, "\\d+")) |> 
    mutate(line = as.numeric(line) - 0.5) |> 
    transmute(match, market_name = "Player Disposals", player_name, line, over_price = price, prop_id)

# Over lines
over_lines <-
    player_disposals_markets |> 
    filter(type == "Over") |> 
    mutate(market_name = "Player Disposals") |>
    select(match, market_name, player_name, line, over_price = price, prop_id) |> 
    bind_rows(alternate_player_disposals_markets)

# Under lines
under_lines <-
    player_disposals_markets |> 
    filter(type == "Under") |> 
    mutate(market_name = "Player Disposals") |>
    select(match, market_name, player_name, line, under_price = price, under_prop_id = prop_id)

# Combine
tab_player_disposals_markets <-
    over_lines |>
    full_join(under_lines) |> 
    select(match, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
    mutate(agency = "TAB")

# Fix team names
tab_player_disposals_markets <-
    tab_player_disposals_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(match = paste(home_team, "v", away_team))

#===============================================================================
# Player Goals
#===============================================================================

# Filter to player goals markets
player_goals_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "Player Goals$"))

# Alternate Player Goals
alternate_player_goals_markets <-
    all_tab_markets |> 
    filter(str_detect(market_name, "\\d+\\+ Goals$"))

# Extract player names
player_goals_markets <-
    player_goals_markets |> 
    filter(str_detect(prop_name, "Over|Under")) |>
    mutate(player_name = str_extract(prop_name, "^.*(?=\\s(\\d+))")) |> 
    mutate(player_name = str_remove_all(player_name, "( Over)|( Under)")) |> 
    mutate(line = str_extract(prop_name, "[0-9\\.]{1,4}")) |> 
    mutate(line = as.numeric(line)) |>
    mutate(type = str_detect(prop_name, "Over|\\+")) |> 
    mutate(type = ifelse(type, "Over", "Under")) |> 
    mutate(line = if_else(market_name == "Alternate Player Goals", line - 0.5, line))

alternate_player_goals_markets <-
    alternate_player_goals_markets |>
    mutate(player_name = str_remove(prop_name, " \\(.*\\)")) |>
    mutate(line = str_extract(market_name, "\\d+")) |> 
    mutate(line = as.numeric(line) - 0.5) |> 
    transmute(match, market_name = "Player Goals", player_name, line, over_price = price, prop_id)

# Over lines
over_lines <-
    player_goals_markets |> 
    filter(type == "Over") |> 
    mutate(market_name = "Player Goals") |>
    select(match, market_name, player_name, line, over_price = price, prop_id) |> 
    bind_rows(alternate_player_goals_markets)

# Under lines
under_lines <-
    player_goals_markets |> 
    filter(type == "Under") |> 
    mutate(market_name = "Player Goals") |>
    select(match, market_name, player_name, line, under_price = price, under_prop_id = prop_id)

# Combine
tab_player_goals_markets <-
    over_lines |>
    full_join(under_lines) |> 
    select(match, market_name, player_name, line, over_price, under_price, prop_id, under_prop_id) |> 
    mutate(agency = "TAB")

# Fix team names
tab_player_goals_markets <-
    tab_player_goals_markets |> 
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(match = paste(home_team, "v", away_team))

#===============================================================================
# Write to CSV------------------------------------------------------------------
#===============================================================================

tab_player_disposals_markets |> write_csv("Data/scraped_odds/tab_player_disposals.csv")
tab_player_goals_markets |> write_csv("Data/scraped_odds/tab_player_goals.csv")
}

#===============================================================================
# Run safe function
#===============================================================================

safe_main_tab <- safely(main_tab)
safe_main_tab()
