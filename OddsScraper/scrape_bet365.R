# Libraries
library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)

# Function to fix team names
source("Functions/fix_team_names.R")

#===============================================================================
# Use rvest to get main market information-------------------------------------#
#===============================================================================

get_head_to_head <- function() {

# Read scraped HTML from the BET365_HTML Folder
scraped_file <- list.files("Data/BET365_HTML", full.names = TRUE, pattern = "h2h")[[1]]

# Get Teams
bet365_teams <-
    read_html(scraped_file) |> 
    html_nodes(".src-ParticipantFixtureDetailsHigher_TeamWrapper ") |> 
    html_text()

# Get H2H Odds
bet365_h2h_odds <-
    read_html(scraped_file) |> 
    html_nodes(".src-ParticipantOddsOnly50_Odds") |> 
    html_text()

# Get Handicap
bet365_handicap <-
    read_html(scraped_file) |> 
    html_nodes(".src-ParticipantCenteredStacked50_Handicap") |> 
    html_text()

# Get Handicap Price
bet365_handicap_price <-
    read_html(scraped_file) |> 
    html_nodes(".src-ParticipantCenteredStacked50_Odds") |> 
    html_text()

# Remove empty strings
bet365_handicap_price <- bet365_handicap_price[!bet365_handicap_price == ""]

# Get Start Time
bet365_start_time <-
    read_html(scraped_file) |> 
    html_nodes(".sgl-MarketFixtureDetailsLabel") |>
    html_nodes(".rcl-MarketHeaderLabel-isdate, .src-ParticipantFixtureDetailsHigher_BookCloses ") |> 
    html_text()

# Get indices that contain a number surrounded by spaces
start_time_indices <- which(str_detect(bet365_start_time, " \\d+ "))

# Empty list
result_list <- list()

# Split into chunks from each index to the next and from the last to the end of the vector
for (i in 1:length(start_time_indices)) {
    if (i != length(start_time_indices)) {
        result = (bet365_start_time[start_time_indices[i]:start_time_indices[i + 1]])
        result = result[-length(result)]
    } else {
        result = (bet365_start_time[start_time_indices[i]:length(bet365_start_time)])
    }
    result_list[[i]] <- result
}

# Turn each vector into a tibble
start_dates <- map(result_list, ~ expand_grid(.x[1], .x[2:length(.x)])) |> bind_rows()
names(start_dates) <- c("start_date", "start_time")

#===============================================================================
# Create head to head table----------------------------------------------------#
#===============================================================================

# Get Home teams - Odd elements
home_teams <- bet365_teams[seq(1, length(bet365_teams), 2)]
home_odds <- bet365_h2h_odds[seq(1, length(bet365_h2h_odds), 2)]

home_h2h <- tibble(home_teams, home_odds) |>
    bind_cols(start_dates)

# Get Away teams - Even elements
away_teams <- bet365_teams[seq(2, length(bet365_teams), 2)]
away_odds <- bet365_h2h_odds[seq(2, length(bet365_h2h_odds), 2)]

away_h2h <- tibble(away_teams, away_odds) |>
    bind_cols(start_dates)

# Combine together into one table
bet365_h2h <-
    bind_cols(home_h2h, away_h2h) |>
    mutate(home_teams = fix_team_names(home_teams),
           away_teams = fix_team_names(away_teams)) |>
    transmute(match = paste(home_teams, away_teams, sep = " v "),
              start_date = `start_date...3`,
              start_time = `start_time...4`, 
              market_name = "Head To Head",
              home_team = home_teams,
              home_win = as.numeric(home_odds),
              away_team = away_teams,
              away_win = as.numeric(away_odds)) |>
    mutate(start_time = dmy_hm(paste(start_date, "2023", start_time))) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "Bet365") |> 
    select(-start_date)

# Write to csv
write_csv(bet365_h2h, "Data/scraped_odds/bet365_h2h.csv")

#===============================================================================
# Create Handicap table--------------------------------------------------------#
#===============================================================================

# Get Home teams - Odd elements
home_teams <- bet365_teams[seq(1, length(bet365_teams), 2)]
home_handicap <- bet365_handicap[seq(1, length(bet365_handicap), 2)]
home_handicap_price <- bet365_handicap_price[seq(1, length(bet365_handicap_price), 2)]

home_handicap <- tibble(home_teams, home_handicap, home_handicap_price) |>
    bind_cols(start_dates)

# Get Away teams - Even elements
away_teams <- bet365_teams[seq(2, length(bet365_teams), 2)]
away_handicap <- bet365_handicap[seq(2, length(bet365_handicap), 2)]
away_handicap_price <- bet365_handicap_price[seq(2, length(bet365_handicap_price), 2)]

away_handicap <- tibble(away_teams, away_handicap, away_handicap_price) |>
    bind_cols(start_dates)

# Combine together into one table
bet365_handicap <-
    bind_cols(home_handicap, away_handicap) |>
    mutate(home_teams = fix_team_names(home_teams),
           away_teams = fix_team_names(away_teams)) |>
    transmute(match = paste(home_teams, away_teams, sep = " v "),
              start_date = `start_date...4`,
              start_time = `start_time...5`, 
              market_name = "Line",
              home_team = home_teams,
              home_line = as.numeric(home_handicap),
              home_win = as.numeric(home_handicap_price),
              away_team = away_teams,
              away_line = as.numeric(away_handicap),
              away_win = as.numeric(away_handicap_price)) |>
    mutate(start_time = dmy_hm(paste(start_date, "2023", start_time))) |> 
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "Bet365") |> 
    select(-start_date)

# Write to csv
write_csv(bet365_handicap, "Data/scraped_odds/bet365_line.csv")
}

##%######################################################%##
#                                                          #
####                    Player Props                    ####
#                                                          #
##%######################################################%##
# 
# # Read scraped HTML from the BET365_HTML Folder
# scraped_files_player <- list.files("Data/BET365_HTML", full.names = TRUE, pattern = "player")
# 
# get_player_props <- function() {
# 
# # Main Function
# main <- function(scraped_file) {
# 
# # Get Markets
# bet365_player_markets <-
#     read_html(scraped_file) |> 
#     html_nodes(".gl-MarketGroup")
# 
# # Functions to get the information from each section----------------------------
# 
# #===============================================================================
# # Player Points----------------------------------------------------------------#
# #===============================================================================
# 
# # Function to get player points
# get_player_points <- function(html_market_node) {
#     # Market Name
#     market_name <-
#         html_market_node |> 
#         html_elements(".gl-MarketGroupButton_Text ") |> 
#         html_text()
#     
#     # Player Names
#     player_names <-
#         html_market_node |> 
#         html_elements(".srb-ParticipantLabelWithTeam_Name ") |> 
#         html_text()
#     
#     # Team Names
#     team_names <-
#         html_market_node |> 
#         html_elements(".srb-ParticipantLabelWithTeam_Team ") |> 
#         html_text()
#     
#     # Get column list
#     column_list <-
#         html_market_node |> 
#         html_elements(".gl-Market_General-columnheader")
#     
#     # Function to get odds and handicap from each column header-----------------
#     get_handicaps_and_odds <- function(column) {
#         # Name
#         col_name <-
#         column |>
#             html_elements(".gl-MarketColumnHeader ") |>
#             html_text()
#         
#         # Handicaps
#         col_handicaps <-
#         column |>
#             html_elements(".gl-ParticipantCenteredStacked_Handicap") |>
#             html_text()
#         
#         # Odds
#         col_odds <-
#         column |>
#             html_elements(".gl-ParticipantCenteredStacked_Odds") |>
#             html_text()
#         
#         # Combine into tibble
#         output <- tibble(col_handicaps, col_odds)
#         
#         # Append column name to tibble columns
#         names(output) <- paste(col_name, names(output), sep = "_")
#         
#         # Return tibble
#         return(output)
#     }
#     
#     # Get odds and handicaps for each column
#     handicaps_and_odds <-
#         map(column_list, get_handicaps_and_odds) |>
#         map(~ if (nrow(.) == 0) NULL else .) |>
#         bind_cols()
#     
#     # Combine into one tibble
#         tibble(market_name, team_names, player_names) |>
#         bind_cols(handicaps_and_odds)
# }
# 
# # Get player points node
# all_nodes_names <-  
#     bet365_player_markets |> 
#     html_elements(".gl-MarketGroupButton_Text ") |> 
#     html_text()
# 
# player_points_index <- which(str_detect(all_nodes_names, "^Player Points$"))
# 
# # Get player points data
# bet365_player_points <- get_player_points(bet365_player_markets[player_points_index])
# 
# # Tidy
# bet365_player_points <-
#     bet365_player_points |> 
#     filter(Over_col_handicaps == Under_col_handicaps) |> 
#     mutate(team_names = fix_team_names(team_names)) |>
#     transmute(player_name = player_names,
#               team_name = team_names,
#               market_name,
#               line = as.numeric(Over_col_handicaps),
#               over_price = as.numeric(Over_col_odds),
#               under_price = as.numeric(Under_col_odds)) |>
#     mutate(margin = round((1/over_price + 1/under_price), digits = 3)) |>
#     mutate(agency = "Bet365")
# 
# #===============================================================================
# # Player Points Milestones-----------------------------------------------------#
# #===============================================================================
# 
# # Function to get player points milestones
# get_player_points_milestones <- function(html_market_node) {
#     # Market Name
#     market_name <-
#         html_market_node |> 
#         html_elements(".gl-MarketGroupButton_Text ") |> 
#         html_text()
#     
#     # Player Names
#     player_names <-
#         html_market_node |> 
#         html_elements(".srb-ParticipantLabelWithTeam_Name ") |> 
#         html_text()
#     
#     # Team Names
#     team_names <-
#         html_market_node |> 
#         html_elements(".srb-ParticipantLabelWithTeam_Team ") |> 
#         html_text()
#     
#     # Get column list
#     column_list <-
#         html_market_node |> 
#         html_elements(".srb-HScrollPlaceColumnMarket")
#     
#     # Function to get odds and handicap from each column header-----------------
#     get_handicaps_and_odds <- function(column) {
#         # Name
#         col_name <-
#             column |>
#             html_elements(".srb-HScrollPlaceHeader ") |>
#             html_text()
#         
#         # Odds
#         col_odds <-
#             column |>
#             html_elements(".gl-ParticipantOddsOnly_Odds") |>
#             html_text()
#         
#         # Combine into tibble
#         output <- tibble(col_odds)
#         
#         # Append column name to tibble columns
#         names(output) <- paste(col_name, names(output), sep = "_")
#         
#         # Return tibble
#         return(output)
#     }
#     
#     # Get odds and handicaps for each column
#     handicaps_and_odds <-
#         map(column_list, get_handicaps_and_odds) |>
#         map(~ if (nrow(.) == 0) NULL else .) |>
#         bind_cols()
#     
#     # Combine into one tibble
#     tibble(market_name, team_names, player_names) |>
#         bind_cols(handicaps_and_odds)
# }
# 
# # Get player points milestones node
# player_points_index <- which(str_detect(all_nodes_names, "^Player Points Milestones$"))
# 
# # Get player points data
# bet365_player_points_milestones <- get_player_points_milestones(bet365_player_markets[player_points_index])
# 
# # Tidy
# bet365_player_points_milestones <-
#     bet365_player_points_milestones |> 
#     pivot_longer(cols = -c(market_name, team_names, player_names), names_to = "line", values_to = "price") |> 
#     mutate(line = str_extract(line, "\\d+")) |> 
#     filter(price != "") |> 
#     mutate(team_names = fix_team_names(team_names)) |>
#     transmute(player_name = player_names,
#               team_name = team_names,
#               market_name = "Player Points",
#               line = as.numeric(line),
#               over_price = as.numeric(price)) |> 
#     mutate(line = line - 0.5)
# 
# #===============================================================================
# # Get list of derived dataframes and return
# #===============================================================================
# 
# list(
#     "Player Points" = bet365_player_points,
#     "Player Points Milestones" = bet365_player_points_milestones,
#     "Player Assists" = bet365_player_assists,
#     "Player Assists Milestones" = bet365_player_assists_milestones,
#     "Player Rebounds" = bet365_player_rebounds,
#     "Player Rebounds Milestones" = bet365_player_rebounds_milestones,
#     "Player Threes Made" = bet365_player_threes,
#     "Player Points-Assists-Rebounds" = bet365_player_points_assists_rebounds
# )
# 
# }
# 
# ##%######################################################%##
# #                                                          #
# ####            Map to files and output data            ####
# #                                                          #
# ##%######################################################%##
# 
# # Get list of data
# list_of_scraped_data <- map(scraped_files_player, main)
# 
# # Map over list and get dataframes for player points
# player_points <- 
#     list_of_scraped_data |>
#     map(~.x[c("Player Points", "Player Points Milestones")]) |>
#     map_df(bind_rows) |> 
#     mutate(agency = "Bet365") |> 
#     left_join(next_match[, c("team", "opposition_team", "match")], by = c("team_name" = "team")) |> 
#     select(match, market_name, player_name, player_team = team_name, opposition_team, line, over_price, under_price, agency) |> 
#     separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE)
#     
# 
# # Map over list and get dataframes for player assists
# player_assists <- 
#     list_of_scraped_data |>
#     map(~.x[c("Player Assists", "Player Assists Milestones")]) |>
#     map_df(bind_rows) |> 
#     mutate(agency = "Bet365") |> 
#     left_join(next_match[, c("team", "opposition_team", "match")], by = c("team_name" = "team")) |> 
#     select(match, market_name, player_name, player_team = team_name, opposition_team, line, over_price, under_price, agency) |> 
#     separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE)
# 
# # Map over list and get dataframes for player rebounds
# player_rebounds <- 
#     list_of_scraped_data |>
#     map(~.x[c("Player Rebounds", "Player Rebounds Milestones")]) |>
#     map_df(bind_rows) |> 
#     mutate(agency = "Bet365") |> 
#     left_join(next_match[, c("team", "opposition_team", "match")], by = c("team_name" = "team")) |> 
#     select(match, market_name, player_name, player_team = team_name, opposition_team, line, over_price, under_price, agency) |> 
#     separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE)
# 
# # Write to CSV------------------------------------------------------------------
# 
# player_points |> write_csv("Data/scraped_odds/bet365_player_points.csv")
# player_assists |> write_csv("Data/scraped_odds/bet365_player_assists.csv")
# player_rebounds |> write_csv("Data/scraped_odds/bet365_player_rebounds.csv")
# }
# 
# Create safe version of functions-----------------------------------------------
get_head_to_head_safe <- safely(get_head_to_head, otherwise = NULL)
# get_player_props_safe <- safely(get_player_props, otherwise = NULL)
# 
# # Run functions-----------------------------------------------------------------
tryCatch(get_head_to_head(), error = function(e) print("Error in get_head_to_head()"))
# tryCatch(get_player_props(), error = function(e) print("Error in get_player_props()"))
