##%######################################################%##
#                                                          #
####          Scrape the afl player positions           ####
####              from the afl fantasy API              ####
#                                                          #
##%######################################################%##

# libraries and functions
library(tidyverse)
library(request)
library(openxlsx)
library(readxl)

# Get URL of data from API
url = "https://fantasy.afl.com.au/data/afl/players.json?_=1677977794603"

# Get Data from API
scraped_fantasy_data <-
  api(url) %>%
  http()

##%######################################################%##
#                                                          #
####     Create a function to extract relevant data     ####
#                                                          #
##%######################################################%##

get_afl_api_player_data <- function(player_data){
  # Name and price data
  first_name = player_data$first_name
  last_name = player_data$last_name
  team_id = player_data$squad_id
  price = player_data$cost
  projected_average = player_data$stats$proj_avg
  adp = player_data$stats$adp
  
  # Position Boolean Variables
  forward_status = 4 %in% player_data$positions
  ruck_status =  3 %in% player_data$positions
  midfield_status = 2 %in% player_data$positions
  defender_status = 1 %in% player_data$positions
  
  # Get url of player photo
  photo_url = paste("https://fantasy.afl.com.au/assets/media/players/afl/", player_data$id ,"_450.webp", sep = "")
  
  # Return Dataframe with each derived variable as a column
  tibble(
    first_name,
    last_name,
    team_id,
    price,
    projected_average,
    adp,
    forward_status,
    ruck_status,
    midfield_status,
    defender_status,
    photo_url
  )
}

##%######################################################%##
#                                                          #
####                  Get output data                   ####
#                                                          #
##%######################################################%##

afl_player_api_data <-
  scraped_fantasy_data |> 
  map(get_afl_api_player_data) |> 
  reduce(bind_rows) |>
  mutate(player_full_name = paste(first_name, last_name)) |> 
  mutate(team_name = case_when(
    team_id == 10 ~ "Adelaide Crows",
    team_id == 20 ~ "Brisbane Lions",
    team_id == 30 ~ "Carlton",
    team_id == 40 ~ "Collingwood Magpies",
    team_id == 50 ~ "Essendon Bombers",
    team_id == 60 ~ "Fremantle Dockers",
    team_id == 70 ~ "Geelong Cats",
    team_id == 1000 ~ "Gold Coast Suns",
    team_id == 1010 ~ "GWS Giants",
    team_id == 80 ~ "Hawthorn Hawks",
    team_id == 90 ~ "Melbourne Demons",
    team_id == 100 ~ "North Melbourne Kangaroos",
    team_id == 110 ~ "Port Adelaide Power",
    team_id == 120 ~ "Richmond Tigers",
    team_id == 130 ~ "St Kilda Saints",
    team_id == 160 ~ "Sydney Swans",
    team_id == 150 ~ "West Coast Eagles",
    team_id == 140 ~ "Western Bulldogs"
  )) |>
  relocate(player_full_name, team_name, .after = last_name)

# Write out as starting data for 2025 season
write_rds(afl_player_api_data, "Data/2025_start_positions_and_prices.rds")


afl_player_api_data$position <- apply(afl_player_api_data[, c("midfield_status", "defender_status", "ruck_status", "forward_status")], 1, function(row) {
  # Get the positions that are TRUE
  positions <- c()
  if(row["midfield_status"]) positions <- c(positions, "Mid")
  if(row["defender_status"]) positions <- c(positions, "Def")
  if(row["ruck_status"]) positions <- c(positions, "Ruc")
  if(row["forward_status"]) positions <- c(positions, "Fwd")
  
  # Combine them in the order specified
  paste(positions, collapse = "/")
})

# Define position hierarchy and their priority
position_priority <- c("forward_status", "defender_status", "ruck_status", "midfield_status")
position_names <- c("Forward", "Defender", "Ruck", "Midfield")

# Ensure position columns exist in the dataset
missing_columns <- setdiff(position_priority, names(afl_player_api_data))
if (length(missing_columns) > 0) stop("Error: Missing columns - ", paste(missing_columns, collapse = ", "))

# Convert position columns to numeric (if necessary)
afl_player_api_data <- afl_player_api_data %>%
  mutate(across(all_of(position_priority), ~as.numeric(.)))

# Function to determine the primary position
get_primary_position <- function(forward_status, defender_status, ruck_status, midfield_status) {
  positions <- c(forward_status, defender_status, ruck_status, midfield_status)
  position_index <- which(positions == 1)
  
  if (length(position_index) > 0) {
    return(position_names[position_index[1]])
  } else {
    return(NA_character_)
  }
}

# Assign primary position using pmap_chr() with explicit argument names
afl_player_api_data <- afl_player_api_data %>%
  mutate(Primary_Position = pmap_chr(select(., all_of(position_priority)), 
                                     ~get_primary_position(..1, ..2, ..3, ..4)))

# Remove players with no valid position
afl_player_api_data <- afl_player_api_data %>% filter(!is.na(Primary_Position))

# Split data by position
position_dfs <- split(afl_player_api_data, afl_player_api_data$Primary_Position)

# Write to an Excel file
output_file <- "players_by_position.xlsx"
wb <- createWorkbook()

for (pos in names(position_dfs)) {
  addWorksheet(wb, pos)
  writeData(wb, sheet = pos, position_dfs[[pos]])
}

saveWorkbook(wb, output_file, overwrite = TRUE)

cat("Excel file saved as", output_file)

# afl_player_api_data |> openxlsx::write.xlsx("players.xlsx")
