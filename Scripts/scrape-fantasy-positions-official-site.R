##%######################################################%##
#                                                          #
####          Scrape the afl player positions           ####
####              from the afl fantasy API              ####
#                                                          #
##%######################################################%##

# libraries and functions
library(tidyverse)
library(request)

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
  relocate(player_full_name, .after = last_name)


# Write out as starting data for 2024 season
write_rds(afl_player_api_data, "Data/2024_start_positions_and_prices.rds")
