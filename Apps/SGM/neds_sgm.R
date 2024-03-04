library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)


# Neds SGM-----------------------------------------------------------------
neds_sgm <-
  read_csv("../../Data/scraped_odds/neds_player_disposals.csv") |> 
  rename(price = over_price,
         number_of_disposals = line) |> 
  select(-contains("under"))

#===============================================================================
# Function to get API URL
#===============================================================================

create_api_url <- function(event_id, market_ids, entrant_ids) {
  # Base URL for the API endpoint
  base_url <- "https://api.neds.com.au/v2/same-game-multi/get-odds"
  
  # Start constructing the `same_game_multies` parameter
  same_game_multies <- sprintf('{"%s":{"event_id":"%s","selections":[', event_id, event_id)
  
  # Add each market and entrant pair to the selections
  selections <- vector("list", length = length(market_ids))
  for (i in seq_along(market_ids)) {
    selections[[i]] <- sprintf('{"market_id":"%s","entrant_id":"%s"}', market_ids[i], entrant_ids[i])
  }
  
  # Combine selections into a single string
  selections_str <- paste(selections, collapse = ",")
  
  # Complete the construction of the `same_game_multies` parameter
  same_game_multies <- paste0(same_game_multies, selections_str, "]}}")
  
  # URL-encode the `same_game_multies` parameter
  encoded_same_game_multies <- URLencode(same_game_multies)
  
  # Construct the full URL
  full_url <- sprintf('%s?same_game_multies=%s', base_url, encoded_same_game_multies)
  
  return(full_url)
}

#===============================================================================
# Function to get SGM Price
#===============================================================================

call_sgm_neds <- function(data, player_names, disposal_counts) {
  if (length(player_names) != length(disposal_counts)) {
    stop("Both lists should have the same length")
  }
  
  filtered_df <- data.frame()
  for (i in seq_along(player_names)) {
    temp_df <- data %>%
      filter(player_name == player_names[i],
             number_of_disposals == disposal_counts[i])
    filtered_df <- bind_rows(filtered_df, temp_df)
  }
  
  # Use Filtered DF to get the API URL
  event_id <- filtered_df$event_id[1]
  market_ids <- filtered_df$market_id
  entrant_ids <- filtered_df$entrant_id
  
  neds_sgm_url <- create_api_url(event_id, market_ids, entrant_ids)
  
  # Call the API
  response <-
    httr::GET(neds_sgm_url,httr::add_headers(`Content-Type` = "application/json")) %>%
    httr::content("parsed")
  
  denominator <- response$prices[[1]]$odds$denominator
  numerator <- response$prices[[1]]$odds$numerator
  
  adjusted_price = (numerator / denominator) + 1
  
  unadjusted_price <- prod(filtered_df$price)
  
  adjustment_factor <- adjusted_price / unadjusted_price
  
  combined_list <- paste(player_names, disposal_counts, sep = ": ")
  player_string <- paste(combined_list, collapse = ", ")
  
  output_data <- data.frame(
    Selections = player_string,
    Unadjusted_Price = unadjusted_price,
    Adjusted_Price = adjusted_price,
    Adjustment_Factor = adjustment_factor,
    Agency = 'Neds'
  )
  
  return(output_data)
  
}

# call_sgm_neds(
#   data = neds_sgm,
#   player_names = c("Nick Blakey", "Angus Sheldrick"),
#   disposal_counts = c("14.5", "14.5")
# )
