library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)

# Dabble SGM-----------------------------------------------------------------
dabble_sgm <-
  read_csv("../../Data/scraped_odds/dabble_player_disposals.csv") |> 
  bind_rows(read_csv("../../Data/scraped_odds/dabble_player_goals.csv")) |> 
  bind_rows(read_csv("../../Data/scraped_odds/dabble_player_fantasy_points.csv")) |>
  rename(price = over_price) |> 
  distinct(match, player_name, line, market_name, agency, .keep_all = TRUE) |> 
  select(-contains("under")) |> 
  filter(price == 1.79)

#===============================================================================
# Function to get SGM Price
#===============================================================================

call_sgm_dabble <- function(data, player_names, stat_counts, markets) {
  if (length(player_names) != length(stat_counts)) {
    stop("Both lists should have the same length")
  }
  
  filtered_df <- data.frame()
  for (i in seq_along(player_names)) {
    temp_df <- data %>%
      filter(player_name == player_names[i],
             line == stat_counts[i],
             market_name == markets[i])
    filtered_df <- bind_rows(filtered_df, temp_df)
  }
  
  if (nrow(filtered_df) != length(player_names)) {
    return(NULL)
  }
  
  # Filter to only include markets with a price of 1.79
  filtered_df <- filtered_df |> 
    filter(price == 1.79)
  
  # If no markets are left after filtering, return NULL
  if (nrow(filtered_df) == 0) {
    return(NULL)
  }
  
  # Calculate adjusted price
  adjusted_price <- prod(filtered_df$price)
  
  # Unadjusted price is the same as adjusted price for Dabble
  unadjusted_price <- adjusted_price
  
  # Adjustment factor is 1
  adjustment_factor <- 1
  
  combined_list <- paste(player_names, stat_counts, sep = ": ")
  player_string <- paste(combined_list, collapse = ", ")
  market_string <- paste(markets, collapse = ", ")
  
  output_data <- data.frame(
    Selections = player_string,
    Markets = market_string,
    Unadjusted_Price = unadjusted_price,
    Adjusted_Price = adjusted_price,
    Adjustment_Factor = adjustment_factor,
    Agency = 'Dabble'
  )
  
  return(output_data)
  
}
