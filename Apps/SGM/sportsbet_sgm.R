library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(mongolite)
uri <- Sys.getenv("mongodb_connection_string")

# Sportsbet SGM-----------------------------------------------------------------
sbsgm_con <- mongo(collection = "Sportsbet-SGM", db = "Odds", url = uri)
sportsbet_sgm <- sbsgm_con$find('{}') |> tibble()

sportsbet_sgm <-
  rename(
    sportsbet_sgm,
    eventExternalId = match_id,
    competitionExternalId = comp_id,
    classExternalId = class_id,
    marketExternalId = market_id,
    outcomeExternalId = player_id
  )

#==============================================================================
# Function to get SGM data
#=-=============================================================================

get_sgm_sportsbet <- function(data, player_names, disposal_counts) {
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
  
  outcomes_list <- lapply(1:nrow(filtered_df), function(i) {
    list(marketExternalId = as.integer(filtered_df$marketExternalId[i]),
         outcomeExternalId = as.integer(filtered_df$outcomeExternalId[i]))
  })
  
  payload <- list(
    classExternalId = as.integer(filtered_df$classExternalId[1]),
    competitionExternalId = as.integer(filtered_df$competitionExternalId[1]),
    eventExternalId = as.integer(filtered_df$eventExternalId[1]),
    outcomesExternalIds = outcomes_list
  )
  
  return(payload)
}

#==============================================================================
# Make Post Request
#==============================================================================

call_sgm_sportsbet <- function(data, player_names, disposal_counts) {
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
  
  unadjusted_price <- prod(filtered_df$price)
  
  payload <- get_sgm_sportsbet(data, player_names, disposal_counts)
  
  url <- 'https://www.sportsbet.com.au/apigw/multi-pricer/combinations/price'
  
  headers <- c('User-Agent' = 'Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Mobile Safari/537.36',
               'Content-Type' = 'application/json;charset=UTF-8')
  
  response <- POST(url, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers), encode = "json")
  
  # Check if the request was successful
  if (http_error(response)) {
    stop("API request failed: ", content(response, "text"))
  }
  
  response_content <- content(response, "parsed")
  
  # Check if the response contains the expected data
  if (!"price" %in% names(response_content)) {
    stop("Unexpected API response: 'price' not found")
  }
  
  adjusted_price <- 1 + (response_content$price$numerator / response_content$price$denominator)
  adjustment_factor <- adjusted_price / unadjusted_price
  
  combined_list <- paste(player_names, disposal_counts, sep = ": ")
  player_string <- paste(combined_list, collapse = ", ")
  
  output_data <- data.frame(
    Selections = player_string,
    Unadjusted_Price = unadjusted_price,
    Adjusted_Price = adjusted_price,
    Adjustment_Factor = adjustment_factor,
    Agency = 'Sportsbet'
  )
  
  return(output_data)
}
