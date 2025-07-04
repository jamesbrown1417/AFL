library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)
library(R.utils)

# TAB SGM-----------------------------------------------------------------------
tab_sgm_list <-
  list(
  read_csv("../../Data/scraped_odds/tab_player_disposals.csv"),
  read_csv("../../Data/scraped_odds/tab_player_goals.csv"),
  read_csv("../../Data/scraped_odds/tab_player_tackles.csv"),
  read_csv("../../Data/scraped_odds/tab_player_marks.csv")
)

tab_sgm <-
  tab_sgm_list |> 
  keep(~nrow(.x) > 0) |>
  bind_rows() |>
  rename(price = over_price) |>  
  distinct(match, player_name, line, market_name, agency, .keep_all = TRUE) |>
  select(-contains("under"))

#==============================================================================
# Function to get SGM data
#===============================================================================

# Function to get SGM data
get_sgm_tab <- function(data, player_names, stat_counts, markets) {
  if (length(player_names) != length(stat_counts)) {
    stop("Both lists should have the same length")
  }
  
  filtered_df <- data.frame()
  for (i in seq_along(player_names)) {
    temp_df <- data %>% 
      filter(player_name == player_names[i] &
               line == stat_counts[i] &
               market_name == markets[i])
    filtered_df <- bind_rows(filtered_df, temp_df)
  }
  
  # Get the 'id' column as a list
  id_list <- filtered_df$prop_id
  
  # Create the propositions list using the id_list
  propositions <- lapply(id_list, function(id) list(type = unbox("WIN"), propositionId = unbox(id)))
  
  return(propositions)
}

#==============================================================================
# Make Post Request
#==============================================================================

# Make Post Request
call_sgm_tab <- function(data, player_names, stat_counts, markets) {
  tryCatch({
    if (length(player_names) != length(stat_counts)) {
      stop("Both lists should have the same length")
    }
    
    filtered_df <- data.frame()
    for (i in seq_along(player_names)) {
      temp_df <- data %>% 
        filter(player_name == player_names[i] &
                 line == stat_counts[i] &
                 market_name == markets[i])
      filtered_df <- bind_rows(filtered_df, temp_df)
    }
    
    if (nrow(filtered_df) != length(player_names)) {
      return(NULL)
    }
    
    # Unadjusted price
    unadjusted_price <- prod(filtered_df$price)
    
    # Get propositions
    propositions <- get_sgm_tab(data, player_names, stat_counts, markets)
    
    url <- "https://api.beta.tab.com.au/v1/pricing-service/enquiry"
    
    headers <- c(
      "Accept" = "application/json, text/plain, */*",
      "Accept-Encoding" = "gzip, deflate, br, zstd",
      "Accept-Language" = "en-US,en;q=0.9",
      "Content-Type" = "application/json;charset=UTF-8",
      "Origin" = "https://www.tab.com.au",
      "Referer" = "https://www.tab.com.au/",
      "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/136.0.0.0 Safari/537.36",
      "sec-ch-ua" = '"Chromium";v="136", "Google Chrome";v="136", "Not.A/Brand";v="99"',
      "sec-ch-ua-mobile" = "?0",
      "sec-ch-ua-platform" = '"macOS"',
      "Cookie" = "YOUR_COOKIE_STRING_HERE"  # You'll need to add the full cookie string
    )
    
    payload <- list(
      clientDetails = list(jurisdiction = unbox("SA"), channel = unbox("web")),
      bets = list(
        list(
          type = unbox("FIXED_ODDS"),
          legs = list(
            list(
              type = unbox("SAME_GAME_MULTI"),
              propositions = propositions
            )
          )
        )
      ),
      returnValidationMatrix = unbox(TRUE)  # Added this line
    )
    
    # Try response, if nothing in 3 seconds, make it null
    response <- tryCatch({
      POST(url, 
           body = toJSON(payload), 
           add_headers(.headers = headers), 
           encode = "json", 
           timeout(5),
           config = config(http_version = 1.1))  # Force HTTP/1.1
    }, error = function(e) {
      return(NULL)
    })
    
    if(is.null(response)) {
      return(data.frame(
        Selections = NA_character_,
        Markets = NA_character_,
        Unadjusted_Price = NA_real_,
        Adjusted_Price = NA_real_,
        Adjustment_Factor = NA_real_,
        Agency = NA_character_
      ))
    }
    
    response_content <- content(response, "parsed")
    adjusted_price <- as.numeric(response_content$bets[[1]]$legs[[1]]$odds$decimal)
    adjustment_factor <- adjusted_price / unadjusted_price
    combined_list <- paste(player_names, stat_counts, sep = ": ")
    market_string <- paste(markets, collapse = ", ")
    player_string <- paste(combined_list, collapse = ", ")
    
    output_data <- tryCatch({
      data.frame(
        Selections = player_string,
        Markets = market_string,
        Unadjusted_Price = unadjusted_price,
        Adjusted_Price = adjusted_price,
        Adjustment_Factor = adjustment_factor,
        Agency = 'TAB'
      )
    }, error = function(e) {
      data.frame(
        Selections = NA_character_,
        Markets = NA_character_,
        Unadjusted_Price = NA_real_,
        Adjusted_Price = NA_real_,
        Adjustment_Factor = NA_real_,
        Agency = NA_character_
      )
    })
    
    return(output_data)
    
  }, error = function(e) {
    print(paste("Error: ", e))
  })
}

# call_sgm_tab(
#   data = tab_sgm,
#   player_names = c("Zach Guthrie", "Gryan Miers"),
#   stat_counts = c(14.5, 19.5),
#   markets = c("Player Disposals", "Player Disposals")
# )