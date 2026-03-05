library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)

# Safe function to read CSV files
safe_read_csv <- function(path, ...)  {
  if (file.exists(path)) {
    tryCatch({
      df <- readr::read_csv(path, show_col_types = FALSE)
      if (nrow(df) > 0) return(df)
    }, error = function(e) {})
  }
  return(tibble::tibble(match=character(), player_name=character(), line=numeric(), market_name=character(), agency=character(), over_price=numeric()))
}


# BetRight SGM------------------------------------------------------------------
betright_sgm_list <- list(
  safe_read_csv("../../Data/scraped_odds/betright_player_disposals.csv"),
  safe_read_csv("../../Data/scraped_odds/betright_player_goals.csv"),
  safe_read_csv("../../Data/scraped_odds/betright_player_tackles.csv"),
  safe_read_csv("../../Data/scraped_odds/betright_player_marks.csv"),
  safe_read_csv("../../Data/scraped_odds/betright_player_fantasy_points.csv")
)

betright_sgm_raw <-
  betright_sgm_list |>
  keep(~nrow(.x) > 0) |>
  bind_rows()

# Build Over/Under rows (BetRight CSVs may not always include Unders)
if (nrow(betright_sgm_raw) > 0 && "match" %in% names(betright_sgm_raw)) {
  betright_over <- betright_sgm_raw |>
    filter(!is.na(over_price)) |>
    transmute(match = .data$match,
              player_name = .data$player_name,
              line = .data$line,
              market_name = .data$market_name,
              agency = .data$agency,
              type = "Over",
              price = .data$over_price,
              group_by_header = .data$group_by_header,
              event_id = .data$event_id,
              outcome_name = .data$outcome_name,
              outcome_id = .data$outcome_id,
              fixed_market_id = .data$fixed_market_id)

  betright_under <- tibble()
  if (all(c("under_price", "outcome_name_under", "outcome_id_under", "fixed_market_id_under") %in% names(betright_sgm_raw))) {
    betright_under <- betright_sgm_raw |>
      filter(!is.na(under_price)) |>
      transmute(match = .data$match,
                player_name = .data$player_name,
                line = .data$line,
                market_name = .data$market_name,
                agency = .data$agency,
                type = "Under",
                price = .data$under_price,
                group_by_header = .data$group_by_header,
                event_id = .data$event_id,
                outcome_name = .data$outcome_name_under,
                outcome_id = .data$outcome_id_under,
                fixed_market_id = .data$fixed_market_id_under)
  }

  betright_sgm <- bind_rows(betright_over, betright_under) |>
    distinct(match, player_name, line, market_name, type, agency, .keep_all = TRUE)
} else {
  betright_sgm <- tibble(
    match = character(),
    player_name = character(),
    line = numeric(),
    market_name = character(),
    agency = character(),
    type = character(),
    price = numeric(),
    group_by_header = character(),
    event_id = character(),
    outcome_name = character(),
    outcome_id = character(),
    fixed_market_id = character()
  )
}

#===============================================================================
# Function to get SGM data
#===============================================================================

# Function to get SGM data
get_sgm_betright <- function(data, player_names, stat_counts, markets, types) {

  if (length(player_names) != length(stat_counts)) {
    stop("Both lists should have the same length")
  }

  filtered_df <- data.frame()
  for (i in 1:length(player_names)) {
    temp_df <- data[data$player_name == player_names[i] &
                      data$line == stat_counts[i] &
                      data$market_name == markets[i] &
                      data$type == types[i], ]
    if (nrow(temp_df) == 0) {
      stop(paste("No data found for", player_names[i], "with", stat_counts[i], markets[i], types[i], "."))
    }
    filtered_df <- rbind(filtered_df, temp_df)
  }

  header <- filtered_df$group_by_header
  event_id <- filtered_df$event_id
  outcome_name <- filtered_df$outcome_name
  outcome_id <- filtered_df$outcome_id
  fixed_market_id <- filtered_df$fixed_market_id
  points <- "0"
  fixed_win <- filtered_df$price

  payload <- lapply(1:length(player_names), function(i) {
    list(
      eventId = unlist(event_id[i]),
      outcomeId = unlist(outcome_id[i]),
      marketType = "WIN",
      fixedWin = unlist(fixed_win[i]),
      fixedMarketId = unlist(fixed_market_id[i]),
      marketTypeDesc = "Win",
      groupByHeader = header[i],
      points = points,
      outcomeName = outcome_name[i]
    )
  })

  return(payload)
}


#==============================================================================
# Make Post Request
#==============================================================================

# Make POST request
call_sgm_betright <- function(data, player_names, stat_counts, markets, types) {
  if (length(player_names) != length(stat_counts)) {
    stop("Both lists should have the same length")
  }

  filtered_df <- data.frame()
  for (i in 1:length(player_names)) {
    temp_df <- data[data$player_name == player_names[i] &
                      data$line == stat_counts[i] &
                      data$market_name == markets[i] &
                      data$type == types[i], ]
    if (nrow(temp_df) == 0) {
      stop(paste("No data found for", player_names[i], "with", stat_counts[i], markets[i], types[i], "."))
    }
    filtered_df <- rbind(filtered_df, temp_df)
  }

  if (nrow(filtered_df) != length(player_names)) {
    return(NULL)
  }

  unadjusted_price <- prod(filtered_df$price)

  payload <- get_sgm_betright(data, player_names, stat_counts, markets, types)

  url <- "https://sgm-api.betright.com.au/Pricing/SgmPrice?"

  headers <- add_headers('User-Agent' = 'Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Mobile Safari/537.36',
                         'Content-Type' = 'application/json;charset=UTF-8',
                         'Origin' = 'https://betright.com.au',
                         'Referer' = 'https://betright.com.au/')

  response <- POST(url, headers, body = toJSON(payload, auto_unbox = TRUE))

  if (http_error(response)) {
    stop("HTTP error occurred while calling API.")
  }

  response_content <- fromJSON(content(response, "text"))

  if (!"price" %in% names(response_content)) {
    stop("No price information found in the API response.")
  }

  adjusted_price <- as.numeric(response_content$price)
  adjustment_factor <- adjusted_price / unadjusted_price
  player_string <- paste(paste(player_names, stat_counts, sep = ": "), collapse = ", ")
  market_string <- paste(markets, collapse = ", ")

  output_data <- data.frame(
    Selections = player_string,
    Markets = market_string,
    Unadjusted_Price = unadjusted_price,
    Adjusted_Price = adjusted_price,
    Adjustment_Factor = adjustment_factor,
    Agency = 'Betright'
  )

  return(output_data)
}

# call_sgm_betright(
#   data = betright_sgm,
#   player_names = c("James Jordon", "Joe Daniher"),
#   stat_counts = c(14.5, 14.5),
#   markets = c("Player Disposals", "Player Disposals"),
#   types = c("Over", "Over")
# )
