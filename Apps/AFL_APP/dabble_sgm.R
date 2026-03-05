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


# Dabble SGM-----------------------------------------------------------------

# Helper function to read CSV and return empty tibble if 0 rows
safe_read <- function(file) {
  if (!file.exists(file)) return(tibble())
  df <- safe_read_csv(file)
  if (nrow(df) == 0) return(tibble()) else return(df)
}

dabble_sgm_raw <-
  safe_read("../../Data/scraped_odds/dabble_player_disposals.csv") |>
  bind_rows(safe_read("../../Data/scraped_odds/dabble_player_goals.csv")) |>
  bind_rows(safe_read("../../Data/scraped_odds/dabble_player_fantasy_points.csv")) |>
  bind_rows(safe_read("../../Data/scraped_odds/dabble_player_tackles.csv")) |>
  bind_rows(safe_read("../../Data/scraped_odds/dabble_player_marks.csv"))

if (nrow(dabble_sgm_raw) > 0 && "match" %in% names(dabble_sgm_raw)) {
  # Build Over/Under rows (no API adjustment used)
  dabble_over <- dabble_sgm_raw |>
    transmute(match, player_name, line, market_name, agency, type = "Over",
              price = over_price)

  dabble_under <- tibble()
  if ("under_price" %in% names(dabble_sgm_raw)) {
    dabble_under <- dabble_sgm_raw |>
      filter(!is.na(under_price)) |>
      transmute(match, player_name, line, market_name, agency, type = "Under",
                price = under_price)
  }

  dabble_sgm <- bind_rows(dabble_over, dabble_under) |>
    distinct(match, player_name, line, market_name, type, agency, .keep_all = TRUE)
} else {
  dabble_sgm <- tibble(
    match = character(),
    player_name = character(),
    line = numeric(),
    market_name = character(),
    agency = character(),
    type = character(),
    price = numeric()
  )
}

#===============================================================================
# Function to get SGM Price
#===============================================================================

call_sgm_dabble <- function(data, player_names, stat_counts, markets, types) {
  if (length(player_names) != length(stat_counts)) {
    stop("Both lists should have the same length")
  }

  filtered_df <- data.frame()
  for (i in seq_along(player_names)) {
    temp_df <- data %>%
      filter(player_name == player_names[i],
             line == stat_counts[i],
             market_name == markets[i],
             type == types[i])
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
