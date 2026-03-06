library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)
library(R.utils)

tab_bootstrap_url <- "https://www.tab.com.au/"
tab_pricing_url <- "https://api.beta.tab.com.au/v1/pricing-service/enquiry"
tab_client_jurisdiction <- Sys.getenv("TAB_CLIENT_JURISDICTION", unset = "NSW")
tab_user_agent <- paste(
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/136.0.0.0 Safari/537.36"
)

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


# TAB SGM-----------------------------------------------------------------------
tab_sgm_list <-
  list(
  safe_read_csv("../../Data/scraped_odds/tab_player_disposals.csv"),
  safe_read_csv("../../Data/scraped_odds/tab_player_goals.csv"),
  safe_read_csv("../../Data/scraped_odds/tab_player_tackles.csv"),
  safe_read_csv("../../Data/scraped_odds/tab_player_marks.csv")
)

tab_sgm_raw <-
  tab_sgm_list |>
  keep(~nrow(.x) > 0) |>
  bind_rows()

if (nrow(tab_sgm_raw) > 0 && "match" %in% names(tab_sgm_raw)) {
  # Build Over/Under rows with appropriate proposition IDs and price
  tab_over <- tab_sgm_raw |>
    transmute(match = .data$match,
              player_name = .data$player_name,
              line = .data$line,
              market_name = .data$market_name,
              agency = .data$agency,
              type = "Over",
              price = .data$over_price,
              prop_id_sgm = .data$prop_id)

  tab_under <- tibble()
  if ("under_price" %in% names(tab_sgm_raw)) {
    tab_under <- tab_sgm_raw |>
      filter(!is.na(under_price) | !is.na(under_prop_id)) |>
      transmute(match = .data$match,
                player_name = .data$player_name,
                line = .data$line,
                market_name = .data$market_name,
                agency = .data$agency,
                type = "Under",
                price = .data$under_price,
                prop_id_sgm = .data$under_prop_id)
  }

  tab_sgm <- bind_rows(tab_over, tab_under) |>
    distinct(match, player_name, line, market_name, type, agency, .keep_all = TRUE)
} else {
  tab_sgm <- tibble(
    match = character(),
    player_name = character(),
    line = numeric(),
    market_name = character(),
    agency = character(),
    type = character(),
    price = numeric(),
    prop_id_sgm = character()
  )
}

#==============================================================================
# Function to get SGM data
#===============================================================================

# Function to get SGM data
get_sgm_tab <- function(data, player_names, stat_counts, markets, types) {
  if (length(player_names) != length(stat_counts)) {
    stop("Both lists should have the same length")
  }

  filtered_df <- data.frame()
  for (i in seq_along(player_names)) {
    temp_df <- data %>%
      filter(player_name == player_names[i] &
               line == stat_counts[i] &
               market_name == markets[i] &
               type == types[i])
    filtered_df <- bind_rows(filtered_df, temp_df)
  }

  # Get the proposition ID column as a list
  id_list <- filtered_df$prop_id_sgm

  # Create the propositions list using the id_list
  propositions <- lapply(id_list, function(id) list(type = unbox("WIN"), propositionId = unbox(id)))

  return(propositions)
}

submit_tab_pricing_request <- function(payload_json) {
  if (!requireNamespace("curl", quietly = TRUE)) {
    return(NULL)
  }

  handle <- curl::new_handle()
  curl::handle_setopt(handle, followlocation = TRUE, timeout = 15)
  curl::handle_setheaders(handle, "user-agent" = tab_user_agent)

  bootstrap_response <- tryCatch(
    curl::curl_fetch_memory(tab_bootstrap_url, handle = handle),
    error = function(e) NULL
  )

  if (is.null(bootstrap_response) || bootstrap_response$status_code >= 400) {
    return(NULL)
  }

  curl::handle_reset(handle)
  curl::handle_setheaders(
    handle,
    "accept" = "application/json, text/plain, */*",
    "accept-language" = "en-US,en;q=0.9",
    "content-type" = "application/json;charset=UTF-8",
    "origin" = "https://www.tab.com.au",
    "referer" = "https://www.tab.com.au/",
    "user-agent" = tab_user_agent
  )
  curl::handle_setopt(handle, timeout = 15, postfields = payload_json, customrequest = "POST")

  pricing_response <- tryCatch(
    curl::curl_fetch_memory(tab_pricing_url, handle = handle),
    error = function(e) NULL
  )

  if (is.null(pricing_response) || pricing_response$status_code >= 400) {
    return(NULL)
  }

  tryCatch(
    fromJSON(rawToChar(pricing_response$content), simplifyVector = FALSE),
    error = function(e) NULL
  )
}

#==============================================================================
# Make Post Request
#==============================================================================

# Make Post Request
call_sgm_tab <- function(data, player_names, stat_counts, markets, types) {
  tryCatch({
    if (length(player_names) != length(stat_counts)) {
      stop("Both lists should have the same length")
    }

    filtered_df <- data.frame()
    for (i in seq_along(player_names)) {
      temp_df <- data %>%
        filter(player_name == player_names[i] &
                 line == stat_counts[i] &
                 market_name == markets[i] &
                 type == types[i])
      filtered_df <- bind_rows(filtered_df, temp_df)
    }

    if (nrow(filtered_df) != length(player_names)) {
      return(NULL)
    }

    # Unadjusted price
    unadjusted_price <- prod(filtered_df$price)

    # Get propositions
    propositions <- get_sgm_tab(data, player_names, stat_counts, markets, types)

    payload <- list(
      clientDetails = list(
        jurisdiction = unbox(tab_client_jurisdiction),
        channel = unbox("web")
      ),
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
      returnValidationMatrix = unbox(TRUE)
    )

    response_content <- submit_tab_pricing_request(
      toJSON(payload, auto_unbox = TRUE)
    )

    if (is.null(response_content)) {
      return(NULL)
    }

    bet_status <- purrr::pluck(response_content, "bets", 1, "status", .default = NA_character_)
    adjusted_price <- as.numeric(
      purrr::pluck(
        response_content,
        "bets", 1, "legs", 1, "odds", "decimal",
        .default = NA_character_
      )
    )

    if (!identical(bet_status, "ok") || is.na(adjusted_price)) {
      return(NULL)
    }

    adjustment_factor <- adjusted_price / unadjusted_price
    combined_list <- paste(player_names, stat_counts, sep = ": ")
    market_string <- paste(markets, collapse = ", ")
    player_string <- paste(combined_list, collapse = ", ")

    output_data <- data.frame(
      Selections = player_string,
      Markets = market_string,
      Unadjusted_Price = unadjusted_price,
      Adjusted_Price = adjusted_price,
      Adjustment_Factor = adjustment_factor,
      Agency = "TAB"
    )

    return(output_data)

  }, error = function(e) {
    return(NULL)
  })
}

call_sgm_tab(
  data = tab_sgm,
  player_names = c("Zach Guthrie", "Gryan Miers"),
  stat_counts = c(14.5, 19.5),
  markets = c("Player Disposals", "Player Disposals"),
  types = c("Over", "Over")
)
