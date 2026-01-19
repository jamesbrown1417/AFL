
# Function to get all market combinations for 2 or 3 players from the same match
get_player_combos <- function(player_data, selected_players, market_filter = NULL) {

  num_players <- length(selected_players)
  if (num_players < 2 || num_players > 3) {
    return(data.frame(Message = "Please select 2 or 3 players."))
  }

  # Optional: filter by market
  if (!is.null(market_filter) && market_filter != "All") {
    player_data <- player_data |>
      filter(market_name == market_filter)
  }

  # Get all agencies that have markets for all selected players in the given match
  agencies_with_all_players <- player_data |>
    filter(player_name %in% selected_players) |>
    group_by(agency) |>
    filter(n_distinct(player_name) == num_players) |>
    distinct(agency) |>
    pull(agency)

  # Function to get combos for a single agency
  get_combos_by_agency <- function(agency_name, data, current_players) {
    agency_data <- data |> filter(agency == agency_name)

    # Get markets for each player
    player_markets_list <- lapply(current_players, function(p) {
      agency_data |> filter(player_name == p)
    })

    # If any player has no markets for this agency, return NULL
    if (any(sapply(player_markets_list, nrow) == 0)) {
        return(NULL)
    }

    # Create a list of row indices for expand.grid
    grid_indices <- lapply(player_markets_list, function(df) 1:nrow(df))
    names(grid_indices) <- paste0("p", 1:length(current_players), "_row")

    # Create all combinations of markets using row indices
    all_combos_indices <- do.call(expand.grid, grid_indices)

    # If no combos, return NULL
    if (nrow(all_combos_indices) == 0) {
        return(NULL)
    }

    # Calculate prices and create selection strings for each combo
    selection_details <- lapply(1:nrow(all_combos_indices), function(i) {
      combo_row <- all_combos_indices[i, ]
      prices <- c()
      selections <- c()

      for (j in 1:length(current_players)) {
        player_market_data <- player_markets_list[[j]]
        market_row_index <- combo_row[[j]]
        
        prices <- c(prices, player_market_data$price[market_row_index])
        selections <- c(selections,
          paste(
            current_players[j], "-",
            player_market_data$market_name[market_row_index],
            player_market_data$line[market_row_index]
          )
        )
      }
      
      data.frame(
        Selections = paste(selections, collapse = ", "),
        Multi_Price = round(prod(prices), 2)
      )
    })

    combos_with_data <- bind_rows(selection_details)
    combos_with_data$Agency <- agency_name
    combos_with_data$Match <- player_markets_list[[1]]$match[1] # Get match from first player

    # Reorder columns
    combos_with_data <- combos_with_data |>
        select(Match, Agency, Selections, Multi_Price)

    return(combos_with_data)
  }

  # Map over the agencies to get all combinations
  all_agency_combos <- map_dfr(agencies_with_all_players, get_combos_by_agency, data = player_data, current_players = selected_players)

  # If no combos, return a message
  if (nrow(all_agency_combos) == 0) {
    return(data.frame(Message = "No combinations found for the selected players and market."))
  }

  # Pivot to wide format, resolving duplicates by taking the max price
  wide_combos <- all_agency_combos |>
    pivot_wider(
      names_from = Agency,
      values_from = Multi_Price,
      id_cols = c(Match, Selections),
      values_fn = max, # Use max to resolve duplicates
      values_fill = NA # Still fill with NA if no value
    )

  return(wide_combos)
}