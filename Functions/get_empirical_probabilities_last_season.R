#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(zoo)
`%notin%` <- Negate(`%in%`)

is_integer_line <- function(line) {
  isTRUE(is.finite(line)) && abs(line - round(line)) < sqrt(.Machine$double.eps)
}

is_over_market_line <- function(values, line) {
  if (is_integer_line(line)) {
    values >= line
  } else {
    values > line
  }
}

#===============================================================================
# Read in past season stats
#===============================================================================

combined_stats <-
  read_rds("Data/afl_fantasy_2015_2025_data.rds") |> 
  filter(season_name == "2025")

#===============================================================================
# Create a function that takes a player name + line and returns their hit rate
#===============================================================================

get_empirical_prob_season <- function(player_full_name, line, stat) {
  
  # Use the latest season present in the data (not system year)
  last_season <-
    combined_stats |>
    pull(season_name) |>
    as.numeric() |>
    max(na.rm = TRUE) |>
    as.character()
  
  # Filter for player
  player_stats <-
    combined_stats |> 
    filter(season_name == last_season) |>
    filter(player_full_name == !!player_full_name) |> 
    arrange(desc(start_time_utc))
  
  # Ensure 'stat' column exists
  if(!stat %in% names(player_stats)) {
    stop("Stat column does not exist in the dataset")
  }
  
  # Calculate proportion of games above 'line' for season
  last_season_stats <-
    player_stats |> 
    mutate(
      above_line = as.numeric(is_over_market_line(.data[[stat]], line))) |> 
    summarise(
      n_games_2025 = n(),
      emp_prob_2025 = ifelse(n() == 0, NA_real_, mean(above_line, na.rm = TRUE))
    )
  
  return(last_season_stats)
}
