#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
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
  read_rds("Data/afl_fantasy_2015_2025_data.rds")

current_season_stats <- read_rds("Data/afl_fantasy_2026_data.rds")

combined_stats <- bind_rows(combined_stats, current_season_stats)

#===============================================================================
# Create a function that takes a player name + line and returns their hit rate
#===============================================================================

get_empirical_prob <- function(player_full_name, line, stat) {

  # Filter for player
  player_stats <-
    combined_stats |> 
    filter(player_full_name == !!player_full_name) |>
    mutate(
      game_date = suppressWarnings(ymd_hms(start_time_utc, quiet = TRUE, tz = "UTC")),
      game_date = coalesce(game_date, as.POSIXct(start_time_utc, tz = "UTC"))
    ) |>
    arrange(desc(game_date), desc(start_time_utc))
  
  # Ensure 'stat' column exists
  if(!stat %in% names(player_stats)) {
    stop("Stat column does not exist in the dataset")
  }

  # If player has no historical rows, return nothing
  if (nrow(player_stats) == 0) {
    return(
      tibble(
        player_full_name = character(),
        line = numeric(),
        emp_prob_last_3 = numeric(),
        emp_prob_last_5 = numeric(),
        emp_prob_last_7 = numeric(),
        emp_prob_last_10 = numeric()
      )
    )
  }

  calc_emp_prob <- function(n_games) {
    if (nrow(player_stats) < n_games) {
      return(NA_real_)
    }
    
    mean(is_over_market_line(head(player_stats[[stat]], n_games), line), na.rm = TRUE)
  }

  # Calculate proportions from the latest n games by date
  last_games_stats <-
    tibble(
      player_full_name = player_full_name,
      line = line,
      emp_prob_last_3 = calc_emp_prob(3),
      emp_prob_last_5 = calc_emp_prob(5),
      emp_prob_last_7 = calc_emp_prob(7),
      emp_prob_last_10 = calc_emp_prob(10)
    )
  
  return(last_games_stats)
}
