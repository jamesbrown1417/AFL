## %######################################################%##
#                                                          #
####                       Set up                       ####
#                                                          #
## %######################################################%##

# Load the required libraries
library(shiny)
library(tidyverse)
library(bslib)
library(gridlayout)
library(DT)
library(googlesheets4)
library(googledrive)
library(readxl)
library(zoo)
library(shinythemes)

# Define helper operators
`%notin%` <- Negate(`%in%`)

# Determine the operating system
os_type <- Sys.info()["sysname"]

# # Google sheets authentification -----------------------------------------------
# options(gargle_oauth_cache = ".secrets")
# drive_auth(cache = ".secrets", email = "cuzzy.punting@gmail.com")
# gs4_auth(token = drive_token())

# Read in data
all_player_stats <- read_rds("../../Data/afl_fantasy_2015_2025_data.rds")
data_2026 <- read_rds("../../Data/afl_fantasy_2026_data.rds")
all_player_stats <- bind_rows(all_player_stats, data_2026)
player_names <- sort(unique(all_player_stats$player_full_name))
team_stats <- read_rds("../../Data/afl_team_stats_2021_2025.rds")

# Fix CBA Percentage
all_player_stats$cba_percentage <- round(all_player_stats$cba_percentage, 3)

# Agencies List
agencies <- c("TAB", "Pointsbet", "Neds", "Sportsbet", "Bet365", "Unibet", "BetRight", "Betr", "Dabble", "Betfair")

# ===============================================================================
# Read in and normalise DVP Data
# ===============================================================================

# Read in data
dvp_data <-
  read_csv("../../DVP/dvp_data.csv")

# Read in position data---------------------------------------------------------
player_positions <-
  read_csv("../../DVP/AFL-Players-Positions-2025.csv") |>
  rename(Position = position, player_name = player_full_name)

dvp_data <-
  dvp_data %>%
  mutate(dvp = ifelse(market_name == "Player Goals", rnorm(nrow(dvp_data)), dvp)) |>
  group_by(market_name) %>%
  mutate(
    DVP_Category = cut(
      dvp,
      breaks = quantile(dvp, probs = 0:5 / 5, na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("Terrible", "Bad", "Neutral", "Good", "Excellent")
    )
  ) %>%
  mutate(DVP_Category = as.character(DVP_Category)) |>
  mutate(DVP_Category = ifelse(market_name == "Player Goals", "Neutral", DVP_Category)) |>
  ungroup() %>%
  select(Position = Pos, opposition_team = Opponent, market_name, DVP_Category)

# ===============================================================================
# Read in odds data
# ===============================================================================

# Define dummy dataframes for missing odds files
dummy_h2h <- tibble(
  match = character(),
  start_time = as.POSIXct(character())
)
dummy_line <- tibble(
  match = character()
)
dummy_player <- tibble(
  player_name = character(),
  opposition_team = character(),
  market_name = character(),
  line = numeric(),
  over_price = numeric(),
  variation = numeric(),
  agency = character(),
  empirical_prob_over_2025 = numeric(),
  diff_over_2025 = numeric(),
  emp_prob_last_10 = numeric(),
  diff_over_last_10 = numeric(),
  match = character()
)

# Safe function to read RDS files
safe_read_rds <- function(path, dummy_df) {
  if (file.exists(path)) {
    tryCatch(
      {
        df <- read_rds(path)
        if (nrow(df) > 0) {
          return(df)
        }
      },
      error = function(e) {}
    )
  }
  return(dummy_df)
}

# Conditional logic for loading data based on OS
if (
  # os_type == "Windows"
  TRUE
) {
  # Read RDS Data for Windows
  h2h_data <- safe_read_rds("../../Data/processed_odds/all_h2h.rds", dummy_h2h) |> arrange(start_time)
  line_data <- safe_read_rds("../../Data/processed_odds/all_line.rds", dummy_line)
  player_disposals_data <- safe_read_rds("../../Data/processed_odds/all_player_disposals.rds", dummy_player)
  player_goals_data <- safe_read_rds("../../Data/processed_odds/all_player_goals.rds", dummy_player)
  player_fantasy_data <- safe_read_rds("../../Data/processed_odds/all_player_fantasy_points.rds", dummy_player)
  player_marks_data <- safe_read_rds("../../Data/processed_odds/all_player_marks.rds", dummy_player)
  player_tackles_data <- safe_read_rds("../../Data/processed_odds/all_player_tackles.rds", dummy_player)
  player_kicks_data <- safe_read_rds("../../Data/processed_odds/all_player_kicks.rds", dummy_player)
  player_handballs_data <- safe_read_rds("../../Data/processed_odds/all_player_handballs.rds", dummy_player)
  player_hitouts_data <- safe_read_rds("../../Data/processed_odds/all_player_hitouts.rds", dummy_player)
  player_clearances_data <- safe_read_rds("../../Data/processed_odds/all_player_clearances.rds", dummy_player)
} else {
  # Google Sheets Data for other OS
  ss_name <- gs4_find("AFL Data")
  h2h_data <- read_sheet(ss = ss_name, sheet = "H2H")
  line_data <- read_sheet(ss = ss_name, sheet = "Line")
  player_disposals_data <- read_sheet(ss = ss_name, sheet = "Player Disposals")
  player_goals_data <- read_sheet(ss = ss_name, sheet = "Player Goals")
  player_fantasy_data <- read_sheet(ss = ss_name, sheet = "Player Fantasy Points")
  player_hitouts_data <- tryCatch(read_sheet(ss = ss_name, sheet = "Player Hitouts"), error = function(e) dummy_player)
  player_clearances_data <- tryCatch(read_sheet(ss = ss_name, sheet = "Player Clearances"), error = function(e) dummy_player)
}

# Add DVP Data------------------------------------------------------------------

player_disposals_data <-
  player_disposals_data |>
  left_join(player_positions, relationship = "many-to-one") |>
  left_join(dvp_data, by = c("opposition_team", "Position", "market_name"), relationship = "many-to-one") |>
  relocate(Position, DVP_Category, .after = player_name)

player_goals_data <-
  player_goals_data |>
  left_join(player_positions, relationship = "many-to-one") |>
  left_join(dvp_data, by = c("opposition_team", "Position", "market_name"), relationship = "many-to-one") |>
  relocate(Position, DVP_Category, .after = player_name)

player_fantasy_data <-
  player_fantasy_data |>
  left_join(player_positions, relationship = "many-to-one") |>
  left_join(dvp_data, by = c("opposition_team", "Position", "market_name"), relationship = "many-to-one") |>
  relocate(Position, DVP_Category, .after = player_name)

player_marks_data <-
  player_marks_data |>
  left_join(player_positions, relationship = "many-to-one") |>
  left_join(dvp_data, by = c("opposition_team", "Position", "market_name"), relationship = "many-to-one") |>
  relocate(Position, DVP_Category, .after = player_name)

player_tackles_data <-
  player_tackles_data |>
  left_join(player_positions, relationship = "many-to-one") |>
  left_join(dvp_data, by = c("opposition_team", "Position", "market_name"), relationship = "many-to-one") |>
  relocate(Position, DVP_Category, .after = player_name)

player_kicks_data <-
  player_kicks_data |>
  left_join(player_positions, relationship = "many-to-one") |>
  left_join(dvp_data, by = c("opposition_team", "Position", "market_name"), relationship = "many-to-one") |>
  relocate(Position, DVP_Category, .after = player_name)

player_handballs_data <-
  player_handballs_data |>
  left_join(player_positions, relationship = "many-to-one") |>
  left_join(dvp_data, by = c("opposition_team", "Position", "market_name"), relationship = "many-to-one") |>
  relocate(Position, DVP_Category, .after = player_name)

player_hitouts_data <-
  player_hitouts_data |>
  left_join(player_positions, relationship = "many-to-one") |>
  left_join(dvp_data, by = c("opposition_team", "Position", "market_name"), relationship = "many-to-one") |>
  relocate(Position, DVP_Category, .after = player_name)

player_clearances_data <-
  player_clearances_data |>
  left_join(player_positions, relationship = "many-to-one") |>
  left_join(dvp_data, by = c("opposition_team", "Position", "market_name"), relationship = "many-to-one") |>
  relocate(Position, DVP_Category, .after = player_name)

# List of players available in any odds dataset
player_names_odds <- sort(unique(c(
  player_disposals_data$player_name,
  player_goals_data$player_name,
  player_fantasy_data$player_name,
  player_marks_data$player_name,
  player_tackles_data$player_name,
  player_kicks_data$player_name,
  player_handballs_data$player_name,
  player_hitouts_data$player_name,
  player_clearances_data$player_name
)))

# ===============================================================================
# SGM App Data Loading and Functions
# ===============================================================================

# Safe function to read CSV files
safe_read_csv <- function(path) {
  if (file.exists(path)) {
    tryCatch(
      {
        df <- readr::read_csv(path, show_col_types = FALSE)
        if (nrow(df) > 0) {
          return(df)
        }
      },
      error = function(e) {}
    )
  }
  return(tibble::tibble())
}

# Source SGM scripts
source("betright_sgm.R")
source("tab_sgm.R")
source("sportsbet_sgm.R")
source("pointsbet_sgm.R")
source("neds_sgm.R")
source("bet365_sgm.R")
source("dabble_sgm.R")
source("player_combos.R")

# Matches in order
matches_in_order <-
  h2h_data %>%
  distinct(match) |>
  pull()

# Compare SGM function
compare_sgm <- function(player_names, stat_counts, markets, types, non_tab_stat_counts = stat_counts) {
  empty_sgm_result <- tibble(
    Selections = character(),
    Markets = character(),
    Unadjusted_Price = numeric(),
    Adjusted_Price = numeric(),
    Adjustment_Factor = numeric(),
    Agency = character()
  )

  normalise_sgm_result <- function(df) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      return(empty_sgm_result)
    }

    missing_cols <- setdiff(names(empty_sgm_result), names(df))
    for (col in missing_cols) {
      df[[col]] <- NA
    }

    df |>
      as_tibble() |>
      select(all_of(names(empty_sgm_result))) |>
      mutate(
        Unadjusted_Price = as.numeric(Unadjusted_Price),
        Adjusted_Price = as.numeric(Adjusted_Price),
        Adjustment_Factor = as.numeric(Adjustment_Factor)
      )
  }

  sgm_retry_attempts <- 3L
  sgm_retry_delay_seconds <- 0.35

  # Function to handle errors in the call_sgm functions
  handle_call_sgm <- function(func, sgm, player_names, stat_counts, markets, types) {
    for (attempt in seq_len(sgm_retry_attempts)) {
      result <- tryCatch(
        {
          normalise_sgm_result(func(sgm, player_names, stat_counts, markets, types))
        },
        error = function(e) {
          empty_sgm_result
        }
      )

      if (nrow(result) > 0) {
        return(result)
      }

      if (attempt < sgm_retry_attempts) {
        Sys.sleep(sgm_retry_delay_seconds)
      }
    }

    empty_sgm_result
  }

  # Get individual dataframes
  pointsbet_data <- handle_call_sgm(call_sgm_pointsbet, pointsbet_sgm, player_names, non_tab_stat_counts, markets, types)
  sportsbet_data <- handle_call_sgm(call_sgm_sportsbet, sportsbet_sgm, player_names, non_tab_stat_counts, markets, types)
  tab_data <- handle_call_sgm(call_sgm_tab, tab_sgm, player_names, stat_counts, markets, types)
  betright_data <- handle_call_sgm(call_sgm_betright, betright_sgm, player_names, non_tab_stat_counts, markets, types)
  neds_data <- handle_call_sgm(call_sgm_neds, neds_sgm, player_names, non_tab_stat_counts, markets, types)
  bet365_data <- handle_call_sgm(call_sgm_bet365, bet365_sgm, player_names, non_tab_stat_counts, markets, types)
  dabble_data <- handle_call_sgm(call_sgm_dabble, dabble_sgm, player_names, non_tab_stat_counts, markets, types)

  combined_sgm <- bind_rows(pointsbet_data, sportsbet_data, tab_data, betright_data, neds_data, bet365_data, dabble_data)

  if (nrow(combined_sgm) == 0) {
    return(empty_sgm_result)
  }

  combined_sgm |>
    mutate(
      Adjusted_Price = round(as.numeric(Adjusted_Price), 2),
      Unadjusted_Price = round(as.numeric(Unadjusted_Price), 2),
      Adjustment_Factor = round(as.numeric(Adjustment_Factor), 2)
    ) |>
    arrange(desc(Adjusted_Price))
}

# Compare CGM function
compare_cgm <- function(player_names_cross, lines_cross, market_names_cross, types_cross) {
  empty_cgm_result <- tibble(
    Selections = character(),
    Matches = character(),
    Markets = character(),
    Price = numeric(),
    Agency = character()
  )

  normalise_cgm_result <- function(df) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      return(empty_cgm_result)
    }

    missing_cols <- setdiff(names(empty_cgm_result), names(df))
    for (col in missing_cols) {
      df[[col]] <- NA
    }

    df |>
      as_tibble() |>
      select(all_of(names(empty_cgm_result))) |>
      mutate(Price = as.numeric(Price))
  }

  # List of each agency data
  all_data <- list(pointsbet_sgm, sportsbet_sgm, tab_sgm, betright_sgm, neds_sgm, bet365_sgm, dabble_sgm)

  # Function to get cross game multi data
  get_cgm <- function(data, player_names_cross, lines_cross, market_names_cross, types_cross) {
    if (length(player_names_cross) == 0) {
      return(NULL)
    }

    if (length(player_names_cross) != length(lines_cross) || length(lines_cross) != length(market_names_cross)) {
      stop("All lists should have the same length")
    }

    filtered_df <- data.frame()
    for (i in seq_along(player_names_cross)) {
      temp_df <- data %>%
        filter(
          player_name == player_names_cross[i],
          line == lines_cross[i],
          market_name == market_names_cross[i],
          type == types_cross[i]
        )
      filtered_df <- bind_rows(filtered_df, temp_df)
    }

    if (nrow(filtered_df) != length(player_names_cross)) {
      return(NULL)
    }

    price <- prod(filtered_df$price)

    combined_list <- paste(player_names_cross, lines_cross, sep = ": ")
    player_string <- paste(combined_list, collapse = ", ")
    market_string <- paste(market_names_cross, collapse = ", ")
    match_string <- paste(filtered_df$match, collapse = ", ")

    output_data <- data.frame(
      Selections = player_string,
      Matches = match_string,
      Markets = market_string,
      Price = round(price, 2),
      Agency = first(data$agency)
    )

    return(output_data)
  }

  # Function to handle errors in the get_cgm function
  handle_get_cgm <- function(data, player_names_cross, lines_cross, market_names_cross, types_cross) {
    tryCatch(
      {
        normalise_cgm_result(get_cgm(data, player_names_cross, lines_cross, market_names_cross, types_cross))
      },
      error = function(e) {
        # Return no rows if an error occurs for this agency
        empty_cgm_result
      }
    )
  }

  # Map over list of dataframes
  cgm_all <- map_dfr(all_data, handle_get_cgm, player_names_cross, lines_cross, market_names_cross, types_cross) %>%
    mutate(Price = as.numeric(Price)) |>
    arrange(desc(Price))

  return(cgm_all)
}

# SGM data for display - combine all player data
all_player_data <-
  player_disposals_data |>
  bind_rows(player_goals_data) |>
  bind_rows(player_marks_data) |>
  bind_rows(player_tackles_data) |>
  bind_rows(player_fantasy_data) |>
  bind_rows(player_kicks_data) |>
  bind_rows(player_handballs_data) |>
  bind_rows(player_hitouts_data) |>
  bind_rows(player_clearances_data)

# Build overs with unified columns
disposals_sgm_overs <- all_player_data |>
  mutate(
    type = "Over",
    price = over_price,
    empirical_probability_2025 = empirical_prob_over_2025,
    diff_2025 = diff_over_2025,
    prob_last_10_sgm = emp_prob_last_10,
    diff_last_10_sgm = diff_over_last_10
  )

# Build unders with unified columns
disposals_sgm_unders <- all_player_data |>
  filter(!is.na(under_price)) |>
  mutate(
    type = "Under",
    price = under_price,
    empirical_probability_2025 = empirical_prob_under_2025,
    diff_2025 = diff_under_2025,
    prob_last_10_sgm = empirical_prop_under_last_10,
    diff_last_10_sgm = diff_under_last_10
  )

disposals_sgm <- bind_rows(disposals_sgm_overs, disposals_sgm_unders)

# Create market best
disposals_sgm <-
  disposals_sgm |>
  group_by(match, player_name, market_name, line, type) |>
  arrange(desc(price), .by_group = TRUE) |>
  mutate(
    max_player_diff = max(diff_last_10_sgm, na.rm = TRUE),
    second_best_price = if_else(n() >= 2, nth(price, 2), NA_real_),
    market_best = row_number() == 1
  ) |>
  ungroup()

# Unique matches for SGM
matches <- matches_in_order

# Unique agencies for SGM
agencies_sgm <-
  disposals_sgm |>
  distinct(agency) |>
  pull()

# Add Dabble to the agencies list
agencies_sgm <- c(agencies_sgm, "Dabble") |> unique()

# Create disposals dataframe to display
disposals_display <-
  disposals_sgm |>
  group_by(player_name, match, line, market_name, type) |>
  mutate(
    next_best_diff = if_else(market_best,
      ((1 / second_best_price) - (1 / price)),
      NA_real_
    )
  ) |>
  ungroup() |>
  arrange(desc(max_player_diff)) |>
  transmute(match,
    player_name,
    Position,
    type,
    Matchup = DVP_Category,
    market_name,
    line,
    price,
    agency,
    prob_2025 = round(empirical_probability_2025, 2),
    diff_2025 = round(diff_2025, 2),
    prob_last_10 = round(prob_last_10_sgm, 2),
    diff_last_10 = round(diff_last_10_sgm, 2),
    next_best_diff = round(100 * next_best_diff, 1),
    market_best
  )

# Add home_away variable
all_player_stats <-
  all_player_stats |>
  mutate(home_away = ifelse(player_team == home_team, "Home", "Away"))

# Make margin variable negative if loss
all_player_stats <-
  all_player_stats |>
  mutate(margin = ifelse((match_result == "Away Win" &
    home_away == "Home") |
    (match_result == "Home Win" &
      home_away == "Away"), -margin,
  margin
  ))


# Make weather category Indoors if at Marvel Stadium
all_player_stats <-
  all_player_stats |>
  mutate(weather_category = ifelse(venue == "Marvel Stadium", "Indoors", weather_category))

# Add gameId variable
all_player_stats <-
  all_player_stats |>
  mutate(gameId = paste0(season_name, round, match_name))

# Function to get correlation between players-----------------------------------
get_player_correlation <- function(data, seasons = NULL, name_a, name_b, metric_a, metric_b, line_a = NULL, line_b = NULL) {
  # Column names for later use
  col_name_a <- paste0(name_a, " ", metric_a)
  col_name_b <- paste0(name_b, " ", metric_b)

  # Get dataframe for player A
  df_player_a <-
    data %>%
    filter(Player == name_a & Season %in% seasons) |>
    select(gameId, Player, all_of(metric_a)) |>
    rename(!!col_name_a := all_of(metric_a))

  # Get dataframe for player B
  df_player_b <-
    data %>%
    filter(Player == name_b & Season %in% seasons) |>
    select(gameId, Player, all_of(metric_b)) |>
    rename(!!col_name_b := all_of(metric_b))

  # Merge the two dataframes
  df_merged <- inner_join(df_player_a, df_player_b, by = "gameId")

  # Compute correlation
  correlation <- cor(df_merged[[col_name_a]], df_merged[[col_name_b]], method = "pearson")
  cat(sprintf("The correlation between %s and %s is: %f\n", col_name_a, col_name_b, correlation))

  # Calculate quadrant statistics if lines are provided
  quadrant_stats <- NULL
  plot_subtitle <- sprintf("Correlation between %s and %s", col_name_a, col_name_b)

  if (!is.null(line_a) && !is.null(line_b)) {
    # Calculate individual probabilities for independence comparison
    prob_a_under <- sum(df_merged[[col_name_a]] < line_a) / nrow(df_merged)
    prob_a_over <- sum(df_merged[[col_name_a]] >= line_a) / nrow(df_merged)
    prob_b_under <- sum(df_merged[[col_name_b]] < line_b) / nrow(df_merged)
    prob_b_over <- sum(df_merged[[col_name_b]] >= line_b) / nrow(df_merged)

    # Expected probabilities under independence
    expected_both_under <- prob_a_under * prob_b_under * 100
    expected_both_over <- prob_a_over * prob_b_over * 100
    expected_a_over_b_under <- prob_a_over * prob_b_under * 100
    expected_a_under_b_over <- prob_a_under * prob_b_over * 100

    # Create quadrant categories
    df_merged <- df_merged %>%
      mutate(
        quadrant = case_when(
          .data[[col_name_a]] < line_a & .data[[col_name_b]] < line_b ~ "Both Under",
          .data[[col_name_a]] >= line_a & .data[[col_name_b]] < line_b ~ "Player 1 Over, Player 2 Under",
          .data[[col_name_a]] < line_a & .data[[col_name_b]] >= line_b ~ "Player 1 Under, Player 2 Over",
          .data[[col_name_a]] >= line_a & .data[[col_name_b]] >= line_b ~ "Both Over"
        )
      )

    # Calculate actual quadrant statistics
    quadrant_stats <- df_merged %>%
      group_by(quadrant) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(
        actual_pct = round(count / sum(count) * 100, 1),
        expected_pct = case_when(
          quadrant == "Both Under" ~ round(expected_both_under, 1),
          quadrant == "Both Over" ~ round(expected_both_over, 1),
          quadrant == "Player 1 Over, Player 2 Under" ~ round(expected_a_over_b_under, 1),
          quadrant == "Player 1 Under, Player 2 Over" ~ round(expected_a_under_b_over, 1)
        ),
        difference = actual_pct - expected_pct
      )

    # Update subtitle with quadrant info
    both_under_actual <- quadrant_stats$actual_pct[quadrant_stats$quadrant == "Both Under"]
    both_over_actual <- quadrant_stats$actual_pct[quadrant_stats$quadrant == "Both Over"]
    both_under_expected <- quadrant_stats$expected_pct[quadrant_stats$quadrant == "Both Under"]
    both_over_expected <- quadrant_stats$expected_pct[quadrant_stats$quadrant == "Both Over"]

    if (length(both_under_actual) == 0) both_under_actual <- 0
    if (length(both_over_actual) == 0) both_over_actual <- 0
    if (length(both_under_expected) == 0) both_under_expected <- 0
    if (length(both_over_expected) == 0) both_over_expected <- 0

    plot_subtitle <- sprintf(
      "Both Under: %s%% (exp: %s%%) | Both Over: %s%% (exp: %s%%) | Lines: %s, %s",
      both_under_actual, both_under_expected, both_over_actual, both_over_expected, line_a, line_b
    )
  }

  # Create base plot
  p <- ggplot(df_merged, aes(x = .data[[col_name_a]], y = .data[[col_name_b]])) +
    geom_point(color = "#3498db", alpha = 0.6, size = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "#e74c3c", linetype = "dashed") +
    labs(
      x = col_name_a,
      y = col_name_b,
      title = "Player Performance Correlation",
      subtitle = plot_subtitle,
      caption = sprintf("Pearson's r: %.2f", correlation)
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, color = "grey50"),
      plot.caption = element_text(hjust = 1, color = "grey50"),
      text = element_text(size = 12),
      axis.title = element_text(face = "bold"),
      legend.position = "none"
    ) +
    annotate(
      "text",
      x = max(df_merged[[col_name_a]]), y = min(df_merged[[col_name_b]]),
      label = sprintf("r = %.2f", correlation),
      hjust = 1, vjust = 0, size = 5, color = "red1", fontface = "italic"
    )

  # Add quadrant lines and labels if lines are provided
  if (!is.null(line_a) && !is.null(line_b)) {
    p <- p +
      geom_vline(xintercept = line_a, color = "darkgreen", linetype = "solid", alpha = 0.7, size = 1) +
      geom_hline(yintercept = line_b, color = "darkgreen", linetype = "solid", alpha = 0.7, size = 1) +
      annotate("text",
        x = line_a, y = max(df_merged[[col_name_b]]),
        label = paste("Line:", line_a), hjust = -0.1, vjust = 1,
        color = "darkgreen", fontface = "bold", size = 3.5
      ) +
      annotate("text",
        x = max(df_merged[[col_name_a]]), y = line_b,
        label = paste("Line:", line_b), hjust = 1, vjust = -0.1,
        color = "darkgreen", fontface = "bold", size = 3.5
      )

    # Add quadrant labels with actual vs expected probabilities
    x_mid_left <- (min(df_merged[[col_name_a]]) + line_a) / 2
    x_mid_right <- (line_a + max(df_merged[[col_name_a]])) / 2
    y_mid_bottom <- (min(df_merged[[col_name_b]]) + line_b) / 2
    y_mid_top <- (line_b + max(df_merged[[col_name_b]])) / 2

    # Get actual and expected percentages for each quadrant
    both_under_stats <- quadrant_stats[quadrant_stats$quadrant == "Both Under", ]
    both_over_stats <- quadrant_stats[quadrant_stats$quadrant == "Both Over", ]
    p1_over_p2_under_stats <- quadrant_stats[quadrant_stats$quadrant == "Player 1 Over, Player 2 Under", ]
    p1_under_p2_over_stats <- quadrant_stats[quadrant_stats$quadrant == "Player 1 Under, Player 2 Over", ]

    # Create labels with actual vs expected
    both_under_label <- if (nrow(both_under_stats) > 0) {
      sprintf("Both Under\n%s%% (exp: %s%%)", both_under_stats$actual_pct, both_under_stats$expected_pct)
    } else {
      "Both Under\n0% (exp: 0%)"
    }

    both_over_label <- if (nrow(both_over_stats) > 0) {
      sprintf("Both Over\n%s%% (exp: %s%%)", both_over_stats$actual_pct, both_over_stats$expected_pct)
    } else {
      "Both Over\n0% (exp: 0%)"
    }

    p1_over_p2_under_label <- if (nrow(p1_over_p2_under_stats) > 0) {
      sprintf("P1 Over, P2 Under\n%s%% (exp: %s%%)", p1_over_p2_under_stats$actual_pct, p1_over_p2_under_stats$expected_pct)
    } else {
      "P1 Over, P2 Under\n0% (exp: 0%)"
    }

    p1_under_p2_over_label <- if (nrow(p1_under_p2_over_stats) > 0) {
      sprintf("P1 Under, P2 Over\n%s%% (exp: %s%%)", p1_under_p2_over_stats$actual_pct, p1_under_p2_over_stats$expected_pct)
    } else {
      "P1 Under, P2 Over\n0% (exp: 0%)"
    }

    p <- p +
      annotate("text",
        x = x_mid_left, y = y_mid_bottom,
        label = both_under_label, hjust = 0.5, vjust = 0.5,
        color = "red", fontface = "bold", size = 3.5, alpha = 0.8
      ) +
      annotate("text",
        x = x_mid_right, y = y_mid_bottom,
        label = p1_over_p2_under_label, hjust = 0.5, vjust = 0.5,
        color = "orange", fontface = "bold", size = 3.5, alpha = 0.8
      ) +
      annotate("text",
        x = x_mid_left, y = y_mid_top,
        label = p1_under_p2_over_label, hjust = 0.5, vjust = 0.5,
        color = "orange", fontface = "bold", size = 3.5, alpha = 0.8
      ) +
      annotate("text",
        x = x_mid_right, y = y_mid_top,
        label = both_over_label, hjust = 0.5, vjust = 0.5,
        color = "green", fontface = "bold", size = 3.5, alpha = 0.8
      )
  }

  return(p)
}

# Function to compare player performance w or w/o teammate----------------------
compare_performance <- function(data, seasons = NULL, name, teammate_name, metric) {
  # Filter the data for games with the main player
  df_player <-
    data %>%
    filter(Player == name) %>%
    filter(Season %in% seasons)

  # Find the game IDs where the teammate also played
  games_with_teammate <-
    data %>%
    filter(Season %in% seasons) %>%
    filter(Player == teammate_name) %>%
    pull(gameId)

  # Label each game as 'With Teammate' or 'Without Teammate'
  df_player <- df_player %>%
    mutate(Teammate = if_else(gameId %in% games_with_teammate, "With Teammate", "Without Teammate"))

  # Calculate mean and count for both conditions
  summary_stats <- df_player %>%
    group_by(Teammate) %>%
    summarise(mean_val = mean(!!sym(metric), na.rm = TRUE), n_games = n())

  # Create the violin plot
  plot <- ggplot(df_player, aes(x = Teammate, y = !!sym(metric), fill = Teammate)) +
    geom_violin(trim = FALSE, position = position_dodge(width = 0.9)) +
    geom_boxplot(width = 0.1, position = position_dodge(width = 0.9)) +
    labs(
      title = paste("Performance of", name, "with and without", teammate_name),
      x = "Condition",
      y = metric
    ) +
    scale_fill_manual(values = c("Without Teammate" = "orange1", "With Teammate" = "royalblue1")) +
    annotate("text",
      x = Inf, y = Inf,
      label = paste(
        "With Teammate: ", summary_stats$n_games[summary_stats$Teammate == "With Teammate"],
        " games, Mean ", round(summary_stats$mean_val[summary_stats$Teammate == "With Teammate"], 2), "\n",
        "Without Teammate: ", summary_stats$n_games[summary_stats$Teammate == "Without Teammate"],
        " games, Mean ", round(summary_stats$mean_val[summary_stats$Teammate == "Without Teammate"], 2)
      ),
      hjust = 1, vjust = 1
    ) +
    theme_minimal()

  return(plot)
}

# Function to compare player performance w or w/o teammate----------------------
compare_performance_table <- function(data, seasons = NULL, name, teammate_name) {
  # Filter the data for games with the main player
  df_player <-
    data %>%
    filter(Player == name) %>%
    filter(Season %in% seasons)

  # Find the game IDs where the teammate also played
  games_with_teammate <-
    data %>%
    filter(Season %in% seasons) %>%
    filter(Player == teammate_name) %>%
    pull(gameId)

  # Label each game as 'With Teammate' or 'Without Teammate'
  df_player <- df_player %>%
    mutate(Teammate = if_else(gameId %in% games_with_teammate, "With Teammate", "Without Teammate"))

  # Calculate mean and count for both conditions
  summary_stats <-
    df_player %>%
    group_by(Teammate) %>%
    summarise(
      n_games = n(),
      `AVG Disposals` = mean(Disposals, na.rm = TRUE),
      `AVG Goals` = mean(Goals, na.rm = TRUE),
      `AVG Fantasy` = mean(Fantasy, na.rm = TRUE),
      `AVG CBA%` = mean(CBA, na.rm = TRUE)
    ) |>
    mutate(across(`AVG Disposals`:`AVG CBA%`, ~ round(., 2)))

  return(summary_stats)
}

# Function to compare player performance under certain conditions---------------
player_contrasts <- function(data, seasons = NULL, name, grouping_vars) {
  # Filter the data for games with the main player
  df_player <-
    data %>%
    filter(Player == name) %>%
    filter(Season %in% seasons) |>
    rename(home_away = `Home / Away`)

  # Create margin_group variable
  df_player <-
    df_player %>%
    mutate(
      margin_group = case_when(
        Margin >= 40 ~ "40+ Win",
        between(Margin, 1, 39) ~ "1-39 Win",
        Margin == 0 ~ "Draw",
        between(Margin, -39, -1) ~ "1-39 Loss",
        Margin <= -40 ~ "40+ Loss"
      )
    ) |>
    mutate(margin_group = factor(
      margin_group,
      levels = c("40+ Win", "1-39 Win", "Draw", "1-39 Loss", "40+ Loss")
    ))

  # Calculate mean and count for both conditions
  summary_stats <-
    df_player %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(
      n_games = n(),
      `AVG Disposals` = mean(Disposals, na.rm = TRUE),
      `AVG Goals` = mean(Goals, na.rm = TRUE),
      `AVG Fantasy` = mean(Fantasy, na.rm = TRUE),
      `AVG CBA%` = mean(CBA, na.rm = TRUE)
    ) |>
    mutate(across(`AVG Disposals`:`AVG CBA%`, ~ round(., 2)))

  return(summary_stats)
}

filtered_player_stats_2 <-
  all_player_stats |>
  arrange(start_time_utc) |>
  mutate(game_number = row_number()) |>
  select(
    Date = start_time_utc,
    Season = season_name,
    gameId,
    Round = round,
    Home = home_team,
    Venue = venue,
    Weather = weather_category,
    Away = away_team,
    Player = player_full_name,
    Team = player_team,
    `Home / Away` = home_away,
    Margin = margin,
    Opposition = opposition_team,
    TOG = tog_percentage,
    Disposals = disposals,
    Kicks = kicks,
    Handballs = handballs,
    Marks = marks,
    Goals = goals,
    Behinds = behinds,
    Tackles = tackles,
    Hitouts = hitouts,
    Frees_For = frees_for,
    Frees_Against = frees_against,
    Fantasy = fantasy_points,
    CBA = cba_percentage,
    game_number
  ) |>
  arrange(desc(Date))

# ===============================================================================
# UI
# ===============================================================================

ui <- page_navbar(
  title = "AFL",
  selected = "Player Stats",
  collapsible = TRUE,
  theme = bslib::bs_theme(version = 5),
  fillable = TRUE, # Ensure the page itself is fillable

  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    # --- THIS IS THE CRITICAL CSS FIX ---
    tags$style(HTML("
      .dt-fill-container {
        height: 78vh;
        display: flex;
        flex-direction: column;
      }
      .dt-fill-container .dataTables_wrapper,
      .dt-fill-container .dataTables_scroll {
        height: 100%;
        display: flex;
        flex-direction: column;
        flex-grow: 1;
      }
      .dt-fill-container .dataTables_scrollBody {
        flex-grow: 1;
        overflow-y: auto;
      }
    "))
  ),
  nav_panel(
    title = "Player Stats",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Settings"),
        selectInput(
          "player_name_input_a",
          "Select Player:",
          choices = player_names,
          selectize = TRUE,
          selected = "Tim English"
        ),
        selectInput("season_input_a", "Select Season:", choices = all_player_stats$season_name |> unique(), multiple = TRUE, selectize = TRUE, selected = c("2025", "2024")),
        selectInput("stat_input_a", "Select Statistic:", choices = c("Disposals", "Fantasy", "Tackles", "Marks", "Goals", "Kicks", "Handballs"), selected = "Disposals"),
        selectInput("opp_input_a", "Select Opposition:", choices = c(all_player_stats$opposition_team |> unique() |> sort()), multiple = TRUE),
        selectInput("venue_input_a", "Select Venue:", choices = c(all_player_stats$venue |> unique() |> sort()), multiple = TRUE),
        selectInput("weather_input_a", "Select Weather:", choices = c(all_player_stats$weather_category |> unique() |> sort()), multiple = TRUE),
        checkboxGroupInput("home_status", "Home / Away Games", choices = list("Home" = "Home", "Away" = "Away"), selected = c("Home", "Away")),
        markdown(mds = c("__Select Margin Range:__")),
        numericInput("margin_min", "Minimum", value = -200),
        numericInput("margin_max", "Maximum", value = 200),
        markdown(mds = c("__Select Only Last n Games:__")),
        numericInput("last_games", "Number of Games", value = NA),
        markdown(mds = c("__Select Reference Line:__")),
        radioButtons("line_mode", "Mode:",
          choices = list("Single Line" = "single", "Interval" = "interval"),
          selected = "single"
        ),
        conditionalPanel(
          condition = "input.line_mode == 'single'",
          numericInput("reference_line", "Line Value", value = 19.5)
        ),
        conditionalPanel(
          condition = "input.line_mode == 'interval'",
          numericInput("lower_bound", "Lower Bound", value = 19.5),
          numericInput("upper_bound", "Upper Bound", value = 25.5)
        ),
        markdown(mds = c("__Select TOG Range:__")),
        numericInput("minutes_minimum", "Min TOG %", value = 0)
      ),
      mainPanel(
        width = 9,
        card(
          full_screen = TRUE,
          card_body(
            tabsetPanel(
              id = "stat_tabs",
              tabPanel("Plot", plotOutput(outputId = "plot", height = "75vh")),
              tabPanel("Table", div(class = "dt-fill-container", DTOutput(outputId = "player_stat_table")))
            )
          )
        )
      )
    )
  ),
  nav_panel(
    title = "Team Stats",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("General Settings"),
        selectInput("season_input_b", "Select Season:",
          choices = team_stats$season_name |> unique() |> sort(decreasing = TRUE),
          multiple = TRUE,
          selectize = TRUE,
          selected = team_stats$season_name |> unique()
        ),
        markdown(mds = c("__Select Only Last n Games:__")),
        numericInput("last_games_team", "Number of Games per Team", value = NA),
        hr(),
        h4("Analysis Settings"),
        conditionalPanel(
          condition = "input.team_tabs == 'Head-to-Head'",
          selectInput("team_a", "Select Team A:",
            choices = c("", sort(unique(c(team_stats$home_team, team_stats$away_team)))),
            selected = ""
          ),
          selectInput("team_b", "Select Team B:",
            choices = c("", sort(unique(c(team_stats$home_team, team_stats$away_team)))),
            selected = ""
          )
        ),
        conditionalPanel(
          condition = "input.team_tabs == 'Opposition Analysis'",
          tagList(

            # league-wide toggle (you already added this earlier; keep it here)
            checkboxInput(
              "aggregate_league",
              label = "Show league aggregate (one row per opposition)",
              value = FALSE
            ),

            # opponent selector – only visible when NOT in league-aggregate mode
            conditionalPanel(
              condition = "!input.aggregate_league",
              selectInput(
                "selected_opposition",
                "Filter by Opposition (optional):",
                choices = c("All" = "", sort(unique(c(
                  team_stats$home_team,
                  team_stats$away_team
                )))),
                selected = ""
              )
            ),
            numericInput("min_games_opp",
              "Minimum Games vs Opposition",
              value = 1, # make single-season views work
              min   = 1
            )
          )
        ),
        checkboxInput("aggregate_opp",
          label = "Show league aggregate (all teams)",
          value = FALSE
        ),
        conditionalPanel(
          condition = "input.team_tabs == 'Venue Stats'",
          tagList(
            # league-wide toggle (shared with Opposition view)
            checkboxInput(
              "aggregate_league",
              label = "Show league aggregate (one row per venue)",
              value = FALSE
            ),

            # venue selector — hidden when league aggregate is on
            conditionalPanel(
              condition = "!input.aggregate_league",
              selectInput(
                "selected_venue",
                "Filter by Venue (optional):",
                choices  = c("All" = "", sort(unique(team_stats$venue))),
                selected = ""
              )
            ),
            numericInput("min_games_venue",
              "Minimum Games at Venue",
              value = 1, # use 1 so single-season views never vanish
              min   = 1
            )
          )
        ),
        conditionalPanel(
          condition = "input.team_tabs == 'Performance Trends'",
          radioButtons("team_metric", "Performance Metric:",
            choices = list("Score" = "Score", "Fantasy Points" = "Fantasy"),
            selected = "Score"
          )
        )
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          id = "team_tabs",
          tabPanel(
            "Team Summary",
            card(
              full_screen = TRUE,
              card_body(
                div(class = "dt-fill-container", DTOutput(outputId = "team_summary_table"))
              )
            )
          ),
          tabPanel(
            "Home/Away Splits",
            card(
              full_screen = TRUE,
              card_body(
                div(class = "dt-fill-container", DTOutput(outputId = "home_away_table"))
              )
            )
          ),
          tabPanel(
            "Opposition Analysis",
            card(
              full_screen = TRUE,
              card_header("Team Performance vs Opposition - Disposals, Marks & Tackles"),
              card_body(
                div(class = "dt-fill-container", DTOutput(outputId = "opposition_table"))
              )
            )
          ),
          tabPanel(
            "Venue Stats",
            card(
              full_screen = TRUE,
              card_header("Team Performance by Venue - Disposals, Marks & Tackles"),
              card_body(
                div(class = "dt-fill-container", DTOutput(outputId = "venue_specific_table"))
              )
            )
          ),
          tabPanel(
            "Performance Trends",
            card(
              full_screen = TRUE,
              card_body(
                plotOutput(outputId = "team_performance_plot", height = "75vh")
              )
            )
          ),
          tabPanel(
            "Head-to-Head",
            card(
              full_screen = TRUE,
              card_body(
                plotOutput(outputId = "h2h_comparison_plot", height = "75vh")
              )
            )
          ),
          tabPanel(
            "Venue Performance",
            card(
              full_screen = TRUE,
              card_header("Win Rate & Scoring by Venue"),
              card_body(
                div(class = "dt-fill-container", DTOutput(outputId = "venue_table"))
              )
            )
          ),
          tabPanel(
            "Weather Impact",
            card(
              full_screen = TRUE,
              card_body(
                div(class = "dt-fill-container", DTOutput(outputId = "weather_table"))
              )
            )
          )
        )
      )
    )
  ),
  nav_panel(
    title = "Odds Screen",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Settings"),
        selectInput("agency_input", "Select Agencies:", choices = agencies, multiple = TRUE, selectize = TRUE, selected = agencies),
        selectInput("market_input", "Select Market:", choices = c("H2H", "Total", "Line", "Disposals", "Fantasy", "Goals", "Marks", "Tackles", "Kicks", "Handballs", "Hitouts", "Clearances"), multiple = FALSE),
        selectInput("match_input", "Select Matches:", choices = h2h_data$match |> unique(), multiple = TRUE, selectize = FALSE, selected = h2h_data$match |> unique()),
        selectInput("matchup_input", "Select Difficulty:", choices = player_disposals_data$DVP_Category |> unique(), multiple = TRUE, selectize = FALSE, selected = player_disposals_data$DVP_Category |> unique()),
        selectInput(
          "player_name_input_b",
          "Select Player:",
          choices = c("", player_names_odds),
          selectize = TRUE,
          selected = ""
        ),
        checkboxInput("only_unders", "Only Show Markets With Unders", value = FALSE),
        checkboxInput("only_best", "Only Show Best Market Odds - Overs", value = FALSE),
        checkboxInput("only_best_unders", "Only Show Best Market Odds - Unders", value = FALSE),
        markdown(mds = c("__Select Odds Range:__")),
        numericInput("odds_minimum", "Min Odds", value = NA),
        numericInput("odds_maximum", "Max Odds", value = NA)
      ),
      mainPanel(
        width = 9,
        card(card_body(div(class = "dt-fill-container", DTOutput(outputId = "scraped_odds_table"))))
      )
    )
  ),
  nav_panel(
    title = "With / Without Teammate",
    fluidRow(
      column(
        width = 4,
        card(
          card_header("Settings"),
          card_body(
            selectInput(
              "player_name",
              "Select Player:",
              choices = player_names,
              selectize = TRUE,
              selected = "Christian Petracca"
            ),
            selectInput(
              "teammate_name",
              "Select Teammate:",
              choices = player_names,
              selectize = TRUE,
              selected = "Clayton Oliver"
            ),
            selectInput("season_input", "Select Season:", choices = all_player_stats$season_name |> unique(), multiple = TRUE, selectize = TRUE, selected = c("2024")),
            selectInput("metric_input", "Select Statistic:", choices = c("Disposals", "Fantasy", "Goals", "Marks", "Tackles", "Kicks", "Handballs"), multiple = FALSE, selected = "Fantasy")
          )
        )
      ),
      column(
        width = 8,
        card(
          card_body(
            tabsetPanel(
              id = "with_without_tabs",
              tabPanel("Plot", plotOutput(outputId = "with_without_plot_output", height = "75vh")),
              tabPanel("Table", div(class = "dt-fill-container", DTOutput(outputId = "with_without_table_output")))
            )
          )
        )
      )
    )
  ),
  nav_panel(
    title = "Player Correlations",
    fluidRow(
      column(
        width = 4,
        card(
          card_header("Settings"),
          card_body(
            selectInput(
              "player_name_corr",
              "Select Player 1:",
              choices = player_names,
              selectize = TRUE,
              selected = "Adam Treloar"
            ),
            selectInput("metric_input_corr_a", "Select Statistic:", choices = c("Disposals", "Fantasy", "Goals", "Marks", "Tackles", "Kicks", "Handballs"), selected = "Disposals"),
            numericInput("line_input_corr_a", "Player 1 Line:", value = 25, min = 0, max = 100, step = 0.5),
            selectInput(
              "teammate_name_corr",
              "Select Player 2:",
              choices = player_names,
              selectize = TRUE,
              selected = "Tim English"
            ),
            selectInput("metric_input_corr_b", "Select Statistic:", choices = c("Disposals", "Fantasy", "Goals", "Marks", "Tackles", "Kicks", "Handballs"), selected = "Disposals"),
            numericInput("line_input_corr_b", "Player 2 Line:", value = 25, min = 0, max = 100, step = 0.5),
            selectInput("season_input_corr", "Select Season:", choices = all_player_stats$season_name |> unique(), multiple = TRUE, selectize = TRUE, selected = c("2025", "2024"))
          )
        )
      ),
      column(
        width = 8,
        card(card_body(plotOutput(outputId = "corr_plot_output", height = "800px", width = "100%")))
      )
    )
  ),
  nav_panel(
    title = "SGM",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "match",
          "Select Match",
          choices = matches,
          selected = NULL
        ),
        selectInput(
          "agency_sgm",
          "Select Agency",
          choices = agencies_sgm,
          selected = NULL
        ),
        selectInput(
          "market",
          "Select Market",
          choices = c("Player Disposals", "Player Goals", "Player Marks", "Player Tackles", "Player Fantasy Points", "Player Kicks", "Player Handballs", "Player Hitouts", "Player Clearances"),
          selected = c("Player Disposals", "Player Goals", "Player Marks", "Player Tackles", "Player Fantasy Points", "Player Kicks", "Player Handballs", "Player Hitouts", "Player Clearances"),
          multiple = TRUE
        ),
        selectInput(
          "matchup",
          "Select Difficulty",
          choices = c("Terrible", "Bad", "Neutral", "Good", "Excellent"),
          selected = c("Terrible", "Bad", "Neutral", "Good", "Excellent"),
          multiple = TRUE
        ),
        checkboxInput("best_odds", "Only Show Best Market Odds?", value = FALSE),
        bslib::input_switch(
          "tab_miss_by_one_mode",
          "TAB miss-by-one mode",
          value = FALSE
        ),
        h3("Selections"),
        DT::dataTableOutput("selected"),
        h3("SGM Information"),
        uiOutput("summary"),
        h3("Odds Comparison"),
        actionButton("get_comparison", label = "Compare Odds"),
        actionButton("clear_comparison", label = "Clear Selections"),
        DT::dataTableOutput("odds_compare")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Player List",
            DT::dataTableOutput("table")
          )
        )
      )
    )
  ),
  nav_panel(
    title = "Cross Game Multi",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "agency_cross",
          "Select Agency",
          choices = agencies_sgm,
          selected = NULL
        ),
        selectInput(
          "market_cross",
          "Select Market",
          choices = c("Player Disposals", "Player Goals", "Player Marks", "Player Tackles", "Player Fantasy Points", "Player Kicks", "Player Handballs", "Player Hitouts", "Player Clearances"),
          selected = c("Player Disposals", "Player Goals", "Player Marks", "Player Tackles", "Player Fantasy Points", "Player Kicks", "Player Handballs", "Player Hitouts", "Player Clearances"),
          multiple = TRUE
        ),
        selectInput(
          "matchup_cross",
          "Select Difficulty",
          choices = c("Terrible", "Bad", "Neutral", "Good", "Excellent"),
          selected = c("Terrible", "Bad", "Neutral", "Good", "Excellent"),
          multiple = TRUE
        ),
        checkboxInput("best_odds_cross", "Only Show Best Market Odds?", value = FALSE),
        h3("Selections"),
        DT::dataTableOutput("selected_cross"),
        h3("Multi Information"),
        uiOutput("summary_cross"),
        actionButton("get_comparison_cross", label = "Compare Odds"),
        actionButton("clear_comparison_cross", label = "Clear Selections"),
        DT::dataTableOutput("odds_compare_cross")
      ),
      mainPanel(
        DT::dataTableOutput("table_cross")
      )
    )
  ),
  nav_panel(
    title = "Player Combos",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "match_combos",
          "Select Match",
          choices = matches,
          selected = NULL
        ),
        selectInput(
          "market_filter",
          "Filter by Market",
          choices = c("All", "Player Disposals", "Player Goals", "Player Marks", "Player Tackles", "Player Fantasy Points", "Player Kicks", "Player Handballs", "Player Hitouts", "Player Clearances"),
          selected = "All"
        ),
        sliderInput(
          "price_range",
          "Price Range",
          min = 1,
          max = 1000,
          value = c(1, 1000)
        ),
        actionButton("get_combos", "Get Combinations")
      ),
      mainPanel(
        uiOutput("player_selection_ui"),
        DT::dataTableOutput("combos_table")
      )
    )
  )
)

# ===============================================================================
# Server
# ===============================================================================

server <- function(input, output) {
  bs_themer()
  # =============================================================================
  # Filter player stats
  # =============================================================================

  filtered_player_stats <- reactive({
    # Filter player stats
    filtered_player_stats <-
      all_player_stats |>
      filter(
        player_full_name == input$player_name_input_a,
        season_name %in% input$season_input_a,
        tog_percentage >= input$minutes_minimum,
        margin >= input$margin_min,
        margin <= input$margin_max,
        home_away %in% input$home_status
      ) |>
      arrange(start_time_utc) |>
      mutate(game_number = row_number()) |>
      select(
        Date = start_time_utc,
        Round = round,
        Home = home_team,
        Venue = venue,
        Weather = weather_category,
        Away = away_team,
        Player = player_full_name,
        Team = player_team,
        Opposition = opposition_team,
        Margin = margin,
        TOG = tog_percentage,
        Disposals = disposals,
        Kicks = kicks,
        Handballs = handballs,
        Marks = marks,
        Goals = goals,
        Behinds = behinds,
        Tackles = tackles,
        Hitouts = hitouts,
        Frees_For = frees_for,
        Frees_Against = frees_against,
        Fantasy = fantasy_points,
        CBA = cba_percentage,
        game_number
      ) |>
      arrange(desc(Date))

    # Filter by last n games
    if (!is.na(input$last_games)) {
      filtered_player_stats <-
        filtered_player_stats |>
        slice_head(n = input$last_games)
    }

    # Filter by opposition team
    if (!is.null(input$opp_input_a)) {
      filtered_player_stats <-
        filtered_player_stats |>
        filter(Opposition %in% input$opp_input_a)
    }

    # Filter by weather
    if (!is.null(input$weather_input_a)) {
      filtered_player_stats <-
        filtered_player_stats |>
        filter(Weather %in% input$weather_input_a)
    }

    # Filter by venue
    if (!is.null(input$venue_input_a)) {
      filtered_player_stats <-
        filtered_player_stats |>
        filter(Venue %in% input$venue_input_a)
    }

    # Return filtered player stats
    return(filtered_player_stats)
  })

  # =============================================================================
  # Get Proportion above reference line
  # =============================================================================

  proportion_above_reference_line <- reactive({
    if (input$line_mode == "single") {
      # Single line mode - existing logic
      proportion_above_reference_line <-
        filtered_player_stats() |>
        filter(!!sym(input$stat_input_a) >= input$reference_line) |>
        nrow() / nrow(filtered_player_stats())

      # Get implied Odds
      implied_odds <- 1 / proportion_above_reference_line
      implied_odds_under <- 1 / (1 - proportion_above_reference_line)

      # Get string to output
      output_string <- paste0(
        "Proportion Above Reference Line: ",
        round(proportion_above_reference_line, 2),
        "\n",
        "Implied Odds - Over: ",
        round(implied_odds, 2),
        "\n",
        "Implied Odds - Under: ",
        round(implied_odds_under, 2),
        "\n",
        "Sample Size: ",
        nrow(filtered_player_stats())
      )
    } else {
      # Interval mode - new logic
      req(input$lower_bound, input$upper_bound)

      # Get proportion within interval (between lower and upper bounds)
      proportion_within_interval <-
        filtered_player_stats() |>
        filter(!!sym(input$stat_input_a) > input$lower_bound &
          !!sym(input$stat_input_a) < input$upper_bound) |>
        nrow() / nrow(filtered_player_stats())

      # Get implied odds for interval bet
      implied_odds_interval <- ifelse(proportion_within_interval > 0,
        1 / proportion_within_interval,
        Inf
      )
      implied_odds_outside <- ifelse(proportion_within_interval < 1,
        1 / (1 - proportion_within_interval),
        Inf
      )

      # Get string to output
      output_string <- paste0(
        "Proportion Within Interval (", input$lower_bound, " - ", input$upper_bound, "): ",
        round(proportion_within_interval, 2),
        "\n",
        "Implied Odds - Within Interval: ",
        round(implied_odds_interval, 2),
        "\n",
        "Implied Odds - Outside Interval: ",
        round(implied_odds_outside, 2),
        "\n",
        "Sample Size: ",
        nrow(filtered_player_stats())
      )
    }

    return(output_string)
  })

  # =============================================================================
  # Plot player stats
  # =============================================================================

  output$plot <- renderPlot({
    if (input$line_mode == "single") {
      # Single line mode - existing logic
      df_with_color <- filtered_player_stats() %>%
        mutate(color_condition = ifelse(
          !!sym(input$stat_input_a) >= input$reference_line,
          "limegreen",
          "red1"
        ))

      # Plot player stats
      p <- df_with_color %>%
        ggplot(aes(
          x = game_number,
          y = !!sym(input$stat_input_a),
          color = color_condition
        )) +

        # Basic Elements
        geom_point(size = 4) +
        geom_smooth(
          method = "loess",
          se = FALSE,
          inherit.aes = FALSE,
          mapping = aes(x = game_number, y = !!sym(input$stat_input_a))
        ) +
        geom_hline(
          yintercept = input$reference_line,
          linetype = "dashed",
          color = "grey4",
          size = 1
        ) +

        # Add text
        annotate(
          geom = "text",
          x = 1,
          y = max(filtered_player_stats() %>% pull(!!sym(
            input$stat_input_a
          ))),
          label = proportion_above_reference_line(),
          hjust = 0,
          vjust = 1,
          color = "black",
          size = 6
        )
    } else {
      # Interval mode - new logic
      req(input$lower_bound, input$upper_bound)

      df_with_color <- filtered_player_stats() %>%
        mutate(color_condition = case_when(
          !!sym(input$stat_input_a) > input$lower_bound &
            !!sym(input$stat_input_a) < input$upper_bound ~ "limegreen",
          TRUE ~ "red1"
        ))

      # Plot player stats with interval highlighting
      p <- df_with_color %>%
        ggplot(aes(
          x = game_number,
          y = !!sym(input$stat_input_a),
          color = color_condition
        )) +

        # Basic Elements
        geom_point(size = 4) +
        geom_smooth(
          method = "loess",
          se = FALSE,
          inherit.aes = FALSE,
          mapping = aes(x = game_number, y = !!sym(input$stat_input_a))
        ) +

        # Add shaded rectangle for interval
        annotate(
          "rect",
          xmin = -Inf, xmax = Inf,
          ymin = input$lower_bound, ymax = input$upper_bound,
          alpha = 0.2, fill = "limegreen"
        ) +

        # Add boundary lines
        geom_hline(
          yintercept = input$lower_bound,
          linetype = "dashed",
          color = "darkgreen",
          size = 1
        ) +
        geom_hline(
          yintercept = input$upper_bound,
          linetype = "dashed",
          color = "darkgreen",
          size = 1
        ) +

        # Add text
        annotate(
          geom = "text",
          x = 1,
          y = max(filtered_player_stats() %>% pull(!!sym(
            input$stat_input_a
          ))),
          label = proportion_above_reference_line(),
          hjust = 0,
          vjust = 1,
          color = "black",
          size = 6
        )
    }

    # Common plot elements
    p <- p +
      # Aesthetics
      theme_bw() +
      theme(
        plot.background = element_rect(fill = "white", colour = "white"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      ) +

      # Labels & Titles
      labs(
        title = "",
        x = "Game Number"
      ) +

      # Set manual color scale
      scale_color_identity() +

      # Additional
      theme(legend.position = "none")

    print(p)
  })

  # =============================================================================
  # Table player stats
  # =============================================================================

  output$player_stat_table <- renderDT({
    datatable(
      filtered_player_stats(),
      # Use fillContainer to make the table fill the height from the UI
      fillContainer = TRUE,
      options = list(
        pageLength = 25, # Show more rows by default on a large screen
        autoWidth = TRUE,
        scrollX = TRUE,
        scrollY = TRUE,
        lengthMenu = c(10, 25, 50, 100)
      )
    )
  })

  # =============================================================================
  # Filter team stats
  # =============================================================================

  # Reactive function to filter team stats
  filtered_team_stats <- reactive({
    # Filter team stats
    filtered_team_stats <-
      team_stats |>
      filter(season_name %in% input$season_input_b)

    # Filter by last n games if specified
    if (!is.na(input$last_games_team)) {
      filtered_team_stats <-
        filtered_team_stats |>
        arrange(desc(start_time_utc)) |>
        group_by(home_team) |>
        slice_head(n = input$last_games_team) |>
        ungroup()
    }

    return(filtered_team_stats)
  })

  # Reactive function for team summary statistics
  team_summary_stats <- reactive({
    # Get filtered data
    data <- filtered_team_stats()

    # Create home team stats
    home_stats <- data |>
      select(
        team = home_team,
        goals = home_team_goals,
        behinds = home_team_behinds,
        score = home_team_score,
        disposals = home_team_disposals,
        tackles = home_team_tackles,
        marks = home_team_marks,
        fantasy_points = home_team_fantasy_points,
        opponent_score = away_team_score
      ) |>
      mutate(
        win = score > opponent_score,
        margin = score - opponent_score,
        location = "Home"
      )

    # Create away team stats
    away_stats <- data |>
      select(
        team = away_team,
        goals = away_team_goals,
        behinds = away_team_behinds,
        score = away_team_score,
        disposals = away_team_disposals,
        tackles = away_team_tackles,
        marks = away_team_marks,
        fantasy_points = away_team_fantasy_points,
        opponent_score = home_team_score
      ) |>
      mutate(
        win = score > opponent_score,
        margin = score - opponent_score,
        location = "Away"
      )

    # Combine and summarize
    combined_stats <- bind_rows(home_stats, away_stats) |>
      group_by(team) |>
      summarise(
        Games = n(),
        Wins = sum(win),
        `Win %` = round(mean(win) * 100, 1),
        `Avg Score` = round(mean(score), 1),
        `Avg Opp Score` = round(mean(opponent_score), 1),
        `Avg Margin` = round(mean(margin), 1),
        `Avg Goals` = round(mean(goals), 1),
        `Avg Behinds` = round(mean(behinds), 1),
        `Goal Accuracy %` = round(mean(goals / (goals + behinds)) * 100, 1),
        `Avg Disposals` = round(mean(disposals), 0),
        `Avg Tackles` = round(mean(tackles), 1),
        `Avg Marks` = round(mean(marks), 1),
        `Avg Fantasy` = round(mean(fantasy_points), 0)
      ) |>
      arrange(desc(`Win %`))

    return(combined_stats)
  })

  # Reactive function for home/away splits
  home_away_splits <- reactive({
    data <- filtered_team_stats()

    # Home performance
    home_perf <- data |>
      group_by(team = home_team) |>
      summarise(
        `Home Games` = n(),
        `Home Wins` = sum(home_team_score > away_team_score),
        `Home Win %` = round(mean(home_team_score > away_team_score) * 100, 1),
        `Home Avg Score` = round(mean(home_team_score), 1),
        `Home Avg Fantasy` = round(mean(home_team_fantasy_points), 0)
      )

    # Away performance
    away_perf <- data |>
      group_by(team = away_team) |>
      summarise(
        `Away Games` = n(),
        `Away Wins` = sum(away_team_score > home_team_score),
        `Away Win %` = round(mean(away_team_score > home_team_score) * 100, 1),
        `Away Avg Score` = round(mean(away_team_score), 1),
        `Away Avg Fantasy` = round(mean(away_team_fantasy_points), 0)
      )

    # Combine
    combined <- home_perf |>
      full_join(away_perf, by = "team") |>
      mutate(
        `H/A Win % Diff` = `Home Win %` - `Away Win %`,
        `H/A Score Diff` = `Home Avg Score` - `Away Avg Score`
      ) |>
      arrange(desc(`Home Win %`))

    return(combined)
  })

  # Reactive function for venue performance
  venue_performance <- reactive({
    data <- filtered_team_stats()

    venue_stats <- data |>
      pivot_longer(
        cols = c(home_team, away_team),
        names_to = "home_away",
        values_to = "team"
      ) |>
      mutate(
        score = ifelse(home_away == "home_team", home_team_score, away_team_score),
        opponent_score = ifelse(home_away == "home_team", away_team_score, home_team_score),
        win = score > opponent_score
      ) |>
      group_by(team, venue) |>
      summarise(
        Games = n(),
        Wins = sum(win),
        `Win %` = round(mean(win) * 100, 1),
        `Avg Score` = round(mean(score), 1),
        .groups = "drop"
      ) |>
      filter(Games >= 3) |> # Only show venues with 3+ games
      arrange(team, desc(`Win %`))

    return(venue_stats)
  })

  # Reactive function for weather impact
  weather_impact <- reactive({
    data <- filtered_team_stats()

    weather_stats <- data |>
      pivot_longer(
        cols = c(home_team, away_team),
        names_to = "home_away",
        values_to = "team"
      ) |>
      mutate(
        score = ifelse(home_away == "home_team", home_team_score, away_team_score),
        disposals = ifelse(home_away == "home_team", home_team_disposals, away_team_disposals),
        marks = ifelse(home_away == "home_team", home_team_marks, away_team_marks)
      ) |>
      group_by(team, weather_category) |>
      summarise(
        Games = n(),
        `Avg Score` = round(mean(score), 1),
        `Avg Disposals` = round(mean(disposals), 0),
        `Avg Marks` = round(mean(marks), 1),
        .groups = "drop"
      ) |>
      filter(Games >= 2) |>
      arrange(team, weather_category)

    return(weather_stats)
  })

  # Reactive function for opposition analysis
  opposition_analysis <- reactive({
    data <- filtered_team_stats()

    # long form: every team’s performance vs its opponent ------------------------
    home_vs_opp <- data |>
      transmute(
        team = home_team,
        opposition = away_team,
        disposals = home_team_disposals,
        marks = home_team_marks,
        tackles = home_team_tackles,
        score = home_team_score,
        opp_score = away_team_score
      )

    away_vs_opp <- data |>
      transmute(
        team = away_team,
        opposition = home_team,
        disposals = away_team_disposals,
        marks = away_team_marks,
        tackles = away_team_tackles,
        score = away_team_score,
        opp_score = home_team_score
      )

    opp_stats <- bind_rows(home_vs_opp, away_vs_opp)

    # ---- league-wide aggregate branch -----------------------------------------
    if (isTRUE(input$aggregate_league)) {
      opp_stats |>
        group_by(opposition) |>
        summarise(
          Games = n(),
          Wins = sum(score > opp_score),
          `Win %` = round(mean(score > opp_score) * 100, 1),
          `Avg Disposals` = round(mean(disposals), 0),
          `Total Disposals` = sum(disposals),
          `Avg Marks` = round(mean(marks), 1),
          `Total Marks` = sum(marks),
          `Avg Tackles` = round(mean(tackles), 1),
          `Total Tackles` = sum(tackles),
          `Avg Score` = round(mean(score), 1),
          `Avg Margin` = round(mean(score - opp_score), 1),
          .groups = "drop"
        ) |>
        filter(Games >= input$min_games_opp) |>
        mutate(team = "All Teams") |>
        relocate(team, opposition) |>
        arrange(desc(`Win %`))

      # ---- normal (per-team) branch ---------------------------------------------
    } else {
      # optionally filter to a single opposition
      if (!is.null(input$selected_opposition) && input$selected_opposition != "") {
        opp_stats <- opp_stats |> filter(opposition == input$selected_opposition)
      }

      opp_stats |>
        group_by(team, opposition) |>
        summarise(
          Games = n(),
          Wins = sum(score > opp_score),
          `Win %` = round(mean(score > opp_score) * 100, 1),
          `Avg Disposals` = round(mean(disposals), 0),
          `Total Disposals` = sum(disposals),
          `Avg Marks` = round(mean(marks), 1),
          `Total Marks` = sum(marks),
          `Avg Tackles` = round(mean(tackles), 1),
          `Total Tackles` = sum(tackles),
          `Avg Score` = round(mean(score), 1),
          `Avg Margin` = round(mean(score - opp_score), 1),
          .groups = "drop"
        ) |>
        filter(Games >= input$min_games_opp) |>
        arrange(desc(`Win %`))
    }
  })

  # Reactive function for venue-specific stats
  venue_specific_stats <- reactive({
    data <- filtered_team_stats()

    # long form for every team at every venue ------------------------------------
    home_venue <- data |>
      transmute(
        team = home_team,
        venue,
        disposals = home_team_disposals,
        marks = home_team_marks,
        tackles = home_team_tackles,
        score = home_team_score,
        opp_score = away_team_score
      )

    away_venue <- data |>
      transmute(
        team = away_team,
        venue,
        disposals = away_team_disposals,
        marks = away_team_marks,
        tackles = away_team_tackles,
        score = away_team_score,
        opp_score = home_team_score
      )

    venue_stats <- bind_rows(home_venue, away_venue)

    # ---- league-wide aggregate branch -----------------------------------------
    if (isTRUE(input$aggregate_league)) {
      venue_stats |>
        group_by(venue) |>
        summarise(
          Games = n(),
          Wins = sum(score > opp_score),
          `Win %` = round(mean(score > opp_score) * 100, 1),
          `Avg Disposals` = round(mean(disposals), 0),
          `Total Disposals` = sum(disposals),
          `Avg Marks` = round(mean(marks), 1),
          `Total Marks` = sum(marks),
          `Avg Tackles` = round(mean(tackles), 1),
          `Total Tackles` = sum(tackles),
          `Avg Score` = round(mean(score), 1),
          .groups = "drop"
        ) |>
        filter(Games >= input$min_games_venue) |>
        mutate(team = "All Teams") |>
        relocate(team, venue) |>
        arrange(desc(`Win %`))

      # ---- normal (per-team) branch ---------------------------------------------
    } else {
      # optional filter to a single venue
      if (!is.null(input$selected_venue) && input$selected_venue != "") {
        venue_stats <- venue_stats |> filter(venue == input$selected_venue)
      }

      venue_stats |>
        group_by(team, venue) |>
        summarise(
          Games = n(),
          Wins = sum(score > opp_score),
          `Win %` = round(mean(score > opp_score) * 100, 1),
          `Avg Disposals` = round(mean(disposals), 0),
          `Total Disposals` = sum(disposals),
          `Avg Marks` = round(mean(marks), 1),
          `Total Marks` = sum(marks),
          `Avg Tackles` = round(mean(tackles), 1),
          `Total Tackles` = sum(tackles),
          `Avg Score` = round(mean(score), 1),
          .groups = "drop"
        ) |>
        filter(Games >= input$min_games_venue) |>
        arrange(team, desc(Games))
    }
  })

  # =============================================================================
  # Plots for team stats
  # =============================================================================

  # Team performance over time plot
  output$team_performance_plot <- renderPlot({
    data <- filtered_team_stats()

    # Calculate rolling averages for each team
    team_rolling <- data |>
      pivot_longer(
        cols = c(home_team, away_team),
        names_to = "home_away",
        values_to = "team"
      ) |>
      mutate(
        score = ifelse(home_away == "home_team", home_team_score, away_team_score),
        fantasy = ifelse(home_away == "home_team", home_team_fantasy_points, away_team_fantasy_points)
      ) |>
      arrange(team, start_time_utc) |>
      group_by(team) |>
      mutate(
        game_number = row_number(),
        rolling_avg_score = zoo::rollmean(score, k = 5, fill = NA, align = "right"),
        rolling_avg_fantasy = zoo::rollmean(fantasy, k = 5, fill = NA, align = "right")
      )

    # Create plot based on selected metric
    if (input$team_metric == "Score") {
      p <- team_rolling |>
        ggplot(aes(x = start_time_utc, y = rolling_avg_score, color = team)) +
        geom_line(size = 1.2, alpha = 0.8) +
        geom_point(aes(y = score), alpha = 0.3, size = 2) +
        labs(
          title = "Team Scoring Trends (5-Game Rolling Average)",
          x = "Date",
          y = "Score",
          color = "Team"
        )
    } else {
      p <- team_rolling |>
        ggplot(aes(x = start_time_utc, y = rolling_avg_fantasy, color = team)) +
        geom_line(size = 1.2, alpha = 0.8) +
        geom_point(aes(y = fantasy), alpha = 0.3, size = 2) +
        labs(
          title = "Team Fantasy Points Trends (5-Game Rolling Average)",
          x = "Date",
          y = "Fantasy Points",
          color = "Team"
        )
    }

    p +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 10)
      ) +
      guides(color = guide_legend(nrow = 3))
  })

  # Head-to-head comparison plot
  output$h2h_comparison_plot <- renderPlot({
    data <- filtered_team_stats()

    if (is.null(input$team_a) || is.null(input$team_b)) {
      return(NULL)
    }

    # Get head-to-head matches
    h2h_matches <- data |>
      filter(
        (home_team == input$team_a & away_team == input$team_b) |
          (home_team == input$team_b & away_team == input$team_a)
      ) |>
      mutate(
        team_a_score = ifelse(home_team == input$team_a, home_team_score, away_team_score),
        team_b_score = ifelse(home_team == input$team_b, home_team_score, away_team_score),
        team_a_location = ifelse(home_team == input$team_a, "Home", "Away"),
        winner = case_when(
          team_a_score > team_b_score ~ input$team_a,
          team_b_score > team_a_score ~ input$team_b,
          TRUE ~ "Draw"
        )
      )

    if (nrow(h2h_matches) == 0) {
      plot.new()
      text(0.5, 0.5, "No head-to-head matches found", cex = 1.5)
      return()
    }

    # Create margin plot
    h2h_matches |>
      ggplot(aes(x = start_time_utc, y = team_a_score - team_b_score)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_line(color = "gray70") +
      geom_point(aes(color = winner, shape = team_a_location), size = 4) +
      scale_color_manual(values = c("red", "blue", "gray50")) +
      labs(
        title = paste("Head-to-Head:", input$team_a, "vs", input$team_b),
        subtitle = paste("Total matches:", nrow(h2h_matches)),
        x = "Date",
        y = paste("Margin (", input$team_a, " perspective)", sep = ""),
        color = "Winner",
        shape = paste(input$team_a, "played")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom"
      )
  })

  # =============================================================================
  # Table outputs for team stats
  # =============================================================================

  output$team_summary_table <- renderDT({
    datatable(
      team_summary_stats(),
      fillContainer = TRUE,
      options = list(
        pageLength = 18,
        autoWidth = TRUE,
        scrollX = TRUE,
        scrollY = TRUE
      )
    ) |>
      formatStyle(
        "Win %",
        background = styleColorBar(team_summary_stats()$`Win %`, "lightblue"),
        backgroundSize = "100% 90%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })

  output$home_away_table <- renderDT({
    datatable(
      home_away_splits(),
      fillContainer = TRUE,
      options = list(
        pageLength = 18,
        autoWidth = TRUE,
        scrollX = TRUE,
        scrollY = TRUE
      )
    ) |>
      formatStyle(
        "H/A Win % Diff",
        color = styleInterval(c(-10, 10), c("red", "black", "green")),
        fontWeight = "bold"
      )
  })

  output$venue_table <- renderDT({
    datatable(
      venue_performance(),
      fillContainer = TRUE,
      filter = "top",
      options = list(
        pageLength = 15,
        autoWidth = TRUE,
        scrollX = TRUE,
        scrollY = TRUE
      )
    )
  })

  output$weather_table <- renderDT({
    datatable(
      weather_impact(),
      fillContainer = TRUE,
      filter = "top",
      options = list(
        pageLength = 15,
        autoWidth = TRUE,
        scrollX = TRUE,
        scrollY = TRUE
      )
    )
  })

  output$opposition_table <- renderDT({
    datatable(
      opposition_analysis(),
      fillContainer = TRUE,
      filter = "top",
      options = list(
        pageLength = 15,
        autoWidth = TRUE,
        scrollX = TRUE,
        scrollY = TRUE
      )
    ) |>
      formatStyle(
        "Win %",
        background = styleColorBar(opposition_analysis()$`Win %`, "lightgreen"),
        backgroundSize = "100% 90%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) |>
      formatStyle(
        "Avg Margin",
        color = styleInterval(c(-20, 20), c("red", "black", "green")),
        fontWeight = "bold"
      )
  })

  output$venue_specific_table <- renderDT({
    datatable(
      venue_specific_stats(),
      fillContainer = TRUE,
      filter = "top",
      options = list(
        pageLength = 15,
        autoWidth = TRUE,
        scrollX = TRUE,
        scrollY = TRUE,
        order = list(list(0, "asc"), list(2, "desc")) # Sort by team then games
      )
    ) |>
      formatStyle(
        "Win %",
        background = styleColorBar(venue_specific_stats()$`Win %`, "lightblue"),
        backgroundSize = "100% 90%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })

  # =============================================================================
  # Table Odds
  # =============================================================================

  # Reactive function to scrape odds
  scraped_odds <- reactive({
    # Get odds---------------------------------------------------------------

    # Head to Head
    if (input$market_input == "H2H") {
      odds <-
        h2h_data |>
        filter(match %in% input$match_input) |>
        filter(home_agency %in% input$agency_input & away_agency %in% input$agency_input)
    }

    # Head to Head
    if (input$market_input == "Line") {
      odds <-
        line_data |>
        filter(match %in% input$match_input) |>
        filter(agency %in% input$agency_input)
    }

    # Disposals
    if (input$market_input == "Disposals") {
      odds <-
        player_disposals_data |>
        mutate(variation = round(variation, 2)) |>
        filter(agency %in% input$agency_input) |>
        filter(match %in% input$match_input) |>
        filter(DVP_Category %in% input$matchup_input) |>
        select(-any_of(
          c(
            "match",
            "group_by_header",
            "outcome_name",
            "outcome_name_under",
            "EventKey",
            "MarketKey",
            "OutcomeKey",
            "OutcomeKey_unders"
          )
        ))
    }

    # Goals
    if (input$market_input == "Goals") {
      odds <-
        player_goals_data |>
        mutate(variation = round(variation, 2)) |>
        filter(agency %in% input$agency_input) |>
        filter(match %in% input$match_input) |>
        filter(DVP_Category %in% input$matchup_input) |>
        select(-any_of(
          c(
            "match",
            "group_by_header",
            "outcome_name",
            "outcome_name_under",
            "EventKey",
            "MarketKey",
            "OutcomeKey",
            "OutcomeKey_unders"
          )
        ))
    }

    # Fantasy Points
    if (input$market_input == "Fantasy") {
      odds <-
        player_fantasy_data |>
        mutate(variation = round(variation, 2)) |>
        filter(agency %in% input$agency_input) |>
        filter(match %in% input$match_input) |>
        filter(DVP_Category %in% input$matchup_input) |>
        select(-any_of(
          c(
            "match",
            "group_by_header",
            "outcome_name",
            "outcome_name_under",
            "EventKey",
            "MarketKey",
            "OutcomeKey",
            "OutcomeKey_unders"
          )
        ))
    }

    # Marks
    if (input$market_input == "Marks") {
      odds <-
        player_marks_data |>
        mutate(variation = round(variation, 2)) |>
        filter(agency %in% input$agency_input) |>
        filter(match %in% input$match_input) |>
        filter(DVP_Category %in% input$matchup_input) |>
        select(-any_of(
          c(
            "match",
            "group_by_header",
            "outcome_name",
            "outcome_name_under",
            "EventKey",
            "MarketKey",
            "OutcomeKey",
            "OutcomeKey_unders"
          )
        ))
    }

    # Tackles
    if (input$market_input == "Tackles") {
      odds <-
        player_tackles_data |>
        mutate(variation = round(variation, 2)) |>
        filter(agency %in% input$agency_input) |>
        filter(match %in% input$match_input) |>
        filter(DVP_Category %in% input$matchup_input) |>
        select(-any_of(
          c(
            "match",
            "group_by_header",
            "outcome_name",
            "outcome_name_under",
            "EventKey",
            "MarketKey",
            "OutcomeKey",
            "OutcomeKey_unders"
          )
        ))
    }

    # Kicks
    if (input$market_input == "Kicks") {
      odds <-
        player_kicks_data |>
        mutate(variation = round(variation, 2)) |>
        filter(agency %in% input$agency_input) |>
        filter(match %in% input$match_input) |>
        filter(DVP_Category %in% input$matchup_input) |>
        select(-any_of(
          c(
            "match",
            "group_by_header",
            "outcome_name",
            "outcome_name_under",
            "EventKey",
            "MarketKey",
            "OutcomeKey",
            "OutcomeKey_unders"
          )
        ))
    }

    # Handballs
    if (input$market_input == "Handballs") {
      odds <-
        player_handballs_data |>
        mutate(variation = round(variation, 2)) |>
        filter(agency %in% input$agency_input) |>
        filter(match %in% input$match_input) |>
        filter(DVP_Category %in% input$matchup_input) |>
        select(-any_of(
          c(
            "match",
            "group_by_header",
            "outcome_name",
            "outcome_name_under",
            "EventKey",
            "MarketKey",
            "OutcomeKey",
            "OutcomeKey_unders"
          )
        ))
    }

    # Hitouts
    if (input$market_input == "Hitouts") {
      odds <-
        player_hitouts_data |>
        mutate(variation = round(variation, 2)) |>
        filter(agency %in% input$agency_input) |>
        filter(match %in% input$match_input) |>
        filter(DVP_Category %in% input$matchup_input) |>
        select(-any_of(
          c(
            "match",
            "group_by_header",
            "outcome_name",
            "outcome_name_under",
            "EventKey",
            "MarketKey",
            "OutcomeKey",
            "OutcomeKey_unders"
          )
        ))
    }

    # Clearances
    if (input$market_input == "Clearances") {
      odds <-
        player_clearances_data |>
        mutate(variation = round(variation, 2)) |>
        filter(agency %in% input$agency_input) |>
        filter(match %in% input$match_input) |>
        filter(DVP_Category %in% input$matchup_input) |>
        select(-any_of(
          c(
            "match",
            "group_by_header",
            "outcome_name",
            "outcome_name_under",
            "EventKey",
            "MarketKey",
            "OutcomeKey",
            "OutcomeKey_unders"
          )
        ))
    }


    if (input$only_best == TRUE) {
      odds <-
        odds |>
        arrange(player_name, line, desc(over_price)) |>
        group_by(player_name, line) |>
        slice_head(n = 1) |>
        ungroup()
    }

    if (input$only_best_unders == TRUE) {
      odds <-
        odds |>
        arrange(player_name, line, desc(under_price)) |>
        group_by(player_name, line) |>
        slice_head(n = 1) |>
        ungroup()
    }

    # Odds Range
    if (!is.na(input$odds_minimum)) {
      odds <-
        odds |>
        filter(over_price >= input$odds_minimum)
    }

    if (!is.na(input$odds_maximum)) {
      odds <-
        odds |>
        filter(over_price <= input$odds_maximum)
    }

    if (input$only_unders == TRUE) {
      odds <-
        odds |>
        filter(!is.na(under_price))
    }

    if (input$player_name_input_b != "") {
      odds <-
        odds |>
        filter(str_detect(player_name, input$player_name_input_b))
    }

    # Return odds
    return(odds)
  })

  # Table output
  output$scraped_odds_table <- renderDT({
    datatable(scraped_odds(),
      fillContainer = TRUE,
      filter = "top",
      options = list(
        pageLength = 15,
        autoWidth = FALSE,
        scrollX = TRUE, scrollY = TRUE,
        lengthMenu = c(5, 10, 15, 20, 25, 30)
      )
    )
  })

  # =============================================================================
  # With / Without Teammate
  # =============================================================================

  output$with_without_plot_output <- renderPlot({
    req(input$player_name, input$teammate_name, input$season_input, input$metric_input)

    plot <- compare_performance(
      data = filtered_player_stats_2,
      season = input$season_input,
      name = input$player_name,
      teammate_name = input$teammate_name,
      metric = input$metric_input
    )

    return(plot)
  })

  output$with_without_table_output <- renderDT({
    req(input$player_name, input$teammate_name, input$season_input)

    table <- compare_performance_table(
      data = filtered_player_stats_2,
      season = input$season_input,
      name = input$player_name,
      teammate_name = input$teammate_name
    )

    return(table)
  })

  # =============================================================================
  # Player Correlations
  # =============================================================================

  output$corr_plot_output <- renderPlot({
    req(input$player_name_corr, input$teammate_name_corr, input$season_input_corr, input$metric_input_corr_b, input$metric_input_corr_a)

    plot <-
      get_player_correlation(
        data = filtered_player_stats_2,
        seasons = input$season_input_corr,
        name_a = input$player_name_corr,
        name_b = input$teammate_name_corr,
        metric_a = input$metric_input_corr_a,
        metric_b = input$metric_input_corr_b,
        line_a = input$line_input_corr_a,
        line_b = input$line_input_corr_b
      )

    return(plot)
  })

  # =============================================================================
  # SGM Tab Server Logic
  # =============================================================================

  # For the "SGM" panel
  output$table <- renderDT(
    {
      filtered_data <-
        disposals_display[disposals_display$match == input$match &
          disposals_display$agency == input$agency_sgm &
          disposals_display$Matchup %in% input$matchup &
          disposals_display$market_name %in% input$market, ]

      # Filter for Dabble's specific price requirement in the Player List
      if (input$agency_sgm == "Dabble") {
        filtered_data <- filtered_data |> filter(price == 1.79)
      }

      if (input$best_odds) {
        filtered_data <- filtered_data |>
          filter(market_best) |>
          select(-market_best)
      } else {
        filtered_data <- filtered_data |> select(-next_best_diff)
      }

      datatable(filtered_data, selection = "multiple", filter = "top")
    },
    server = FALSE
  )

  observeEvent(input$table_rows_selected, {
    output$selected <- renderDT({
      if (!is.null(input$table_rows_selected)) {
        filtered_data <-
          disposals_display[disposals_display$match == input$match &
            disposals_display$agency == input$agency_sgm &
            disposals_display$Matchup %in% input$matchup &
            disposals_display$market_name %in% input$market, ]

        # Filter for Dabble's specific price requirement
        if (input$agency_sgm == "Dabble") {
          filtered_data <- filtered_data |> filter(price == 1.79)
        }

        if (input$best_odds) {
          filtered_data <- filtered_data |>
            filter(market_best) |>
            select(-market_best)
        }
        selected_data <- filtered_data[input$table_rows_selected, c("player_name", "line", "market_name", "price")]
        datatable(selected_data)
      }
    })
  })

  # Get the table proxy
  proxy <- dataTableProxy("table")

  # Get the table proxy for the cross game multi
  proxy_cross <- dataTableProxy("table_cross")



  # SGM Comparison
  observeEvent(input$get_comparison, {
    if (is.null(input$table_rows_selected) || length(input$table_rows_selected) == 0) {
      output$odds_compare <- renderDT({
        datatable(tibble())
      })
      return(invisible(NULL))
    }

    # Get selected data
    filtered_data <- disposals_display[disposals_display$match == input$match &
      disposals_display$agency == input$agency_sgm &
      disposals_display$Matchup %in% input$matchup &
      disposals_display$market_name %in% input$market, ]

    # Filter for Dabble's specific price requirement
    if (input$agency_sgm == "Dabble") {
      filtered_data <- filtered_data |> filter(price == 1.79)
    }

    if (input$best_odds) {
      filtered_data <- filtered_data |>
        filter(market_best) |>
        select(-market_best)
    }
    selected_data <- filtered_data[input$table_rows_selected, c("player_name", "type", "line", "market_name", "price")]

    player_names <- selected_data$player_name
    types <- selected_data$type
    lines <- selected_data$line
    market_names <- selected_data$market_name
    comparison_lines <- lines

    if (isTRUE(input$tab_miss_by_one_mode) && identical(input$agency_sgm, "TAB")) {
      comparison_lines <- ifelse(
        market_names == "Player Disposals" & types == "Over",
        lines - 1,
        lines
      )
    }

    # Call function
    comparison_df <- tryCatch(
      {
        compare_sgm(
          player_names = player_names,
          stat_counts = lines,
          non_tab_stat_counts = comparison_lines,
          markets = market_names,
          types = types
        )
      },
      error = function(e) {
        tibble()
      }
    )

    # populate DTOutput
    output$odds_compare <- renderDT({
      datatable(comparison_df)
    })
  })

  # Observe the click event on the "clear_rows" button
  observeEvent(input$clear_comparison, {
    # Deselect all rows in the table
    selectRows(proxy, NULL)
  })

  observeEvent(input$clear_comparison_cross, {
    # Deselect all rows in the table
    selectRows(proxy_cross, NULL)
  })

  output$summary <- renderUI({
    if (!is.null(input$table_rows_selected)) {
      filtered_data <- disposals_display[disposals_display$match == input$match &
        disposals_display$agency == input$agency_sgm &
        disposals_display$Matchup %in% input$matchup &
        disposals_display$market_name %in% input$market, ]

      # Filter for Dabble's specific price requirement
      if (input$agency_sgm == "Dabble") {
        filtered_data <- filtered_data |> filter(price == 1.79)
      }

      if (input$best_odds) {
        filtered_data <- filtered_data |>
          filter(market_best) |>
          select(-market_best)
      }
      selected_data <- filtered_data[input$table_rows_selected, ]
      uncorrelated_price <- prod(selected_data$price)
      empirical_price <- 1 / prod(selected_data$prob_last_10)
      HTML(paste0(
        "<strong>Uncorrelated Price:</strong>", " $", round(uncorrelated_price, 2), "<br/>",
        " <strong>Theoretical Uncorrelated Price:</strong>", " $", round(empirical_price, 2)
      ))
    }
  })

  # =============================================================================
  # Cross Game Multi Tab Server Logic
  # =============================================================================

  # For the "Cross Game Multi" panel
  output$table_cross <- renderDT(
    {
      filtered_data_cross <- disposals_display[disposals_display$agency == input$agency_cross &
        disposals_display$Matchup %in% input$matchup_cross &
        disposals_display$market_name %in% input$market_cross, ]

      if (input$best_odds_cross) {
        filtered_data_cross <- filtered_data_cross |>
          filter(market_best) |>
          select(-market_best)
      }

      datatable(filtered_data_cross, selection = "multiple", filter = "top")
    },
    server = FALSE
  )

  observeEvent(input$table_cross_rows_selected, {
    output$selected_cross <- renderDT({
      if (!is.null(input$table_cross_rows_selected)) {
        filtered_data_cross <- disposals_display[disposals_display$agency == input$agency_cross &
          disposals_display$Matchup %in% input$matchup_cross &
          disposals_display$market_name %in% input$market_cross, ]

        if (input$best_odds_cross) {
          filtered_data_cross <- filtered_data_cross |>
            filter(market_best) |>
            select(-market_best)
        }

        selected_data_cross <- filtered_data_cross[input$table_cross_rows_selected, c("player_name", "line", "market_name", "price")]
        datatable(selected_data_cross)
      }
    })
  })

  # Cross Game Comparison
  observeEvent(input$get_comparison_cross, {
    if (is.null(input$table_cross_rows_selected) || length(input$table_cross_rows_selected) == 0) {
      output$odds_compare_cross <- renderDT({
        datatable(tibble())
      })
      return(invisible(NULL))
    }

    # Get selected data
    filtered_data_cross <- disposals_display[disposals_display$agency == input$agency_cross &
      disposals_display$Matchup %in% input$matchup_cross &
      disposals_display$market_name %in% input$market_cross, ]

    if (input$best_odds_cross) {
      filtered_data_cross <- filtered_data_cross |>
        filter(market_best) |>
        select(-market_best)
    }

    selected_data_cross <- filtered_data_cross[input$table_cross_rows_selected, c("player_name", "type", "line", "market_name", "price", "agency")]

    player_names_cross <- selected_data_cross$player_name
    types_cross <- selected_data_cross$type
    lines_cross <- selected_data_cross$line
    market_names_cross <- selected_data_cross$market_name

    # Call function
    comparison_df_cross <- tryCatch(
      {
        compare_cgm(
          market_names_cross = market_names_cross,
          player_names_cross = player_names_cross,
          lines_cross = lines_cross,
          types_cross = types_cross
        )
      },
      error = function(e) {
        tibble()
      }
    )

    # populate DTOutput
    output$odds_compare_cross <- renderDT({
      datatable(comparison_df_cross)
    })
  })

  output$summary_cross <- renderUI({
    if (!is.null(input$table_cross_rows_selected)) {
      filtered_data_cross <- disposals_display[disposals_display$agency == input$agency_cross &
        disposals_display$Matchup %in% input$matchup_cross &
        disposals_display$market_name %in% input$market_cross, ]

      if (input$best_odds_cross) {
        filtered_data_cross <- filtered_data_cross |>
          filter(market_best) |>
          select(-market_best)
      }

      selected_data_cross <- filtered_data_cross[input$table_cross_rows_selected, ]
      uncorrelated_price_cross <- prod(selected_data_cross$price)
      empirical_price_cross <- 1 / prod(selected_data_cross$prob_2025)
      empirical_price_cross_l10 <- 1 / prod(selected_data_cross$prob_last_10)
      diff <- 1 / empirical_price_cross - 1 / uncorrelated_price_cross
      diff_l10 <- 1 / empirical_price_cross_l10 - 1 / uncorrelated_price_cross
      HTML(
        paste0(
          "<strong>Multi Price:</strong>", " $", round(uncorrelated_price_cross, 2), "<br/>",
          " <strong>Theoretical Multi Price:</strong>", " $", round(empirical_price_cross, 2), "<br/>",
          " <strong>Edge L10:</strong>", " ", round(100 * diff_l10, 3), "%"
        ), "<br/>",
        " <strong>Edge 2025:</strong>", " ", round(100 * diff, 3), "%"
      )
    }
  })

  # =============================================================================
  # Player Combos Tab Server Logic
  # =============================================================================

  # For the "Player Combos" panel
  output$player_selection_ui <- renderUI({
    DT::dataTableOutput("player_table_combos")
  })

  output$player_table_combos <- renderDT({
    filtered_data <- disposals_display |>
      filter(match == input$match_combos) |>
      distinct(player_name, .keep_all = TRUE) |>
      select(player_name, Position, Matchup)

    datatable(filtered_data, selection = "multiple", options = list(pageLength = 10))
  })

  observeEvent(input$get_combos, {
    # Get selected players
    selected_rows <- input$player_table_combos_rows_selected

    if (is.null(selected_rows) || length(selected_rows) < 2 || length(selected_rows) > 3) {
      output$combos_table <- renderDT({
        datatable(data.frame(Message = "Please select 2 or 3 players."))
      })
      return()
    }

    filtered_data <- disposals_display |>
      filter(match == input$match_combos) |>
      distinct(player_name, .keep_all = TRUE) |>
      select(player_name, Position, Matchup)

    selected_players <- filtered_data[selected_rows, ]$player_name

    # Get market filter
    market_filter <- if (input$market_filter == "All") NULL else input$market_filter

    # Get player data
    player_data <- disposals_display |>
      filter(match == input$match_combos)

    # Get combos
    combos_df <- get_player_combos(player_data, selected_players, market_filter)

    # Filter by price range
    if (nrow(combos_df) > 0 && "Message" %notin% names(combos_df)) {
      # Find agency columns - they are the ones that are not Match or Selections
      agency_cols <- setdiff(names(combos_df), c("Match", "Selections"))

      # Get max price across all agencies
      combos_df$max_price <- do.call(pmax, c(combos_df[agency_cols], na.rm = TRUE))

      combos_df <- combos_df |>
        filter(max_price >= input$price_range[1] & max_price <= input$price_range[2]) |>
        select(-max_price)
    }

    # Display table
    output$combos_table <- renderDT({
      if (nrow(combos_df) > 0 && "Message" %notin% names(combos_df)) {
        agency_cols <- setdiff(names(combos_df), c("Match", "Selections"))
        agency_indices <- which(names(combos_df) %in% agency_cols)

        datatable(
          combos_df,
          escape = FALSE,
          options = list(
            pageLength = 10,
            columnDefs = list(list(targets = 1, render = JS(
              "function(data, type, row, meta){",
              "  if(type === 'display'){ return data.replace(/, /g, '<br/>'); }",
              "  return data;",
              "}"
            ))),
            rowCallback = JS(
              "function(row, data) {",
              sprintf("var agency_indices = [%s];", paste(agency_indices, collapse = ",")),
              "var prices = [];",
              "agency_indices.forEach(function(i) {",
              "  var price = parseFloat(data[i-1]);",
              "  if (!isNaN(price)) { prices.push(price); }",
              "});",
              "if (prices.length > 0) {",
              "  var max_price = Math.max.apply(null, prices);",
              "  agency_indices.forEach(function(i) {",
              "    var cell_value = parseFloat(data[i-1]);",
              "    if (cell_value === max_price) {",
              "      $('td', row).eq(i-1).css('background-color', 'rgba(144, 238, 144, 0.5)');",
              "    }",
              "  });",
              "}",
              "}"
            )
          )
        )
      } else {
        datatable(combos_df)
      }
    })
  })
}

# ===============================================================================
# Run App
# ===============================================================================

shinyApp(ui, server)
