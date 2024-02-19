# Libraries and functions-------------------------------------------------------
library(tidyverse)
library(googlesheets4)
library(googledrive)

# Get empirical probability function
# source("Scripts/get_empirical_probabilities.R")

# Get fixtures data
current_season_fixture <- read_rds("Data/current_fixture.rds")

# Google sheets authentification -----------------------------------------------
options(gargle_oauth_cache = ".secrets")
drive_auth(cache = ".secrets", email = "cuzzy.punting@gmail.com")
gs4_auth(token = drive_token())

# # Run all odds scraping scripts-----------------------------------------------
run_scraping <- function(script_name) {
  tryCatch({
    source(script_name)
  }, error = function(e) {
    cat("Odds not released yet for:", script_name, "\n")
  })
}

# Run all odds scraping scripts
# run_scraping("OddsScraper/scrape_betr.R")
# run_scraping("OddsScraper/scrape_BetRight.R")
# run_scraping("OddsScraper/scrape_Palmerbet.R")
run_scraping("OddsScraper/scrape_pointsbet.R")
run_scraping("OddsScraper/scrape_sportsbet.R")
run_scraping("OddsScraper/scrape_TAB.R")
# run_scraping("OddsScraper/scrape_TopSport.R")
run_scraping("OddsScraper/scrape_bet365.R")
# run_scraping("OddsScraper/scrape_bluebet.R")
run_scraping("OddsScraper/scrape_neds.R")
# run_scraping("OddsScraper/scrape_unibet.R")

##%######################################################%##
#                                                          #
####                    Head to Head                    ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_odds_files <-
  list.files("Data/scraped_odds", full.names = TRUE, pattern = "h2h") |>
  map(read_csv) |>
  # Keep if nrow of dataframe greater than 0
  keep(~nrow(.x) > 0) |>
  reduce(bind_rows)

# For each match, get all home wins
all_home <-
  all_odds_files |>
  arrange(match, start_time, desc(home_win)) |>
  select(match, start_time, market_name, home_team, home_win, home_agency = agency) |> 
  mutate(start_time = date(start_time)) |> 
  select(-start_time)

# For each match, get all away wins
all_away <-
  all_odds_files |>
  arrange(match, start_time, desc(away_win)) |>
  select(match, start_time, market_name, away_team, away_win, away_agency = agency) |> 
  mutate(start_time = date(start_time)) |> 
  select(-start_time)

# Combine
all_odds_h2h <-
  all_home |>
  full_join(all_away, relationship = "many-to-many", by = c("match", "market_name")) |>
  mutate(margin = (1/home_win + 1/away_win)) |> 
  mutate(margin = round(100*(margin - 1), digits = 3)) |> 
  arrange(margin) |>
  left_join(current_season_fixture) |>
  relocate(round, start_time, venue, .after = match)

# Google Sheets-----------------------------------------------------
# sheet <- gs4_find("AFL Data")
# sheet_write(sheet, data = all_odds_h2h, sheet = "H2H")

# Write as RDS
all_odds_h2h |> write_rds("Data/processed_odds/all_h2h.rds")

##%######################################################%##
#                                                          #
####                      Line                          ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_odds_files <-
  list.files("Data/scraped_odds", full.names = TRUE, pattern = "line") |>
  map(read_csv) |>
  # Keep if nrow of dataframe greater than 0
  keep(~nrow(.x) > 0) |>
  reduce(bind_rows)

# Arrange to get biggest discrepancies
all_odds_line <-
  all_odds_files |>
  arrange(match, start_time, home_line) |>
  group_by(match, home_team, away_team) |> 
  mutate(min_home_line = min(home_line), max_home_line = max(home_line)) |>
  mutate(diff = max_home_line - min_home_line) |>
  ungroup() |>
  arrange(desc(diff), start_time) |> 
  select(-min_home_line, -max_home_line)

# Google Sheets-----------------------------------------------------
# sheet <- gs4_find("AFL Data")
# sheet_write(sheet, data = all_odds_line, sheet = "Line")

# Write as RDS
all_odds_line |> write_rds("Data/processed_odds/all_line.rds")

##%######################################################%##
#                                                          #
####                 Player Disposals                   ####
#                                                          #
##%######################################################%##

# # Get all scraped odds files and combine
# all_player_disposals <-
#   list.files("Data/scraped_odds", full.names = TRUE, pattern = "Player Disposals") |>
#   map(read_csv) |>
#   # Ignore null elements
#   keep(~nrow(.x) > 0) |>
#   reduce(bind_rows) |> 
#   arrange(player_name, line, desc(over_price)) |> 
#   select(-matches("id"))
# 
# # Add empirical probabilities---------------------------------------------------
# 
# # Disposals
# distinct_point_combos <-
#   all_player_disposals |> 
#   distinct(player_name, line)
# 
# player_emp_probs_2023 <-
#   pmap(distinct_point_combos, get_empirical_prob, "Disposals", "2023", .progress = TRUE) |> 
#   bind_rows() |> 
#   select(player_name, line, games_played_2023 = games_played, empirical_prob_2023)
# 
# player_emp_probs_2024 <- 
#   pmap(distinct_point_combos, get_empirical_prob, "Disposals", "2024", .progress = TRUE) |> 
#   bind_rows() |> 
#   select(player_name, line, games_played_2024 = games_played, empirical_prob_2024, empirical_prob_last_10)
# 
# all_player_disposals <-
#   all_player_disposals |>
#   mutate(
#     implied_prob_over = 1 / over_price,
#     implied_prob_under = 1 / under_price
#   ) |>
#   left_join(player_emp_probs_2022_23, by = c("player_name", "line")) |>
#   left_join(player_emp_probs_2023_24, by = c("player_name", "line")) |>
#   rename(empirical_prob_over_2022_23 = empirical_prob_2023,
#          empirical_prob_over_2023_24 = empirical_prob_2024) |>
#   mutate(empirical_prob_under_2022_23 = 1 - empirical_prob_over_2022_23,
#          empirical_prob_under_2023_24 = 1 - empirical_prob_over_2023_24) |>
#   mutate(
#     diff_over_2022_23 = empirical_prob_over_2022_23 - implied_prob_over,
#     diff_under_2022_23 = empirical_prob_under_2022_23 - implied_prob_under,
#     diff_over_2023_24 = empirical_prob_over_2023_24 - implied_prob_over,
#     diff_under_2023_24 = empirical_prob_under_2023_24 - implied_prob_under,
#     diff_over_last_10 = empirical_prob_last_10 - implied_prob_over,
#     diff_under_last_10 = (1 - empirical_prob_last_10) - implied_prob_under
#   ) |>
#   relocate(agency, .after = diff_under_last_10) |>
#   mutate_if(is.double, round, 2) |>
#   filter(!is.na(opposition_team)) |>
#   left_join(NBA_schedule, by = "match") |>
#   relocate(start_time, .after = match) |>
#   filter(match %in% next_week_games$match) |> 
#   group_by(player_name, line) |>
#   mutate(
#     min_implied_prob = min(implied_prob_over, na.rm = TRUE),
#     max_implied_prob = max(implied_prob_over, na.rm = TRUE)
#   ) |>
#   mutate(variation = max_implied_prob - min_implied_prob) |>
#   ungroup() |>
#   select(-min_implied_prob,-max_implied_prob) |>
#   arrange(desc(variation), player_name, desc(over_price), line)
# 
# # Add to google sheets
# sheet_write(sheet, data = all_player_disposals, sheet = "Player Disposals")
# 
# # Write as RDS
# all_player_disposals |> write_rds("Data/processed_odds/all_player_disposals.rds")
