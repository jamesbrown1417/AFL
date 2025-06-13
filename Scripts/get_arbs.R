# library(shiny)
library(tidyverse)

#===============================================================================
# Read in Data
#===============================================================================

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

# Get Start Times
start_times <-
  all_odds_files |>
  select(match, start_time) |>
  distinct(match, .keep_all = TRUE)

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
  arrange(margin)

##%######################################################%##
#                                                          #
####                   Player Disposals                 ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_disposals <-
  list.files("Data/scraped_odds",
             full.names = TRUE,
             pattern = "player_disposals") |>
  map(read_csv) |>
  # Ignore null elements
  keep( ~ nrow(.x) > 0) |>
  map(~select(.x, -matches("id"))) |>
  reduce(bind_rows) |>
  arrange(player_name, line, desc(over_price))

##%######################################################%##
#                                                          #
####              Player Fantasy Points                 ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_fantasy_points <-
  list.files("Data/scraped_odds",
             full.names = TRUE,
             pattern = "player_fantasy_points") |>
  map(read_csv, num_threads = 12 ) |>
  # Make line and over_price numeric
  map(~mutate(.x, line = as.numeric(line),
              over_price = as.numeric(over_price))) |>
  # Ignore null elements
  # keep( ~ nrow(.x) > 0) |>
  map(~select(.x, -matches("id"))) |>
  reduce(bind_rows)

##%######################################################%##
#                                                          #
####                   Player Goals                     ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_goals <-
  list.files("Data/scraped_odds",
             full.names = TRUE,
             pattern = "player_goals") |>
  map(read_csv) |>
  # Ignore null elements
  keep( ~ nrow(.x) > 0) |>
  map(~select(.x, -matches("id"))) |>
  reduce(bind_rows) |>
  arrange(player_name, line, desc(over_price))

##%######################################################%##
#                                                          #
####                   Player Marks                     ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_marks <-
  list.files("Data/scraped_odds",
             full.names = TRUE,
             pattern = "player_marks") |>
  # Keep files more than 200 bytes in size
  keep(~file.info(.x)$size > 200) |>
  map(read_csv) |>
  
  # Ignore null elements
  # keep( ~ nrow(.x) > 0) |>
  map(~select(.x, -matches("id"))) |>
  reduce(bind_rows) |>
  arrange(player_name, line, desc(over_price))

##%######################################################%##
#                                                          #
####                   Player Tackles                   ####
#                                                          #
##%######################################################%##

# Get all scraped odds files and combine
all_player_tackles <-
  list.files("Data/scraped_odds",
             full.names = TRUE,
             pattern = "player_tackles") |>
  # Keep files more than 200 bytes in size
  keep(~file.info(.x)$size > 200) |>
  map(read_csv) |>
  # Ignore null elements
  # keep( ~ nrow(.x) > 0) |>
  map(~select(.x, -matches("id"))) |>
  reduce(bind_rows) |>
  arrange(player_name, line, desc(over_price))


##%######################################################%##
#                                                          #
####   Get all over under comparisons of same market    ####
#                                                          #
##%######################################################%##

# H2H---------------------------------------------------------------------------
h2h_arbs <-
  all_odds_h2h |> 
  mutate(margin = -1*margin) |> 
  filter(margin > 0)

# Disposals------------------------------------------------------------------------
disposals_unders <-
  all_player_disposals |>
  filter(market_name == "Player Disposals") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    under_line = line,
    under_price,
    opposition_team,
    agency
  ) |>
  filter(!is.na(under_price)) |>
  rename(under_agency = agency)

disposals_overs <-
  all_player_disposals |>
  filter(market_name == "Player Disposals") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    over_line = line,
    over_price,
    opposition_team,
    agency
  ) |>
  rename(over_agency = agency)

disposals_arbs <-
  disposals_unders |>
  inner_join(
    disposals_overs,
    by = c(
      "match",
      "market_name",
      "player_name",
      "player_team",
      "opposition_team"
    ),
    relationship = "many-to-many"
  ) |>
  filter(under_line >= over_line) |>
  relocate(under_price, .after = over_price) |>
  mutate(margin = 1 / under_price + 1 / over_price) |>
  arrange(margin) |>
  mutate(margin = (1 - margin)) |>
  mutate(margin = 100 * margin) |>
  # filter(margin > 0) |>
  distinct(match, player_name, over_line, under_line, over_agency, under_agency, .keep_all = TRUE) |>
  relocate(over_price, over_agency, under_price, under_agency, .after = opposition_team)

# Fantasy Points----------------------------------------------------------------------
fantasy_points_unders <-
  all_player_fantasy_points |>
  filter(market_name == "Player Fantasy Points") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    under_line = line,
    under_price,
    opposition_team,
    agency
  ) |>
  filter(!is.na(under_price)) |>
  rename(under_agency = agency)

fantasy_points_overs <-
  all_player_fantasy_points |>
  filter(market_name == "Player Fantasy Points") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    over_line = line,
    over_price,
    opposition_team,
    agency
  ) |>
  rename(over_agency = agency)

fantasy_points_arbs <-
  fantasy_points_unders |>
  inner_join(
    fantasy_points_overs,
    by = c(
      "match",
      "market_name",
      "player_name",
      "player_team",
      "opposition_team"
    ),
    relationship = "many-to-many"
  ) |>
  filter(under_line >= over_line) |>
  relocate(under_price, .after = over_price) |>
  mutate(under_price = as.numeric(under_price)) |>
  mutate(over_price = as.numeric(over_price)) |>
  mutate(margin = 1 / under_price + 1 / over_price) |>
  arrange(margin) |>
  mutate(margin = (1 - margin)) |>
  mutate(margin = 100 * margin) |>
  # filter(margin > 0) |>
  distinct(match, player_name, over_line, under_line, over_agency, under_agency, .keep_all = TRUE) |>
  relocate(over_price, over_agency, under_price, under_agency, .after = opposition_team)

# Goals-------------------------------------------------------------------------
goals_unders <-
  all_player_goals |>
  filter(market_name == "Player Goals") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    under_line = line,
    under_price,
    opposition_team,
    agency
  ) |>
  filter(!is.na(under_price)) |>
  rename(under_agency = agency)

goals_overs <-
  all_player_goals |>
  filter(market_name == "Player Goals") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    over_line = line,
    over_price,
    opposition_team,
    agency
  ) |>
  rename(over_agency = agency)

goals_arbs <-
  goals_unders |>
  inner_join(
    goals_overs,
    by = c(
      "match",
      "market_name",
      "player_name",
      "player_team",
      "opposition_team"
    ),
    relationship = "many-to-many"
  ) |>
  filter(under_line >= over_line) |>
  relocate(under_price, .after = over_price) |>
  mutate(under_price = as.numeric(under_price)) |>
  mutate(over_price = as.numeric(over_price)) |>
  mutate(margin = 1 / under_price + 1 / over_price) |>
  arrange(margin) |>
  mutate(margin = (1 - margin)) |>
  mutate(margin = 100 * margin) |>
  # filter(margin > 0) |>
  distinct(match, player_name, over_line, under_line, over_agency, under_agency, .keep_all = TRUE) |>
  relocate(over_price, over_agency, under_price, under_agency, .after = opposition_team)

# Marks-------------------------------------------------------------------------
marks_unders <-
  all_player_marks |>
  filter(market_name == "Player Marks") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    under_line = line,
    under_price,
    opposition_team,
    agency
  ) |>
  filter(!is.na(under_price)) |>
  rename(under_agency = agency)

marks_overs <-
  all_player_marks |>
  filter(market_name == "Player Marks") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    over_line = line,
    over_price,
    opposition_team,
    agency
  ) |>
  rename(over_agency = agency)

marks_arbs <-
  marks_unders |>
  inner_join(
    marks_overs,
    by = c(
      "match",
      "market_name",
      "player_name",
      "player_team",
      "opposition_team"
    ),
    relationship = "many-to-many"
  ) |>
  filter(under_line >= over_line) |> 
  relocate(under_price, .after = over_price) |>
  mutate(under_price = as.numeric(under_price)) |>
  mutate(over_price = as.numeric(over_price)) |>
  mutate(margin = 1 / under_price + 1 / over_price) |>
  arrange(margin) |>
  mutate(margin = (1 - margin)) |>
  mutate(margin = 100 * margin) |>
  # filter(margin > 0) |>
  distinct(match, player_name, over_line, under_line, over_agency, under_agency, .keep_all = TRUE) |>
  relocate(over_price, over_agency, under_price, under_agency, .after = opposition_team)

# Tackles------------------------------------------------------------------------
tackles_unders <-
  all_player_tackles |>
  filter(market_name == "Player Tackles") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    under_line = line,
    under_price,
    opposition_team,
    agency
  ) |>
  filter(!is.na(under_price)) |>
  rename(under_agency = agency)

tackles_overs <-
  all_player_tackles |>
  filter(market_name == "Player Tackles") |>
  select(
    match,
    market_name,
    player_name,
    player_team,
    over_line = line,
    over_price,
    opposition_team,
    agency
  ) |>
  rename(over_agency = agency)

tackles_arbs <-
  tackles_unders |>
  inner_join(
    tackles_overs,
    by = c(
      "match",
      "market_name",
      "player_name",
      "player_team",
      "opposition_team"
    ),
    relationship = "many-to-many"
  ) |>
  filter(under_line >= over_line) |>
  relocate(under_price, .after = over_price) |>
  mutate(under_price = as.numeric(under_price)) |>
  mutate(over_price = as.numeric(over_price)) |>
  mutate(margin = 1 / under_price + 1 / over_price) |>
  arrange(margin) |>
  mutate(margin = (1 - margin)) |>
  mutate(margin = 100 * margin) |>
  # filter(margin > 0) |>
  distinct(match, player_name, over_line, under_line, over_agency, under_agency, .keep_all = TRUE) |>
  relocate(over_price, over_agency, under_price, under_agency, .after = opposition_team)

#===============================================================================
# Get all ARBs together
#===============================================================================

all_arbs <-
  list(
    disposals_arbs,
    fantasy_points_arbs,
    goals_arbs,
    marks_arbs,
    tackles_arbs
  ) |>
  # Keep if nrow of dataframe greater than 0
  keep(~nrow(.x) > 0) |>
  reduce(bind_rows) |>
  arrange(desc(margin)) |> 
  filter(!is.na(player_name))

# H2H Arbs
h2h_arbs
all_arbs

# Save all arbs
write_csv(all_arbs, "Data/all_arbs.csv")
