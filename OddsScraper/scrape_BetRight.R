# Libraries
library(tidyverse)
library(rvest)
library(httr2)
library(glue)

# Player names file
player_names <- read_rds("Data/2026_start_positions_and_prices.rds")
player_names <- player_names |> select(player_full_name, team_name)

# Function to fix team names
source("Functions/fix_team_names.R")

BETRIGHT_CLIENT_ID <- "TBvFnuNgkJi1i3nzHE7pD2"
BETRIGHT_API_BASE <- "https://next-api.betright.com.au"

value_or_na <- function(value, default = NA) {
  if (is.null(value) || length(value) == 0) {
    default
  } else {
    value
  }
}

betright_get_json <- function(url) {
  request(url) |>
    req_headers(
      "X-Client-Id" = BETRIGHT_CLIENT_ID,
      "Accept" = "application/json, text/plain, */*",
      "Origin" = "https://www.betright.com.au",
      "Referer" = "https://www.betright.com.au/"
    ) |>
    req_retry(max_tries = 3) |>
    req_perform() |>
    resp_body_json()
}

clean_player_name <- function(player_name) {
  case_when(
    str_detect(player_name, "Nicholas Daicos") ~ "Nick Daicos",
    str_detect(player_name, "Lachlan Schultz") ~ "Lachie Schultz",
    str_detect(player_name, "Callum Brown") ~ "Callum M. Brown",
    str_detect(player_name, "Harrison Himmelberg") ~ "Harry Himmelberg",
    str_detect(player_name, "Ashley Johnson") ~ "Ash Johnson",
    .default = player_name
  )
}

team_from_abbrev <- function(abbrev) {
  case_when(
    abbrev == "ADEL" ~ "Adelaide Crows",
    abbrev == "BL" ~ "Brisbane Lions",
    abbrev == "CARL" ~ "Carlton",
    abbrev == "COLL" ~ "Collingwood Magpies",
    abbrev == "ESS" ~ "Essendon Bombers",
    abbrev == "FRE" ~ "Fremantle Dockers",
    abbrev == "GEEL" ~ "Geelong Cats",
    abbrev %in% c("GC", "GCFC") ~ "Gold Coast Suns",
    abbrev == "GWS" ~ "GWS Giants",
    abbrev == "HAW" ~ "Hawthorn Hawks",
    abbrev == "MELB" ~ "Melbourne Demons",
    abbrev %in% c("NM", "NMFC") ~ "North Melbourne Kangaroos",
    abbrev == "PORT" ~ "Port Adelaide Power",
    abbrev == "RICH" ~ "Richmond Tigers",
    abbrev == "STK" ~ "St Kilda Saints",
    abbrev == "SYD" ~ "Sydney Swans",
    abbrev == "WB" ~ "Western Bulldogs",
    abbrev == "WCE" ~ "West Coast Eagles",
    TRUE ~ NA_character_
  )
}

empty_player_prop <- function() {
  tibble(
    match = character(),
    home_team = character(),
    away_team = character(),
    market_name = character(),
    player_name = character(),
    player_team = character(),
    line = numeric(),
    over_price = numeric(),
    under_price = numeric(),
    agency = character(),
    opposition_team = character(),
    group_by_header = character(),
    event_id = integer(),
    outcome_name = character(),
    outcome_id = integer(),
    fixed_market_id = integer()
  )
}

#===============================================================================
# Category markets
#===============================================================================

betright_url <- glue("{BETRIGHT_API_BASE}/Sports/Category?categoryId=79")

betright_response <- betright_get_json(betright_url)

matches <- betright_response$masterCategories[[1]]$categories[[1]]$masterEvents

matches <-
  matches |>
  keep(~ .x$masterEventClassName == "Matches")

if (length(matches) == 0) {
  stop("BetRight AFL category response did not contain any match events.")
}

get_market_info <- function(market_record) {
  tibble(
    event_id = value_or_na(market_record$eventId, NA_integer_),
    market = value_or_na(market_record$marketDesc, NA_character_),
    propositions = value_or_na(market_record$outcomeName, NA_character_),
    prices = value_or_na(market_record$price, NA_real_),
    handicaps = value_or_na(market_record$points, NA_real_)
  )
}

get_match_info <- function(match_record) {
  market_info <- map(match_record$markets, get_market_info) |> bind_rows()

  tibble(
    match = match_record$masterEventName,
    match_id = match_record$masterEventId,
    start_time = match_record$minAdvertisedStartTime,
    event_id = market_info$event_id,
    market_name = market_info$market,
    propositions = market_info$propositions,
    prices = market_info$prices,
    handicaps = market_info$handicaps
  )
}

all_betright_markets <-
  map(matches, get_match_info) |> bind_rows()

#===============================================================================
# Head to head markets
#===============================================================================

home_teams <-
  all_betright_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(str_detect(market_name, "Win")) |>
  mutate(market_name = "Head To Head") |>
  group_by(match) |>
  filter(row_number() == 1) |>
  ungroup() |>
  transmute(match, match_id, start_time, market_name, home_team, away_team, home_win = prices)

away_teams <-
  all_betright_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(str_detect(market_name, "Win")) |>
  mutate(market_name = "Head To Head") |>
  group_by(match) |>
  filter(row_number() == 2) |>
  ungroup() |>
  transmute(match, match_id, start_time, market_name, home_team, away_team, away_win = prices)

betright_head_to_head_markets <-
  home_teams |>
  left_join(
    away_teams,
    by = c("match", "match_id", "start_time", "market_name", "home_team", "away_team")
  ) |>
  mutate(
    home_team = fix_team_names(home_team),
    away_team = fix_team_names(away_team),
    match = paste(home_team, "v", away_team),
    margin = round((1 / home_win + 1 / away_win), digits = 3),
    agency = "BetRight"
  ) |>
  select(match, start_time, market_name, home_team, home_win, away_team, away_win, margin, agency)

write_csv(betright_head_to_head_markets, "Data/scraped_odds/betright_h2h.csv")

#===============================================================================
# Line Markets
#===============================================================================

home_teams_line <-
  all_betright_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(str_detect(market_name, "Handicap")) |>
  mutate(market_name = "Line") |>
  group_by(match) |>
  filter(row_number() == 1) |>
  ungroup() |>
  transmute(
    match,
    match_id,
    start_time,
    market_name,
    home_team,
    away_team,
    home_win = prices,
    home_line = handicaps
  )

away_teams_line <-
  all_betright_markets |>
  separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  filter(str_detect(market_name, "Handicap")) |>
  mutate(market_name = "Line") |>
  group_by(match) |>
  filter(row_number() == 2) |>
  ungroup() |>
  transmute(
    match,
    match_id,
    start_time,
    market_name,
    home_team,
    away_team,
    away_win = prices,
    away_line = handicaps
  )

betright_line_markets <-
  home_teams_line |>
  left_join(
    away_teams_line,
    by = c("match", "match_id", "start_time", "market_name", "home_team", "away_team")
  ) |>
  mutate(
    home_team = fix_team_names(home_team),
    away_team = fix_team_names(away_team),
    match = paste(home_team, "v", away_team),
    margin = round((1 / home_win + 1 / away_win), digits = 3),
    agency = "BetRight"
  ) |>
  select(
    match,
    start_time,
    market_name,
    home_team,
    home_win,
    home_line,
    away_team,
    away_win,
    away_line,
    margin,
    agency
  )

write_csv(betright_line_markets, "Data/scraped_odds/betright_line.csv")

#===============================================================================
# Player Props
#===============================================================================

fetch_master_event_events <- function(match_id) {
  betright_get_json(glue("{BETRIGHT_API_BASE}/Sports/MasterEventEvents?masterEventId={match_id}"))
}

flatten_master_event_events <- function(response) {
  group_lookup <-
    map_dfr(response$groupLinks, function(group_link) {
      tibble(
        group_type_code = value_or_na(group_link$groupTypeCode, NA_character_),
        group_name = value_or_na(group_link$groupName, NA_character_)
      )
    })

  event_rows <-
    map_dfr(response$events, function(event) {
      if (is.null(event$outcomes) || length(event$outcomes) == 0) {
        return(tibble())
      }

      map_dfr(event$outcomes, function(outcome) {
        tibble(
          match = response$masterEvent$masterEventName,
          match_id = response$masterEvent$masterEventId,
          outcome_title = value_or_na(event$eventName, NA_character_),
          event_id = value_or_na(event$eventId, NA_integer_),
          group_type_code = value_or_na(event$groupTypeCode, NA_character_),
          outcome_name = value_or_na(outcome$outcomeName, NA_character_),
          outcome_id = value_or_na(outcome$outcomeId, NA_integer_),
          fixed_market_id = value_or_na(outcome$fixedMarketId, NA_integer_),
          market_type_code = value_or_na(outcome$marketTypeCode, NA_character_),
          group_by_header = value_or_na(outcome$groupByHeader, NA_character_),
          points = value_or_na(outcome$points, NA_real_),
          price = value_or_na(outcome$price, NA_real_)
        )
      })
    })

  if (nrow(event_rows) == 0) {
    return(event_rows)
  }

  event_rows |>
    left_join(group_lookup, by = "group_type_code") |>
    mutate(group_by_header = coalesce(group_by_header, group_name, outcome_title))
}

safe_fetch_master_event_events <- safely(fetch_master_event_events)

master_event_results <-
  unique(all_betright_markets$match_id) |>
  map(safe_fetch_master_event_events)

master_event_errors <-
  master_event_results |>
  keep(~ !is.null(.x$error)) |>
  map_chr(~ conditionMessage(.x$error))

if (length(master_event_errors) > 0) {
  warning(
    "Some BetRight MasterEventEvents requests failed: ",
    paste(master_event_errors, collapse = " | ")
  )
}

betright_player_stats <-
  master_event_results |>
  map("result") |>
  compact() |>
  map(flatten_master_event_events) |>
  bind_rows()

build_player_prop_market <- function(prop_stats, event_pattern, market_label) {
  if (nrow(prop_stats) == 0) {
    return(empty_player_prop())
  }

  prop_stats |>
    filter(str_detect(outcome_title, regex(event_pattern, ignore_case = TRUE))) |>
    mutate(
      player_name = str_remove(outcome_name, "\\s+\\d+\\+$"),
      player_name = clean_player_name(player_name),
      player_team_from_abbrev = team_from_abbrev(str_match(outcome_title, "\\(([^()]*)\\)\\s*$")[, 2]),
      line = as.numeric(str_extract(outcome_name, "\\d+(?=\\+)")) - 0.5,
      over_price = price
    ) |>
    filter(!is.na(player_name), !is.na(line), !is.na(over_price)) |>
    select(
      match,
      player_name,
      player_team_from_abbrev,
      line,
      over_price,
      group_by_header,
      event_id,
      outcome_name,
      outcome_id,
      fixed_market_id
    ) |>
    separate(match, into = c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(
      home_team = fix_team_names(home_team),
      away_team = fix_team_names(away_team),
      match = paste(home_team, "v", away_team)
    ) |>
    left_join(player_names[, c("player_full_name", "team_name")], by = c("player_name" = "player_full_name")) |>
    mutate(
      player_team = case_when(
        player_team_from_abbrev == home_team | player_team_from_abbrev == away_team ~ player_team_from_abbrev,
        team_name == home_team | team_name == away_team ~ team_name,
        TRUE ~ coalesce(player_team_from_abbrev, team_name)
      ),
      opposition_team = case_when(
        player_team == away_team ~ home_team,
        player_team == home_team ~ away_team,
        TRUE ~ NA_character_
      ),
      market_name = market_label,
      agency = "BetRight",
      under_price = NA_real_
    ) |>
    transmute(
      match,
      home_team,
      away_team,
      market_name,
      player_name,
      player_team,
      line,
      over_price,
      under_price,
      agency,
      opposition_team,
      group_by_header,
      event_id,
      outcome_name,
      outcome_id,
      fixed_market_id
    ) |>
    distinct() |>
    arrange(match, player_name, line)
}

betright_player_disposals <-
  build_player_prop_market(
    betright_player_stats,
    "^Player Disposals\\s+-\\s+",
    "Player Disposals"
  )

betright_player_goals <-
  build_player_prop_market(
    betright_player_stats,
    "^Player Goals\\s+-\\s+",
    "Player Goals"
  )

betright_player_marks <-
  build_player_prop_market(
    betright_player_stats,
    "^Player Marks\\s+-\\s+",
    "Player Marks"
  )

betright_player_tackles <-
  build_player_prop_market(
    betright_player_stats,
    "^Player Tackles\\s+-\\s+",
    "Player Tackles"
  )

betright_player_fantasy_points <-
  build_player_prop_market(
    betright_player_stats,
    "(Player Fantasy|AFL Fantasy|Fantasy Points)\\s+-\\s+",
    "Player Fantasy Points"
  )

#===============================================================================
# Write to CSV
#===============================================================================

betright_player_disposals |> write_csv("Data/scraped_odds/betright_player_disposals.csv")
betright_player_goals |> write_csv("Data/scraped_odds/betright_player_goals.csv")
betright_player_marks |> write_csv("Data/scraped_odds/betright_player_marks.csv")
betright_player_tackles |> write_csv("Data/scraped_odds/betright_player_tackles.csv")
betright_player_fantasy_points |> write_csv("Data/scraped_odds/betright_player_fantasy_points.csv")
