# Libraries
library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)
library(glue)

# Player names file
player_names <- read_rds("Data/2026_start_positions_and_prices.rds")
player_names <- player_names |> select(player_full_name, team_name)

# Function to fix team names
source("Functions/fix_team_names.R")

#===============================================================================
# Use rvest to get main market information-------------------------------------#
#===============================================================================

get_head_to_head <- function() {

# Read scraped HTML from the BET365_HTML Folder
scraped_file <- list.files("Data/BET365_HTML", full.names = TRUE, pattern = "h2h")[[1]]

# Get Teams
bet365_teams <-
    read_html(scraped_file) |> 
    html_nodes(".src-ParticipantFixtureDetailsHigher_TeamWrapper ") |> 
    html_text()

# Get H2H Odds
bet365_h2h_odds <-
    read_html(scraped_file) |> 
    html_nodes(".src-ParticipantOddsOnly50_Odds") |> 
    html_text()

# Get Handicap
bet365_handicap <-
    read_html(scraped_file) |> 
    html_nodes(".src-ParticipantCenteredStacked50_Handicap") |> 
    html_text()

# Get indices of elements that do contain "O" or "U"
totals_indices <- which(str_detect(bet365_handicap, "O|U"))

# Get Handicap Price
bet365_handicap_price <-
    read_html(scraped_file) |> 
    html_nodes(".src-ParticipantCenteredStacked50_Odds") |> 
    html_text()

# Remove empty strings
bet365_handicap_price <- bet365_handicap_price[!bet365_handicap_price == ""]

# Remove Totals Indices
if (length(totals_indices) > 0) {
  bet365_handicap <- bet365_handicap[-totals_indices]
  bet365_handicap_price <- bet365_handicap_price[-totals_indices]
}

# Get Start Time
bet365_start_time <-
    read_html(scraped_file) |> 
    html_nodes(".sgl-MarketFixtureDetailsLabel") |>
    html_nodes(".rcl-MarketHeaderLabel-isdate, .src-ParticipantFixtureDetailsHigher_BookCloses ") |> 
    html_text()

#===============================================================================
# Create head to head table----------------------------------------------------#
#===============================================================================

# Get Home teams - Odd elements
home_teams <- bet365_teams[seq(1, length(bet365_teams), 2)]
home_odds <- bet365_h2h_odds[seq(1, length(bet365_h2h_odds), 2)]

home_h2h <- tibble(home_teams, home_odds)

# Get Away teams - Even elements
away_teams <- bet365_teams[seq(2, length(bet365_teams), 2)]
away_odds <- bet365_h2h_odds[seq(2, length(bet365_h2h_odds), 2)]

away_h2h <- tibble(away_teams, away_odds)
  
# Combine together into one table
bet365_h2h <-
    bind_cols(home_h2h, away_h2h) |>
    mutate(home_teams = fix_team_names(home_teams),
           away_teams = fix_team_names(away_teams)) |>
    transmute(match = paste(home_teams, away_teams, sep = " v "),
              market_name = "Head To Head",
              home_team = home_teams,
              home_win = as.numeric(home_odds),
              away_team = away_teams,
              away_win = as.numeric(away_odds)) |>
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "Bet365")

# Write to csv
write_csv(bet365_h2h, "Data/scraped_odds/bet365_h2h.csv")

#===============================================================================
# Create Handicap table--------------------------------------------------------#
#===============================================================================

# Get Home teams - Odd elements
home_teams <- bet365_teams[seq(1, length(bet365_teams), 2)]
home_handicap <- bet365_handicap[seq(1, length(bet365_handicap), 2)]
# Remove empty strings
home_handicap <- home_handicap[!home_handicap == " "]
  
home_handicap_price <- bet365_handicap_price[seq(1, length(bet365_handicap_price), 2)]

home_handicap <- tibble(home_teams, home_handicap, home_handicap_price)
  
# Get Away teams - Even elements
away_teams <- bet365_teams[seq(2, length(bet365_teams), 2)]
away_handicap <- bet365_handicap[seq(2, length(bet365_handicap), 2)]
# Remove empty strings
away_handicap <- away_handicap[!away_handicap == " "]
away_handicap_price <- bet365_handicap_price[seq(2, length(bet365_handicap_price), 2)]

away_handicap <- tibble(away_teams, away_handicap, away_handicap_price)
  
# Combine together into one table
bet365_handicap <-
    bind_cols(home_handicap, away_handicap) |>
    mutate(home_teams = fix_team_names(home_teams),
           away_teams = fix_team_names(away_teams)) |>
    transmute(match = paste(home_teams, away_teams, sep = " v "),
              market_name = "Line",
              home_team = home_teams,
              home_line = as.numeric(home_handicap),
              home_win = as.numeric(home_handicap_price),
              away_team = away_teams,
              away_line = as.numeric(away_handicap),
              away_win = as.numeric(away_handicap_price)) |>
    mutate(margin = round((1/home_win + 1/away_win), digits = 3)) |> 
    mutate(agency = "Bet365")

# Write to csv
write_csv(bet365_handicap, "Data/scraped_odds/bet365_line.csv")
}

# Create safe version of functions-----------------------------------------------
get_head_to_head_safe <- safely(get_head_to_head, otherwise = NULL)

# # Run functions-----------------------------------------------------------------
tryCatch(get_head_to_head(), error = function(e) print("Error in get_head_to_head()"))

##%######################################################%##
#                                                          #
####                    Player Props                    ####
#                                                          #
##%######################################################%##

clean_bet365_player_name <- function(player_name) {
    player_name |>
        str_remove("^\\s*\\d+\\s+") |>
        str_squish()
}

extract_bet365_match_name <- function(bet365) {
    bet365_match_header <-
        bet365 |>
        html_nodes(".cm-MatchBettingReactHeader") |>
        html_text2() |>
        str_squish()

    bet365_match_header <- str_replace_all(bet365_match_header, "\\s+vs?\\s+", " v ")
    bet365_match_name <- str_match(bet365_match_header, "\\d{1,2}:\\d{2}\\s+(.*)$")[, 2]
    bet365_match_name <- bet365_match_name[!is.na(bet365_match_name)]

    if (length(bet365_match_name) == 0) {
        bet365 |>
            html_nodes(".sph-FixturePodHeader_TeamName ") |>
            html_text() |>
            glue_collapse(sep = " v ")
    } else {
        bet365_match_name[1]
    }
}

bet365_ancestor_text <- function(node, levels = 8) {
    current <- node
    for (i in seq_len(levels)) {
        parent <- xml2::xml_parent(current)
        if (inherits(parent, "xml_missing")) {
            break
        }
        current <- parent
    }

    current |>
        html_text2() |>
        str_squish()
}

select_bet365_market_node <- function(nodes, include, exclude = character()) {
    if (length(nodes) == 0) {
        return(NULL)
    }

    exclude_pattern <- if (length(exclude) > 0) as.character(glue_collapse(exclude, sep = "|")) else NULL

    for (level in 0:8) {
        node_text <- map_chr(seq_along(nodes), \(index) bet365_ancestor_text(nodes[[index]], levels = level))
        matches <- str_detect(node_text, regex(include, ignore_case = TRUE))

        if (!is.null(exclude_pattern)) {
            matches <- matches & !str_detect(node_text, regex(exclude_pattern, ignore_case = TRUE))
        }

        if (any(matches)) {
            return(nodes[[which(matches)[1]]])
        }
    }

    NULL
}

read_bet365_disposals_modern <- function(bet365, bet365_match_name) {
    market_pod <-
        bet365 |>
        html_nodes(".gl-MarketGroupPod") |>
        select_bet365_market_node(
            include = "Player Disposals Milestones|Disposals Milestones",
            exclude = c("Total Player Disposals", "Match Ups", "Most Disposals")
        )

    if (is.null(market_pod)) {
        stop("Could not find Bet365 Player Disposals Milestones market")
    }

    player_names <-
        market_pod |>
        html_nodes(".srb-ParticipantLabelWithTeam_Name, .srb-ParticipantLabel_Name") |>
        html_text2() |>
        clean_bet365_player_name()

    odds_columns <- market_pod |> html_nodes(".srb-HScrollPlaceColumnMarket")

    headers <-
        odds_columns |>
        map_chr(\(column) {
            header <- column |> html_node(".srb-HScrollPlaceHeader") |> html_text2()
            if (length(header) == 0 || is.na(header)) "" else str_squish(header)
        })

    keep_columns <- str_detect(headers, "^\\d+\\+$")
    odds_columns <- odds_columns[keep_columns]
    headers <- headers[keep_columns]

    if (length(player_names) == 0 || length(headers) == 0) {
        stop("Bet365 modern disposals market is missing player names or disposal columns")
    }

    map2_dfr(headers, seq_along(headers), \(header, index) {
        odds <-
            odds_columns[[index]] |>
            html_nodes(".gl-ParticipantOddsOnly_Odds") |>
            html_text2() |>
            str_squish()

        if (length(odds) != length(player_names)) {
            stop(glue(
                "Bet365 modern disposals count mismatch for {header}: ",
                "{length(odds)} odds for {length(player_names)} players"
            ))
        }

        tibble(
            match = bet365_match_name,
            player_name = player_names,
            number_of_disposals = header,
            price = parse_number(na_if(odds, ""))
        )
    }) |>
        filter(!is.na(price)) |>
        mutate(implied_probability = 1 / price)
}

read_bet365_disposal_lines_modern <- function(bet365, bet365_match_name) {
    market_pod <-
        bet365 |>
        html_nodes(".gl-MarketGroupPod") |>
        select_bet365_market_node(include = "Total Player Disposals")

    if (is.null(market_pod)) {
        return(tibble(
            match = character(),
            player_name = character(),
            number_of_disposals = character(),
            over_price = numeric(),
            under_price = numeric()
        ))
    }

    player_names <-
        market_pod |>
        html_nodes(".srb-ParticipantLabelWithTeam_Name, .srb-ParticipantLabel_Name") |>
        html_text2() |>
        clean_bet365_player_name()

    participants <- market_pod |> html_nodes(".gl-ParticipantCenteredStacked")
    n_players <- length(player_names)

    if (n_players == 0 || length(participants) < n_players * 2) {
        stop(glue(
            "Bet365 modern disposal line count mismatch: ",
            "{length(participants)} line prices for {n_players} players"
        ))
    }

    handicaps <-
        participants |>
        html_node(".gl-ParticipantCenteredStacked_Handicap") |>
        html_text2() |>
        str_squish()

    odds <-
        participants |>
        html_node(".gl-ParticipantCenteredStacked_Odds") |>
        html_text2() |>
        str_squish()

    over_index <- seq_len(n_players)
    under_index <- n_players + seq_len(n_players)

    if (!all(handicaps[over_index] == handicaps[under_index])) {
        warning("Bet365 modern disposal line over/under handicaps do not align", call. = FALSE)
    }

    tibble(
        match = bet365_match_name,
        player_name = player_names,
        number_of_disposals = handicaps[over_index],
        over_price = parse_number(odds[over_index]),
        under_price = parse_number(odds[under_index])
    )
}

# Function to read in disposals html and output table
read_bet365_disposals_html <- function(html_path) {
    
    # Read in the txt data as html
    bet365 <- read_html(html_path)
    
    bet365_match_name <- extract_bet365_match_name(bet365)
    
    # Extract the disposals data table----------------------------------------------
    # Get the disposals table
    bet365_disposals <-
        bet365 |>
        html_nodes(".bbl-FilteredMarketGroupWithHScrollerContainer_Wide")

    if (length(bet365_disposals) == 0) {
        return(read_bet365_disposals_modern(bet365, bet365_match_name))
    }

    bet365_disposals <-
        bet365_disposals |>
        select_bet365_market_node(include = "Disposals", exclude = c("Goalscorer", "Goal"))

    if (is.null(bet365_disposals)) {
        stop("Could not find Bet365 Disposals milestone table")
    }
    
    # Player names
    bet365_disposals_player_names <-
        bet365_disposals |>
        html_nodes(".bbl-BetBuilderParticipantLabel_Name") |>
        html_text2() |>
        clean_bet365_player_name()

    headers <-
        bet365_disposals |>
        html_nodes(".bbl-MarketColumnHeader40Scrolled_Label, .bbl-MarketColumnHeader40Scrolled") |>
        html_text2() |>
        str_squish() |>
        unique()

    headers <- headers[str_detect(headers, "^\\d+\\+$")]
    odds_columns <- bet365_disposals |> html_nodes(".bbl-Market40Scrolled")

    if (length(bet365_disposals_player_names) == 0 || length(headers) == 0 || length(odds_columns) == 0) {
        stop("Bet365 Disposals milestone table is missing player names or odds columns")
    }

    n_columns <- min(length(headers), length(odds_columns))

    bet365_disposals_odds_df <-
        map2_dfr(headers[seq_len(n_columns)], seq_len(n_columns), \(header, index) {
            odds_nodes <- odds_columns[[index]] |> html_nodes(".bbl-BetBuilderParticipant_Odds")
            if (length(odds_nodes) == 0) {
                odds_nodes <- odds_columns[[index]] |> html_nodes(".bbl-BetBuilderParticipant")
            }

            odds <-
                odds_nodes |>
                html_text(trim = TRUE) |>
                str_squish()

            n_odds <- min(length(odds), length(bet365_disposals_player_names))

            tibble(
                match = bet365_match_name,
                player_name = bet365_disposals_player_names[seq_len(n_odds)],
                number_of_disposals = header,
                price = parse_number(odds[seq_len(n_odds)])
            )
        }) |>
        filter(!is.na(price)) |>
        mutate(implied_probability = 1 / price)
    
    # Remove any numbers from player_name
    bet365_disposals_odds_df <-
        bet365_disposals_odds_df |>
        mutate(player_name = clean_bet365_player_name(player_name))
  
    # Return table
    bet365_disposals_odds_df
}

# Function to read in goals html and output table
read_bet365_goals_html <- function(html_path) {
    
    # Read in the txt data as html
    bet365 <- read_html(html_path)
    
    bet365_match_name <- extract_bet365_match_name(bet365)
    
    # Extract the goals data table----------------------------------------------
    # Get the goals table
    bet365_goals <-
        bet365 |>
        html_nodes(".bbl-FilteredMarketGroupWithHScrollerContainer_Wide")

    if (length(bet365_goals) == 0) {
        return(NULL)
    }

    bet365_goals <-
        bet365_goals |>
        select_bet365_market_node(include = "Goalscorer|Multi Scorer", exclude = "Disposals")

    if (is.null(bet365_goals)) {
        return(NULL)
    }
    
    # Player names
    bet365_goals_player_names <-
        bet365_goals |>
        html_nodes(".bbl-BetBuilderParticipantLabel_Name") |>
        html_text()
    
    # Determine if multi scorer is selected
    selected <-
        bet365 |>
        html_nodes(".bbl-TabSwitcherItem-selected") |>
        html_text()
    
    multi_scorer <- "Multi Scorer" %in% selected
    
    # Odds
    bet365_goal_odds_nodes <- bet365_goals |> html_nodes(".bbl-BetBuilderParticipant_Odds")
    if (length(bet365_goal_odds_nodes) == 0) {
        bet365_goal_odds_nodes <- bet365_goals |> html_nodes(".bbl-BetBuilderParticipant")
    }

    bet365_goal_odds <-
        bet365_goal_odds_nodes |>
        html_text(trim = TRUE)
    
    # Get indices for each player
    bet365_indices <- seq_along(bet365_goals_player_names)
    
    # Get 1+ goals odds
    bet365_goal_odds_1plus <-
        bet365_goal_odds[bet365_indices]
    
    # Get 2+ goals odds
    bet365_goal_odds_2plus <-
        bet365_goal_odds[bet365_indices + length(bet365_goals_player_names)]
    
    # Get 3+ goals odds
    bet365_goal_odds_3plus <-
        bet365_goal_odds[bet365_indices + length(bet365_goals_player_names) * 2]
    
    # Get 4+ goals odds
    bet365_goal_odds_4plus <-
        bet365_goal_odds[bet365_indices + length(bet365_goals_player_names) * 3]
    
    # Get 5+ goals odds
    bet365_goal_odds_5plus <-
        bet365_goal_odds[bet365_indices + length(bet365_goals_player_names) * 4]
    
    # Get 6+ goals odds
    bet365_goal_odds_6plus <-
        bet365_goal_odds[bet365_indices + length(bet365_goals_player_names) * 5]
    
    # Create data frame
    bet365_goal_odds_df <-
        tibble(
            player = bet365_goals_player_names,
            odds_1plus = bet365_goal_odds_1plus,
            odds_2plus = bet365_goal_odds_2plus,
            odds_3plus = bet365_goal_odds_3plus,
            odds_4plus = bet365_goal_odds_4plus,
            odds_5plus = bet365_goal_odds_5plus,
            odds_6plus = bet365_goal_odds_6plus
        )
    
    # Pivot longer
    bet365_goal_odds_df <-
        bet365_goal_odds_df |>
        pivot_longer(
            cols = odds_1plus:odds_6plus,
            names_to = "goals",
            values_to = "odds"
        ) |>
        mutate(
            goals = str_remove(goals, "odds_"),
            goals = str_replace(goals, "plus", "+"),
            odds = as.numeric(odds)
        ) |>
        filter(!is.na(odds)) |>
        mutate(match = bet365_match_name) |>
        select(
            match,
            player_name = player,
            number_of_goals = goals,
            price = odds
        ) |>
        mutate(implied_probability = 1 / price)
    
    # Remove any numbers from player_name
    bet365_goal_odds_df <-
        bet365_goal_odds_df |>
        mutate(player_name = str_remove(player_name, "\\d+"))
  
    # Return table if multi_scorer is selected, else return NULL
    if (multi_scorer) {
        return(bet365_goal_odds_df)
    } else {
        return(NULL)
    }
}

# Function to read disposal lines
read_bet365_disposal_lines_html <- function(html_path) {
  
  # Read in the txt data as html
  bet365 <- read_html(html_path)
  
  bet365_match_name <- extract_bet365_match_name(bet365)
  
  # Extract the disposals data table----------------------------------------------
  # Get the disposals table
  bet365_disposal_lines <-
    bet365 |>
    html_nodes(".bbl-BetBuilderMarketGroupContainer ")

  if (length(bet365_disposal_lines) == 0) {
    return(read_bet365_disposal_lines_modern(bet365, bet365_match_name))
  }

  bet365_disposal_lines <-
    bet365_disposal_lines |>
    select_bet365_market_node(include = "Player Disposals|Total Player Disposals|Disposals", exclude = c("Goalscorer", "Goal"))

  if (is.null(bet365_disposal_lines)) {
    return(tibble(
      match = character(),
      player_name = character(),
      number_of_disposals = character(),
      over_price = numeric(),
      under_price = numeric()
    ))
  }
  
  # Player names
  bet365_disposals_player_names <-
    bet365_disposal_lines |>
    html_nodes(".bbl-BetBuilderParticipantLabel_Name") |>
    html_text2() |>
    clean_bet365_player_name()
  
  # Odds
  bet365_disposals_odds <-
    bet365_disposal_lines |>
    html_nodes(".bbl-BetBuilderParticipant_Odds") |>
    html_text2() |>
    str_squish()
  
  # Lines
  bet365_disposals_lines <-
    bet365_disposal_lines |>
    html_nodes(".bbl-BetBuilderParticipant_Handicap") |>
    html_text2() |>
    str_squish()

  n_players <- length(bet365_disposals_player_names)

  if (n_players == 0 || length(bet365_disposals_lines) < n_players * 2 || length(bet365_disposals_odds) < n_players * 2) {
    stop(glue(
      "Bet365 disposal line count mismatch: ",
      "{length(bet365_disposals_odds)} odds and {length(bet365_disposals_lines)} lines for {n_players} players"
    ))
  }

  over_index <- seq_len(n_players)
  under_index <- n_players + seq_len(n_players)

  # Create data frame for Overs
  bet365_disposal_lines_odds_df_overs <-
    tibble(
      match = bet365_match_name,
      player_name = bet365_disposals_player_names,
      number_of_disposals = bet365_disposals_lines[over_index],
      over_price = bet365_disposals_odds[over_index],
    )
  
  # Create data frame for Unders
  bet365_disposal_lines_odds_df_unders <-
    tibble(
      match = bet365_match_name,
      player_name = bet365_disposals_player_names,
      number_of_disposals = bet365_disposals_lines[under_index],
      under_price = bet365_disposals_odds[under_index],
    )
  
  # Merge the two data frames
  bet365_disposal_lines_odds_df <-
    bet365_disposal_lines_odds_df_overs |>
    left_join(bet365_disposal_lines_odds_df_unders, by = c("match", "player_name", "number_of_disposals"))
  
  # Remove any numbers from player_name
  bet365_disposal_lines_odds_df <-
    bet365_disposal_lines_odds_df |>
    mutate(player_name = clean_bet365_player_name(player_name))

  # Return table
  bet365_disposal_lines_odds_df
}

# Map Over the Files------------------------------------------------------------
goals_list <- list.files("Data/BET365_HTML", pattern = "players_a", full.names = TRUE)
disposals_list <- list.files("Data/BET365_HTML", pattern = "players_b", full.names = TRUE)
disposal_lines_list <- list.files("Data/BET365_HTML", pattern = "players_b", full.names = TRUE)

# Create safe versions of each function
read_bet365_goals_html <- safely(read_bet365_goals_html)
read_bet365_disposals_html <- safely(read_bet365_disposals_html)
read_bet365_disposal_lines_html <- safely(read_bet365_disposal_lines_html, otherwise = NULL)

empty_bet365_goals_raw <- tibble(
  match = character(),
  player_name = character(),
  number_of_goals = character(),
  price = numeric(),
  implied_probability = numeric()
)

empty_bet365_disposals_raw <- tibble(
  match = character(),
  player_name = character(),
  number_of_disposals = character(),
  price = numeric(),
  implied_probability = numeric()
)

empty_bet365_disposal_lines_raw <- tibble(
  match = character(),
  player_name = character(),
  number_of_disposals = character(),
  over_price = numeric(),
  under_price = numeric()
)

bind_safe_results <- function(results, label, empty_result) {
  errors <-
    results |>
    keep(~ !is.null(.x$error)) |>
    map_chr(~ conditionMessage(.x$error))

  if (length(errors) > 0) {
    warning(
      glue(
        "Bet365 {label}: {length(errors)} HTML file(s) could not be parsed: ",
        "{glue_collapse(unique(errors), sep = '; ')}"
      ),
      call. = FALSE
    )
  }

  parsed <- results |> map_dfr(~ .x$result)

  if (ncol(parsed) == 0) {
    return(empty_result)
  }

  parsed
}

# Get all data
bet365_goals <-
  map(goals_list, read_bet365_goals_html) |>
  bind_safe_results("goals", empty_bet365_goals_raw) |>
  mutate(player_name = str_remove(player_name, "\\d+"))

bet365_disposals <-
  map(disposals_list, read_bet365_disposals_html) |>
  bind_safe_results("disposals", empty_bet365_disposals_raw) |>
  mutate(player_name = str_remove(player_name, "\\d+"))

bet365_disposals_lines <-
  map(disposal_lines_list, read_bet365_disposal_lines_html) |>
  bind_safe_results("disposal lines", empty_bet365_disposal_lines_raw)

# If empty give columns
if (nrow(bet365_disposals_lines) == 0) {
  bet365_disposals_lines <- tibble(
    match = character(),
    player_name = character(),
    opposition_team = character(),
    number_of_disposals = character(),
    over_price = numeric(),
    under_price = numeric()
  )
}

# Add match info
bet365_goals <-
bet365_goals |> 
    separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team), away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(
        player_name = case_when(
            player_name == "Matthew Roberts" ~ "Matt Roberts",
            player_name == "Jacob Van Rooyen" ~ "Jacob van Rooyen",
            player_name == "Kamdyn Mcintosh" ~ "Kamdyn McIntosh",
            player_name == "Malcolm Rosas Jnr" ~ "Malcolm Rosas",
            .default = player_name
        )
    ) |>
    left_join(player_names, by = c("player_name" = "player_full_name")) |>
    mutate(line = as.numeric(str_extract(number_of_goals, "\\d+"))) |>
    mutate(line = line - 0.5) |> 
    rename(player_team = team_name) |> 
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |> 
    transmute(match,
              home_team,
              away_team,
              player_team,
              opposition_team,
              market_name = "Player Goals",
              player_name,
              line,
              over_price = price,
              agency = "Bet365")

# Add match info - Disposals
bet365_disposals <-
bet365_disposals |> 
    separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
    mutate(home_team = fix_team_names(home_team), away_team = fix_team_names(away_team)) |>
    mutate(match = paste(home_team, "v", away_team)) |>
    mutate(
        player_name = case_when(
            player_name == "Matthew Roberts" ~ "Matt Roberts",
            player_name == "Jacob Van Rooyen" ~ "Jacob van Rooyen",
            player_name == "Kamdyn Mcintosh" ~ "Kamdyn McIntosh",
            player_name == "Malcolm Rosas Jnr" ~ "Malcolm Rosas",
            .default = player_name
        )
    ) |>
    left_join(player_names, by = c("player_name" = "player_full_name")) |>
    mutate(line = as.numeric(str_extract(number_of_disposals, "\\d+"))) |>
    mutate(line = line - 0.5) |> 
    rename(player_team = team_name) |> 
    mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |> 
    transmute(match,
              home_team,
              away_team,
              player_team,
              opposition_team,
              market_name = "Player Disposals",
              player_name,
              line,
              over_price = price,
              agency = "Bet365")

# Add match info - Disposal lines
bet365_disposals_lines <-
  bet365_disposals_lines |> 
  separate(match, c("home_team", "away_team"), sep = " v ", remove = FALSE) |>
  mutate(home_team = fix_team_names(home_team), away_team = fix_team_names(away_team)) |>
  mutate(match = paste(home_team, "v", away_team)) |>
  mutate(
    player_name = case_when(
    player_name == "Matthew Roberts" ~ "Matt Roberts",
    player_name == "Jacob Van Rooyen" ~ "Jacob van Rooyen",
    player_name == "Kamdyn Mcintosh" ~ "Kamdyn McIntosh",
    player_name == "Malcolm Rosas Jnr" ~ "Malcolm Rosas",
    .default = player_name
  )
  ) |>
  left_join(player_names, by = c("player_name" = "player_full_name")) |>
  mutate(line = as.numeric(number_of_disposals)) |> 
  rename(player_team = team_name) |> 
  mutate(opposition_team = if_else(home_team == player_team, away_team, home_team)) |> 
  mutate(over_price = as.numeric(over_price),
 under_price = as.numeric(under_price)) |>
  transmute(match,
            home_team,
            away_team,
            player_team,
            opposition_team,
            market_name = "Player Disposals",
            player_name,
            line,
            over_price,
            under_price,
            agency = "Bet365")

# Fix opposition team if table is null
if (nrow(bet365_disposals_lines) == 0) {
  bet365_disposals_lines <- 
    bet365_disposals_lines |> 
    mutate(opposition_team = "")}

# Combine
bet365_disposals <-
  bet365_disposals |>
  bind_rows(bet365_disposals_lines)

# Write to rds
write_csv(bet365_disposals, "Data/scraped_odds/bet365_player_disposals.csv")
write_csv(bet365_goals, "Data/scraped_odds/bet365_player_goals.csv")
