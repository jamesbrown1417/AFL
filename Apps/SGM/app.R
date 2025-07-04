#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(readxl)  
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Read in and normalise DVP Data
#===============================================================================

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
      breaks = quantile(dvp, probs = 0:5/5, na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("Terrible", "Bad", "Neutral", "Good", "Excellent")
    )
  ) %>%
    mutate(DVP_Category = as.character(DVP_Category)) |> 
    mutate(DVP_Category = ifelse(market_name == "Player Goals", "Neutral", DVP_Category)) |> 
  ungroup() %>%
  select(Position = Pos, opposition_team = Opponent, market_name, DVP_Category)

# Head to head data
h2h <- read_csv("../../Data/scraped_odds/sportsbet_h2h.csv")

# Matches in order
matches_in_order <-
  h2h %>%
  distinct(match) |> 
  pull()

#===============================================================================
# Create compare sgm function
#===============================================================================

# Source scripts
source("betright_sgm.R")
source("tab_sgm.R")
source("sportsbet_sgm.R")
source("pointsbet_sgm.R")
source("neds_sgm.R")
source("bet365_sgm.R")
source("dabble_sgm.R")
source("player_combos.R")

#===============================================================================
# Create compare sgm function
#===============================================================================

compare_sgm <- function(player_names, stat_counts, markets) {
  # Function to handle errors in the call_sgm functions
  handle_call_sgm <- function(func, sgm, player_names, stat_counts, markets) {
    tryCatch({
      func(sgm, player_names, stat_counts, markets)
    }, error = function(e) {
      # Return a dataframe with NA values if an error occurs
      data.frame(Selections=NA, Markets = NA, Unadjusted_Price=NA, Adjusted_Price=NA, Adjustment_Factor=NA, Agency=NA)
    })
  }
  
  # Get individual dataframes
  pointsbet_data <- handle_call_sgm(call_sgm_pointsbet, pointsbet_sgm, player_names, stat_counts, markets)
  sportsbet_data <- handle_call_sgm(call_sgm_sportsbet, sportsbet_sgm, player_names, stat_counts, markets)
  tab_data <- handle_call_sgm(call_sgm_tab, tab_sgm, player_names, stat_counts, markets)
  betright_data <- handle_call_sgm(call_sgm_betright, betright_sgm, player_names, stat_counts, markets)
  neds_data <- handle_call_sgm(call_sgm_neds, neds_sgm, player_names, stat_counts, markets)
  bet365_data <- handle_call_sgm(call_sgm_bet365, bet365_sgm, player_names, stat_counts, markets)
  dabble_data <- handle_call_sgm(call_sgm_dabble, dabble_sgm, player_names, stat_counts, markets)
  
  # Bind together and return
  bind_rows(pointsbet_data, sportsbet_data, tab_data, betright_data, neds_data, bet365_data, dabble_data) |>
    mutate(Adjusted_Price = round(Adjusted_Price, 2),
           Unadjusted_Price = round(Unadjusted_Price, 2),
           Adjustment_Factor = round(Adjustment_Factor, 2)
           ) |>
    arrange(desc(Adjusted_Price))
}

#===============================================================================
# Compare CGM function
#===============================================================================

compare_cgm <- function(player_names_cross, lines_cross, market_names_cross) {
  # List of each agency data
  all_data <- list(pointsbet_sgm, sportsbet_sgm, tab_sgm, betright_sgm, neds_sgm, bet365_sgm, dabble_sgm)
  
  # Function to get cross game multi data
  get_cgm <- function(data, player_names_cross, lines_cross, market_names_cross) {
    if (length(player_names_cross) != length(lines_cross) || length(lines_cross) != length(market_names_cross)) {
      stop("All lists should have the same length")
    }
    
    filtered_df <- data.frame()
    for (i in seq_along(player_names_cross)) {
      temp_df <- data %>%
        filter(player_name == player_names_cross[i],
               line == lines_cross[i],
               market_name == market_names_cross[i])
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
  handle_get_cgm <- function(data, player_names_cross, lines_cross, market_names_cross) {
    tryCatch({
      get_cgm(data, player_names_cross, lines_cross, market_names_cross)
    }, error = function(e) {
      # Return a dataframe with NA values if an error occurs
      data.frame(Selections = NA, Matches = NA, Markets = NA, Price = NA, Agency = NA)
    })
  }
  
  # Map over list of dataframes
  cgm_all <- map_dfr(all_data, handle_get_cgm, player_names_cross, lines_cross, market_names_cross) %>%
    arrange(desc(Price))
  
  return(cgm_all)
}

# Read in datasets--------------------------------------------------------------
disposals <-
  read_rds("../../Data/processed_odds/all_player_disposals.rds") |> 
  rename(price = over_price,
         empirical_probability_2024 = empirical_prob_over_2024,
         diff_2024 = diff_over_2024)

goals <-
  read_rds("../../Data/processed_odds/all_player_goals.rds") |> 
  rename(price = over_price,
         empirical_probability_2024 = empirical_prob_over_2024,
         diff_2024 = diff_over_2024)

marks <- 
  read_rds("../../Data/processed_odds/all_player_marks.rds") |> 
  rename(price = over_price,
         empirical_probability_2024 = empirical_prob_over_2024,
         diff_2024 = diff_over_2024)

tackles <- 
  read_rds("../../Data/processed_odds/all_player_tackles.rds") |> 
  rename(price = over_price,
         empirical_probability_2024 = empirical_prob_over_2024,
         diff_2024 = diff_over_2024)

fantasy_points <- 
  read_rds("../../Data/processed_odds/all_player_fantasy_points.rds") |> 
  rename(price = over_price,
         empirical_probability_2024 = empirical_prob_over_2024,
         diff_2024 = diff_over_2024)

disposals <-
  disposals |>
  bind_rows(goals) |> 
  bind_rows(marks) |>
  bind_rows(tackles) |>
  bind_rows(fantasy_points) |>
  left_join(player_positions, relationship = "many-to-one") |>
  left_join(dvp_data, by = c("opposition_team", "Position", "market_name"), relationship = "many-to-one") |> 
  relocate(Position, DVP_Category, .after = player_name)

# Create market best
disposals <-
  disposals |>
  group_by(match, player_name, market_name, line) |>
  arrange(desc(price), .by_group = TRUE) |>
  mutate(
    max_player_diff = max(diff_over_last_10, na.rm = TRUE),
    second_best_price = if_else(n() >= 2, nth(price, 2), NA_real_),
    market_best = row_number() == 1
  ) |>
  ungroup()

# Unique matches
matches <- matches_in_order

# Unique agencies
agencies <-
  disposals |>
  distinct(agency) |>
  pull()

# Add Dabble to the agencies list
agencies <- c(agencies, "Dabble") |> unique()

# Create disposals dataframe to display
disposals_display <-
  disposals |>
  group_by(player_name, match, line, market_name) |>
  mutate(
    next_best_diff = if_else(market_best,
                            ((1/second_best_price) - (1/price)),
                             NA_real_)
  ) |>
  ungroup() |>
  arrange(desc(max_player_diff)) |>
  transmute(match,
         player_name,
         Position,
         Matchup = DVP_Category,
         market_name,
         line,
         price,
         agency,
         prob_2024 = round(empirical_probability_2024, 2),
         diff_2024 = round(diff_2024, 2),
         prob_last_10 = round(emp_prob_last_10, 2),
         diff_last_10 = round(diff_over_last_10, 2),
         next_best_diff = round(100 * next_best_diff, 1),
         market_best)

# Get correlations
correlations_2024 <-
  read_rds("../../Data/player_correlations_disposals_23.rds") |>
  mutate_if(is.numeric, round, digits = 2)

##%######################################################%##
#                                                          #
####                         UI                         ####
#                                                          #
##%######################################################%##

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Multitool"),
  
  tabsetPanel(
    
    # SGM Tab
    tabPanel("SGM",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "match",
                   "Select Match",
                   choices = matches,
                   selected = NULL
                 ),
                 selectInput(
                   "agency",
                   "Select Agency",
                   choices = agencies,
                   selected = NULL
                 ),
                 selectInput(
                   "market",
                   "Select Market",
                   choices = c("Player Disposals", "Player Goals", "Player Marks", "Player Tackles", "Player Fantasy Points"),
                   selected = c("Player Disposals", "Player Goals", "Player Marks", "Player Tackles", "Player Fantasy Points"),
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
                 h3("Selections"),
                 DT::dataTableOutput("selected"),
                 h3("Pairwise Correlations"),
                 DT::dataTableOutput("correlations"),
                 h3("SGM Information"),
                 uiOutput("summary"),
                 h3("Odds Comparison"),
                 actionButton("get_comparison", label = "Compare Odds"),
                 actionButton("clear_comparison", label = "Clear Selections"),
                 DT::dataTableOutput("odds_compare")
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Player List", 
                            DT::dataTableOutput("table")
                   )
                   )
               )
             )
    ),
    
    # Cross Game Multi Tab
    tabPanel("Cross Game Multi",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "agency_cross",
                   "Select Agency",
                   choices = agencies,
                   selected = NULL
                 ),
                 selectInput(
                   "market_cross",
                   "Select Market",
                   choices = c("Player Disposals", "Player Goals", "Player Marks", "Player Tackles", "Player Fantasy Points"),
                   selected = c("Player Disposals", "Player Goals", "Player Marks", "Player Tackles", "Player Fantasy Points"),
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
    
    # Player Combos Tab
    tabPanel("Player Combos",
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
                   choices = c("All", "Player Disposals", "Player Goals", "Player Marks", "Player Tackles", "Player Fantasy Points"),
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
)

##%######################################################%##
#                                                          #
####                       Server                       ####
#                                                          #
##%######################################################%##

server <- function(input, output, session) {
  
  # For the "SGM" panel
  output$table <- renderDT({
    filtered_data <-
      disposals_display[disposals_display$match == input$match &
                          disposals_display$agency == input$agency &
                          disposals_display$Matchup %in% input$matchup &
                          disposals_display$market_name %in% input$market,]
    
    # Filter for Dabble's specific price requirement in the Player List
    if (input$agency == "Dabble") {
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
  }, server = FALSE) # We are setting this as FALSE for client-side processing of the DataTable
  
  
  observeEvent(input$table_rows_selected,{
    output$selected <- renderDT({
      if(!is.null(input$table_rows_selected)){
        filtered_data <-
          disposals_display[disposals_display$match == input$match &
                              disposals_display$agency == input$agency &
                              disposals_display$Matchup %in% input$matchup &
                              disposals_display$market_name %in% input$market,]
        
        # Filter for Dabble's specific price requirement
        if (input$agency == "Dabble") {
          filtered_data <- filtered_data |> filter(price == 1.79)
        }
        
        if (input$best_odds) {filtered_data <- filtered_data |> filter(market_best) |> select(-market_best)}
        selected_data <- filtered_data[input$table_rows_selected, c("player_name", "line", "market_name", "price")]
        datatable(selected_data)
      }
    })
  })
  
  # Get the table proxy
  proxy <- dataTableProxy("table")
  
  # Get the table proxy for the cross game multi
  proxy_cross <- dataTableProxy("table_cross")
  
  output$correlations <- renderDT({
    filtered_data <- disposals_display[disposals_display$match == input$match & disposals_display$agency == input$agency,]
    
    # Filter for Dabble's specific price requirement
    if (input$agency == "Dabble") {
      filtered_data <- filtered_data |> filter(price == 1.79)
    }
    
    if (input$best_odds) {filtered_data <- filtered_data |> filter(market_best) |> select(-market_best)}
    selected_data <- filtered_data[input$table_rows_selected, c("player_name", "line", "market_name", "price")]
    
    correlations_table <- correlations_2024 |> filter(player_a %in% selected_data$player_name & player_b %in% selected_data$player_name)
    datatable(correlations_table)
  })
  
  # SGM Comparison
  observeEvent(input$get_comparison, {
    # Get selected data
    filtered_data <- disposals_display[disposals_display$match == input$match &
                                         disposals_display$agency == input$agency &
                                         disposals_display$Matchup %in% input$matchup &
                                         disposals_display$market_name %in% input$market,]
    
    # Filter for Dabble's specific price requirement
    if (input$agency == "Dabble") {
      filtered_data <- filtered_data |> filter(price == 1.79)
    }
    
    if (input$best_odds) {filtered_data <- filtered_data |> filter(market_best) |> select(-market_best)}
    selected_data <- filtered_data[input$table_rows_selected, c("player_name", "line", "market_name", "price")]
    
    player_names = selected_data$player_name
    lines = selected_data$line
    market_names = selected_data$market_name
    
    # Call function
    comparison_df <- compare_sgm(
      player_names = player_names,
      stat_counts = lines,
      markets = market_names)
    
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
  
  # observeEvent(input$get_combos, {
  #   # Get selected data
  #   filtered_data <- disposals_display[disposals_display$match == input$match & disposals_display$agency == input$agency,]
  #   if (input$best_odds) {filtered_data <- filtered_data |> filter(market_best) |> select(-market_best)}
  #   selected_data <- filtered_data[input$table_rows_selected, c("player_name", "number_of_disposals", "price")]
  #   
  #   player_names = selected_data$player_name
  #   number_of_disposals = selected_data$number_of_disposals
  #   
  #   # Call function
  #   combo_df <- sgm_combinations(player_names, number_of_disposals)
  #   
  #   # populate DTOutput
  #   output$all_selected_combinations <- renderDT({
  #     datatable(combo_df, extensions = "Buttons", options = list(buttons = c('copy', 'csv', 'excel')))
  #   })
  # })
  # 
  
  output$summary <- renderUI({
    if(!is.null(input$table_rows_selected)){
      filtered_data <- disposals_display[disposals_display$match == input$match &
                                           disposals_display$agency == input$agency &
                                           disposals_display$Matchup %in% input$matchup &
                                           disposals_display$market_name %in% input$market,]
      
      # Filter for Dabble's specific price requirement
      if (input$agency == "Dabble") {
        filtered_data <- filtered_data |> filter(price == 1.79)
      }
      
      if (input$best_odds) {filtered_data <- filtered_data |> filter(market_best) |> select(-market_best)}
      selected_data <- filtered_data[input$table_rows_selected, ]
      uncorrelated_price <- prod(selected_data$price)
      empirical_price <- 1 / prod(selected_data$prob_last_10)
      HTML(paste0("<strong>Uncorrelated Price:</strong>", " $", round(uncorrelated_price, 2), "<br/>",
                  " <strong>Theoretical Uncorrelated Price:</strong>", " $", round(empirical_price, 2)))
    }
  })
  
  # For the "Cross Game Multi" panel
  output$table_cross <- renderDT({
    filtered_data_cross <- disposals_display[disposals_display$agency == input$agency_cross &
                                               disposals_display$Matchup %in% input$matchup_cross &
                                             disposals_display$market_name %in% input$market_cross,]
    
    if (input$best_odds_cross) {filtered_data_cross <- filtered_data_cross |> filter(market_best) |> select(-market_best)}
    
    datatable(filtered_data_cross, selection = "multiple", filter = "top")
  }, server = FALSE) 
  
  observeEvent(input$table_cross_rows_selected,{
    output$selected_cross <- renderDT({
      if(!is.null(input$table_cross_rows_selected)){
        filtered_data_cross <- disposals_display[disposals_display$agency == input$agency_cross &
                                                 disposals_display$Matchup %in% input$matchup_cross &
                                                 disposals_display$market_name %in% input$market_cross,]
        
        if (input$best_odds_cross) {filtered_data_cross <- filtered_data_cross |> filter(market_best) |> select(-market_best)}
        
        selected_data_cross <- filtered_data_cross[input$table_cross_rows_selected, c("player_name", "line", "market_name", "price")]
        datatable(selected_data_cross)
      }
    })
  })
  
  # Cross Game Comparison
  observeEvent(input$get_comparison_cross, {
    # Get selected data
    filtered_data_cross <- disposals_display[disposals_display$agency == input$agency_cross &
                                               disposals_display$Matchup %in% input$matchup_cross &
                                               disposals_display$market_name %in% input$market_cross,]
    
    if (input$best_odds_cross) {filtered_data_cross <- filtered_data_cross |> filter(market_best) |> select(-market_best)}
    
    selected_data_cross <- filtered_data_cross[input$table_cross_rows_selected, c("player_name", "line", "market_name", "price", "agency")]
    
    player_names_cross = selected_data_cross$player_name
    lines_cross = selected_data_cross$line
    market_names_cross = selected_data_cross$market_name
    
    # Call function
    comparison_df_cross <-
      compare_cgm(market_names_cross = market_names_cross,
                  player_names_cross = player_names_cross,
                  lines_cross = lines_cross)
    
    # populate DTOutput
    output$odds_compare_cross <- renderDT({
      datatable(comparison_df_cross)
    })
  })
  
  output$summary_cross <- renderUI({
    if(!is.null(input$table_cross_rows_selected)){
      filtered_data_cross <- disposals_display[disposals_display$agency == input$agency_cross &
                                               disposals_display$Matchup %in% input$matchup_cross &
                                               disposals_display$market_name %in% input$market_cross,]
      
      if (input$best_odds_cross) {filtered_data_cross <- filtered_data_cross |> filter(market_best) |> select(-market_best)}
      
      selected_data_cross <- filtered_data_cross[input$table_cross_rows_selected, ]
      uncorrelated_price_cross <- prod(selected_data_cross$price)
      empirical_price_cross <- 1 / prod(selected_data_cross$prob_2024)
      empirical_price_cross_l10 <- 1 / prod(selected_data_cross$prob_last_10)
      diff = 1/empirical_price_cross - 1/uncorrelated_price_cross
      diff_l10 = 1/empirical_price_cross_l10 - 1/uncorrelated_price_cross
      HTML(paste0("<strong>Multi Price:</strong>", " $", round(uncorrelated_price_cross, 2), "<br/>",
                  " <strong>Theoretical Multi Price:</strong>", " $", round(empirical_price_cross, 2), "<br/>",
                  " <strong>Edge L10:</strong>", " ", round(100*diff_l10, 3), "%"), "<br/>",
                  " <strong>Edge 2024:</strong>", " ", round(100*diff, 3), "%")
    }
  })
  
  # For the "Player Combos" panel
  output$player_selection_ui <- renderUI({
    DT::dataTableOutput("player_table_combos")
  })
  
  output$player_table_combos <- renderDT({
    filtered_data <- disposals_display |> 
      filter(match == input$match_combos) |> 
      distinct(player_name, .keep_all = TRUE) |> 
      select(player_name, Position, Matchup)
    
    datatable(filtered_data, selection = 'multiple', options = list(pageLength = 10))
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

##%######################################################%##
#                                                          #
####                      Run App                       ####
#                                                          #
##%######################################################%##

shinyApp(ui = ui, server = server)
