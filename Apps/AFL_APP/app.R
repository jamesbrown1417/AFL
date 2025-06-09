##%######################################################%##
#                                                          #
####                       Set up                       ####
#                                                          #
##%######################################################%##

# Load the required libraries
library(shiny)
library(tidyverse)
library(bslib)
library(gridlayout)
library(DT)
library(googlesheets4)
library(googledrive)
library(readxl)

# Determine the operating system
os_type <- Sys.info()["sysname"]

# # Google sheets authentification -----------------------------------------------
# options(gargle_oauth_cache = ".secrets")
# drive_auth(cache = ".secrets", email = "cuzzy.punting@gmail.com")
# gs4_auth(token = drive_token())

# Read in data
all_player_stats <- read_rds("../../Data/afl_fantasy_2015_2024_data.rds")
data_2025 <- read_rds("../../Data/afl_fantasy_2025_data.rds")
all_player_stats <- bind_rows(all_player_stats, data_2025)

# Fix CBA Percentage
all_player_stats$cba_percentage <- round(all_player_stats$cba_percentage, 3)

# Agencies List
agencies = c("TAB", "Pointsbet", "Neds", "Sportsbet", "Bet365", "Unibet", "BetRight", "Betr", "Dabble", "Betfair")

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

#===============================================================================
# Read in odds data
#===============================================================================

# Conditional logic for loading data based on OS
if (
  # os_type == "Windows"
  TRUE
  ) {
  # Read RDS Data for Windows
  h2h_data <- read_rds("../../Data/processed_odds/all_h2h.rds")
  line_data <- read_rds("../../Data/processed_odds/all_line.rds")
  player_disposals_data <- read_rds("../../Data/processed_odds/all_player_disposals.rds")
  player_goals_data <- read_rds("../../Data/processed_odds/all_player_goals.rds")
  player_fantasy_data <- read_rds("../../Data/processed_odds/all_player_fantasy_points.rds")
  player_marks_data <- read_rds("../../Data/processed_odds/all_player_marks.rds")
  player_tackles_data <- read_rds("../../Data/processed_odds/all_player_tackles.rds")
} else {
  # Google Sheets Data for other OS
  ss_name <- gs4_find("AFL Data")
  h2h_data <- read_sheet(ss = ss_name, sheet = "H2H")
  line_data <- read_sheet(ss = ss_name, sheet = "Line")
  player_disposals_data <- read_sheet(ss = ss_name, sheet = "Player Disposals")
  player_goals_data <- read_sheet(ss = ss_name, sheet = "Player Goals")
  player_fantasy_data <- read_sheet(ss = ss_name, sheet = "Player Fantasy Points")
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
                              home_away == "Away"),-margin,
                         margin))
  

# Make weather category Indoors if at Marvel Stadium
all_player_stats <-
  all_player_stats |>
  mutate(weather_category = ifelse(venue == "Marvel Stadium", "Indoors", weather_category))

# Add gameId variable
all_player_stats <-
  all_player_stats |>
  mutate(gameId = paste0(season_name, round, match_name))

# Function to get correlation between players-----------------------------------
get_player_correlation <- function(data, seasons = NULL, name_a, name_b, metric_a, metric_b) {
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
  
  # Create plot
  ggplot(df_merged, aes(x = .data[[col_name_a]], y = .data[[col_name_b]])) +
    geom_point(color = "#3498db", alpha = 0.6, size = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "#e74c3c", linetype = "dashed") +
    labs(
      x = col_name_a, 
      y = col_name_b,
      title = "Player Performance Correlation",
      subtitle = sprintf("Correlation between %s and %s", col_name_a, col_name_b),
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
      "text", x = max(df_merged[[col_name_a]]), y = min(df_merged[[col_name_b]]), 
      label = sprintf("r = %.2f", correlation), 
      hjust = 1, vjust = 0, size = 5, color = "red1", fontface = "italic"
    )
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
    filter(Player == teammate_name) %>% pull(gameId)
  
  # Label each game as 'With Teammate' or 'Without Teammate'
  df_player <- df_player %>% 
    mutate(Teammate = if_else(gameId %in% games_with_teammate, 'With Teammate', 'Without Teammate'))
  
  # Calculate mean and count for both conditions
  summary_stats <- df_player %>% group_by(Teammate) %>% summarise(mean_val = mean(!!sym(metric), na.rm = TRUE), n_games = n())
  
  # Create the violin plot
  plot <- ggplot(df_player, aes(x = Teammate, y = !!sym(metric), fill = Teammate)) +
    geom_violin(trim = FALSE, position = position_dodge(width = 0.9)) +
    geom_boxplot(width = 0.1, position = position_dodge(width = 0.9)) +
    labs(title = paste("Performance of", name, "with and without", teammate_name),
         x = "Condition",
         y = metric) +
    scale_fill_manual(values = c("Without Teammate" = "orange1", "With Teammate" = "royalblue1")) +
    annotate("text", x = Inf, y = Inf, 
             label = paste("With Teammate: ", summary_stats$n_games[summary_stats$Teammate == "With Teammate"], 
                           " games, Mean ", round(summary_stats$mean_val[summary_stats$Teammate == "With Teammate"], 2), "\n",
                           "Without Teammate: ", summary_stats$n_games[summary_stats$Teammate == "Without Teammate"], 
                           " games, Mean ", round(summary_stats$mean_val[summary_stats$Teammate == "Without Teammate"], 2)), 
             hjust = 1, vjust = 1) +
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
    filter(Player == teammate_name) %>% pull(gameId)
  
  # Label each game as 'With Teammate' or 'Without Teammate'   
  df_player <- df_player %>% 
    mutate(Teammate = if_else(gameId %in% games_with_teammate, 'With Teammate', 'Without Teammate'))
  
  # Calculate mean and count for both conditions
  summary_stats <-
    df_player %>%
    group_by(Teammate) %>%
    summarise(n_games = n(),
              `AVG Disposals` = mean(Disposals, na.rm = TRUE),
              `AVG Goals` = mean(Goals, na.rm = TRUE),
              `AVG Fantasy` = mean(Fantasy, na.rm = TRUE),
              `AVG CBA%` = mean(CBA, na.rm = TRUE)) |> 
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
    summarise(n_games = n(),
              `AVG Disposals` = mean(Disposals, na.rm = TRUE),
              `AVG Goals` = mean(Goals, na.rm = TRUE),
              `AVG Fantasy` = mean(Fantasy, na.rm = TRUE),
              `AVG CBA%` = mean(CBA, na.rm = TRUE)) |> 
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

#===============================================================================
# UI
#===============================================================================

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
        textInput("player_name_input_a", "Select Player:", value = "Tim English"),
        selectInput("season_input_a", "Select Season:", choices = all_player_stats$season_name |> unique(), multiple = TRUE, selectize = TRUE, selected = c("2025","2024")),
        selectInput("stat_input_a", "Select Statistic:", choices = c("Disposals", "Fantasy", "Tackles", "Marks", "Goals"), selected = "Disposals"),
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
        numericInput("reference_line", "Line Value", value = 19.5),
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
        h4("Settings"),
        selectInput("season_input_b", "Select Season:", choices = all_player_stats$season_name |> unique(), multiple = TRUE, selectize = TRUE, selected = all_player_stats$season_name |> unique()),
        markdown(mds = c("__Select Only Last n Games:__")),
        numericInput("last_games_team", "Number of Games", value = NA)
      ),
      mainPanel(
        width = 9,
        card(card_body(div(class = "dt-fill-container", DTOutput(outputId = "team_metric_table"))))
      )
    )
  ),
  nav_panel(title = "Odds Screen",
            sidebarLayout(
              sidebarPanel(
                width = 3,
                h4("Settings"),
                selectInput("agency_input", "Select Agencies:", choices = agencies, multiple = TRUE, selectize = TRUE, selected = agencies),
                selectInput("market_input", "Select Market:", choices = c("H2H", "Total","Line", "Disposals", "Fantasy", "Goals", "Marks", "Tackles"), multiple = FALSE),
                selectInput("match_input", "Select Matches:", choices = h2h_data$match |> unique(), multiple = TRUE, selectize = FALSE, selected = h2h_data$match |> unique()),
                selectInput("matchup_input", "Select Difficulty:", choices = player_disposals_data$DVP_Category |> unique(), multiple = TRUE, selectize = FALSE, selected = player_disposals_data$DVP_Category |> unique()),
                textInput("player_name_input_b", "Select Player:", value = NA),
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
      column(width = 4,
             card(
               card_header("Settings"),
               card_body(
                 textInput("player_name", "Select Player:", value = "Christian Petracca"),
                 textInput("teammate_name", "Select Teammate:", value = "Clayton Oliver"),
                 selectInput("season_input", "Select Season:", choices = all_player_stats$season_name |> unique(), multiple = TRUE, selectize = TRUE, selected = c("2024")),
                 selectInput("metric_input", "Select Statistic:", choices = c("Disposals", "Fantasy", "Goals", "Marks", "Tackles"), multiple = FALSE, selected = "Fantasy")
               )
             )
      ),
      column(width = 8,
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
      column(width = 4,
             card(
               card_header("Settings"),
               card_body(
                 textInput("player_name_corr", "Select Player 1:", value = "Adam Treloar"),
                 selectInput("metric_input_corr_a", "Select Statistic:", choices = c("Disposals", "Fantasy", "Goals", "Marks", "Tackles"), selected = "Disposals"),
                 textInput("teammate_name_corr", "Select Player 2:", value = "Tim English"),
                 selectInput("metric_input_corr_b", "Select Statistic:", choices = c("Disposals", "Fantasy", "Goals", "Marks", "Tackles"), selected = "Disposals"),
                 selectInput("season_input_corr", "Select Season:", choices = all_player_stats$season_name |> unique(), multiple = TRUE, selectize = TRUE, selected = c("2025","2024"))
               )
             )
      ),
      column(width = 8,
             card(card_body(plotOutput(outputId = "corr_plot_output", height = "800px", width = "100%")))
      )
    )
  )
)

#===============================================================================
# Server
#===============================================================================

server <- function(input, output) {
  bs_themer()
  #=============================================================================
  # Filter player stats
  #=============================================================================
  
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
      select(Date = start_time_utc,
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
             game_number) |> 
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
  
  #=============================================================================
  # Get Proportion above reference line
  #=============================================================================
  
  proportion_above_reference_line <- reactive({
    # Get proportion above reference line
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
    
    return(output_string)
    
  })
  
  #=============================================================================
  # Plot player stats
  #=============================================================================
  
  output$plot <- renderPlot({
    # Create a new variable that checks if the y-value is above the reference line
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
      )+
      
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
      ) +
      
      # Aesthetics
      theme_bw() +
      theme(
        plot.background = element_rect(fill = "white", colour = "white"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      ) +
      
      # Labels & Titles
      labs(title = "",
           x = "Game Number") +
      
      # Set manual color scale
      scale_color_identity() +
      
      # Additional
      theme(legend.position = "none")
    
    print(p)
  })
  
  #=============================================================================
  # Table player stats
  #=============================================================================
  
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
  
  #=============================================================================
  # Filter team stats
  #=============================================================================
  
  # # Reactive function to filter team stats
  # filtered_team_stats <- reactive({
  #   
  #   # Filter team stats
  #   filtered_team_stats <-
  #     all_team_stats |>
  #     filter(season %in% input$season_input_b)
  #   
  #   # Filter by last n games
  #   if (!is.na(input$last_games_team)) {
  #     filtered_team_stats <-
  #       filtered_team_stats |>
  #       group_by(teamId) |> 
  #       slice_head(n = input$last_games_team) |> 
  #       ungroup()
  #   }
  #   
  #   # Summarise stats
  #   filtered_team_stats <-
  #     filtered_team_stats |>
  #     select(
  #       teamName,
  #       possessions,
  #       pacePer40,
  #       offensiveRating,
  #       defensiveRating,
  #       netRating,
  #       assistPercentage,
  #       defensiveReboundPercentage,
  #       offensiveReboundPercentage,
  #       reboundPercentage,
  #       trueShootingPercentage,
  #       effectiveFieldGoalPercentage
  #     ) |> 
  #     group_by(teamName) |>
  #     summarise(across(.cols = where(is.numeric),
  #                      .fns = list(mean = mean))) |> 
  #     mutate(across(.cols = where(is.numeric), .fns = round, 2))
  #   
  #   # Return filtered team stats
  #   return(filtered_team_stats)
  #   
  # })
  
  #=============================================================================
  # Table team stats
  #=============================================================================
  
  # output$team_metric_table <- renderDT({
  #   datatable(
  #     filtered_team_stats(),
  #     options = list(pageLength = 15, autoWidth = TRUE),
  #     width = "100%",
  #     height = "800px"
  #   )
  # })
  
  #=============================================================================
  # Table Odds
  #=============================================================================
  
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
        filter(DVP_Category  %in% input$matchup_input) |>
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
        filter(DVP_Category  %in% input$matchup_input) |>
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
        filter(DVP_Category  %in% input$matchup_input) |>
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
        filter(DVP_Category  %in% input$matchup_input) |>
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
        filter(DVP_Category  %in% input$matchup_input) |>
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
              ))
  })
  
  #=============================================================================
  # With / Without Teammate
  #=============================================================================
  
  output$with_without_plot_output <- renderPlot({
    req(input$player_name, input$teammate_name, input$season_input, input$metric_input)

    plot <- compare_performance(
      data = filtered_player_stats_2,
      season = input$season_input,
      name = input$player_name,
      teammate_name = input$teammate_name,
      metric = input$metric_input)
    
    return(plot)
  })
  
  output$with_without_table_output <- renderDT({
    req(input$player_name, input$teammate_name, input$season_input)
    
    table <- compare_performance_table(
      data = filtered_player_stats_2,
      season = input$season_input,
      name = input$player_name,
      teammate_name = input$teammate_name)
    
    return(table)
  })
  
  #=============================================================================
  # Player Correlations
  #=============================================================================
  
  output$corr_plot_output <- renderPlot({
    req(input$player_name_corr, input$teammate_name_corr, input$season_input_corr, input$metric_input_corr_b, input$metric_input_corr_a)
    
    plot <-
      get_player_correlation(
        data = filtered_player_stats_2,
        seasons = input$season_input_corr,
        name_a = input$player_name_corr,
        name_b = input$teammate_name_corr,
        metric_a = input$metric_input_corr_a,
        metric_b = input$metric_input_corr_b
      )
    
    return(plot)
  })
}

#===============================================================================
# Run App
#===============================================================================

shinyApp(ui, server)
