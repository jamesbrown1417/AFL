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
library(zoo)

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
team_stats <- read_rds("../../Data/afl_team_stats_2021_2025.rds")

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
  h2h_data <- read_rds("../../Data/processed_odds/all_h2h.rds") |> arrange(start_time)
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
        h4("General Settings"),
        selectInput("season_input_b", "Select Season:", 
                    choices = team_stats$season_name |> unique() |> sort(decreasing = TRUE), 
                    multiple = TRUE, 
                    selectize = TRUE, 
                    selected = team_stats$season_name |> unique()),
        markdown(mds = c("__Select Only Last n Games:__")),
        numericInput("last_games_team", "Number of Games per Team", value = NA),
        hr(),
        h4("Analysis Settings"),
        conditionalPanel(
          condition = "input.team_tabs == 'Head-to-Head'",
          selectInput("team_a", "Select Team A:", 
                      choices = c("", sort(unique(c(team_stats$home_team, team_stats$away_team)))),
                      selected = ""),
          selectInput("team_b", "Select Team B:", 
                      choices = c("", sort(unique(c(team_stats$home_team, team_stats$away_team)))),
                      selected = "")
        ),
        conditionalPanel(
          condition = "input.team_tabs == 'Opposition Analysis'",
          selectInput("selected_opposition", "Filter by Opposition (optional):", 
                      choices = c("All" = "", sort(unique(c(team_stats$home_team, team_stats$away_team)))),
                      selected = ""),
          numericInput("min_games_opp", "Minimum Games vs Opposition", value = 1, min = 1)
        ),
        checkboxInput("aggregate_opp",
                      label = "Show league aggregate (all teams)",
                      value = FALSE),
        conditionalPanel(
          condition = "input.team_tabs == 'Venue Stats'",
          selectInput("selected_venue", "Filter by Venue (optional):", 
                      choices = c("All" = "", sort(unique(team_stats$venue))),
                      selected = ""),
          numericInput("min_games_venue", "Minimum Games at Venue", value = 1, min = 1)
        ),
        conditionalPanel(
          condition = "input.team_tabs == 'Performance Trends'",
          radioButtons("team_metric", "Performance Metric:",
                       choices = list("Score" = "Score", "Fantasy Points" = "Fantasy"),
                       selected = "Score")
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
    filter(Games >= 3) |>  # Only show venues with 3+ games
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
  
  # create a long form of every team’s performance vs its opponent -------------
  home_vs_opp <- data |>
    transmute(team        = home_team,
              opposition  = away_team,
              disposals   = home_team_disposals,
              marks       = home_team_marks,
              tackles     = home_team_tackles,
              score       = home_team_score,
              opp_score   = away_team_score)
  
  away_vs_opp <- data |>
    transmute(team        = away_team,
              opposition  = home_team,
              disposals   = away_team_disposals,
              marks       = away_team_marks,
              tackles     = away_team_tackles,
              score       = away_team_score,
              opp_score   = home_team_score)
  
  opp_stats <- bind_rows(home_vs_opp, away_vs_opp)
  
  # optional filter by the selected opposition ---------------------------------
  if (!is.null(input$selected_opposition) && input$selected_opposition != "")
    opp_stats <- opp_stats |> filter(opposition == input$selected_opposition)
  
  ## ── NEW: league-wide aggregate toggle ──────────────────────────────────────
  if (isTRUE(input$aggregate_opp)) {
    opp_stats |>
      summarise(
        Games            = n(),
        Wins             = sum(score > opp_score),
        `Win %`          = round(mean(score > opp_score) * 100, 1),
        `Avg Disposals`  = round(mean(disposals), 0),
        `Total Disposals`= sum(disposals),
        `Avg Marks`      = round(mean(marks), 1),
        `Total Marks`    = sum(marks),
        `Avg Tackles`    = round(mean(tackles), 1),
        `Total Tackles`  = sum(tackles),
        `Avg Score`      = round(mean(score), 1),
        `Avg Margin`     = round(mean(score - opp_score), 1)
      ) |>
      mutate(team = "All Teams") |>
      relocate(team)
  } else {
    opp_stats |>
      group_by(team, opposition) |>
      summarise(
        Games            = n(),
        Wins             = sum(score > opp_score),
        `Win %`          = round(mean(score > opp_score) * 100, 1),
        `Avg Disposals`  = round(mean(disposals), 0),
        `Total Disposals`= sum(disposals),
        `Avg Marks`      = round(mean(marks), 1),
        `Total Marks`    = sum(marks),
        `Avg Tackles`    = round(mean(tackles), 1),
        `Total Tackles`  = sum(tackles),
        `Avg Score`      = round(mean(score), 1),
        `Avg Margin`     = round(mean(score - opp_score), 1),
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
    transmute(team        = home_team,
              venue,
              disposals   = home_team_disposals,
              marks       = home_team_marks,
              tackles     = home_team_tackles,
              score       = home_team_score,
              opp_score   = away_team_score)
  
  away_venue <- data |>
    transmute(team        = away_team,
              venue,
              disposals   = away_team_disposals,
              marks       = away_team_marks,
              tackles     = away_team_tackles,
              score       = away_team_score,
              opp_score   = home_team_score)
  
  venue_stats <- bind_rows(home_venue, away_venue)
  
  # optional filter by the chosen venue ----------------------------------------
  if (!is.null(input$selected_venue) && input$selected_venue != "")
    venue_stats <- venue_stats |> filter(venue == input$selected_venue)
  
  ## ── NEW: league-wide aggregate toggle ──────────────────────────────────────
  if (isTRUE(input$aggregate_opp)) {
    venue_stats |>
      summarise(
        Games            = n(),
        Wins             = sum(score > opp_score),
        `Win %`          = round(mean(score > opp_score) * 100, 1),
        `Avg Disposals`  = round(mean(disposals), 0),
        `Total Disposals`= sum(disposals),
        `Avg Marks`      = round(mean(marks), 1),
        `Total Marks`    = sum(marks),
        `Avg Tackles`    = round(mean(tackles), 1),
        `Total Tackles`  = sum(tackles),
        `Avg Score`      = round(mean(score), 1)
      ) |>
      mutate(team = "All Teams") |>
      relocate(team)
  } else {
    venue_stats |>
      group_by(team, venue) |>
      summarise(
        Games            = n(),
        Wins             = sum(score > opp_score),
        `Win %`          = round(mean(score > opp_score) * 100, 1),
        `Avg Disposals`  = round(mean(disposals), 0),
        `Total Disposals`= sum(disposals),
        `Avg Marks`      = round(mean(marks), 1),
        `Total Marks`    = sum(marks),
        `Avg Tackles`    = round(mean(tackles), 1),
        `Total Tackles`  = sum(tackles),
        `Avg Score`      = round(mean(score), 1),
        .groups = "drop"
      ) |>
      filter(Games >= input$min_games_venue) |>
      arrange(team, desc(Games))
  }
})

#=============================================================================
# Plots for team stats
#=============================================================================

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

#=============================================================================
# Table outputs for team stats
#=============================================================================

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
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
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
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
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
      order = list(list(0, 'asc'), list(2, 'desc'))  # Sort by team then games
    )
  ) |>
    formatStyle(
      "Win %",
      background = styleColorBar(venue_specific_stats()$`Win %`, "lightblue"),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )
})

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
