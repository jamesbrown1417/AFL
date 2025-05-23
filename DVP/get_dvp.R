# Libraries and functions-------------------------------------------------------
library(tidyverse)
library(future)
library(furrr)
library(readxl)

# Set up parallel processing----------------------------------------------------
plan(multisession)

# Read in position data---------------------------------------------------------
player_positions <-
  fitzRoy::fetch_player_details_afl(season = 2025)

player_positions <-
player_positions |> 
  transmute(player_full_name = paste(firstName, surname),
            position = factor(position))

# Read in player stats----------------------------------------------------------
all_player_stats_2015_2025 <-
  read_rds("Data/afl_fantasy_2015_2024_data.rds") |> 
  bind_rows(read_rds("Data/afl_fantasy_2025_data.rds"))

# Select only the columns we need-----------------------------------------------
all_player_stats_2015_2025 <-
  all_player_stats_2015_2025 |>
  select(
    player_full_name,
    player_team,
    season_name,
    round,
    match_name,
    home_team,
    away_team,
    start_time_utc,
    opposition_team,
    disposals,
    marks,
    tackles,
    fantasy_points,
    goals,
    tog_percentage
  ) |> 
  filter(str_detect(round, "Final", negate = TRUE)) |> 
  filter(tog_percentage >= 30)

# Get each team's last 10 games-------------------------------------------------
home_team_last_10_games <-
  all_player_stats_2015_2025 |> 
  distinct(match_name, round, season_name, start_time_utc, home_team) |> 
  rename(team = home_team)

away_team_last_10_games <-
  all_player_stats_2015_2025 |> 
  distinct(match_name, round, season_name, start_time_utc, away_team) |> 
  rename(team = away_team)

last_10_games <-
  bind_rows(home_team_last_10_games, away_team_last_10_games) |> 
  arrange(team, desc(start_time_utc)) |> 
  group_by(team) |>
  slice_head(n = 10) |> 
  ungroup() |> 
  mutate(match_id = paste(match_name, round, season_name, sep = "_"))

# Filter player stats to only include last 10 games-----------------------------
all_player_stats_last_10 <-
  all_player_stats_2015_2025 |> 
  mutate(match_id = paste(match_name, round, season_name, sep = "_")) |>
  filter(match_id %in% last_10_games$match_id)

# Join with positions-----------------------------------------------------------
all_player_stats_last_10 <-
  all_player_stats_last_10 |> 
  left_join(player_positions) |> 
  filter(!is.na(position)) |> 
  arrange(desc(start_time_utc), opposition_team, desc(disposals)) |> 
  rename(pos_1_factor = position)

#===============================================================================
# Function to get DVP for a position
#===============================================================================

get_dvp <- function(team, stat) {
  
  # Stats transformation
  stats_table <-
    all_player_stats_last_10 |> 
    transmute(Player = player_full_name,
              Team = player_team,
              Opponent = opposition_team,
              date = start_time_utc,
              tog_percentage,
              disposals,
              marks,
              tackles,
              fantasy_points,
              goals,
              Pos = pos_1_factor) |> 
    mutate(DisposalsPer100TOG = disposals / (tog_percentage/100),
           MarksPer100TOG = marks / (tog_percentage/100),
           TacklesPer100TOG = tackles / (tog_percentage/100),
           GoalsPer100TOG = goals / (tog_percentage/100),
           FantasyPointsPer100TOG = fantasy_points / (tog_percentage/100))
  
  # Vs team
  stats_table_vs_team <-
    stats_table |> 
    filter(Opponent == team) |>
    arrange(desc(date))
  
  # Vs others
  stats_table_vs_others <-
    stats_table |> 
    filter(Opponent != team) |>
    arrange(Player, desc(date)) |> 
    group_by(Player) |> 
    mutate(match_number = dense_rank(desc(date))) |> 
    mutate(games_played = max(match_number))
  
  # Get median vs team
  med_vs_team <-
    stats_table_vs_team |> 
    group_by(Player, Pos, Team, Opponent) |> 
    summarise(med_disposals_vs = median(DisposalsPer100TOG, na.rm = TRUE),
              med_marks_vs = median(MarksPer100TOG, na.rm = TRUE),
              med_tackles_vs = median(TacklesPer100TOG, na.rm = TRUE),
              med_goals_vs = mean(GoalsPer100TOG, na.rm = TRUE),
              med_fantasy_points_vs = median(FantasyPointsPer100TOG, na.rm = TRUE))
  
  # Get median vs all other teams in period
  med_vs_others <-
    stats_table_vs_others |> 
    group_by(Player, Pos, Team) |> 
    summarise(med_disposals_others = median(DisposalsPer100TOG, na.rm = TRUE),
              med_marks_others = median(MarksPer100TOG, na.rm = TRUE),
              med_tackles_others = median(TacklesPer100TOG, na.rm = TRUE),
              med_goals_others = mean(GoalsPer100TOG, na.rm = TRUE),
              med_fantasy_points_others = median(FantasyPointsPer100TOG, na.rm = TRUE))
  
  # Join Together
  dvp_data <-
    med_vs_team |>
    left_join(med_vs_others, by = c("Player", "Pos", "Team")) |> 
    transmute(Player,
              Pos,
              Team,
              Opponent,
              disposals_diff = med_disposals_vs - med_disposals_others,
              marks_diff = med_marks_vs - med_marks_others,
              tackles_diff = med_tackles_vs - med_tackles_others,
              goals_diff = med_goals_vs - med_goals_others,
              fantasy_points_diff = med_fantasy_points_vs - med_fantasy_points_others)
  
  # Get for desired stat
  stat_var <- match.arg(stat, choices = c("disposals", "marks", "tackles", "fantasy_points", "goals"))
  diff_var <- paste0(stat_var, "_diff")
  
  dvp_data |> 
    group_by(Pos, Opponent) |>
    summarise(games = n(),
              median_diff = median(!!rlang::sym(diff_var), na.rm = TRUE)) |> 
    arrange(desc(median_diff))
}


# Get team list
team_list <-
  all_player_stats_last_10 |> 
  pull(opposition_team) |> 
  unique()

#===============================================================================
# Get DVP for each stat
#===============================================================================

# Get disposals DVP
disposals_dvp <-
  team_list |> 
  map_df(get_dvp, stat = "disposals") |> 
  bind_rows() |> 
  arrange(Pos, desc(median_diff))

# Get marks DVP
marks_dvp <-
  team_list |> 
  map_df(get_dvp, stat = "marks") |> 
  bind_rows() |> 
  arrange(Pos, desc(median_diff))

# Get tackles DVP
tackles_dvp <-
  team_list |> 
  map_df(get_dvp, stat = "tackles") |> 
  bind_rows() |> 
  arrange(Pos, desc(median_diff))

# Get fantasy DVP
fantasy_points_dvp <-
  team_list |> 
  map_df(get_dvp, stat = "fantasy_points") |> 
  bind_rows() |> 
  arrange(Pos, desc(median_diff))

# Get goals DVP
goals_dvp <-
  team_list |> 
  map_df(get_dvp, stat = "goals") |> 
  bind_rows() |> 
  arrange(Pos, desc(median_diff))

#===============================================================================
# Save Data
#===============================================================================

# DVP Data
disposals_dvp_combine <-
  disposals_dvp |> 
  rename(dvp = median_diff) |> 
  mutate(dvp = 0.8*dvp) |> 
  mutate(market_name = "Player Disposals")

marks_dvp_combine <-
  marks_dvp |> 
  rename(dvp = median_diff) |> 
  mutate(dvp = 0.8*dvp) |> 
  mutate(market_name = "Player Marks")

tackles_dvp_combine <-
  tackles_dvp |> 
  rename(dvp = median_diff) |> 
  mutate(dvp = 0.8*dvp) |> 
  mutate(market_name = "Player Tackles")

fantasy_points_dvp_combine <-
  fantasy_points_dvp |> 
  rename(dvp = median_diff) |> 
  mutate(dvp = 0.8*dvp) |> 
  mutate(market_name = "Player Fantasy Points")

goals_dvp_combine <-
  goals_dvp |> 
  rename(dvp = median_diff) |> 
  mutate(dvp = 0.8*dvp) |> 
  mutate(market_name = "Player Goals")

# Combine
dvp_data <-
  bind_rows(disposals_dvp_combine,
            marks_dvp_combine,
            tackles_dvp_combine,
            fantasy_points_dvp_combine,
            goals_dvp_combine)

# Write out
write_csv(dvp_data, "DVP/dvp_data.csv")
write_csv(player_positions, "DVP/AFL-Players-Positions-2025.csv")