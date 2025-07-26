library(tidyverse)
library(TTR)

# Read in data
afl_fantasy_2015_2024_data <- readRDS("Data/afl_fantasy_2015_2023_data.rds")
afl_fantasy_2025_data <- readRDS("Data/afl_fantasy_2024_data.rds")

# Combine data
combined_stats <-
  bind_rows(afl_fantasy_2015_2024_data, afl_fantasy_2025_data)

# Create a function to get player data for a given statistic
get_player_stat_data <- function(player_name, stat) {
  combined_stats %>%
    filter(player_full_name == player_name) %>%
    select(match_name, round, season_name, start_time_utc, home_team, away_team, player_name = player_full_name, !!sym(stat)) %>%
    arrange(desc(start_time_utc))
}

get_player_stat_data("Lachie Neale", "disposals")


alpha_from_hl <- function(hl) 1 - 0.5^(1/hl)
cummean_vec   <- function(x) cumsum(x) / seq_along(x)

safe_ewm <- function(x, hl) {
  a <- alpha_from_hl(hl)
  if (length(x) <= 2L) return(cummean_vec(x))     # stable fallback for very short series
  TTR::EMA(x, ratio = a)
}

safe_ewm2 <- function(x, hl) {
  # for second moment streams
  if (length(x) <= 2L) return(cummean_vec(x))
  TTR::EMA(x, ratio = alpha_from_hl(hl))
}

hl_s <- 2; hl_m <- 5; hl_l <- 15

features <- combined_stats %>%
  arrange(player_full_name, start_time_utc) %>%
  group_by(player_full_name) %>%
  mutate(
    ew_s = safe_ewm(disposals, hl_s),
    ew_m = safe_ewm(disposals, hl_m),
    ew_l = safe_ewm(disposals, hl_l),
    
    ew_s_sq = safe_ewm2(disposals^2, hl_s),
    ew_m_sq = safe_ewm2(disposals^2, hl_m),
    ew_l_sq = safe_ewm2(disposals^2, hl_l),
    
    sd_s = sqrt(pmax(ew_s_sq - ew_s^2, 0)),
    sd_m = sqrt(pmax(ew_m_sq - ew_m^2, 0)),
    sd_l = sqrt(pmax(ew_l_sq - ew_l^2, 0)),
    
    mom_s_m = ew_s - ew_m,
    mom_m_l = ew_m - ew_l,
    mom_s_l = ew_s - ew_l,
    
    slope_s = ew_s - lag(ew_s),
    slope_m = ew_m - lag(ew_m),
    slope_l = ew_l - lag(ew_l),
    
    z_m = (disposals - ew_m) / sd_m,
    z_l = (disposals - ew_l) / sd_l
  ) %>%
  ungroup()