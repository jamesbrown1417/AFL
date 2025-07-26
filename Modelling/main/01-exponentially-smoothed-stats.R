library(tidyverse)
library(TTR)

# Read in data
afl_fantasy_2015_2024_data <- readRDS("Data/afl_fantasy_2015_2024_data.rds")
afl_fantasy_2025_data <- readRDS("Data/afl_fantasy_2025_data.rds")

# Combine data
combined_stats <-
  bind_rows(afl_fantasy_2015_2024_data, afl_fantasy_2025_data)

#===============================================================================
# Get weighted averages
#===============================================================================

alpha_from_hl <- function(hl) 1 - 0.5^(1/hl)

# Robust EMA: handles empty vectors, all-NA runs, and carries forward when x is NA.
ema_vec <- function(x, alpha) {
  # validate alpha
  if (length(alpha) != 1L || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a single finite number in (0,1).")
  }
  x <- as.numeric(x)
  n <- length(x)
  if (n == 0L) return(numeric(0))
  
  out <- rep(NA_real_, n)
  
  # seed at first non-NA observation
  first <- which(!is.na(x))[1]
  if (is.na(first)) {
    # all NA -> leave as NA
    return(out)
  }
  out[first] <- x[first]
  
  if (first < n) {
    for (i in (first + 1):n) {
      prev <- out[i - 1]
      if (is.na(x[i])) {
        # no new info -> flat forward the previous EMA
        out[i] <- prev
      } else if (is.na(prev)) {
        # previous EMA missing (shouldn't happen with this logic), fall back to x[i]
        out[i] <- x[i]
      } else {
        out[i] <- alpha * x[i] + (1 - alpha) * prev
      }
    }
  }
  
  # positions before 'first' stay NA (no prior info)
  out
}

add_feats <- function(g, a_s, a_m, a_l) {
  x  <- as.numeric(g$disposals)
  x2 <- x^2
  
  ew_s <- ema_vec(x,  a_s)
  ew_m <- ema_vec(x,  a_m)
  ew_l <- ema_vec(x,  a_l)
  
  ew_s2 <- ema_vec(x2, a_s)
  ew_m2 <- ema_vec(x2, a_m)
  ew_l2 <- ema_vec(x2, a_l)
  
  sd_s <- sqrt(pmax(ew_s2 - ew_s^2, 0))
  sd_m <- sqrt(pmax(ew_m2 - ew_m^2, 0))
  sd_l <- sqrt(pmax(ew_l2 - ew_l^2, 0))
  
  # Strictly prior-info predictors
  g$pred_ew_s <- dplyr::lag(ew_s)
  g$pred_ew_m <- dplyr::lag(ew_m)
  g$pred_ew_l <- dplyr::lag(ew_l)
  
  g$pred_sd_s <- dplyr::lag(sd_s)
  g$pred_sd_m <- dplyr::lag(sd_m)
  g$pred_sd_l <- dplyr::lag(sd_l)
  
  g$pred_mom_s_m <- dplyr::lag(ew_s - ew_m)
  g$pred_mom_m_l <- dplyr::lag(ew_m - ew_l)
  g$pred_mom_s_l <- dplyr::lag(ew_s - ew_l)
  
  g$pred_slope_s <- dplyr::lag(ew_s - dplyr::lag(ew_s))
  g$pred_slope_m <- dplyr::lag(ew_m - dplyr::lag(ew_m))
  g$pred_slope_l <- dplyr::lag(ew_l - dplyr::lag(ew_l))
  
  g
}

# ---- driver ----
build_ewm_features <- function(data,
                               hl_s = 2, hl_m = 5, hl_l = 15,
                               group_cols = c("player_full_name"),
                               reset_by_season = FALSE) {
  a_s <- alpha_from_hl(hl_s)
  a_m <- alpha_from_hl(hl_m)
  a_l <- alpha_from_hl(hl_l)
  
  gcols <- if (reset_by_season) c(group_cols, "season_name") else group_cols
  
  data %>%
    arrange(across(all_of(c(group_cols, "start_time_utc")))) %>%
    group_by(across(all_of(gcols))) %>%
    group_modify(~ add_feats(.x, a_s, a_m, a_l)) %>%
    ungroup()
}

# Build dataset
combined_feats <- build_ewm_features(combined_stats, reset_by_season = FALSE)

# Write to data folder
write_rds(combined_feats, "Modelling/main/Data/afl_ewm_features.rds")
