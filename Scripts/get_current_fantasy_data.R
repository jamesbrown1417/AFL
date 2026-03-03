# Get all afl fantasy in current season and save as an RDS file:

# Libraries and functions
library(purrr)
library(tidyverse)

# Function to fix team names
source("Functions/fix_team_names.R")

# Source functions
source("Functions/data_processing_functions.R")

# Apply function to years
raw_2026_data <- tryCatch(
  get_fantasy_data(season = 2026),
  error = function(e) {
    message("No 2026 fantasy data available yet; writing an empty 2026 dataset.")
    NULL
  }
)

if (is.null(raw_2026_data) || nrow(raw_2026_data) == 0) {
  template_path <- "Data/afl_fantasy_2015_2025_data.rds"

  if (!file.exists(template_path)) {
    stop("Data/afl_fantasy_2015_2025_data.rds is required to define 2026 output schema.")
  }

  afl_fantasy_2026_data <- read_rds(template_path) |> slice_head(n = 0)
} else {
  # Fix team names
  afl_fantasy_2026_data <-
    raw_2026_data |> 
    mutate(player_team = fix_team_names(player_team)) |>
    mutate(opposition_team = fix_team_names(opposition_team)) |> 
    mutate(home_team = fix_team_names(home_team)) |>
    mutate(away_team = fix_team_names(away_team)) |> 
    mutate(match = paste0(home_team, " Vs ", away_team))
}

# Output as an RDS object
saveRDS(afl_fantasy_2026_data, "Data/afl_fantasy_2026_data.rds")
