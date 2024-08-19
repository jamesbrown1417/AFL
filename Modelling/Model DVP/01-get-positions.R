#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(fitzRoy)
library(uwot)  # Changed from umap to uwot
library(dbscan)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Read in Fantasy Data
#===============================================================================

# Player Stats
player_stats_2024 <-
fetch_player_stats_afl(season=2024)

# Get Player Attributes
player_attributes_2024 <-
fetch_player_details_afl(season = 2024)

# Perform One Hot Encoding of position column
player_attributes_2024_onehot <-
  player_attributes_2024 |>
  mutate(position = factor(position)) |>
  pivot_wider(names_from = position, values_from = position, values_fn = length, values_fill = 0) |> 
  rename(player_id = providerId) |> 
  select(player_id, KEY_FORWARD:MIDFIELDER_FORWARD)

player_stats_table <-
player_stats_2024 |>
  filter(timeOnGroundPercentage >= 60) |>
  mutate(
    player_full_name = paste(player.givenName, player.surname, sep = " "),
    player_id = player.playerId,
    player_team = team.name,
    match_date = utcStartTime,
    season = year(utcStartTime),
    player_position = factor(player.player.position)
  ) |>
  select(
    player_full_name,
    player_id,
    player_team,
    match_date,
    season,
    # player_position,
    goals:extendedStats.kickinsPlayon,-superGoals
  ) |> 
  left_join(player_attributes_2024_onehot, by = "player_id")

#===============================================================================
# Function to perform UMAP, using player's last 10 games
#===============================================================================

# Create Function
perform_umap <- function(player_stats_table, cutoff_date) {
  
  # Select only the columns we need
  umap_data <-
    player_stats_table |>
    filter(match_date <= cutoff_date) |>
    select(
      goals:MIDFIELDER_FORWARD
    ) |>
    # Scale numeric columns
    mutate_if(is.numeric, scale) |>
    # Remove columns with all missing values
    select_if(~!all(is.na(.)))
  
  # Create a UMAP object using uwot
  umap_result <- uwot::umap(
    umap_data,
    n_neighbors = 200,
    n_components = 20,
    metric = "cosine",
    min_dist = 0.01,
    n_threads = parallel::detectCores() - 1,  # Use all but one core
    batch = TRUE,  # Use batch processing for large datasets
    # pca = 50,  # Perform initial PCA for speedup
    fast_sgd = TRUE  # Use fast stochastic gradient descent
  )
  
  # Create a list to mimic the structure of the umap package output
  umap_object <- list(
    layout = umap_result,
    player_full_name = player_stats_table$player_full_name,
    match_date = player_stats_table$match_date
  )
  
  # Perform HDBSCAN on results from UMAP
  hdbscan_result <- hdbscan(umap_object$layout, minPts = 50) 
  
  # Create data frame from the umap object
  umap_df <-
    umap_object$layout |>
    as_tibble(name_repair = "minimal") |>
    mutate(player_full_name = umap_object$player_full_name) |>
    mutate(match_date = umap_object$match_date) |>
    relocate(player_full_name, match_date, .before = V1) |> 
    rename(UMAP1 = V1, UMAP2 = V2) |> 
    # Add cluster labels
    mutate(cluster = hdbscan_result$cluster) 
  
  # Get only players with 10 datapoints
  player_positions <-
    umap_df |> 
    group_by(player_full_name) |>
    filter(n() >= 10) |>
    # Get last 10 games
    arrange(player_full_name, desc(match_date)) |>
    slice_head(n = 10)
  
  # Get each player's most common position in the last 10
  player_positions <-
    player_positions |> 
    group_by(player_full_name) |> 
    summarise(
      player_position = names(which.max(table(cluster)))
    )
  
  # Return the player positions
  return(player_positions)
}

