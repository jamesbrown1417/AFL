#===============================================================================
# 01 - Get Modelling Data
#===============================================================================

get_fantasy_data <- function(season, round_number = NULL) {
  
  # Get match data
  match_data <- fitzRoy::fetch_results_afl(season = season, round_number = round_number)
  
  match_data <-
    match_data |>
    tidyr::separate(col = match.name, into = c("home_team", "away_team"), sep = " Vs ", remove = FALSE) |> 
    dplyr::mutate(
      home_team = fix_team_names(home_team),
      away_team = fix_team_names(away_team)
    ) |> 
    dplyr::mutate(
      match.name = paste(home_team, "Vs", away_team)
    )
  
  # Get player details
  player_details <-
    fitzRoy::fetch_player_details_afl(season = season) |>
    dplyr::transmute(
      player_id = providerId,
      player_team = team,
      player_dob = lubridate::ymd(dateOfBirth)
    )
  
  # get player stats
  player_stats <- fitzRoy::fetch_player_stats_afl(season = season, round_number = round_number)
  
  # Fix team names
  player_stats <-
    player_stats |>
    dplyr::mutate(
      home.team.name = fix_team_names(home.team.name),
      away.team.name = fix_team_names(away.team.name))
  
  # Select columns for match details
  match_data <-
    match_data |>
    dplyr::transmute(
      match_name = match.name,
      venue = venue.name,
      start_time_utc = lubridate::ymd_hms(match.utcStartTime),
      round = round.name,
      season_name = round.year,
      temperature = weather.tempInCelsius,
      weather_description = weather.description,
      weather_category = weather.weatherType,
      home_team,
      away_team,
      home_team_goals = homeTeamScore.matchScore.goals,
      home_team_behinds = homeTeamScore.matchScore.behinds,
      home_team_score = homeTeamScore.matchScore.totalScore,
      away_team_goals = awayTeamScore.matchScore.goals,
      away_team_behinds = awayTeamScore.matchScore.behinds,
      away_team_score = awayTeamScore.matchScore.totalScore
    ) |>
    dplyr::mutate(
      match_result = dplyr::case_when(
        home_team_score > away_team_score ~ "Home Win",
        away_team_score > home_team_score ~ "Away Win",
        away_team_score == home_team_score ~ "Draw"
      )
    ) |>
    dplyr::mutate(margin = abs(home_team_score - away_team_score)) |>
    dplyr::mutate(
      match_result_string = dplyr::case_when(
        match_result == "Home Win" ~ glue::glue(
          "{home_team} {home_team_goals}.{home_team_behinds} {home_team_score} def {away_team} {away_team_goals}.{away_team_behinds} {away_team_score} by {margin} points"
        ),
        match_result == "Away Win" ~ glue::glue(
          "{away_team} {away_team_goals}.{away_team_behinds} {away_team_score} def {home_team} {home_team_goals}.{home_team_behinds} {home_team_score} by {margin} points"
        ),
        match_result == "Draw" ~ glue::glue(
          "{home_team} {home_team_goals}.{home_team_behinds} {home_team_score} drew with {away_team} {away_team_goals}.{away_team_behinds} {away_team_score}"
        )
      )
    )
  
  # Select columns for player stats
  player_stats <-
    player_stats |> 
    dplyr::mutate(match_name = paste0(home.team.name, " Vs ", away.team.name)) |> 
    dplyr::filter(match_name %in% match_data$match_name) |>
    dplyr::mutate(start_time_utc = lubridate::ymd_hms(utcStartTime)) |> 
    dplyr::rename(
      venue = venue.name,
      round = round.name,
      home_team = home.team.name,
      away_team = away.team.name,
      player_id = player.player.player.playerId,
      player_first_name = player.player.player.givenName,
      player_last_name = player.player.player.surname,
      player_full_name = paste(player_first_name, player_last_name),
      player_number = player.player.player.playerJumperNumber,
      fantasy_points = dreamTeamPoints,
      frees_for = freesFor,
      frees_against = freesAgainst,
      total_clearances = clearances.totalClearances,
      metres_gained = metresGained,
      goal_assists = goalAssists,
      tog_percentage = timeOnGroundPercentage,
      cbas = extendedStats.centreBounceAttendances,
      kick_ins = extendedStats.kickins,
      kick_ins_play_on = extendedStats.kickinsPlayon,
      kick_to_handball_ratio = extendedStats.kickToHandballRatio,
      hitout_win_percentage = extendedStats.hitoutWinPercentage
    )
  
  # Combine the two tables
  return_table <-
    match_data |> 
    dplyr::left_join(player_stats)
  
  # Create CBA and kickin percentage variables
  return_table <-
    return_table |> 
    dplyr::mutate(cba_percentage = cbas / (4 + home_team_goals + away_team_goals),
                  kick_in_percentage = kick_ins / (home_team_behinds + away_team_behinds)) |> 
    dplyr::relocate(cba_percentage, .after = cbas) |> 
    dplyr::relocate(kick_in_percentage, .after = kick_ins)
  
  # Add dob info
  return_table <-
    return_table |>
    dplyr::left_join(player_details) |> 
    dplyr::relocate(player_dob, player_team, .after = player_full_name)
  
  # Add opposition team variable
  return_table <- 
    return_table |> 
    dplyr::mutate(opposition_team = dplyr::if_else(player_team == home_team, away_team, home_team)) |> 
    dplyr::relocate(opposition_team, .after = player_team)
  
  # Return Table
  return(return_table)
}