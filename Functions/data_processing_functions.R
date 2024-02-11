#===============================================================================
# 01 - Get Fantasy Data
#===============================================================================

get_fantasy_data <- function(season, round_number = NULL) {
  
  # Get match data
  match_data <- fitzRoy::fetch_results_afl(season = season, round_number = round_number)
  
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
      home_team = match.homeTeam.name,
      away_team = match.awayTeam.name,
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
    dplyr::transmute(
      match_name = paste0(home.team.name, " Vs ", away.team.name),
      venue = venue.name,
      start_time_utc = lubridate::ymd_hms(utcStartTime),
      round = round.name,
      home_team = home.team.name,
      away_team = away.team.name,
      player_id = player.player.player.playerId,
      player_first_name = player.player.player.givenName,
      player_last_name = player.player.player.surname,
      player_full_name = paste(player_first_name, player_last_name),
      player_number = player.player.player.playerJumperNumber,
      fantasy_points = dreamTeamPoints,
      goals,
      behinds,
      disposals,
      kicks,
      handballs,
      marks,
      tackles,
      hitouts,
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
    dplyr::left_join(player_stats, multiple = "all")
  
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

#===============================================================================
# 02 - Create Player Season Summary Table
#===============================================================================

get_season_summary_stats <- function(df, season) {
  output <-
  df |>
    filter(season_name == season) |>
    group_by(player_full_name) |>
    summarise(
      MIN = min(fantasy_points, na.rm = TRUE),
      AVG = mean(fantasy_points, na.rm = TRUE),
      MED = median(fantasy_points, na.rm = TRUE),
      MAX = max(fantasy_points, na.rm = TRUE),
      SD = sd(fantasy_points, na.rm = TRUE),
      GP = n()
    ) |>
    mutate(across(where(is.numeric),  \(x) round(x, digits = 1))) |>
    arrange(desc(MED), desc(AVG)) |> 
    rename(player = player_full_name)
  
  # Add season to var names
  names(output)[-1] <- paste(season, names(output)[-1], sep = " ")
  
  # return data
  return(output)
}

#===============================================================================
# 03 - Create Player Career Summary Table
#===============================================================================

get_career_summary_stats <- function(df) {
  output <-
    df |>
    group_by(player_full_name) |>
    summarise(
      MIN = min(fantasy_points, na.rm = TRUE),
      AVG = mean(fantasy_points, na.rm = TRUE),
      MED = median(fantasy_points, na.rm = TRUE),
      MAX = max(fantasy_points, na.rm = TRUE),
      SD = sd(fantasy_points, na.rm = TRUE),
      GP = n()
    ) |>
    mutate(across(where(is.numeric),  \(x) round(x, digits = 1))) |>
    arrange(desc(MED), desc(AVG)) |> 
    rename(player = player_full_name)
  
  # Add season to var names
  names(output)[-1] <- paste("Career", names(output)[-1], sep = " ")
  
  # return data
  return(output)
}

#===============================================================================
# 04 - Last n games Average
#===============================================================================

get_last_n_average <- function(df, n) {
  output <-
    df |>
    arrange(player_full_name, desc(start_time_utc)) |> 
    group_by(player_full_name) |>
    filter(!is.na(fantasy_points)) |> 
    filter(row_number() <= n) |> 
    summarise(
      AVG = mean(fantasy_points, na.rm = TRUE),
    ) |>
    mutate(across(where(is.numeric),  \(x) round(x, digits = 1))) |>
    arrange(desc(AVG)) |> 
    rename(player = player_full_name)
  
  # Add season to var names
  names(output)[-1] <- paste("LAST", n, names(output)[-1], sep = " ")
  
  # return data
  return(output)
}

