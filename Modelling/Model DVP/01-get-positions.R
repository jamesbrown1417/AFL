#===============================================================================
# Libraries and functions
#===============================================================================

library(tidyverse)
library(fitzRoy)
`%notin%` <- Negate(`%in%`)

#===============================================================================
# Read in Fantasy Data
#===============================================================================

player_stats_2024 <-
fetch_player_stats_afl(season=2024)

player_stats_2024 |> 
  transmute(player_full_name = paste(player.givenName, player.surname, sep = " "),
            player_team = team.name,
            match_date = utcStartTime,
            season = year(utcStartTime)
            )
