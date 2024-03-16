# Libraries
library(tidyverse)
library(rvest)
library(httr2)

# URL to get responses
tab_url = "https://api.beta.tab.com.au/v1/recommendation-service/AFL%20Football/featured?jurisdiction=SA"
tab_url = "https://api.beta.tab.com.au/v1/tab-info-service/sports/AFL%20Football/competitions/AFL?homeState=SA&jurisdiction=SA"

# Function to fix team names
source("Functions/fix_team_names.R")

# Betfair URL


#===============================================================================
# Get Betfair Markets
#===============================================================================