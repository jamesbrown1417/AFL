library(tidyverse)
library(readxl)
afl <- read_excel("afl.xlsx")

# Select only the columns needed------------------------------------------------
afl_odds <-
  afl |>
  select(
Date,
`Home Team`,
`Away Team`,,
`Home Score`,
`Away Score`,
`Home Odds Open`,
`Away Odds Open`,
`Home Odds Close`,
`Away Odds Close`)

# Tidy Names Using Janitor-------------------------------------------------------
afl_odds <- janitor::clean_names(afl_odds)

# Make indicator for a win
afl_odds <-
  afl_odds |>
  # remove draws
  filter(`home_score` != `away_score`) |>
  mutate(
    home_win = if_else(`home_score` > `away_score`, 1, 0),
    away_win = if_else(`away_score` > `home_score`, 1, 0)
  ) |> 
  mutate(home_implied_prob_open = 1 / `home_odds_open`,
         away_implied_prob_open = 1 / `away_odds_open`,
         home_implied_prob_close = 1 / `home_odds_close`,
         away_implied_prob_close = 1 / `away_odds_close`)

# Get Open to closing probability change
afl_odds <-
  afl_odds |>
  mutate(
    home_prob_change = home_implied_prob_close - home_implied_prob_open,
    away_prob_change = away_implied_prob_close - away_implied_prob_open
  )

# Get cases where the odds change forms an arbitrage opportunity
afl_odds_bets_placed <-
  afl_odds |>
  filter(
  1/home_odds_open + 1/away_odds_close < 1|
  1/away_odds_open + 1/home_odds_close < 1
  ) %>%
  mutate(
    team_bet_on = if_else(home_odds_open > home_odds_close, "Home", "Away")) |> 
  mutate(stake = 100) |>
  mutate(
    # bet size to win 100
    bet_size = if_else(
      team_bet_on == "Home",
      100 / (home_odds_open - 1),
      100 / (away_odds_open - 1)
    ),
    profit_loss = if_else(
      team_bet_on == "Home",
      if_else(home_win == 1, bet_size*(home_odds_open - 1), -bet_size),
      if_else(away_win == 1, bet_size*(away_odds_open - 1), -bet_size)
    )
  )

# Summarise bets placed, total staked, total profit/loss and ROI
afl_odds_bets_placed |>
  summarise(
    bets_placed = n(),
    total_staked = sum(bet_size),
    total_profit_loss = sum(profit_loss),
    roi = 100*(total_profit_loss / total_staked)
  )
