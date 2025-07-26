library(tidyverse)
library(brms)

# Read in data
combined_feats <- read_rds("Modelling/main/Data/afl_ewm_features.rds")

priors <- c(
  prior(normal(0, 1), class = "b"),
  prior(student_t(3, 0, 2.5), class = "Intercept"),
  prior(exponential(1), class = "sd"),
  prior(exponential(1), class = "shape")
)

fit_nb <- brm(
  disposals ~ s_pred_ew_l + s_mom_s_l + s_sd_l + (1|player_full_name),
  family = negbinomial(),
  data = combined_feats,
  prior = priors,
  chains = 4, cores = 4, iter = 4000, seed = 42,
  control = list(adapt_delta = 0.95, max_treedepth = 12)
)