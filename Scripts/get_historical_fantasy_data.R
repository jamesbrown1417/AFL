# Get all afl fantasy data since 2015 and save as an RDS file:

# Source functions
source("Functions/data_processing_functions.R")

# Libraries and functions
library(purrr)

# Vector of years
years = 2015:2023

# Apply function to years
afl_fantasy_2015_2023_data <-
  map(years, get_fantasy_data)

# Bind together the tibbles
afl_fantasy_2015_2023_data <-
  afl_fantasy_2015_2023_data |> 
  reduce(dplyr::bind_rows)

# Output as an RDS object
saveRDS(afl_fantasy_2015_2023_data, "Data/afl_fantasy_2015_2023_data.rds")

