# Get all afl fantasy data since 2015 and save as an RDS file:

# Source functions
source("Functions/data_processing_functions.R")

# Libraries and functions
library(tidyverse)

# Apply function to years
afl_fantasy_2024_data <- get_fantasy_data(season = 2024)
  
# Output as an RDS object
saveRDS(afl_fantasy_2024_data, "Data/afl_fantasy_2024_data.rds")