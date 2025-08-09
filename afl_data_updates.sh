#!/bin/bash

# Give access to normal path variables
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# Set the current directory to your project folder
cd /Users/jamesbrown/Projects/AFL || exit

# Execute R scripts in sequence
Rscript Scripts/get_current_fantasy_data.R
Rscript DVP/get_dvp.R
Rscript Scripts/get_empirical_probabilities_last_n.R
Rscript Scripts/get_empirical_probabilties_2025.R
