#!/bin/bash

# Give access to normal path variables
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# Set the current directory to your project folder
cd /Users/jamesbrown/Projects/AFL || exit

# Execute R scripts in sequence
Rscript Scripts/get_schedule.R
Rscript Scripts/get_current_fantasy_data.R
backend/.venv/bin/python backend/scripts/generate_dvp.py --refresh-fantasy-positions --refresh-detailed-positions
Rscript Scripts/get_empirical_probabilities_last_n.R
Rscript Scripts/get_empirical_probabilties_2025.R
Rscript Scripts/all_player_home_vs_away.R
Rscript Scripts/all_player_win_vs_loss.R
