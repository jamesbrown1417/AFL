#!/bin/bash

# Set the current directory to your project folder
cd /Users/jamesbrown/Projects/AFL || exit

# Execute R scripts in sequence
Rscript Scripts/get_dvp.R
Rscript Scripts/empirical_probs_last_n.R
Rscript Scripts/empirical_probs_2025.R
Rscript Scripts/get_current_fantasy.R

# Automatically stage all changes
git add .

# Commit changes with a message including "automated commit" and the current timestamp
commitMessage="automated commit: data updates $(date '+%Y-%m-%d %H:%M:%S')"
git commit -m "$commitMessage"

# Push the commit to the 'main' branch on 'origin'
git push origin main