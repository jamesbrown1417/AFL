#!/bin/bash

# Give access to normal path variables
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# Set the current directory to your project folder
cd /Users/jamesbrown/Projects/AFL || exit

# Remove .json and .txt files in specific directories
rm OddsScraper/Neds/*.json
rm Data/BET365_HTML/*.txt

# Execute Python and R scripts
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/get_bet365_html.py
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/get_bet365_player.py

/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/Neds/get_neds_urls.py
Rscript OddsScraper/Neds/get_neds_match_urls.R
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/Neds/get_match_json.py

# Execute R script for getting arbs
Rscript OddsScraper/master_processing_script.R

# Publish reports using Quarto
echo "1" | quarto publish quarto-pub Reports/afl-odds.qmd

# Automatically stage all changes
git add .

# Commit changes with a message including "automated commit" and the current timestamp
commitMessage="automated commit and timestamp $(date '+%Y-%m-%d %H:%M:%S')"
git commit -m "$commitMessage"

# Push the commit to the 'main' branch on 'origin'
git push origin main