#!/bin/bash
set -euo pipefail

# Give access to normal path variables
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# Set the current directory to your project folder
cd /Users/jamesbrown/Projects/AFL || exit

# Remove cached Bet365 scraper artifacts that must be regenerated.
find Data/BET365_HTML -maxdepth 1 -name '*.txt' -delete

# Execute the Bet365 scraper
if ! /Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/bet365_afl_single.py; then
    echo "Bet365 scrape failed."
    exit 1
fi
