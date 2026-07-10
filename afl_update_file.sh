#!/bin/bash
set -euo pipefail

# Give access to normal path variables
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# Set the current directory to your project folder
cd /Users/jamesbrown/Projects/AFL || exit

# Remove cached scraper artifacts that must be regenerated.
rm -f OddsScraper/Neds/neds_response.json OddsScraper/Neds/neds_afl_match_urls.csv
find OddsScraper/Neds -maxdepth 1 -name 'data_*.json' -delete

# Execute Python and R scripts
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/TAB/get-TAB-response.py

/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/Neds/get_neds_urls.py
Rscript OddsScraper/Neds/get_neds_match_urls.R
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/Neds/get_match_json.py

# Execute R script for getting arbs
Rscript OddsScraper/master_processing_script.R
Rscript Scripts/get_arbs.R

# Refresh DVP before backend import so matchup labels in metrics_json stay current.
backend/.venv/bin/python backend/scripts/generate_dvp.py --refresh-fantasy-positions --refresh-detailed-positions

# Generate a read-only health report from the production output files just created.
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 scraper_tests/run.py --mode production --no-fail

# Refresh backend DuckDB state from the newly scraped/processed files.
if ! (
    cd /Users/jamesbrown/Projects/AFL/backend || exit 1
    ./.venv/bin/python scripts/run_import_once.py
); then
    echo "Backend data update failed. Aborting before publish/push."
    exit 1
fi

# Automatically stage all changes
cd /Users/jamesbrown/Projects/AFL/
git add .

# Commit changes with a message including "automated commit" and the current timestamp
commitMessage="automated commit and timestamp $(date '+%Y-%m-%d %H:%M:%S')"
git commit -m "$commitMessage"

# Push the commit to the 'main' branch on 'origin'
git push origin main
