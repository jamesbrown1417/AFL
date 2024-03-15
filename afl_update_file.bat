C:/Python311/python.exe c:/Users/james/R_Projects/AFL-2024/OddsScraper/get_bet365_html.py
C:/Python311/python.exe c:/Users/james/R_Projects/AFL-2024/OddsScraper/get_bet365_player.py

cd /d "C:\Users\james\R_Projects\AFL-2024"
Rscript OddsScraper\master_processing_script.R
echo 1 | quarto publish quarto-pub Reports\outlier-odds.qmd
