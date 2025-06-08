# Import Modules=============================================================
from selenium_driverless import webdriver
from selenium_driverless.types.by import By
from datetime import datetime,  timezone
import pandas as pd

# Get current timestamp=======================================================
now = datetime.now()
time_stamp = now.strftime("%Y-%m-%d_%H-%M-%S")

# Read in schedule=============================================================

# Read csv (no header col)
schedule_df = pd.read_csv("Data/current_fixture.csv")

# Convert to datetime (keeps UTC timezone)
schedule_df['start_time'] = pd.to_datetime(schedule_df['start_time'])

# Get games that have not started
schedule_df = schedule_df[schedule_df['start_time'] > datetime.now(timezone.utc)]

# Get next or current round
current_round = schedule_df.iloc[0]['round']

# Get schedule for current round
schedule_df_current = schedule_df[schedule_df['round'] == current_round]

# Get number of remaining games
remaining_games = len(schedule_df_current)

# Get H2H HTML===============================================================
import asyncio

async def main():
    options = webdriver.ChromeOptions()
    # options.add_argument("--headless=True")

    async with webdriver.Chrome(options=options) as driver:
        await driver.get('https://www.bet365.com.au/#/AC/B36/C21011844/D48/E360013/F48')
        await driver.sleep(0.1)
        
        # wait 100s for elem to exist
        elem = await driver.find_element(By.XPATH, "//div[contains(@class, 'gl-MarketGroup_Wrapper ')]", timeout=100)
        body_html = await elem.get_attribute('outerHTML')
        
        # Write html to file - overwrite existing file
        with open(f"Data/BET365_HTML/h2h_html.txt", 'w') as f:
            f.write(body_html)
        
        # Get all occurences of src-ParticipantFixtureDetailsHigher_Team, we want to click on each one
        team_elements = await driver.find_elements(By.XPATH, "//div[contains(@class, 'src-ParticipantFixtureDetailsHigher_TeamNames')]")
        
        # Print team elements inner text
        for team_element in team_elements:
            print(await team_element.get_attribute('innerText'))
        
        # URL List
        url_list = []
        
        for index in range(len(schedule_df_current)):
            # Get the team elements again as the page has been refreshed
            team_elements = await driver.find_elements(By.XPATH, "//div[contains(@class, 'src-ParticipantFixtureDetailsHigher_TeamNames')]")
            
            # Scroll into view, in the middle of screen
            await driver.execute_script("arguments[0].scrollIntoView(true);", team_elements[index])
            await driver.execute_script("window.scrollBy(0, -150)")
            
            # Wait 5 seconds
            await driver.sleep(0.1)
            
            # Click on the current element
            await team_elements[index].click()
            
            # Get Current URL
            cur_url = await driver.current_url

            # Append 'I99/' to URL
            modified_url = cur_url + 'I99/'
            
            url_list.append(modified_url)
            
            print(modified_url)
            
            # Go back to the previous page
            await driver.back()
            
        # Write URL as a csv
        url_list = '\n'.join(url_list)
        with open(f"Data/BET365_HTML/urls.csv", 'w') as f:
           f.write(url_list)

asyncio.run(main())
