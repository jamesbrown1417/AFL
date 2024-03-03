# Import Modules=============================================================
from selenium_driverless import webdriver
from selenium_driverless.types.by import By
from datetime import datetime

# Get current timestamp=======================================================
now = datetime.now()
time_stamp = now.strftime("%Y-%m-%d_%H-%M-%S")

# Read in CSV of URLs=========================================================
import pandas as pd
# Read csv (no header col)
url_df = pd.read_csv('Data/BET365_HTML/urls.csv', header=None)

# Convert first column to a list
url_df = url_df[0]

# Get H2H HTML===============================================================
import asyncio

async def main():
    options = webdriver.ChromeOptions()
    # options.add_argument("--headless=True")

    async with webdriver.Chrome(options=options) as driver:
        # Test using first url
        await driver.get(url_df[0])
        await driver.sleep(10)
        
        # Click on disposal button
        disposal_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'gl-MarketGroupButton_Text ') and contains(text(), 'Disposals')]")
        await disposal_button.click()
        
        # Get all elements with class 'bbl-ShowMoreForHScroll ' that has text 'Show more'
        button_elements = await driver.find_elements(By.XPATH, "//div[contains(@class, 'bbl-ShowMoreForHScroll ') and contains(text(), 'Show more')]")
        
        print(len(button_elements))
        
        # Click on the first button
        await button_elements[0].click()

        # Scroll into view of the second button
        await driver.execute_script("arguments[0].scrollIntoView(true);", button_elements[1])
        # Click on the second button
        await button_elements[1].click()
        
        await driver.sleep(5)
        
        # Write out html to file------------------------------------------------
        # wait 100s for elem to exist
        elem = await driver.find_elements(By.XPATH, "//div[contains(@class, 'gl-MarketGroupContainer')]", timeout=100)
        body_html_goals = await elem[0].get_attribute('outerHTML')
        body_html_disposals = await elem[1].get_attribute('outerHTML')
        
        # Write html to file - overwrite existing file
        with open(f"Data/BET365_HTML/h2h_html.txt", 'w') as f:
            f.write(body_html)
        
        # Get "bbl-TabSwitcherItem_TabText " element
        tab_elements = await driver.find_elements(By.XPATH, ".//div[contains(@class, 'bbl-TabSwitcherItem_TabText ')]")
        print(len(tab_elements))
        
        # Click on the second tab of second section after scrolling into view
        await driver.execute_script("arguments[0].scrollIntoView(true);", tab_elements[3])
        await tab_elements[3].click()
        
        await driver.sleep(5)
        
        # Get all elements with class 'bbl-ShowMoreForHScroll ' that has text 'Show more'
        button_elements_2 = await driver.find_elements(By.XPATH, "//div[contains(@class, 'bbl-ShowMoreForHScroll ') and contains(text(), 'Show more')]")
        
        print(len(button_elements_2))
        
        await driver.sleep(5)
        
        # Click on the first button after scrolling into view
        await driver.execute_script("arguments[0].scrollIntoView(true);", button_elements_2[0])
        await button_elements_2[0].click()
        
        await driver.sleep(5)


        

asyncio.run(main())