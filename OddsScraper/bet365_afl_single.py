"""
Single-run Bet365 AFL scraper using one driverless Chrome instance.

Performs both steps in order:
1) Load main market page and save H2H HTML
2) Collect player prop URLs and save each match's player HTML (two tabs)
"""

# Import Modules=============================================================
from selenium_driverless import webdriver
from selenium_driverless.types.by import By
from datetime import datetime, timezone
import pandas as pd
import asyncio
import os
from dotenv import load_dotenv

# Get current timestamp=======================================================
now = datetime.now()
time_stamp = now.strftime("%Y-%m-%d_%H-%M-%S")

# Load environment for optional login
load_dotenv()
BET365_USERNAME = os.getenv("BET365USER")
BET365_PASSWORD = os.getenv("BET365PW")

async def collect_h2h_and_urls(driver):
    """Navigate to main AFL page, save H2H HTML, and return list of player URLs.

    Uses upcoming fixtures to determine how many matches to click.
    """

    # Read in schedule=======================================================
    schedule_df = pd.read_csv("Data/current_fixture.csv")
    schedule_df["start_time"] = pd.to_datetime(schedule_df["start_time"])  # keep timezone
    schedule_df = schedule_df[schedule_df["start_time"] > datetime.now(timezone.utc)]
    current_round = schedule_df.iloc[0]["round"]
    schedule_df_current = schedule_df[schedule_df["round"] == current_round]

    # AFL all matches page
    await driver.get("https://www.bet365.com.au/#/AC/B36/C21011844/D48/E360013/F48")
    await driver.sleep(0.5)

    # Attempt to detect main market container; if not present, try login
    container_xpath = "//div[contains(@class, 'gl-MarketGroup_Wrapper')]"
    try:
        elem = await driver.find_element(By.XPATH, container_xpath, timeout=10)
        print("Market container found - already logged in or login not required")
    except Exception:
        print("Market container not found - attempting login")
        if not BET365_USERNAME or not BET365_PASSWORD:
            print("BET365 credentials not found in environment; continuing without login")
        else:
            try:
                login_trigger = await driver.find_element(By.XPATH, "//div[contains(@class, 'hm-MainHeaderRHSLoggedOutWide_Login')]", timeout=10)
                await login_trigger.click()
                await driver.sleep(1)

                username_field = await driver.find_element(By.XPATH, "//input[@placeholder='Username or email address']", timeout=10)
                await username_field.clear()
                await driver.sleep(0.2)
                await username_field.send_keys(BET365_USERNAME)

                password_field = await driver.find_element(By.XPATH, "//input[@placeholder='Password']", timeout=10)
                await password_field.clear()
                await driver.sleep(0.2)
                await password_field.send_keys(BET365_PASSWORD)

                login_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'lms-LoginButton')]", timeout=10)
                await login_button.click()
                print("Clicked login button")

                # Wait for market container after login
                elem = await driver.find_element(By.XPATH, container_xpath, timeout=30)
                print("Market container found after login")
            except Exception as e:
                print(f"Login attempt failed: {e}")
                # Fallback: still try to find the container with a longer wait
                elem = await driver.find_element(By.XPATH, container_xpath, timeout=60)

    # Wait for market container and capture HTML
    # (elem set in blocks above)
    body_html = await elem.get_attribute("outerHTML")

    # Persist H2H HTML
    with open("Data/BET365_HTML/h2h_html.txt", "w") as f:
        f.write(body_html)

    # Discover team rows (match links)
    team_xpath = "//div[contains(@class, 'src-ParticipantFixtureDetailsHigher_TeamNames')]"
    team_elements = await driver.find_elements(By.XPATH, team_xpath)

    # Log discovered fixtures (best-effort)
    for team_element in team_elements:
        try:
            print(await team_element.get_attribute("innerText"))
        except Exception:
            pass

    player_urls = []
    matches_to_click = len(schedule_df_current)

    for index in range(matches_to_click):
        # Re-query elements each loop as DOM may refresh
        team_elements = await driver.find_elements(By.XPATH, team_xpath)

        # Safety: skip if fewer items than expected
        if index >= len(team_elements):
            break

        await driver.execute_script("arguments[0].scrollIntoView(true);", team_elements[index])
        await driver.execute_script("window.scrollBy(0, -150)")
        await driver.sleep(0.1)

        await team_elements[index].click()

        cur_url = await driver.current_url
        # AFL player markets suffix
        modified_player_url = cur_url + "I99/"
        player_urls.append(modified_player_url)

        await driver.back()

    # Optionally persist URL list for debugging/traceability
    try:
        with open("Data/BET365_HTML/urls.csv", "w") as f:
            f.write("\n".join(player_urls))
    except Exception:
        pass

    return player_urls


async def scrape_player_pages(driver, player_urls):
    """Iterate player URLs, expand sections, and save player HTML per match."""

    async def safe_click_show_more_all():
        # Click horizontal carousels "Show more" buttons
        try:
            buttons = await driver.find_elements(By.XPATH, "//div[contains(@class, 'bbl-ShowMoreForHScroll ') and contains(text(), 'Show more')]")
            for b in buttons:
                try:
                    await driver.execute_script("arguments[0].scrollIntoView(true);", b)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await b.click()
                    await driver.sleep(1)
                except Exception:
                    pass
        except Exception:
            pass

        # Click vertical section "Show more" buttons (e.g., specials)
        try:
            v_buttons = await driver.find_elements(By.XPATH, "//div[contains(@class, 'bbl-ShowMore ') and text()='Show more']")
            for vb in v_buttons:
                try:
                    await driver.execute_script("arguments[0].scrollIntoView(true);", vb)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await vb.click()
                    await driver.sleep(1)
                except Exception:
                    pass
        except Exception:
            pass

    for index, url in enumerate(player_urls, start=1):
        try:
            await driver.get(url)
            # Wait for Disposals market group to exist (page ready)
            await driver.find_element(By.XPATH, "//div[contains(@class, 'gl-MarketGroupButton_Text') and text()='Disposals']", timeout=30)

            # Expand Disposals + nested sections where present
            async def maybe_click(xpath_text):
                try:
                    el = await driver.find_element(By.XPATH, f"//div[contains(@class, 'gl-MarketGroupButton_Text') and text()='{xpath_text}']", timeout=3)
                    await driver.execute_script("arguments[0].scrollIntoView(true);", el)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await el.click()
                    await driver.sleep(2)
                except Exception:
                    pass

            await maybe_click("Disposals")
            await maybe_click("Player Disposals")
            await maybe_click("Disposal Specials")

            # Click all visible "Show more" buttons
            await safe_click_show_more_all()

            # Capture first tab/section HTML
            elem = await driver.find_element(By.XPATH, "//div[contains(@class, 'wcl-PageContainer_Colcontainer ')]")
            body_html_players_a = await elem.get_attribute("outerHTML")
            with open(f"Data/BET365_HTML/body_html_players_a_match_{index}.txt", "w") as f:
                f.write(body_html_players_a)

            # Switch to the second tab of second section (matches prior behavior)
            try:
                tab_elements = await driver.find_elements(By.XPATH, "//div[contains(@class, 'bbl-TabSwitcherItem_TabText ')]")
                if len(tab_elements) > 3:
                    await driver.execute_script("arguments[0].scrollIntoView(true);", tab_elements[3])
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await tab_elements[3].click()
                    await driver.sleep(2)
            except Exception:
                pass

            # Expand any additional show-more buttons on this tab
            await safe_click_show_more_all()

            # Capture second tab/section HTML
            elem = await driver.find_element(By.XPATH, "//div[contains(@class, 'wcl-PageContainer_Colcontainer ')]")
            body_html_players_b = await elem.get_attribute("outerHTML")
            with open(f"Data/BET365_HTML/body_html_players_b_match_{index}.txt", "w") as f:
                f.write(body_html_players_b)

        except Exception as e:
            print(f"An error occurred with URL {url}: {e}. Moving to the next URL.")
            continue


async def main():
    options = webdriver.ChromeOptions()
    # options.add_argument("--headless=True")

    async with webdriver.Chrome(options=options) as driver:
        try:
            await driver.minimize_window()
        except Exception:
            pass

        player_urls = await collect_h2h_and_urls(driver)
        await scrape_player_pages(driver, player_urls)


if __name__ == "__main__":
    asyncio.run(main())
