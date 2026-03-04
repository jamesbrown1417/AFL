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

# Load environment variables: try default .env, then fallback to 'env'
load_dotenv()
if os.getenv('BET365USER') is None or os.getenv('BET365PW') is None:
    load_dotenv('/Users/jamesbrown/Projects/AFL/env')

# Read credentials after loading
username = os.getenv('BET365USER')
password = os.getenv('BET365PW')

# Validate credentials early with a clear error
if not username or not password:
    raise RuntimeError(
        "Missing Bet365 credentials. Set BET365USER and BET365PW in .env or env, or export them in the environment."
    )

# Lower-case projection for case-insensitive XPath text matching.
XPATH_LOWER_TEXT = (
    "translate(normalize-space(string(.)), "
    "'ABCDEFGHIJKLMNOPQRSTUVWXYZ', "
    "'abcdefghijklmnopqrstuvwxyz')"
)


async def find_first_element(driver, locator_candidates, timeout_per_candidate=3):
    """Try locators in order and return the first element that can be found."""
    last_error = None
    for by, value in locator_candidates:
        try:
            return await driver.find_element(by, value, timeout=timeout_per_candidate)
        except Exception as exc:
            last_error = exc
    if last_error:
        raise last_error
    raise RuntimeError("No locator candidates provided")


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
    await driver.get("https://www.bet365.com.au/#/AC/B36/C21101752/D48/E360013/F48/")
    await driver.sleep(2)

    # Always perform login each run
    print("Attempting login...")
    login_locator_candidates = [
        # Most stable header container when logged out.
        (By.XPATH, "//div[contains(@class, 'hm-MainHeaderRHSLoggedOutWide_Login')]"),
        # Dynamic hrm-* class token, matched by prefix and label text.
        (
            By.XPATH,
            f"//span[contains(@class, 'hrm-') and (contains({XPATH_LOWER_TEXT}, 'log in') or contains({XPATH_LOWER_TEXT}, 'login'))]",
        ),
        # Generic clickable fallback based on visible label.
        (
            By.XPATH,
            f"//*[self::button or self::a][contains({XPATH_LOWER_TEXT}, 'log in') or contains({XPATH_LOWER_TEXT}, 'login')]",
        ),
    ]
    login_element = await find_first_element(
        driver, login_locator_candidates, timeout_per_candidate=4
    )
    await driver.sleep(2)
    try:
        await login_element.click()
    except Exception:
        await driver.execute_script("arguments[0].click();", login_element)
    await driver.sleep(1)

    username_field = await driver.find_element(By.XPATH, "//input[@placeholder='Username or email address']", timeout=10)
    await username_field.clear()
    await driver.sleep(0.3)
    await username_field.send_keys(username)
    print("Entered username")

    password_field = await driver.find_element(By.XPATH, "//input[@placeholder='Password']", timeout=10)
    await password_field.clear()
    await driver.sleep(0.3)
    await password_field.send_keys(password)
    print("Entered password")

    login_submit_locator_candidates = [
        (
            By.XPATH,
            f"//input[@placeholder='Password']/ancestor::form//*[self::button or self::span][contains({XPATH_LOWER_TEXT}, 'log in') or contains({XPATH_LOWER_TEXT}, 'login')]",
        ),
        (By.XPATH, "//span[starts-with(@class, 'slm')]"),
    ]
    login_button = await find_first_element(
        driver, login_submit_locator_candidates, timeout_per_candidate=3
    )
    try:
        await login_button.click()
    except Exception:
        await driver.execute_script("arguments[0].click();", login_button)
    print("Clicked login button")

    print("Waiting 2 seconds...")
    await driver.sleep(2)

    await driver.minimize_window()

    # Wait for market container after login
    container_xpath = "//div[contains(@class, 'gl-MarketGroup_Wrapper')]"
    elem = await driver.find_element(By.XPATH, container_xpath, timeout=10)
    print("Market container found after login")

    # Wait for market container and capture HTML
    body_html = await elem.get_attribute("outerHTML")

    # Persist H2H HTML
    with open("Data/BET365_HTML/h2h_html.txt", "w") as f:
        f.write(body_html)

    print("Waiting 2 seconds...")
    await driver.sleep(2)

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
        print(f"Getting base URL for match {index}")
        # Re-query elements each loop as DOM may refresh
        team_elements = await driver.find_elements(By.XPATH, team_xpath)

        # Safety: skip if fewer items than expected
        if index >= len(team_elements):
            print(
                f"Skipping match {index}: Index out of range. (Found {len(team_elements)} matches on site, tried accessing index {index})"
            )
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
        await driver.sleep(0.5)

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
            print(f"\n{'='*60}")
            print(f"Processing match {index}")
            print(f"{'='*60}")
            print(f"URL: {url}")

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
                    print(f"  Clicked '{xpath_text}'")
                    await driver.sleep(2)
                except Exception:
                    print(f"  No '{xpath_text}' button found")

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
            print(f"  Saved: Data/BET365_HTML/body_html_players_a_match_{index}.txt")

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
            print(f"  Saved: Data/BET365_HTML/body_html_players_b_match_{index}.txt")

        except Exception as e:
            print(f"  Error with match {index}: {e}. Continuing...")
            continue


async def main():
    options = webdriver.ChromeOptions()
    # options.add_argument("--headless=True")

    async with webdriver.Chrome(options=options) as driver:
        player_urls = await collect_h2h_and_urls(driver)
        await scrape_player_pages(driver, player_urls)


if __name__ == "__main__":
    asyncio.run(main())
