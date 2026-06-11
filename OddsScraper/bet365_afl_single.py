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
from pathlib import Path
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

# Login bypass switch: when enabled, skip the Bet365 login flow entirely and
# scrape public (logged-out) markets. The logged-out shell is currently not
# hydrating AFL/player markets reliably, so production defaults to logging in.
# Override with env BET365_BYPASS_LOGIN=yes only when needed.
BYPASS_LOGIN = os.getenv("BET365_BYPASS_LOGIN", "no").strip().lower() in ("1", "true", "yes", "on")

# Validate credentials early with a clear error (only needed when actually logging in)
if not BYPASS_LOGIN and (not username or not password):
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


def describe_exception(exc):
    message = str(exc).strip()
    if message:
        return f"{type(exc).__name__}: {message}"
    return f"{type(exc).__name__}: <no message>"


async def wait_for_event_url(driver, previous_url, timeout=10):
    """Wait until a fixture click has completed and the URL points at an event."""
    loop = asyncio.get_event_loop()
    deadline = loop.time() + timeout
    last_url = previous_url

    while loop.time() < deadline:
        last_url = await driver.current_url
        if last_url != previous_url and "/D19/E" in last_url:
            return last_url
        await driver.sleep(0.25)

    raise RuntimeError(
        f"Timed out waiting for event URL after click. "
        f"previous_url={previous_url}; last_url={last_url}"
    )


async def click_fixture_and_get_event_url(driver, fixture_element):
    previous_url = await driver.current_url

    try:
        await fixture_element.click()
        return await wait_for_event_url(driver, previous_url, timeout=8)
    except Exception as first_error:
        print(f"  Normal fixture click did not navigate: {describe_exception(first_error)}")

    await driver.execute_script("arguments[0].click();", fixture_element)
    return await wait_for_event_url(driver, previous_url, timeout=10)


async def wait_for_player_container_html(driver, timeout=30):
    """Return player page container HTML once it contains actual market content."""
    loop = asyncio.get_event_loop()
    deadline = loop.time() + timeout
    last_html = ""
    last_error = None

    while loop.time() < deadline:
        try:
            elem = await driver.find_element(
                By.XPATH,
                "//div[contains(@class, 'wcl-PageContainer_Colcontainer')]",
                timeout=2,
            )
            last_html = await elem.get_attribute("outerHTML")
            has_header = "cm-MatchBettingReactHeader" in last_html
            has_market = "gl-MarketGroupButton_Text" in last_html
            has_player_rows = "bbl-BetBuilderParticipantLabel" in last_html
            if len(last_html) > 1000 and has_header and (has_market or has_player_rows):
                return elem, last_html
        except Exception as exc:
            last_error = exc
        await driver.sleep(1)

    current_url = await driver.current_url
    detail = f"last_html_bytes={len(last_html)}; current_url={current_url}"
    if last_error:
        detail = f"{detail}; last_error={describe_exception(last_error)}"
    raise RuntimeError(f"Player page did not load usable market content ({detail})")


async def save_player_container_html(driver, path):
    elem, html = await wait_for_player_container_html(driver)
    Path(path).write_text(html)
    print(f"  Saved: {path} ({len(html)} bytes)")
    return html


async def click_market_group(driver, label, timeout=10):
    label_lower = label.lower()
    group_xpath = (
        "//div[contains(@class, 'gl-MarketGroupPod') "
        f"and .//div[contains(@class, 'gl-MarketGroupButton_Text') and {XPATH_LOWER_TEXT}='{label_lower}']]"
    )
    button_xpath = group_xpath + "//div[contains(@class, 'gl-MarketGroupButton')]"

    button = await driver.find_element(By.XPATH, button_xpath, timeout=timeout)
    await driver.execute_script("arguments[0].scrollIntoView(true);", button)
    await driver.execute_script("window.scrollBy(0, -150)")
    await driver.execute_script("arguments[0].click();", button)
    print(f"  Clicked '{label}'")

    loop = asyncio.get_event_loop()
    deadline = loop.time() + timeout
    while loop.time() < deadline:
        is_open = await driver.execute_script(
            """
            const label = arguments[0].toLowerCase();
            const pods = Array.from(document.querySelectorAll('.gl-MarketGroupPod'));
            const pod = pods.find((node) => {
                const text = (node.innerText || '').toLowerCase();
                return text.split('\\n').some((line) => line.trim() === label);
            });
            if (!pod) return false;
            const hasOpenClass = pod.className.includes('gl-MarketGroup_Open') ||
                Boolean(pod.querySelector('.gl-MarketGroup_Open'));
            const hasMarketContent = Boolean(
                pod.querySelector('.bbl-FilteredMarketGroupWithHScrollerContainer_Wide') ||
                pod.querySelector('.bbl-BetBuilderMarketGroupContainer')
            );
            return hasOpenClass && hasMarketContent;
            """,
            label_lower,
        )
        if is_open:
            return
        await driver.sleep(0.5)

    raise RuntimeError(f"Timed out waiting for '{label}' market group to open")


async def dump_debug_html(driver, path):
    try:
        html = await driver.execute_script("return document.documentElement.outerHTML")
        current_url = await driver.current_url
        payload = f"<!-- current_url: {current_url} -->\n{html or ''}"
        Path(path).write_text(payload)
        print(f"  Debug HTML saved: {path} ({len(payload)} bytes)")
    except Exception as exc:
        print(f"  Could not save debug HTML {path}: {describe_exception(exc)}")


async def wait_for_main_market_container(driver, timeout=30):
    container_xpath = "//div[contains(@class, 'gl-MarketGroup_Wrapper')]"
    last_error = None

    for attempt in range(1, 3):
        try:
            return await driver.find_element(By.XPATH, container_xpath, timeout=timeout)
        except Exception as exc:
            last_error = exc
            print(
                f"Market container not ready on attempt {attempt}: "
                f"{describe_exception(exc)}"
            )
            if attempt < 2:
                await driver.refresh()
                await driver.sleep(5)

    await dump_debug_html(driver, "Data/BET365_HTML/main_market_load_error.txt")
    raise RuntimeError(
        f"Could not find Bet365 main market container: {describe_exception(last_error)}"
    )


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

    # Warm up the Bet365 shell before loading the AFL deep link. Direct hash
    # routes intermittently leave the app on the generic sports shell.
    await driver.get("https://www.bet365.com.au/")
    await driver.sleep(5)

    # AFL all matches page
    await driver.get("https://www.bet365.com.au/#/AC/B36/C21101752/D48/E360013/F48/")
    await driver.sleep(8)

    # Login each run, unless bypassed (Bet365 odds are public when logged out)
    if BYPASS_LOGIN:
        print("BYPASS_LOGIN enabled - skipping login, scraping logged-out markets")
    else:
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

    # Keep the window active. Bet365 lazy-loads several market panels and can
    # return empty shells when the page is backgrounded/minimized.

    elem = await wait_for_main_market_container(driver)
    print("Market container found after login")

    # Capture HTML
    body_html = await elem.get_attribute("outerHTML")

    # Persist H2H HTML
    with open("Data/BET365_HTML/h2h_html.txt", "w") as f:
        f.write(body_html)

    print("Waiting 2 seconds...")
    await driver.sleep(2)

    # Discover team rows (match links)
    team_xpath = (
        "//div[contains(@class, 'src-ParticipantFixtureDetailsHigher') "
        "and contains(@class, 'src-ParticipantFixtureDetailsHigher-wide') "
        "and not(contains(@class, 'Hidden'))]"
    )
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

        cur_url = await click_fixture_and_get_event_url(driver, team_elements[index])
        # AFL player markets suffix
        modified_player_url = cur_url.split("/I")[0].rstrip("/") + "/I99/"
        player_urls.append(modified_player_url)
        print(f"  Player URL: {modified_player_url}")

        await driver.back()
        await driver.sleep(1.5)

    # Optionally persist URL list for debugging/traceability
    try:
        with open("Data/BET365_HTML/urls.csv", "w") as f:
            f.write("\n".join(player_urls))
    except Exception:
        pass

    return player_urls


async def scrape_player_pages(driver, player_urls):
    """Iterate player URLs, expand sections, and save player HTML per match."""
    saved_matches = 0
    failures = []

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
            await driver.sleep(2)
            await wait_for_player_container_html(driver, timeout=30)

            # The default SGM page opens Goalscorer. Save it first, then open
            # Disposals explicitly and save that market separately.
            await safe_click_show_more_all()
            await save_player_container_html(
                driver,
                f"Data/BET365_HTML/body_html_players_a_match_{index}.txt",
            )

            await click_market_group(driver, "Disposals")
            await driver.sleep(2)
            await safe_click_show_more_all()
            await save_player_container_html(
                driver,
                f"Data/BET365_HTML/body_html_players_b_match_{index}.txt",
            )
            saved_matches += 1

        except Exception as e:
            reason = describe_exception(e)
            failures.append((index, url, reason))
            print(f"  Error with match {index}: {reason}. Continuing...")
            continue

    print(f"Saved usable Bet365 player HTML for {saved_matches}/{len(player_urls)} matches")
    if failures:
        print("Bet365 player-page failures:")
        for index, url, reason in failures:
            print(f"  match {index}: {reason}; url={url}")

    if player_urls and saved_matches == 0:
        raise RuntimeError("No usable Bet365 player HTML files were saved")


async def main():
    options = webdriver.ChromeOptions()
    # options.add_argument("--headless=True")

    async with webdriver.Chrome(options=options) as driver:
        player_urls = await collect_h2h_and_urls(driver)
        await scrape_player_pages(driver, player_urls)


if __name__ == "__main__":
    asyncio.run(main())
