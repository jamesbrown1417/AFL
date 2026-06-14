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
from dotenv import dotenv_values

# Get current timestamp=======================================================
now = datetime.now()
time_stamp = now.strftime("%Y-%m-%d_%H-%M-%S")

PROJECT_ROOT = Path(__file__).resolve().parents[1]
BET365_ENV_KEYS = {"BET365USER", "BET365PW", "BET365_BYPASS_LOGIN"}


def load_bet365_env():
    """Load Bet365 settings deterministically without leaking stale shell values."""
    values = {}
    sources = {}

    # Lowest to highest priority. The project .env intentionally wins over an
    # inherited shell env so edited credentials are used immediately.
    for env_path in (PROJECT_ROOT / "env", PROJECT_ROOT / ".env"):
        if not env_path.exists():
            continue
        for key, value in dotenv_values(env_path).items():
            if key in BET365_ENV_KEYS and value not in (None, ""):
                values[key] = value
                sources[key] = str(env_path)

    for key in BET365_ENV_KEYS:
        if key in values:
            os.environ[key] = values[key]
        elif os.getenv(key) not in (None, ""):
            sources[key] = "process environment"

    return sources


BET365_ENV_SOURCES = load_bet365_env()

# Read credentials after loading.
username = os.getenv("BET365USER")
password = os.getenv("BET365PW")

# Login bypass switch: when enabled, skip the Bet365 login flow entirely and
# scrape public (logged-out) markets. The logged-out shell is currently not
# hydrating AFL/player markets reliably, so production defaults to logging in.
# Override with env BET365_BYPASS_LOGIN=yes/no when needed.
def env_bool(name, default=False):
    raw = os.getenv(name)
    if raw is None:
        return default
    return raw.strip().lower() in {"1", "true", "t", "yes", "y", "on"}


BYPASS_LOGIN = env_bool("BET365_BYPASS_LOGIN", default=False)


def describe_env_source(key):
    source = BET365_ENV_SOURCES.get(key, "not set")
    if source not in {"process environment", "not set"}:
        source = Path(source).name
    return source

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
            has_market = any(
                token in last_html
                for token in (
                    "gl-MarketGroupButton_Text",
                    "cm-MarketGroupWithIconsButton_Text",
                    "srb-HScrollPlaceColumnMarket",
                    "gl-MarketGroupPod",
                )
            )
            has_player_rows = any(
                token in last_html
                for token in (
                    "bbl-BetBuilderParticipantLabel",
                    "srb-ParticipantLabel",
                    "srb-ParticipantLabelWithTeam",
                    "gl-ParticipantCenteredStacked",
                )
            )
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


async def save_player_disposals_html(driver, path, timeout=20):
    """Save Player-tab disposals HTML only after line and milestone markets exist."""
    loop = asyncio.get_event_loop()
    deadline = loop.time() + timeout
    last_html = ""
    last_metrics = {}

    while loop.time() < deadline:
        elem, last_html = await wait_for_player_container_html(driver, timeout=5)
        has_total_lines = "Total Player Disposals" in last_html
        has_milestones = (
            "Player Disposals Milestones" in last_html
            or "srb-HScrollPlaceColumnMarket" in last_html
            or "bbl-FilteredMarketGroupWithHScrollerContainer_Wide" in last_html
        )
        last_metrics = await driver.execute_script(
            """
            const pods = Array.from(document.querySelectorAll('.gl-MarketGroupPod'));
            const totalPod = pods.find((pod) => (pod.innerText || '').includes('Total Player Disposals'));
            if (!totalPod) {
                return { totalNames: 0, totalPrices: 0, totalHasShowMore: false };
            }
            const totalNames = totalPod.querySelectorAll(
                '.srb-ParticipantLabelWithTeam_Name, .srb-ParticipantLabel_Name'
            ).length;
            const totalPrices = totalPod.querySelectorAll('.gl-ParticipantCenteredStacked').length;
            const totalHasShowMore = Array.from(totalPod.querySelectorAll('.msl-ShowMore_Link, .bbl-ShowMoreForHScroll, .bbl-ShowMore'))
                .some((node) => ((node.innerText || node.textContent || '').trim().toLowerCase() === 'show more'));
            return { totalNames, totalPrices, totalHasShowMore };
            """
        )
        total_lines_complete = (
            last_metrics.get("totalNames", 0) > 0
            and last_metrics.get("totalPrices", 0) >= last_metrics.get("totalNames", 0) * 2
            and not last_metrics.get("totalHasShowMore", True)
        )
        if has_total_lines and has_milestones and total_lines_complete:
            Path(path).write_text(last_html)
            print(f"  Saved: {path} ({len(last_html)} bytes)")
            return last_html
        await driver.sleep(1)

    raise RuntimeError(
        "Player disposals HTML did not contain both milestone and Total Player Disposals markets "
        f"with complete line rows (last_html_bytes={len(last_html)}; metrics={last_metrics})"
    )


async def click_market_group(driver, label, timeout=10):
    label_lower = label.lower()
    has_content_before_click = await driver.execute_script(
        """
        const label = arguments[0].toLowerCase();
        const pods = Array.from(document.querySelectorAll('.gl-MarketGroupPod'));
        return pods.some((pod) => {
            const text = (pod.innerText || '').toLowerCase();
            return text.includes(label) && Boolean(
                pod.querySelector('.bbl-FilteredMarketGroupWithHScrollerContainer_Wide') ||
                pod.querySelector('.bbl-BetBuilderMarketGroupContainer') ||
                pod.querySelector('.srb-HScrollPlaceColumnMarket') ||
                pod.querySelector('.gl-ParticipantCenteredStacked')
            );
        });
        """,
        label_lower,
    )
    if has_content_before_click:
        print(f"  '{label}' market content already available")
        return

    button_xpath = (
        "//div[contains(@class, 'gl-MarketGroupPod') "
        "and .//*[("
        "contains(@class, 'gl-MarketGroupButton_Text') or "
        "contains(@class, 'cm-MarketGroupWithIconsButton_Text')"
        f") and contains({XPATH_LOWER_TEXT}, '{label_lower}')]]"
        "//*[contains(@class, 'gl-MarketGroupButton') or "
        "contains(@class, 'cm-MarketGroupWithIconsButton')]"
    )

    try:
        button = await driver.find_element(By.XPATH, button_xpath, timeout=timeout)
    except Exception:
        already_available = await driver.execute_script(
            """
            const label = arguments[0].toLowerCase();
            const pods = Array.from(document.querySelectorAll('.gl-MarketGroupPod'));
            return pods.some((pod) => {
                const text = (pod.innerText || '').toLowerCase();
                return text.includes(label) && Boolean(
                    pod.querySelector('.bbl-FilteredMarketGroupWithHScrollerContainer_Wide') ||
                    pod.querySelector('.bbl-BetBuilderMarketGroupContainer') ||
                    pod.querySelector('.srb-HScrollPlaceColumnMarket') ||
                    pod.querySelector('.gl-ParticipantCenteredStacked')
                );
            });
            """,
            label_lower,
        )
        if already_available:
            print(f"  '{label}' market content already available")
            return
        raise

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
                return text.includes(label);
            });
            if (!pod) return false;
            const hasOpenClass = pod.className.includes('gl-MarketGroup_Open') ||
                Boolean(pod.querySelector('.gl-MarketGroup_Open'));
            const hasMarketContent = Boolean(
                pod.querySelector('.bbl-FilteredMarketGroupWithHScrollerContainer_Wide') ||
                pod.querySelector('.bbl-BetBuilderMarketGroupContainer') ||
                pod.querySelector('.srb-HScrollPlaceColumnMarket') ||
                pod.querySelector('.gl-ParticipantCenteredStacked')
            );
            return hasMarketContent || hasOpenClass;
            """,
            label_lower,
        )
        if is_open:
            return
        await driver.sleep(0.5)

    raise RuntimeError(f"Timed out waiting for '{label}' market group to open")


async def click_market_nav(driver, label, timeout=10):
    label_lower = label.lower()
    nav_xpath = (
        "//div[contains(@class, 'sph-MarketGroupNavBarButton') "
        f"and .//div[contains(@class, 'sph-MarketGroupNavBarButton_Content') and {XPATH_LOWER_TEXT}='{label_lower}']]"
    )

    selected = await driver.execute_script(
        """
        const label = arguments[0].toLowerCase();
        const buttons = Array.from(document.querySelectorAll('.sph-MarketGroupNavBarButton'));
        return buttons.some((button) => {
            const text = (button.innerText || '').trim().toLowerCase();
            return text === label && button.className.includes('sph-MarketGroupNavBarButton_Selected');
        });
        """,
        label_lower,
    )
    if selected:
        print(f"  '{label}' tab already selected")
        return

    nav_button = await driver.find_element(By.XPATH, nav_xpath, timeout=timeout)
    await driver.execute_script("arguments[0].scrollIntoView({block: 'center', inline: 'center'});", nav_button)
    await driver.execute_script("arguments[0].click();", nav_button)
    print(f"  Clicked '{label}' tab")

    loop = asyncio.get_event_loop()
    deadline = loop.time() + timeout
    while loop.time() < deadline:
        selected = await driver.execute_script(
            """
            const label = arguments[0].toLowerCase();
            const buttons = Array.from(document.querySelectorAll('.sph-MarketGroupNavBarButton'));
            return buttons.some((button) => {
                const text = (button.innerText || '').trim().toLowerCase();
                return text === label && button.className.includes('sph-MarketGroupNavBarButton_Selected');
            });
            """,
            label_lower,
        )
        if selected:
            return
        await driver.sleep(0.5)

    raise RuntimeError(f"Timed out waiting for '{label}' tab to become selected")


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

    print(
        "Bet365 config: "
        f"user_source={describe_env_source('BET365USER')}; "
        f"password_source={describe_env_source('BET365PW')}; "
        f"bypass_login={BYPASS_LOGIN}"
    )

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
        total_clicked = 0
        idle_passes = 0

        for _ in range(50):
            result = await driver.execute_script(
                """
                const candidates = Array.from(document.querySelectorAll(
                    '.bbl-ShowMoreForHScroll, .bbl-ShowMore, .msl-ShowMore_Link'
                ));
                const buttons = candidates.filter((node) => {
                    const text = (node.innerText || node.textContent || '').trim().toLowerCase();
                    return text === 'show more';
                });
                if (buttons.length === 0) {
                    return { clicked: false, remaining: 0 };
                }
                const button = buttons[0];
                const clickTarget = button.closest('.msl-ShowMore') || button;
                button.scrollIntoView({ block: 'center', inline: 'center' });
                clickTarget.click();
                return { clicked: true, remaining: buttons.length - 1 };
                """
            )

            if result and result.get("clicked"):
                total_clicked += 1
                idle_passes = 0
                await driver.sleep(2)
                continue

            if idle_passes >= 4:
                break

            idle_passes += 1
            await driver.execute_script("window.scrollBy(0, Math.floor(window.innerHeight * 0.85));")
            await driver.sleep(1.25)

        if total_clicked > 0:
            print(f"  Clicked {total_clicked} show-more control(s)")

        remaining = await driver.execute_script(
            """
            return Array.from(document.querySelectorAll(
                '.bbl-ShowMoreForHScroll, .bbl-ShowMore, .msl-ShowMore_Link'
            )).filter((node) => {
                const text = (node.innerText || node.textContent || '').trim().toLowerCase();
                return text === 'show more';
            }).length;
            """
        )
        if remaining:
            print(f"  {remaining} show-more control(s) still present after expansion pass")

    async def scroll_page_to_load_markets():
        for _ in range(4):
            await driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
            await driver.sleep(0.8)
        await driver.execute_script("window.scrollTo(0, 0);")
        await driver.sleep(0.5)

    async def load_usable_player_page(url):
        last_error = None
        event_base_url = url.split("/I")[0].rstrip("/") + "/"
        candidate_urls = [url, event_base_url, url]

        for attempt, candidate_url in enumerate(candidate_urls, start=1):
            if attempt == 1 and BYPASS_LOGIN:
                await driver.get("https://www.bet365.com.au/")
                await driver.sleep(4)

            await driver.get(candidate_url)
            await driver.sleep(2 + attempt)
            try:
                return await wait_for_player_container_html(driver, timeout=25)
            except Exception as exc:
                last_error = exc
                print(
                    f"  Player page load attempt {attempt} failed: "
                    f"{describe_exception(exc)}; url={candidate_url}"
                )
                try:
                    await driver.refresh()
                except Exception:
                    pass
                await driver.sleep(3)
        raise last_error

    for index, url in enumerate(player_urls, start=1):
        try:
            print(f"\n{'='*60}")
            print(f"Processing match {index}")
            print(f"{'='*60}")
            print(f"URL: {url}")

            await load_usable_player_page(url)

            # The default SGM page opens Goalscorer. Save it first, then open
            # the Player tab so player milestone and line markets are saved.
            await safe_click_show_more_all()
            await save_player_container_html(
                driver,
                f"Data/BET365_HTML/body_html_players_a_match_{index}.txt",
            )

            await click_market_nav(driver, "Player")
            await driver.sleep(2)
            await scroll_page_to_load_markets()
            await click_market_group(driver, "Total Player Disposals")
            await driver.sleep(2)
            await safe_click_show_more_all()
            await scroll_page_to_load_markets()
            await safe_click_show_more_all()
            await save_player_disposals_html(
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
