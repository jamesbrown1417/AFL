"""
Single-run Bet365 AFL scraper using one driverless Chrome instance.

Performs both steps in order:
1) Log in from the Bet365 home shell
2) Load main market page and save H2H HTML
3) Load consolidated player goal/disposal screens and save each fixture's expanded HTML
"""

# Import Modules=============================================================
from selenium_driverless import webdriver
from selenium_driverless.types.by import By
import asyncio
import os
import random
import socket
import subprocess
import sys
from pathlib import Path
from urllib.parse import urlsplit
from dotenv import dotenv_values

PROJECT_ROOT = Path(__file__).resolve().parents[1]
BET365_ENV_KEYS = {
    "BET365USER",
    "BET365PW",
    "BET365_BYPASS_LOGIN",
    "BET365_DISPOSALS_URLS",
    "BET365_GOALS_ANYTIME_URLS",
    "BET365_GOALS_MULTISCORER_URLS",
    "BET365_MAX_ATTEMPTS",
    "BET365_RETRY_BACKOFF_SECONDS",
    "BET365_PROXY",
    "BET365_USER_DATA_DIR",
}

# When logging in, we reuse a persistent Chrome profile across runs so the session
# cookie survives and we skip credential entry. Override the location with the
# BET365_USER_DATA_DIR env var; otherwise this default is used.
DEFAULT_BET365_PROFILE_DIR = PROJECT_ROOT / "OddsScraper" / ".bet365_profile"

# Realistic desktop viewports. We vary the window size per run so consecutive
# sessions don't share an identical fingerprint. (We deliberately do NOT spoof
# the user-agent string here: a UA that disagrees with Chrome's client hints is
# a stronger bot signal than a consistent one. See the note in run_scrape_session.)
BET365_VIEWPORTS = [
    (1280, 720),
    (1366, 768),
    (1440, 900),
    (1536, 864),
    (1600, 900),
    (1680, 1050),
    (1920, 1080),
]
BET365_HOME_URL = "https://www.bet365.com.au/"
BET365_H2H_URL = "https://www.bet365.com.au/#/AC/B36/C21101752/D48/E360013/F48/"
BET365_DISPOSALS_URLS = [
    "https://www.bet365.com.au/#/AC/B36/C21101752/D48/E360575/F48/N0/",
]
BET365_GOALS_ANYTIME_URLS = [
    "https://www.bet365.com.au/#/AC/B36/C21101752/D48/E360041/F48/N0/",
]
BET365_GOALS_MULTISCORER_URLS = [
    "https://www.bet365.com.au/#/AC/B36/C21101752/D48/E360267/F48/N0/",
]
BET365_HTML_DIR = Path("Data/BET365_HTML")
BET365_PLAYER_FIXTURE_PATTERNS = [
    "body_html_players_a_match_*.txt",
    "body_html_players_a_multiscorer_match_*.txt",
    "body_html_players_b_match_*.txt",
]
CURRENT_RUN_PLAYER_ARTIFACTS = 0
CLICK_SETTLE_SECONDS = 1.5
SHOW_MORE_PRECLICK_SECONDS = 1.5
# After a show-more click we poll for the expansion to render rather than always
# waiting the worst case: continue the instant the fixture grows, up to the ceiling.
SHOW_MORE_POSTCLICK_MAX_SECONDS = 4.0
SHOW_MORE_POLL_INTERVAL_SECONDS = 0.4
SHOW_MORE_BATCH_COOLDOWN_SECONDS = 6
SHOW_MORE_BATCH_SIZE = 4
WHEEL_SCROLL_MIN_CHUNK = 90
WHEEL_SCROLL_MAX_CHUNK = 280


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

if os.getenv("BET365_DISPOSALS_URLS"):
    BET365_DISPOSALS_URLS = [
        url.strip()
        for url in os.getenv("BET365_DISPOSALS_URLS", "").split(",")
        if url.strip()
    ]

if os.getenv("BET365_GOALS_ANYTIME_URLS"):
    BET365_GOALS_ANYTIME_URLS = [
        url.strip()
        for url in os.getenv("BET365_GOALS_ANYTIME_URLS", "").split(",")
        if url.strip()
    ]

if os.getenv("BET365_GOALS_MULTISCORER_URLS"):
    BET365_GOALS_MULTISCORER_URLS = [
        url.strip()
        for url in os.getenv("BET365_GOALS_MULTISCORER_URLS", "").split(",")
        if url.strip()
    ]

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


def env_int(name, default):
    raw = os.getenv(name)
    if raw is None or raw.strip() == "":
        return default
    try:
        return int(raw.strip())
    except ValueError:
        return default


def jittered(base, spread=0.35):
    """Randomise a sleep duration by +/- `spread` so timing isn't robotically identical.

    Never returns less than half of `base`, so essential settle times are preserved.
    """
    delta = base * spread
    return max(base * 0.5, base + random.uniform(-delta, delta))


def redact_proxy(proxy):
    """Hide any user:pass@ credentials so the proxy can be safely printed in logs."""
    if "@" in proxy:
        scheme, _, rest = proxy.partition("://")
        if rest:
            return f"{scheme}://***@{rest.split('@', 1)[1]}"
        return f"***@{proxy.split('@', 1)[1]}"
    return proxy


def find_free_port():
    """Grab an ephemeral localhost port for the proxy relay to listen on."""
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


def build_pproxy_remote(proxy_url):
    """Translate http://user:pass@host:port/ into pproxy's http://host:port#user:pass form."""
    parts = urlsplit(proxy_url)
    scheme = parts.scheme or "http"
    remote = f"{scheme}://{parts.hostname}:{parts.port}"
    if parts.username:
        cred = parts.username
        if parts.password:
            cred += f":{parts.password}"
        remote += f"#{cred}"
    return remote


async def start_proxy_relay(proxy_url):
    """Run a localhost no-auth relay that forwards to the authenticated upstream proxy.

    selenium_driverless's built-in authenticated-proxy support relies on an MV3
    helper extension that current Chrome refuses to load (its manifest requests
    permissions removed in MV3), so we can't hand Chrome a user:pass proxy
    directly. Instead we run a local pproxy relay and point Chrome at it with a
    credential-free --proxy-server flag, which needs no extension.

    Returns (process, local_proxy_url).
    """
    port = find_free_port()
    remote = build_pproxy_remote(proxy_url)
    proc = subprocess.Popen(
        [sys.executable, "-m", "pproxy", "-l", f"http://127.0.0.1:{port}", "-r", remote],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.PIPE,
    )

    loop = asyncio.get_event_loop()
    deadline = loop.time() + 10
    while loop.time() < deadline:
        if proc.poll() is not None:
            err = proc.stderr.read().decode(errors="replace") if proc.stderr else ""
            raise RuntimeError(f"Proxy relay exited early: {err.strip()}")
        try:
            with socket.create_connection(("127.0.0.1", port), timeout=0.5):
                return proc, f"http://127.0.0.1:{port}"
        except OSError:
            await asyncio.sleep(0.3)

    proc.terminate()
    raise RuntimeError("Proxy relay did not become ready within 10s")


def stop_proxy_relay(proc):
    """Terminate the local proxy relay subprocess, escalating to kill if needed."""
    if not proc or proc.poll() is not None:
        return
    proc.terminate()
    try:
        proc.wait(timeout=5)
    except subprocess.TimeoutExpired:
        proc.kill()


def kill_stale_chrome():
    """Kill leftover driverless Chrome processes before launching a fresh session.

    A crash mid-run (we raise RuntimeError fairly often) can leave a 'Chrome for
    Testing' instance holding the temp profile lock, which makes the next run's
    SPA shell fail to hydrate in a way that looks identical to bot-blocking.
    """
    for pattern in ("Chrome for Testing", "selenium_driverless"):
        try:
            subprocess.run(["pkill", "-f", pattern], check=False, capture_output=True)
        except Exception as exc:
            print(f"  Could not pkill {pattern!r}: {describe_exception(exc)}")


BYPASS_LOGIN = env_bool("BET365_BYPASS_LOGIN", default=False)


def describe_env_source(key):
    source = BET365_ENV_SOURCES.get(key, "not set")
    if source not in {"process environment", "not set"}:
        source = Path(source).name
    return source


def clear_bet365_outputs(patterns):
    """Remove generated Bet365 HTML for markets that are about to be refreshed."""
    BET365_HTML_DIR.mkdir(parents=True, exist_ok=True)
    for pattern in patterns:
        for path in BET365_HTML_DIR.glob(pattern):
            path.unlink()


def count_bet365_artifacts(patterns):
    BET365_HTML_DIR.mkdir(parents=True, exist_ok=True)
    return sum(1 for pattern in patterns for path in BET365_HTML_DIR.glob(pattern) if path.is_file())


def has_partial_player_artifacts():
    return CURRENT_RUN_PLAYER_ARTIFACTS > 0

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


def clamp(value, low, high):
    return max(low, min(high, value))


async def viewport_size(driver):
    return await driver.execute_script(
        """
        return {
            width: window.innerWidth || document.documentElement.clientWidth || 1280,
            height: window.innerHeight || document.documentElement.clientHeight || 720,
        };
        """
    )


async def human_wheel_scroll(driver, delta_y, *, x=None, y=None):
    """Scroll using CDP mouse-wheel events in uneven bursts."""
    viewport = await viewport_size(driver)
    x = int(x if x is not None else random.uniform(viewport["width"] * 0.35, viewport["width"] * 0.75))
    y = int(y if y is not None else random.uniform(viewport["height"] * 0.38, viewport["height"] * 0.78))

    await driver.execute_cdp_cmd(
        "Input.dispatchMouseEvent",
        {"type": "mouseMoved", "x": x + random.randint(-18, 18), "y": y + random.randint(-12, 12)},
    )

    remaining = float(delta_y)
    while abs(remaining) > 8:
        chunk_abs = min(abs(remaining), random.uniform(WHEEL_SCROLL_MIN_CHUNK, WHEEL_SCROLL_MAX_CHUNK))
        chunk = chunk_abs if remaining > 0 else -chunk_abs
        remaining -= chunk
        await driver.execute_cdp_cmd(
            "Input.dispatchMouseEvent",
            {
                "type": "mouseWheel",
                "x": x + random.randint(-6, 6),
                "y": y + random.randint(-6, 6),
                "deltaX": random.uniform(-4, 4),
                "deltaY": chunk,
            },
        )
        await driver.sleep(random.uniform(0.05, 0.22))

    await driver.sleep(random.uniform(0.15, 0.55))


async def human_scroll_to_bottom(driver, max_passes=30):
    for _ in range(max_passes):
        position = await driver.execute_script(
            """
            return {
                scrollY: window.scrollY || document.documentElement.scrollTop || 0,
                viewportHeight: window.innerHeight || document.documentElement.clientHeight || 720,
                scrollHeight: Math.max(
                    document.body?.scrollHeight || 0,
                    document.documentElement?.scrollHeight || 0
                ),
            };
            """
        )
        if position["scrollY"] + position["viewportHeight"] >= position["scrollHeight"] - 25:
            break
        await human_wheel_scroll(driver, random.uniform(420, 900))
        await driver.sleep(random.uniform(0.25, 0.85))


async def human_scroll_to_top(driver, max_passes=30):
    for _ in range(max_passes):
        scroll_y = await driver.execute_script("return window.scrollY || document.documentElement.scrollTop || 0;")
        if scroll_y <= 20:
            break
        await human_wheel_scroll(driver, -random.uniform(420, 900))
        await driver.sleep(random.uniform(0.25, 0.8))


async def get_element_rect(driver, element_script, *script_args):
    script = f"""
        const element = (() => {{
            {element_script}
        }})();
        if (!element) return {{ found: false }};
        const rect = element.getBoundingClientRect();
        return {{
            found: true,
            top: rect.top,
            bottom: rect.bottom,
            left: rect.left,
            right: rect.right,
            width: rect.width,
            height: rect.height,
            centerX: rect.left + rect.width / 2,
            centerY: rect.top + rect.height / 2,
            viewportWidth: window.innerWidth || document.documentElement.clientWidth || 1280,
            viewportHeight: window.innerHeight || document.documentElement.clientHeight || 720,
        }};
    """
    return await driver.execute_script(script, *script_args)


async def human_scroll_to_element(driver, element_script, *script_args, label="target", max_steps=20):
    """Bring an element near the middle of the viewport with wheel scrolling."""
    info = None
    for _ in range(max_steps):
        info = await get_element_rect(driver, element_script, *script_args)
        if not info or not info.get("found"):
            return info

        viewport_height = info.get("viewportHeight", 720)
        target_y = viewport_height * random.uniform(0.40, 0.55)
        center_y = info.get("centerY", target_y)
        top = info.get("top", 0)
        bottom = info.get("bottom", 0)
        comfortably_visible = (
            top >= random.uniform(45, 85)
            and bottom <= viewport_height - random.uniform(45, 90)
            and abs(center_y - target_y) <= random.uniform(55, 95)
        )
        if comfortably_visible:
            break

        delta_y = clamp((center_y - target_y) * random.uniform(0.70, 1.08), -760, 760)
        if abs(delta_y) < 90:
            delta_y = 90 if delta_y >= 0 else -90

        await human_wheel_scroll(driver, delta_y)

    info = await get_element_rect(driver, element_script, *script_args)
    if not info or not info.get("found"):
        return info

    print(
        f"  Wheel-scrolled to {label}: "
        f"y={info.get('centerY', 0):.0f}/{info.get('viewportHeight', 0):.0f}"
    )
    return info


async def human_cdp_click(driver, rect_info, *, label="target"):
    """Click inside an element using mouse movement and press/release events."""
    if not rect_info or not rect_info.get("found"):
        return False

    width = max(1, rect_info.get("width", 1))
    height = max(1, rect_info.get("height", 1))
    x = int(rect_info.get("left", 0) + width * random.uniform(0.38, 0.62))
    y = int(rect_info.get("top", 0) + height * random.uniform(0.35, 0.65))

    try:
        await driver.execute_cdp_cmd(
            "Input.dispatchMouseEvent",
            {"type": "mouseMoved", "x": x + random.randint(-10, 10), "y": y + random.randint(-8, 8)},
        )
        await driver.sleep(random.uniform(0.18, 0.55))
        await driver.execute_cdp_cmd(
            "Input.dispatchMouseEvent",
            {"type": "mouseMoved", "x": x, "y": y},
        )
        await driver.sleep(random.uniform(0.08, 0.22))
        await driver.execute_cdp_cmd(
            "Input.dispatchMouseEvent",
            {"type": "mousePressed", "x": x, "y": y, "button": "left", "clickCount": 1},
        )
        await driver.sleep(random.uniform(0.08, 0.24))
        await driver.execute_cdp_cmd(
            "Input.dispatchMouseEvent",
            {"type": "mouseReleased", "x": x, "y": y, "button": "left", "clickCount": 1},
        )
        print(f"  Mouse-clicked {label}")
        return True
    except Exception as exc:
        print(f"  CDP click failed for {label}: {describe_exception(exc)}")
        return False


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


async def login_to_bet365(driver):
    """Open Bet365 and establish the logged-in shell before AFL deep links."""
    print(
        "Bet365 config: "
        f"user_source={describe_env_source('BET365USER')}; "
        f"password_source={describe_env_source('BET365PW')}; "
        f"bypass_login={BYPASS_LOGIN}"
    )

    BET365_HTML_DIR.mkdir(parents=True, exist_ok=True)

    await driver.get(BET365_HOME_URL)
    await driver.sleep(jittered(5))

    if BYPASS_LOGIN:
        print("BYPASS_LOGIN enabled - skipping login, scraping logged-out markets")
        return

    # If the persistent profile already holds a valid session, skip the credential
    # flow entirely. This is the fast path on every run after the first login; we
    # only fall through to entering credentials when the session has lapsed.
    logged_in_locator_candidates = [
        (By.XPATH, "//div[contains(@class, 'hm-MainHeaderRHSLoggedInWide')]"),
        (By.XPATH, "//div[contains(@class, 'hm-MainHeaderMembersWide')]"),
        (By.XPATH, "//*[contains(@class, 'hm-Balance')]"),
    ]
    try:
        await find_first_element(driver, logged_in_locator_candidates, timeout_per_candidate=3)
        print("Existing Bet365 session detected - skipping login")
        return
    except Exception:
        pass

    print("Attempting login before loading AFL market pages...")
    login_locator_candidates = [
        (By.XPATH, "//div[contains(@class, 'hm-MainHeaderRHSLoggedOutWide_Login')]"),
        (
            By.XPATH,
            f"//span[contains(@class, 'hrm-') and (contains({XPATH_LOWER_TEXT}, 'log in') or contains({XPATH_LOWER_TEXT}, 'login'))]",
        ),
        (
            By.XPATH,
            f"//*[self::button or self::a][contains({XPATH_LOWER_TEXT}, 'log in') or contains({XPATH_LOWER_TEXT}, 'login')]",
        ),
    ]

    try:
        login_element = await find_first_element(
            driver, login_locator_candidates, timeout_per_candidate=4
        )
    except Exception:
        print("No login control found; assuming an existing Bet365 session is active")
        return

    await driver.sleep(1)
    try:
        await login_element.click()
    except Exception:
        await driver.execute_script("arguments[0].click();", login_element)
    await driver.sleep(1)

    username_field = await driver.find_element(
        By.XPATH,
        "//input[@placeholder='Username or email address']",
        timeout=10,
    )
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

    loop = asyncio.get_event_loop()
    deadline = loop.time() + 20
    while loop.time() < deadline:
        form_visible = await driver.execute_script(
            "return Boolean(document.querySelector(\"input[placeholder='Password']\"));"
        )
        if not form_visible:
            await driver.sleep(2)
            print("Login form dismissed")
            return
        await driver.sleep(0.5)

    print("Login form still visible after wait; continuing so downstream checks can surface the failure")


async def save_h2h_html(driver, required=True):
    """Save the AFL main market HTML after login."""
    try:
        await driver.get(BET365_H2H_URL)
        # Short initial settle; wait_for_main_market_container polls until ready.
        await driver.sleep(jittered(4.5))
        elem = await wait_for_main_market_container(driver)
        body_html = await elem.get_attribute("outerHTML")
        (BET365_HTML_DIR / "h2h_html.txt").write_text(body_html)
        print(f"Saved H2H HTML ({len(body_html)} bytes)")
    except Exception as exc:
        existing_h2h = BET365_HTML_DIR / "h2h_html.txt"
        if required or (not existing_h2h.exists() and not has_partial_player_artifacts()):
            raise
        if existing_h2h.exists():
            print(
                "Warning: H2H HTML refresh failed after player markets, "
                f"keeping existing {existing_h2h}: {describe_exception(exc)}"
            )
        else:
            print(
                "Warning: H2H HTML refresh failed after player markets and no H2H HTML exists; "
                "continuing with saved player-market artifacts only: "
                f"{describe_exception(exc)}"
            )
        await write_partial_scrape_summary(
            status="partial",
            failed_step="h2h market",
            error=describe_exception(exc),
            current_run_player_artifacts=CURRENT_RUN_PLAYER_ARTIFACTS,
            disk_player_artifacts=count_bet365_artifacts(BET365_PLAYER_FIXTURE_PATTERNS),
            h2h_exists=existing_h2h.exists(),
        )


async def get_disposals_fixture_metrics(driver):
    return await driver.execute_script(
        """
        const fixtureSelector = '.gl-MarketGroupPod.src-HScrollFixtureSubGroupWithShowMore, .gl-MarketGroupPod.src-FixtureSubGroupWithShowMore';
        const pods = Array.from(document.querySelectorAll(fixtureSelector));
        return pods.map((pod, index) => {
            const match = (pod.querySelector('.src-FixtureSubGroupButton_Text')?.innerText || '').trim();
            const startTime = (pod.querySelector('.src-FixtureSubGroupButton_BookCloses')?.innerText || '').trim();
            const headers = Array.from(pod.querySelectorAll('.srb-HScrollPlaceHeader, .gl-MarketColumnHeader'))
                .map((node) => (node.innerText || '').trim())
                .filter(Boolean);
            const showMore = Array.from(pod.querySelectorAll('.msl-ShowMore_Link, .bbl-ShowMoreForHScroll, .bbl-ShowMore'))
                .filter((node) => (node.innerText || node.textContent || '').trim().toLowerCase() === 'show more');
            return {
                index,
                match,
                startTime,
                closed: pod.className.includes('FixtureSubGroupWithShowMore_Closed'),
                playerCount: pod.querySelectorAll('.srb-ParticipantLabelWithTeam_Name, .srb-ParticipantLabel_Name').length,
                oddsCount: pod.querySelectorAll('.gl-ParticipantOddsOnly_Odds').length,
                columnCount: headers.length,
                headers,
                showMoreCount: showMore.length,
            };
        });
        """
    )


async def wait_for_disposals_market_page(driver, timeout=60):
    loop = asyncio.get_event_loop()
    deadline = loop.time() + timeout
    last_metrics = []

    while loop.time() < deadline:
        last_metrics = await get_disposals_fixture_metrics(driver)
        if any(item.get("match") for item in last_metrics):
            return last_metrics
        await driver.sleep(1)

    await dump_debug_html(driver, str(BET365_HTML_DIR / "fixture_market_load_error.txt"))
    last_metrics = await get_disposals_fixture_metrics(driver)
    if any(item.get("match") for item in last_metrics):
        return last_metrics
    raise RuntimeError(f"Could not find disposal fixture groups: {last_metrics}")


async def open_collapsed_disposal_fixtures(driver):
    total_clicked = 0
    collapsed_button_script = """
        const fixtureSelector = '.gl-MarketGroupPod.src-HScrollFixtureSubGroupWithShowMore, .gl-MarketGroupPod.src-FixtureSubGroupWithShowMore';
        const pods = Array.from(document.querySelectorAll(fixtureSelector));
        const pod = pods.find((node) => node.className.includes('FixtureSubGroupWithShowMore_Closed'));
        if (!pod) return null;
        return pod.querySelector('.src-FixtureSubGroupButton');
    """

    for _ in range(30):
        target = await driver.execute_script(
            """
            const fixtureSelector = '.gl-MarketGroupPod.src-HScrollFixtureSubGroupWithShowMore, .gl-MarketGroupPod.src-FixtureSubGroupWithShowMore';
            const pods = Array.from(document.querySelectorAll(fixtureSelector));
            const pod = pods.find((node) => node.className.includes('FixtureSubGroupWithShowMore_Closed'));
            if (!pod) return { found: false };
            const match = (pod.querySelector('.src-FixtureSubGroupButton_Text')?.innerText || '').trim();
            const button = pod.querySelector('.src-FixtureSubGroupButton');
            return { found: Boolean(button), match };
            """
        )

        if not target or not target.get("found"):
            break

        match = target.get("match") or "collapsed fixture group"
        rect = await human_scroll_to_element(
            driver,
            collapsed_button_script,
            label=f"collapsed fixture {match}",
            max_steps=18,
        )
        clicked = await human_cdp_click(driver, rect, label=f"collapsed fixture {match}")
        if not clicked:
            fallback = await driver.execute_script(
                """
                const fixtureSelector = '.gl-MarketGroupPod.src-HScrollFixtureSubGroupWithShowMore, .gl-MarketGroupPod.src-FixtureSubGroupWithShowMore';
                const pods = Array.from(document.querySelectorAll(fixtureSelector));
                const pod = pods.find((node) => node.className.includes('FixtureSubGroupWithShowMore_Closed'));
                const button = pod?.querySelector('.src-FixtureSubGroupButton');
                if (!button) return false;
                button.click();
                return true;
                """
            )
            clicked = bool(fallback)

        if not clicked:
            break

        total_clicked += 1
        print(f"  Opened fixture group: {match}")
        await driver.sleep(jittered(CLICK_SETTLE_SECONDS))

    metrics = await get_disposals_fixture_metrics(driver)
    closed = [item.get("match") or f"fixture {item.get('index')}" for item in metrics if item.get("closed")]
    if closed:
        raise RuntimeError(f"Could not open collapsed disposal fixture group(s): {closed}")

    if total_clicked:
        print(f"  Opened {total_clicked} collapsed fixture group(s)")


async def click_all_disposal_show_mores(driver):
    total_clicked = 0
    stalled = []
    skipped_target_keys = set()

    async def fixture_show_more_status():
        return await driver.execute_script(
            """
            const fixtureSelector = '.gl-MarketGroupPod.src-HScrollFixtureSubGroupWithShowMore, .gl-MarketGroupPod.src-FixtureSubGroupWithShowMore';
            const pods = Array.from(document.querySelectorAll(fixtureSelector));
            return pods.map((pod, index) => {
                const match = (pod.querySelector('.src-FixtureSubGroupButton_Text')?.innerText || '').trim();
                const buttons = Array.from(pod.querySelectorAll('.msl-ShowMore_Link, .bbl-ShowMoreForHScroll, .bbl-ShowMore'))
                    .filter((node) => {
                        const text = (node.innerText || node.textContent || '').trim().toLowerCase();
                        const rect = node.getBoundingClientRect();
                        return text === 'show more' && rect.width > 0 && rect.height > 0;
                    });
                return {
                    index,
                    match,
                    playerCount: pod.querySelectorAll('.srb-ParticipantLabelWithTeam_Name, .srb-ParticipantLabel_Name').length,
                    oddsCount: pod.querySelectorAll('.gl-ParticipantOddsOnly_Odds').length,
                    htmlBytes: pod.outerHTML.length,
                    showMoreCount: buttons.length,
                };
            });
            """
        )

    async def next_show_more_target():
        return await driver.execute_script(
            """
            const skippedTargetKeys = new Set(arguments[0] || []);
            const fixtureSelector = '.gl-MarketGroupPod.src-HScrollFixtureSubGroupWithShowMore, .gl-MarketGroupPod.src-FixtureSubGroupWithShowMore';
            const pods = Array.from(document.querySelectorAll(fixtureSelector));
            const targets = [];

            pods.forEach((pod, fixtureIndex) => {
                const match = (pod.querySelector('.src-FixtureSubGroupButton_Text')?.innerText || '').trim();
                const buttons = Array.from(pod.querySelectorAll('.msl-ShowMore_Link, .bbl-ShowMoreForHScroll, .bbl-ShowMore'))
                    .filter((node) => {
                        const text = (node.innerText || node.textContent || '').trim().toLowerCase();
                        const rect = node.getBoundingClientRect();
                        return text === 'show more' && rect.width > 0 && rect.height > 0;
                    });

                buttons.forEach((button, buttonIndex) => {
                    const rect = button.getBoundingClientRect();
                    const documentTop = rect.top + (window.scrollY || document.documentElement.scrollTop || 0);
                    const documentLeft = rect.left + (window.scrollX || document.documentElement.scrollLeft || 0);
                    const targetKey = `${fixtureIndex}:${buttonIndex}:${Math.round(documentTop)}:${Math.round(documentLeft)}`;
                    if (skippedTargetKeys.has(targetKey)) {
                        return;
                    }
                    targets.push({
                        fixtureIndex,
                        buttonIndex,
                        targetKey,
                        match,
                        playerCount: pod.querySelectorAll('.srb-ParticipantLabelWithTeam_Name, .srb-ParticipantLabel_Name').length,
                        oddsCount: pod.querySelectorAll('.gl-ParticipantOddsOnly_Odds').length,
                        htmlBytes: pod.outerHTML.length,
                        showMoreCount: buttons.length,
                        documentTop,
                        documentLeft,
                        screenTop: rect.top,
                        screenLeft: rect.left,
                    });
                });
            });

            targets.sort((left, right) => {
                if (left.documentTop !== right.documentTop) {
                    return left.documentTop - right.documentTop;
                }
                if (left.documentLeft !== right.documentLeft) {
                    return left.documentLeft - right.documentLeft;
                }
                if (left.fixtureIndex !== right.fixtureIndex) {
                    return left.fixtureIndex - right.fixtureIndex;
                }
                return left.buttonIndex - right.buttonIndex;
            });

            return targets[0] || null;
            """,
            list(skipped_target_keys),
        )

    async def click_show_more_target(target):
        fixture_index = target.get("fixtureIndex")
        button_index = target.get("buttonIndex")
        show_more_button_script = """
            const fixtureIndex = arguments[0];
            const buttonIndex = arguments[1];
            const fixtureSelector = '.gl-MarketGroupPod.src-HScrollFixtureSubGroupWithShowMore, .gl-MarketGroupPod.src-FixtureSubGroupWithShowMore';
            const pod = Array.from(document.querySelectorAll(fixtureSelector))[fixtureIndex];
            if (!pod) return null;
            return Array.from(pod.querySelectorAll('.msl-ShowMore_Link, .bbl-ShowMoreForHScroll, .bbl-ShowMore'))
                .filter((node) => {
                    const text = (node.innerText || node.textContent || '').trim().toLowerCase();
                    const rect = node.getBoundingClientRect();
                    return text === 'show more' && rect.width > 0 && rect.height > 0;
                })[buttonIndex] || null;
        """

        rect = await human_scroll_to_element(
            driver,
            show_more_button_script,
            fixture_index,
            button_index,
            label=f"show more {target.get('match') or fixture_index}",
            max_steps=20,
        )
        if not rect or not rect.get("found"):
            return {**target, "clicked": False, "reason": "show more disappeared before click"}

        await driver.sleep(jittered(SHOW_MORE_PRECLICK_SECONDS, spread=0.55))
        if await human_cdp_click(driver, rect, label=f"show more {target.get('match') or fixture_index}"):
            return {**target, "clicked": True, "strategy": "cdp_mouse"}

        fallback = await driver.execute_script(
            """
            const fixtureIndex = arguments[0];
            const buttonIndex = arguments[1];
            const fixtureSelector = '.gl-MarketGroupPod.src-HScrollFixtureSubGroupWithShowMore, .gl-MarketGroupPod.src-FixtureSubGroupWithShowMore';
            const pod = Array.from(document.querySelectorAll(fixtureSelector))[fixtureIndex];
            if (!pod) return { clicked: false, reason: 'missing fixture' };

            const button = Array.from(pod.querySelectorAll('.msl-ShowMore_Link, .bbl-ShowMoreForHScroll, .bbl-ShowMore'))
                .filter((node) => {
                    const text = (node.innerText || node.textContent || '').trim().toLowerCase();
                    const rect = node.getBoundingClientRect();
                    return text === 'show more' && rect.width > 0 && rect.height > 0;
                })[buttonIndex];
            if (!button) return { clicked: false, reason: 'no show more' };

            const parent = button.closest('.msl-ShowMore') || button.parentElement;
            const events = ['mouseover', 'mousemove', 'mousedown', 'mouseup', 'click'];
            for (const type of events) {
                button.dispatchEvent(new MouseEvent(type, {
                    bubbles: true,
                    cancelable: true,
                    view: window,
                }));
            }
            if (parent && parent !== button) {
                parent.click();
            } else {
                button.click();
            }
            return { clicked: true, strategy: 'js_fallback' };
            """,
            fixture_index,
            button_index,
        )
        return {**target, **(fallback or {})}

    for _ in range(60):
        target = await next_show_more_target()
        if not target:
            break

        match = target.get("match") or f"fixture {target.get('fixtureIndex')}"
        target_key = target.get("targetKey")
        before = target

        result = await click_show_more_target(target)
        if not result or not result.get("clicked"):
            stalled.append(match)
            if target_key:
                skipped_target_keys.add(target_key)
            await human_wheel_scroll(driver, random.uniform(180, 380))
            await driver.sleep(jittered(2.5, spread=0.5))
            continue

        total_clicked += 1
        strategy = result.get("strategy", "unknown")
        print(
            f"  Clicked show more: {match} "
            f"(screen y={before.get('screenTop', 0):.0f}, x={before.get('screenLeft', 0):.0f}, {strategy})"
        )

        # Poll for the expansion to render instead of always paying the
        # worst-case settle time: continue as soon as the fixture grows or
        # the control disappears, up to SHOW_MORE_POSTCLICK_MAX_SECONDS.
        progressed = False
        deadline = asyncio.get_event_loop().time() + SHOW_MORE_POSTCLICK_MAX_SECONDS
        while True:
            await driver.sleep(SHOW_MORE_POLL_INTERVAL_SECONDS)
            after_statuses = await fixture_show_more_status()
            after = next(
                (
                    status
                    for status in after_statuses
                    if status.get("index") == before.get("fixtureIndex")
                ),
                None,
            )
            if after is None:
                break

            grew = (
                after.get("playerCount", 0) > before.get("playerCount", 0)
                or after.get("oddsCount", 0) > before.get("oddsCount", 0)
                or after.get("htmlBytes", 0) > before.get("htmlBytes", 0) + 500
            )
            disappeared = after.get("showMoreCount", 0) < before.get("showMoreCount", 0)

            if grew or disappeared:
                progressed = True
                break

            if asyncio.get_event_loop().time() >= deadline:
                break

        if not progressed:
            stalled.append(match)
            if target_key:
                skipped_target_keys.add(target_key)
            await human_wheel_scroll(driver, random.uniform(180, 380))
            await driver.sleep(jittered(2.5, spread=0.5))
        elif total_clicked % SHOW_MORE_BATCH_SIZE == 0:
            cooldown = jittered(SHOW_MORE_BATCH_COOLDOWN_SECONDS, spread=0.5)
            print(f"  Cooling down {cooldown:.1f}s after {total_clicked} show-more click(s)")
            await driver.sleep(cooldown)

    if total_clicked:
        print(f"  Clicked {total_clicked} show-more control(s)")

    await human_scroll_to_top(driver)
    await driver.sleep(random.uniform(0.6, 1.3))

    remaining = sum(item.get("showMoreCount", 0) for item in await get_disposals_fixture_metrics(driver))
    if remaining:
        # The expander already retried each control across multiple passes; if a
        # few won't hydrate we move forward and save whatever did expand rather
        # than failing the whole run for one stubborn fixture.
        stalled_detail = f"; stalled={sorted(set(stalled))}" if stalled else ""
        print(
            f"  WARNING: {remaining} show-more control(s) still present after expansion"
            f"{stalled_detail}; continuing with partially expanded fixture(s)"
        )


async def hydrate_disposals_screen(driver):
    await human_scroll_to_bottom(driver)
    await human_scroll_to_top(driver)
    await driver.sleep(random.uniform(0.7, 1.4))

    await open_collapsed_disposal_fixtures(driver)
    await hydrate_disposals_screen_scroll_only(driver)
    await click_all_disposal_show_mores(driver)
    await hydrate_disposals_screen_scroll_only(driver)

    metrics = await get_disposals_fixture_metrics(driver)
    incomplete = [
        item
        for item in metrics
        if item.get("match")
        and (
            item.get("closed")
            or item.get("playerCount", 0) == 0
            or item.get("columnCount", 0) == 0
        )
    ]
    if incomplete:
        raise RuntimeError(f"Disposal fixture group(s) did not hydrate: {incomplete}")

    return metrics


async def hydrate_disposals_screen_scroll_only(driver):
    await human_scroll_to_bottom(driver)
    await human_scroll_to_top(driver)
    await driver.sleep(random.uniform(0.6, 1.2))


async def save_fixture_group_html(
    driver,
    screen_index,
    next_match_index,
    seen_matches,
    market_code,
    all_screen_prefix,
    match_file_prefix,
):
    global CURRENT_RUN_PLAYER_ARTIFACTS

    all_html = await driver.execute_script(
        """
        const grid = document.querySelector('.cm-CouponMarketGrid') ||
            document.querySelector('.wcl-PageContainer_Colcontainer') ||
            document.body;
        return grid.outerHTML;
        """
    )
    all_path = BET365_HTML_DIR / f"{all_screen_prefix}_all_screen_{screen_index}.txt"
    all_path.write_text(all_html)
    print(f"Saved expanded {market_code} screen: {all_path} ({len(all_html)} bytes)")

    fixtures = await driver.execute_script(
        """
        const fixtureSelector = '.gl-MarketGroupPod.src-HScrollFixtureSubGroupWithShowMore, .gl-MarketGroupPod.src-FixtureSubGroupWithShowMore';
        return Array.from(document.querySelectorAll(fixtureSelector))
            .map((pod) => ({
                match: (pod.querySelector('.src-FixtureSubGroupButton_Text')?.innerText || '').trim(),
                startTime: (pod.querySelector('.src-FixtureSubGroupButton_BookCloses')?.innerText || '').trim(),
                playerCount: pod.querySelectorAll('.srb-ParticipantLabelWithTeam_Name, .srb-ParticipantLabel_Name').length,
                columnCount: pod.querySelectorAll('.srb-HScrollPlaceHeader, .gl-MarketColumnHeader').length,
                html: pod.outerHTML,
            }))
            .filter((item) => item.match && item.playerCount > 0 && item.columnCount > 0);
        """
    )

    saved = 0
    for fixture in fixtures:
        match = fixture.get("match", "")
        if match in seen_matches:
            print(f"  Skipping duplicate {market_code} fixture from screen {screen_index}: {match}")
            continue

        seen_matches.add(match)
        path = BET365_HTML_DIR / f"{match_file_prefix}_match_{next_match_index}.txt"
        payload = (
            f"<!-- source: consolidated Bet365 {market_code} market screen {screen_index} -->\n"
            f"<!-- market_code: {market_code} -->\n"
            f"<!-- match: {match} -->\n"
            f"<!-- start_time: {fixture.get('startTime', '')} -->\n"
            f"{fixture.get('html', '')}"
        )
        path.write_text(payload)
        CURRENT_RUN_PLAYER_ARTIFACTS += 1
        print(
            f"  Saved fixture {next_match_index}: {match} "
            f"({fixture.get('playerCount')} players, {fixture.get('columnCount')} columns)"
        )
        next_match_index += 1
        saved += 1

    if saved == 0:
        raise RuntimeError(f"No hydrated fixture HTML was saved from {market_code} screen {screen_index}")

    return next_match_index, saved


async def scrape_consolidated_fixture_screens(
    driver,
    urls,
    market_code,
    all_screen_prefix,
    match_file_prefix,
    clear_patterns,
):
    """Use consolidated fixture screens rather than opening each match URL."""
    seen_matches = set()
    next_match_index = 1
    total_saved = 0
    cleared_previous = False

    for screen_index, url in enumerate(urls, start=1):
        print(f"\n{'='*60}")
        print(f"Processing consolidated {market_code} screen {screen_index}")
        print(f"{'='*60}")
        print(f"URL: {url}")

        await driver.get(url)
        # Short initial settle; wait_for_disposals_market_page polls until ready.
        await driver.sleep(jittered(4.5))
        await wait_for_disposals_market_page(driver)
        if not cleared_previous:
            clear_bet365_outputs(clear_patterns)
            cleared_previous = True
        metrics = await hydrate_disposals_screen(driver)
        for item in metrics:
            if item.get("match"):
                print(
                    "  Fixture: "
                    f"{item.get('match')} | players={item.get('playerCount')} "
                    f"columns={item.get('columnCount')} show_more={item.get('showMoreCount')}"
                )

        next_match_index, saved = await save_fixture_group_html(
            driver,
            screen_index,
            next_match_index,
            seen_matches,
            market_code,
            all_screen_prefix,
            match_file_prefix,
        )
        total_saved += saved

    print(f"Saved Bet365 {market_code} HTML for {total_saved} fixture(s)")


async def scrape_disposal_market_screens(driver):
    await scrape_consolidated_fixture_screens(
        driver,
        BET365_DISPOSALS_URLS,
        market_code="disposals",
        all_screen_prefix="disposals",
        match_file_prefix="body_html_players_b",
        clear_patterns=[
            "body_html_players_b_match_*.txt",
            "disposals_all_screen_*.txt",
        ],
    )


async def scrape_goal_market_screens(driver):
    await scrape_consolidated_fixture_screens(
        driver,
        BET365_GOALS_ANYTIME_URLS,
        market_code="goals_anytime",
        all_screen_prefix="goals_anytime",
        match_file_prefix="body_html_players_a",
        clear_patterns=[
            "body_html_players_a_match_*.txt",
            "body_html_players_a_multiscorer_match_*.txt",
            "goals_anytime_all_screen_*.txt",
            "goals_multiscorer_all_screen_*.txt",
        ],
    )
    await scrape_consolidated_fixture_screens(
        driver,
        BET365_GOALS_MULTISCORER_URLS,
        market_code="goals_multiscorer",
        all_screen_prefix="goals_multiscorer",
        match_file_prefix="body_html_players_a_multiscorer",
        clear_patterns=[],
    )


def save_market_url_trace():
    partial_status = BET365_HTML_DIR / "partial_scrape_status.txt"
    if partial_status.exists():
        partial_status.unlink()
    urls = [
        "# goals_anytime",
        *BET365_GOALS_ANYTIME_URLS,
        "# goals_multiscorer",
        *BET365_GOALS_MULTISCORER_URLS,
        "# disposals",
        *BET365_DISPOSALS_URLS,
    ]
    (BET365_HTML_DIR / "urls.csv").write_text("\n".join(urls))


async def run_market_step_or_finish_partial(step_name, step_coro):
    """Run one scrape step, treating post-artifact throttling as a clean partial run.

    Bet365 often stops hydrating later screens once the IP is soft-throttled. If
    we already have fixture-level player HTML on disk, that is useful input for
    the downstream R parser, so do not fail the whole update just because a later
    page shell degraded.
    """
    before_count = CURRENT_RUN_PLAYER_ARTIFACTS
    try:
        await step_coro()
        return False
    except Exception as exc:
        after_count = CURRENT_RUN_PLAYER_ARTIFACTS
        if after_count > 0:
            print(
                f"WARNING: Bet365 {step_name} stopped before completion after "
                f"{after_count} player fixture artifact(s) were saved in this run. "
                "Assuming throttled/degraded hydration and exiting cleanly with partial data: "
                f"{describe_exception(exc)}"
            )
            await write_partial_scrape_summary(
                status="partial",
                failed_step=step_name,
                error=describe_exception(exc),
                current_run_player_artifacts=after_count,
                current_run_player_artifacts_before_step=before_count,
                disk_player_artifacts=count_bet365_artifacts(BET365_PLAYER_FIXTURE_PATTERNS),
            )
            return True
        raise


async def write_partial_scrape_summary(**fields):
    lines = [f"{key}: {value}" for key, value in fields.items()]
    (BET365_HTML_DIR / "partial_scrape_status.txt").write_text("\n".join(lines) + "\n")


async def run_scrape_session():
    """Open one fresh browser, log in, and scrape every market. Raises on failure."""
    global CURRENT_RUN_PLAYER_ARTIFACTS
    CURRENT_RUN_PLAYER_ARTIFACTS = 0

    options = webdriver.ChromeOptions()
    # options.add_argument("--headless=True")

    # Kill Chrome's background services (Optimization Guide ML-model downloads,
    # component updater, Safe Browsing pings, sync, telemetry). They are useless for
    # scraping and, under --proxy-server, would otherwise burn residential proxy GB
    # (~86 MB/run was going to optimizationguide-pa.googleapis.com). We deliberately
    # avoid --disable-features here: Chrome honors only the last --disable-features
    # switch, so adding our own could clobber selenium_driverless's stealth flags.
    for flag in (
        "--disable-background-networking",
        "--disable-component-update",
        "--disable-sync",
        "--disable-domain-reliability",
        "--no-pings",
    ):
        options.add_argument(flag)

    # Vary the window size per run so consecutive sessions don't present an
    # identical fingerprint. We do NOT spoof the user-agent: setting --user-agent
    # only changes the UA string, not Chrome's client hints (sec-ch-ua), and a
    # mismatch between the two is a stronger bot signal than leaving it alone.
    width, height = random.choice(BET365_VIEWPORTS)
    options.add_argument(f"--window-size={width},{height}")
    print(f"Browser viewport for this run: {width}x{height}")

    # Profile strategy keyed on login mode:
    #   - Logging in (not BYPASS_LOGIN): reuse one persistent Chrome profile so the
    #     session cookie survives between runs and we skip credential entry. Only one
    #     Chrome can open the profile at a time; kill_stale_chrome() clears a stale
    #     lock left by a crashed run.
    #   - Logged-out (BYPASS_LOGIN): leave the default throwaway temp profile, which
    #     driverless auto-cleans, so every run looks like a fresh visitor (paired
    #     with the proxy below).
    if not BYPASS_LOGIN:
        profile_dir = os.getenv("BET365_USER_DATA_DIR", "").strip() or str(DEFAULT_BET365_PROFILE_DIR)
        Path(profile_dir).mkdir(parents=True, exist_ok=True)
        options.user_data_dir = profile_dir
        options.auto_clean_dirs = False  # critical: keep the profile across runs
        print(f"Logged-in mode - persistent Chrome profile: {profile_dir}")
    else:
        print("Logged-out mode - fresh throwaway Chrome profile this run")

    # The hydration failures are an IP-based rate block: a real browser on the
    # same IP is blocked at the same time, but the same page works from a
    # different IP (phone hotspot). Routing through a (residential) proxy resets
    # the rate counter. Set BET365_PROXY to e.g. http://user:pass@host:port/
    # to enable; leave it unset to run direct.
    #
    # We can't hand Chrome the authenticated proxy directly (driverless's auth
    # mechanism needs an MV3 extension that current Chrome won't load), so we run
    # a local relay and give Chrome a credential-free --proxy-server flag.
    #
    # Only proxy when scraping logged-out (BYPASS_LOGIN). When we actually log in,
    # we go direct on the real IP: logging into the account through a rotating
    # residential proxy in a different location is a strong account-security flag.
    proxy = os.getenv("BET365_PROXY", "").strip()
    relay_proc = None
    if proxy and not BYPASS_LOGIN:
        print("Login enabled - scraping direct (not via proxy) to avoid flagging the account login")
        proxy = ""
    if proxy:
        relay_proc, local_proxy = await start_proxy_relay(proxy)
        options.add_argument(f"--proxy-server={local_proxy}")
        # Force any remaining Google/Chrome background traffic direct (real IP) so it
        # never costs proxy GB, even if a request slips past the disable flags above.
        options.add_argument(
            "--proxy-bypass-list=*.googleapis.com;*.google.com;*.gstatic.com;*.gvt1.com;*.gvt2.com"
        )
        print(f"Routing through proxy: {redact_proxy(proxy)} (via local relay {local_proxy})")

    try:
        async with webdriver.Chrome(options=options) as driver:
            await login_to_bet365(driver)
            save_market_url_trace()
            partial = await run_market_step_or_finish_partial("goal markets", lambda: scrape_goal_market_screens(driver))
            if partial:
                return
            partial = await run_market_step_or_finish_partial("disposal markets", lambda: scrape_disposal_market_screens(driver))
            if partial:
                return
            await save_h2h_html(driver, required=False)
    finally:
        stop_proxy_relay(relay_proc)


async def main():
    # A hydration failure (RuntimeError) usually means Bet365 has soft-throttled
    # us and is serving a shell that never populates the markets. Rather than
    # saving junk, back off and retry once on a brand-new browser session.
    max_attempts = max(1, env_int("BET365_MAX_ATTEMPTS", 2))
    backoff_seconds = env_int("BET365_RETRY_BACKOFF_SECONDS", 300)

    for attempt in range(1, max_attempts + 1):
        kill_stale_chrome()
        try:
            await run_scrape_session()
            return
        except RuntimeError as exc:
            print(f"Scrape attempt {attempt}/{max_attempts} failed: {describe_exception(exc)}")
            if attempt >= max_attempts:
                raise
            print(
                "Likely throttled / degraded shell; backing off "
                f"{backoff_seconds}s before a fresh browser session "
                "(override with BET365_RETRY_BACKOFF_SECONDS)..."
            )
            await asyncio.sleep(backoff_seconds)


if __name__ == "__main__":
    asyncio.run(main())
