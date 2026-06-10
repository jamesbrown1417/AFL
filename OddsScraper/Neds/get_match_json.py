import asyncio
import csv
import json
import logging
import pathlib
import sys
from urllib.parse import urlparse

from playwright.async_api import TimeoutError as PlaywrightTimeoutError
from playwright.async_api import async_playwright


CSV_FILE = pathlib.Path("OddsScraper/Neds/neds_afl_match_urls.csv")
OUTPUT_DIR = pathlib.Path("OddsScraper/Neds")
EVENT_CARD_PATH = "/v2/sport/eventcard"
NAVIGATION_TIMEOUT_MS = 60000
RESPONSE_TIMEOUT_MS = 30000

logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s")


def event_id_from_url(url):
    parsed = urlparse(url)
    parts = [part for part in parsed.path.split("/") if part]
    return parts[-1] if parts else ""


def load_matches():
    if not CSV_FILE.exists():
        raise FileNotFoundError(f"Input CSV file not found: {CSV_FILE}")

    with CSV_FILE.open(newline="", encoding="utf-8") as file:
        rows = list(csv.DictReader(file))

    if not rows:
        raise ValueError(f"Input CSV has no rows: {CSV_FILE}")

    matches = []
    for row in rows:
        url = (row.get("url") or "").strip()
        event_id = (row.get("event_id") or event_id_from_url(url)).strip()
        event_name = (row.get("event_name") or event_id).strip()
        if not url or not event_id:
            raise ValueError(f"Input CSV row is missing url or event_id: {row}")
        matches.append({"event_name": event_name, "event_id": event_id, "url": url})

    return matches


def is_event_card_response(response, event_id):
    return EVENT_CARD_PATH in response.url.lower() and event_id in response.url and response.status == 200


def validate_event_card(payload, event_name):
    events = payload.get("events")
    markets = payload.get("markets")
    entrants = payload.get("entrants")
    prices = payload.get("prices")

    if not isinstance(events, dict) or not events:
        raise ValueError(f"{event_name}: event card did not contain an event.")
    if not isinstance(markets, dict) or not markets:
        raise ValueError(f"{event_name}: event card did not contain any markets.")
    if not isinstance(entrants, dict) or not entrants:
        raise ValueError(f"{event_name}: event card did not contain any entrants.")
    if not isinstance(prices, dict) or not prices:
        raise ValueError(f"{event_name}: event card did not contain any prices.")


def clear_previous_outputs():
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    for path in OUTPUT_DIR.glob("data_*.json"):
        path.unlink()


async def capture_event_card(page, match, index):
    logging.info("Processing %s", match["event_name"])
    async with page.expect_response(
        lambda response: is_event_card_response(response, match["event_id"]),
        timeout=RESPONSE_TIMEOUT_MS,
    ) as response_info:
        await page.goto(match["url"], wait_until="domcontentloaded", timeout=NAVIGATION_TIMEOUT_MS)

    response = await response_info.value
    payload = await response.json()
    validate_event_card(payload, match["event_name"])

    output_path = OUTPUT_DIR / f"data_{index}.json"
    output_path.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")
    market_count = len(payload.get("markets", {}))
    logging.info("Saved %s with %s markets", output_path, market_count)


async def main():
    matches = load_matches()
    clear_previous_outputs()
    failures = []

    async with async_playwright() as playwright:
        browser = await playwright.chromium.launch(headless=True)
        try:
            page = await browser.new_page()
            for index, match in enumerate(matches, start=1):
                try:
                    await capture_event_card(page, match, index)
                except PlaywrightTimeoutError as error:
                    message = f"{match['event_name']}: timed out waiting for EventCard response"
                    logging.error("%s: %s", message, error)
                    failures.append(message)
                except Exception as error:
                    message = f"{match['event_name']}: {error}"
                    logging.error(message)
                    failures.append(message)
        finally:
            await browser.close()

    saved_files = sorted(OUTPUT_DIR.glob("data_*.json"))
    if not saved_files:
        raise RuntimeError("No Neds event card JSON files were saved.")
    if failures:
        raise RuntimeError(f"Failed to save {len(failures)} of {len(matches)} Neds event cards: {'; '.join(failures)}")

    logging.info("Saved %s Neds event card JSON files.", len(saved_files))


if __name__ == "__main__":
    try:
        asyncio.run(main())
    except Exception as error:
        logging.error("Neds per-match event card capture failed: %s", error)
        sys.exit(1)
