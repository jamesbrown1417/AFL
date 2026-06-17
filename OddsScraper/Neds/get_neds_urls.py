import asyncio
import json
import logging
import pathlib
import sys

from playwright.async_api import TimeoutError as PlaywrightTimeoutError
from playwright.async_api import async_playwright


NAVIGATE_URL = "https://www.neds.com.au/sports/australian-rules/afl"
CATEGORY_ID = "23d497e6-8aab-4309-905b-9421f42c9bc5"
OUTPUT_FILE = pathlib.Path("OddsScraper/Neds/neds_response.json")
PAGE_LOAD_TIMEOUT_MS = 60000
RESPONSE_TIMEOUT_MS = 90000

logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s")


def is_event_request_response(response):
    return (
        "/v2/sport/eventrequest" in response.url.lower()
        and CATEGORY_ID in response.url
        and response.status == 200
    )


def validate_event_response(payload):
    events = payload.get("events")
    if not isinstance(events, dict) or not events:
        raise ValueError("Neds event response did not contain any events.")


async def main():
    async with async_playwright() as playwright:
        browser = await playwright.chromium.launch(headless=True)
        try:
            page = await browser.new_page()
            logging.info("Navigating to %s", NAVIGATE_URL)

            async with page.expect_response(
                is_event_request_response,
                timeout=RESPONSE_TIMEOUT_MS,
            ) as response_info:
                await page.goto(
                    NAVIGATE_URL,
                    wait_until="domcontentloaded",
                    timeout=PAGE_LOAD_TIMEOUT_MS,
                )

            response = await response_info.value
            logging.info("Captured Neds event list response from %s", response.url)
            payload = await response.json()
            validate_event_response(payload)

            OUTPUT_FILE.parent.mkdir(parents=True, exist_ok=True)
            OUTPUT_FILE.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")
            logging.info("Saved Neds event response to %s", OUTPUT_FILE)
        finally:
            await browser.close()


if __name__ == "__main__":
    try:
        asyncio.run(main())
    except PlaywrightTimeoutError as error:
        logging.error("Timed out waiting for Neds event list response: %s", error)
        sys.exit(1)
    except Exception as error:
        logging.error("Failed to capture Neds event list response: %s", error)
        sys.exit(1)
