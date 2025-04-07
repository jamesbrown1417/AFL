from playwright.async_api import async_playwright, TimeoutError as PlaywrightTimeoutError
import asyncio
import pandas as pd
import json
import pathlib
import logging

# --- Configuration ---
CSV_FILE = pathlib.Path("OddsScraper/Neds/neds_afl_match_urls.csv")
OUTPUT_DIR = pathlib.Path("OddsScraper/Neds/")
# VVVV More specific target URL structure VVVV
TARGET_API_BASE_URL = "https://api.neds.com.au/v2/sport/event-card"
# Selector to wait for on the page as a sign of basic load completion
WAIT_SELECTOR = '[data-testid="market-title"]'
# Timeouts (milliseconds for Playwright, seconds for asyncio)
NAVIGATION_TIMEOUT = 60000  # 60 seconds
RESPONSE_WAIT_TIMEOUT = 45000 # Increased slightly to 45 seconds
SELECTOR_WAIT_TIMEOUT = 30000 # 30 seconds
# VVVV Added explicit wait as requested VVVV
ADDITIONAL_WAIT_MS = 100  # 1 second = 1000 ms

# Configure basic logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
# --- End Configuration ---

async def main():
    # --- Load URLs ---
    try:
        match_urls_df = pd.read_csv(CSV_FILE)
        if "url" not in match_urls_df.columns:
            logging.error(f"CSV file '{CSV_FILE}' must contain a 'url' column.")
            return
        urls = match_urls_df["url"].tolist()
        logging.info(f"Loaded {len(urls)} URLs from {CSV_FILE}")
    except FileNotFoundError:
        logging.error(f"Input CSV file not found at: '{CSV_FILE}'")
        return
    except Exception as e:
        logging.error(f"Error reading CSV file '{CSV_FILE}': {e}")
        return

    # Ensure the output directory exists
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    logging.info(f"Output directory ensured at: {OUTPUT_DIR}")

    file_counter = 1

    async with async_playwright() as p:
        browser = None
        try:
            browser = await p.chromium.launch(headless=True)
            async with browser:
                page = await browser.new_page()
                logging.info("Browser launched (headless) and page created.")

                for url in urls:
                    logging.info(f"--- Processing URL: {url} ---")
                    response_saved_for_url = False
                    try:
                        # --- Define the specific check for the target response URL ---
                        def is_target_event_card_response(resp):
                            # Check starts with the base path AND contains '?id=' query param start
                            is_match = resp.url.startswith(TARGET_API_BASE_URL) and "?id=" in resp.url
                            # Optional: log matches for debugging (can be very verbose)
                            # if is_match: logging.debug(f"Target API Match: {resp.url}")
                            return is_match
                        # --- End of check definition ---

                        logging.info(f"Setting up listener for responses matching '{TARGET_API_BASE_URL}?id=...'")
                        # Use the specific function in expect_response
                        async with page.expect_response(is_target_event_card_response, timeout=RESPONSE_WAIT_TIMEOUT) as response_info:
                            # Navigate
                            await page.goto(url, wait_until="domcontentloaded", timeout=NAVIGATION_TIMEOUT)
                            logging.info(f"Navigation to {url} initiated.")
                            # Wait for selector
                            await page.wait_for_selector(WAIT_SELECTOR, state='visible', timeout=SELECTOR_WAIT_TIMEOUT)
                            logging.info(f"Selector '{WAIT_SELECTOR}' is visible.")

                            # <<< Add the requested explicit wait AFTER selector >>>
                            if ADDITIONAL_WAIT_MS > 0:
                                logging.info(f"Adding an explicit wait of {ADDITIONAL_WAIT_MS}ms...")
                                await page.wait_for_timeout(ADDITIONAL_WAIT_MS)
                            # <<< End of added wait >>>

                        # If expect_response didn't time out, process the specifically matched response
                        response = await response_info.value
                        logging.info(f"Captured specific target API response: {response.url}")

                        # Process the captured response
                        try:
                            json_body = await response.json()
                            # Use pathlib's / operator for cleaner path joining
                            file_path = OUTPUT_DIR / f"data_{file_counter}.json"
                            with open(file_path, 'w', encoding='utf-8') as f:
                                json.dump(json_body, f, ensure_ascii=False, indent=4)
                            logging.info(f"Successfully saved JSON response to {file_path}")
                            file_counter += 1
                            response_saved_for_url = True
                        except json.JSONDecodeError:
                            logging.warning(f"Failed to decode JSON from response: {response.url}")
                        except Exception as e:
                            logging.error(f"Error saving file for {response.url}: {e}")

                    except PlaywrightTimeoutError:
                        # Log which timeout likely occurred (response, navigation, or selector)
                        # The error message itself usually contains details.
                        logging.warning(f"Timeout occurred processing {url}. Check logs. Was it waiting for response, navigation, or selector?")
                    except Exception as e:
                        logging.error(f"An unexpected error occurred processing URL {url}: {e}")

                    if not response_saved_for_url:
                         logging.warning(f"No response matching '{TARGET_API_BASE_URL}?id=...' was successfully saved for URL: {url}")
                    logging.info(f"--- Finished processing URL: {url} ---")

                logging.info("Finished processing all URLs.")
            logging.info("Browser closed.")

        except Exception as e:
            logging.error(f"An error occurred during browser setup or teardown: {e}")
        finally:
            if browser and browser.is_connected():
                 logging.warning("Forcing browser close in finally block (should be rare).")
                 try:
                     await browser.close()
                 except Exception as close_err:
                     logging.error(f"Error during forced browser close in finally block: {close_err}")

if __name__ == "__main__":
    asyncio.run(main())