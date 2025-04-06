from playwright.async_api import async_playwright, TimeoutError as PlaywrightTimeoutError
import asyncio
import pandas as pd
import json
import pathlib  # Use pathlib for path handling
import logging  # For better feedback

# --- Configuration ---
CSV_FILE = pathlib.Path("OddsScraper/Neds/neds_afl_match_urls.csv")
OUTPUT_DIR = pathlib.Path("OddsScraper/Neds/")
# Substring to identify target request URLs
URL_SUBSTRING = 'card'
# Selector to wait for on the page as a sign of basic load completion
WAIT_SELECTOR = '[data-testid="market-title"]'
# Timeouts (milliseconds for Playwright, seconds for asyncio)
NAVIGATION_TIMEOUT = 60000  # 60 seconds
RESPONSE_WAIT_TIMEOUT = 30000 # 30 seconds to wait for the 'card' response
SELECTOR_WAIT_TIMEOUT = 30000 # 30 seconds to wait for the selector

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

    file_counter = 1  # Maintain the original sequential file naming

    async with async_playwright() as p:
        browser = None # Initialize browser variable
        try:
            browser = await p.chromium.launch(headless=True)
            # Use context manager for the browser to ensure cleanup
            async with browser:
                page = await browser.new_page()
                logging.info("Browser launched (headless) and page created.")

                for url in urls:
                    logging.info(f"--- Processing URL: {url} ---")
                    response_saved_for_url = False
                    try:
                        # Use page.expect_response to wait for the *first* response matching the lambda
                        # Start waiting *before* the action that triggers the response (navigation)
                        async with page.expect_response(
                            lambda resp: URL_SUBSTRING in resp.url,
                            timeout=RESPONSE_WAIT_TIMEOUT
                        ) as response_info:
                            # Navigate to the URL. "domcontentloaded" is often sufficient.
                            await page.goto(url, wait_until="domcontentloaded", timeout=NAVIGATION_TIMEOUT)
                            logging.info(f"Navigation to {url} initiated.")

                            # Optionally, wait for a specific element to ensure the core page structure is ready
                            # This might happen before or after the response is received.
                            await page.wait_for_selector(WAIT_SELECTOR, state='visible', timeout=SELECTOR_WAIT_TIMEOUT)
                            logging.info(f"Selector '{WAIT_SELECTOR}' is visible.")

                        # If expect_response didn't time out, the response is available
                        response = await response_info.value
                        logging.info(f"Captured response containing '{URL_SUBSTRING}' from: {response.url}")

                        # Process the captured response
                        try:
                            json_body = await response.json()
                            file_path = OUTPUT_DIR / f"data_{file_counter}.json" # Use pathlib's / operator
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
                        # Timeout could be from expect_response, goto, or wait_for_selector
                        logging.warning(f"Timeout occurred while processing {url}. Check logs for details (e.g., waiting for response, navigation, or selector).")
                    except Exception as e:
                        logging.error(f"An unexpected error occurred processing URL {url}: {e}")

                    if not response_saved_for_url:
                         logging.warning(f"No response containing '{URL_SUBSTRING}' was successfully saved for URL: {url}")
                    logging.info(f"--- Finished processing URL: {url} ---")


                logging.info("Finished processing all URLs.")
            # Browser context automatically closes here
            logging.info("Browser closed.")

        except Exception as e:
            logging.error(f"An error occurred during browser setup or teardown: {e}")
        finally:
            # Belt-and-braces check in case of unexpected exit before context manager closes
            if browser and not browser.is_closed():
                 logging.warning("Forcing browser close in finally block.")
                 await browser.close()


if __name__ == "__main__":
    asyncio.run(main())