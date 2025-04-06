from playwright.async_api import async_playwright
import asyncio
import pathlib  # Use pathlib for path handling
import logging  # Optional: for better feedback

# --- Configuration ---
# Page to navigate to
NAVIGATE_URL = "https://www.neds.com.au/sports/australian-rules/afl"
# The exact URL whose response we want to capture
TARGET_RESPONSE_URL = "https://api.neds.com.au/v2/sport/event-request?category_ids=%5B%2223d497e6-8aab-4309-905b-9421f42c9bc5%22%5D&include_any_team_vs_any_team_events=true"
# Output file path using pathlib
OUTPUT_FILE = pathlib.Path("OddsScraper/Neds/neds_response.json")
# Timeouts (in milliseconds for Playwright, seconds for asyncio.wait_for)
PAGE_LOAD_TIMEOUT = 60000  # 60 seconds
WAIT_FOR_RESPONSE_TIMEOUT = 90 # 90 seconds for waiting the specific response

# Configure basic logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
# --- End Configuration ---

async def main():
    """
    Launches a browser, navigates to a page, intercepts a specific network response,
    saves its body to a file, and then cleans up.
    """
    # Future to signal when the target response has been captured and processed
    response_processed_future = asyncio.Future()

    async with async_playwright() as p:
        browser = None # Initialize browser variable
        try:
            logging.info("Launching browser...")
            browser = await p.chromium.launch()
            # Use context manager for the browser instance to ensure it's closed
            async with browser:
                page = await browser.new_page()
                logging.info("New page created.")

                # --- Define the response handler ---
                async def handle_response(response):
                    # Check if we already processed the response or if the future is done
                    if response_processed_future.done():
                        return

                    if response.url == TARGET_RESPONSE_URL:
                        logging.info(f"Target response received from: {response.url}")
                        try:
                            body = await response.body() # Get raw bytes
                            # Ensure the output directory exists
                            OUTPUT_FILE.parent.mkdir(parents=True, exist_ok=True)
                            # Write the decoded body to the file
                            with open(OUTPUT_FILE, "w", encoding="utf-8") as f:
                                f.write(body.decode("utf-8"))
                            logging.info(f"Response body successfully saved to {OUTPUT_FILE}")
                            # Signal that we're done by setting the future's result
                            response_processed_future.set_result(True)
                        except Exception as e:
                            logging.error(f"Error processing response or writing file: {e}")
                            # Signal failure by setting an exception on the future
                            if not response_processed_future.done():
                                response_processed_future.set_exception(e)
                # --- End response handler definition ---

                # Attach the handler to the 'response' event
                page.on('response', handle_response)
                logging.info("Response handler attached.")

                logging.info(f"Navigating to {NAVIGATE_URL}...")
                try:
                    # Navigate to the target page, wait for network idle as before
                    await page.goto(NAVIGATE_URL, wait_until="networkidle", timeout=PAGE_LOAD_TIMEOUT)
                    logging.info("Navigation complete. Waiting for target network response...")
                except Exception as e:
                    logging.error(f"Error during navigation or page load: {e}")
                    # If navigation fails, signal failure immediately
                    if not response_processed_future.done():
                        response_processed_future.set_exception(e)
                    # No point waiting further if navigation failed
                    raise # Re-raise the exception to stop execution here

                # Wait for the response handler to signal completion (or timeout)
                try:
                    await asyncio.wait_for(response_processed_future, timeout=WAIT_FOR_RESPONSE_TIMEOUT)
                    logging.info("Target response captured and processed successfully.")
                except asyncio.TimeoutError:
                    logging.error(f"Timeout: Did not receive the target response from {TARGET_RESPONSE_URL} within {WAIT_FOR_RESPONSE_TIMEOUT} seconds.")
                except Exception as e:
                    # This catches exceptions set on the future (e.g., file writing error)
                    logging.error(f"An error occurred during response processing: {e}")

            # Browser context automatically closes here (due to 'async with browser:')
            logging.info("Browser context closed.")

        except Exception as e:
            logging.error(f"An unexpected error occurred in main execution: {e}")
        finally:
            # Ensure browser is closed if launched but context manager failed somehow (belt-and-braces)
            if browser and not browser.is_closed():
                 logging.warning("Forcing browser close in finally block.")
                 await browser.close()

# Run the script using the standard Python entry point guard
if __name__ == "__main__":
    asyncio.run(main())