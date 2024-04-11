from playwright.async_api import async_playwright
import asyncio

# The URL pattern we are interested in
betfair_url = "https://www.betfair.com.au/exchange/plus/en/australian-rules-betting-61420"

# Go to url and get html body of page
async def main():
    async with async_playwright() as p:
        # Launch the browser
        browser = await p.chromium.launch(headless=False)
        page = await browser.new_page()
        
        # Go to the target page
        await page.goto(betfair_url, wait_until="networkidle")
        
        # Fetch and decode the response body
        body = await page.content()
        
        # Write out the body of the response to a file
        with open("Data/betfair_response.html", "w") as f:
            f.write(body)
            
        # Close the browser
        await browser.close()
        
        print("Browser closed.")
        
# Run the script
asyncio.run(main())