import json
from playwright.sync_api import sync_playwright

# Define the URL for the GET request (for AFL odds)
tab_url = "https://api.beta.tab.com.au/v1/tab-info-service/sports/AFL%20Football/competitions/AFL?homeState=SA&jurisdiction=SA"

# Set the headers required for the request (simulating a browser request)
headers = {
    "accept": "application/json, text/plain, */*",
    "accept-language": "en-US,en;q=0.9",
    "referer": "https://www.tab.com.au/",
    "sec-ch-ua": '"Not)A;Brand";v="8", "Chromium";v="138", "Google Chrome";v="138"',
    "sec-ch-ua-mobile": "?0",
    "sec-ch-ua-platform": '"macOS"',
    "sec-fetch-dest": "empty",
    "sec-fetch-mode": "cors",
    "sec-fetch-site": "same-site",
    "user-agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36",
}

tab_response = None

with sync_playwright() as p:
    req = p.request.new_context()
    response = None
    try:
        response = req.get(tab_url, headers=headers, timeout=10000)  # 10s in ms
    except Exception:
        response = None

    if response is not None and response.status == 200:
        try:
            tab_response = response.json()
        except ValueError:
            tab_response = None

# If the response was successfully parsed, write the JSON data to a file.
if tab_response is not None:
    with open("OddsScraper/TAB/tab_response.json", "w") as json_file:
        json.dump(tab_response, json_file, indent=4)
else:
    print("No data to write to file.")