import json
import logging
import pathlib
import sys
import urllib.error
import urllib.parse
import urllib.request


NAVIGATE_URL = "https://www.neds.com.au/sports/australian-rules/afl"
GRAPHQL_URL = "https://api.neds.com.au/gql/router"
GRAPHQL_OPERATION_NAME = "SportingCompetitionScreen"
GRAPHQL_PERSISTED_QUERY_HASH = "3f427adebc982bfd437404d8e62e820a8b1ba217097ae9998abe316bd7244c2a"
OUTPUT_FILE = pathlib.Path("OddsScraper/Neds/neds_response.json")
REQUEST_TIMEOUT_SECONDS = 30

GRAPHQL_VARIABLES = {
    "category": "AUSTRALIAN_RULES",
    "regionSlug": "",
    "competitionSlug": "afl",
    "statuses": ["OPEN", "LIVE"],
    "excludeCategoryIds": [],
    "includeLeagues": False,
    "includeUpcomingEvents": True,
    "upcomingEventsGroupBy": "UNSPECIFIED",
    "includeFutures": True,
    "futuresGroupBy": "UNSPECIFIED",
}

GRAPHQL_EXTENSIONS = {
    "persistedQuery": {
        "version": 1,
        "sha256Hash": GRAPHQL_PERSISTED_QUERY_HASH,
    }
}

REQUEST_HEADERS = {
    "Accept": "application/json",
    "Referer": NAVIGATE_URL,
    "User-Agent": (
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
        "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36"
    ),
}

logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s")


def compact_json(value):
    return json.dumps(value, separators=(",", ":"))


def strip_graphql_type(value):
    if not isinstance(value, str):
        return value
    return value.split(":", 1)[1] if ":" in value else value


def build_graphql_url():
    query = urllib.parse.urlencode(
        {
            "variables": compact_json(GRAPHQL_VARIABLES),
            "operationName": GRAPHQL_OPERATION_NAME,
            "extensions": compact_json(GRAPHQL_EXTENSIONS),
        }
    )
    return f"{GRAPHQL_URL}?{query}"


def fetch_graphql_payload():
    request = urllib.request.Request(build_graphql_url(), headers=REQUEST_HEADERS)
    try:
        with urllib.request.urlopen(request, timeout=REQUEST_TIMEOUT_SECONDS) as response:
            return json.load(response)
    except urllib.error.HTTPError as error:
        body = error.read().decode("utf-8", errors="replace")
        raise RuntimeError(f"Neds GraphQL request failed with HTTP {error.code}: {body}") from error


def extract_upcoming_events(graphql_payload):
    errors = graphql_payload.get("errors")
    if errors:
        raise ValueError(f"Neds GraphQL response contained errors: {errors}")

    events = (
        graphql_payload.get("data", {})
        .get("upcomingEvents", {})
        .get("events", {})
        .get("nodes")
    )
    if not isinstance(events, list) or not events:
        raise ValueError("Neds GraphQL response did not contain any upcoming AFL events.")
    return events


def to_legacy_event(event):
    event_id = strip_graphql_type(event.get("id"))
    if not event_id:
        raise ValueError(f"Neds event is missing an id: {event}")

    legacy_event = dict(event)
    legacy_event["id"] = event_id

    competition = dict(legacy_event.get("competition") or {})
    if "id" in competition:
        competition["id"] = strip_graphql_type(competition["id"])
    legacy_event["competition"] = competition

    return event_id, legacy_event


def to_legacy_payload(graphql_payload):
    events = {}
    for event in extract_upcoming_events(graphql_payload):
        event_id, legacy_event = to_legacy_event(event)
        events[event_id] = legacy_event

    if not events:
        raise ValueError("Neds event response did not contain any events.")
    return {"events": events}


def main():
    logging.info("Fetching Neds AFL events from %s", GRAPHQL_URL)
    payload = to_legacy_payload(fetch_graphql_payload())
    OUTPUT_FILE.parent.mkdir(parents=True, exist_ok=True)
    OUTPUT_FILE.write_text(json.dumps(payload, ensure_ascii=False, indent=2), encoding="utf-8")
    logging.info("Saved %s Neds event records to %s", len(payload["events"]), OUTPUT_FILE)


if __name__ == "__main__":
    try:
        main()
    except Exception as error:
        logging.error("Failed to capture Neds event list response: %s", error)
        sys.exit(1)
