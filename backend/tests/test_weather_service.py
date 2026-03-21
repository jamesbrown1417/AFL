from __future__ import annotations

from datetime import timedelta

from app.db.duckdb import connection, fetch_one
from app.services.weather_service import interpret_weather_code
from app.utils.time import utc_now


def test_interpret_weather_code_maps_expected_buckets() -> None:
    assert interpret_weather_code(0) == ("Clear", "clear")
    assert interpret_weather_code(2) == ("Partly cloudy", "partly_cloudy")
    assert interpret_weather_code(3) == ("Cloudy", "cloudy")
    assert interpret_weather_code(45) == ("Fog", "fog")
    assert interpret_weather_code(53) == ("Drizzle", "drizzle")
    assert interpret_weather_code(61) == ("Rain", "rain")
    assert interpret_weather_code(95) == ("Storm", "storm")


def test_odds_weather_attaches_when_cache_row_exists(client, imported_settings) -> None:
    with connection(write=True, settings=imported_settings) as conn:
        event = fetch_one(
            conn,
            """
            SELECT DISTINCT
              e.event_id,
              e.venue,
              e.start_time_utc
            FROM events e
            JOIN markets m ON m.event_id = e.event_id
            JOIN selections s ON s.market_id = m.market_id
            JOIN selection_bookmaker_meta sbm ON sbm.selection_id = s.selection_id
            JOIN bookmakers b ON b.bookmaker_id = sbm.bookmaker_id
            WHERE b.code = 'sportsbet'
              AND e.venue IS NOT NULL
              AND e.start_time_utc IS NOT NULL
            ORDER BY e.start_time_utc
            LIMIT 1
            """,
        )
        assert event is not None
        fetched_at = utc_now().replace(tzinfo=None)
        expires_at = (utc_now() + timedelta(hours=2)).replace(tzinfo=None)
        conn.execute(
            """
            INSERT INTO weather_forecasts (
              venue,
              forecast_hour_utc,
              temperature_c,
              wind_kph,
              precipitation_probability,
              weather_code,
              weather_label,
              weather_icon_code,
              fetched_at,
              expires_at
            )
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT (venue, forecast_hour_utc) DO UPDATE SET
              temperature_c = EXCLUDED.temperature_c,
              wind_kph = EXCLUDED.wind_kph,
              precipitation_probability = EXCLUDED.precipitation_probability,
              weather_code = EXCLUDED.weather_code,
              weather_label = EXCLUDED.weather_label,
              weather_icon_code = EXCLUDED.weather_icon_code,
              fetched_at = EXCLUDED.fetched_at,
              expires_at = EXCLUDED.expires_at
            """,
            [
                event["venue"],
                event["start_time_utc"],
                23.0,
                18.0,
                35.0,
                61,
                "Rain",
                "rain",
                fetched_at,
                expires_at,
            ],
        )

    response = client.get(
        "/api/v1/odds/search",
        params={
            "bookmaker": "sportsbet",
            "event_id": event["event_id"],
            "limit": 10,
        },
    )
    assert response.status_code == 200
    payload = response.json()
    assert payload
    weather = payload[0]["weather"]
    assert weather is not None
    assert weather["temperature_c"] == 23.0
    assert weather["wind_kph"] == 18.0
    assert weather["precip_probability"] == 35.0
    assert weather["label"] == "Rain"
    assert weather["icon_code"] == "rain"
