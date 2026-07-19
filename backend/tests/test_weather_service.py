from __future__ import annotations

from datetime import datetime, timedelta

from app.db.duckdb import connection, fetch_one, fetch_value
from app.services.weather_service import (
    WeatherForecastRow,
    WeatherService,
    interpret_weather_code,
)
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
              precipitation_mm,
              weather_code,
              weather_label,
              weather_icon_code,
              fetched_at,
              expires_at
            )
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT (venue, forecast_hour_utc) DO UPDATE SET
              temperature_c = EXCLUDED.temperature_c,
              wind_kph = EXCLUDED.wind_kph,
              precipitation_probability = EXCLUDED.precipitation_probability,
              precipitation_mm = EXCLUDED.precipitation_mm,
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
                0.8,
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
    assert weather["precip_mm"] == 0.8
    assert weather["label"] == "Rain"
    assert weather["icon_code"] == "rain"


def test_weather_refresh_bulk_writes_fetched_venues(
    imported_settings, monkeypatch
) -> None:
    forecast_hour = datetime(2026, 7, 19, 5)
    service = WeatherService(imported_settings)
    monkeypatch.setattr(
        service,
        "_load_upcoming_venues",
        lambda: ["Adelaide Oval", "Marvel Stadium"],
    )

    def fake_fetch(*, client, venue):
        del client
        return [
            WeatherForecastRow(
                venue=venue.venue_name,
                forecast_hour_utc=forecast_hour,
                temperature_c=19.0,
                wind_kph=12.0,
                precipitation_probability=5.0,
                precipitation_mm=0.0,
                weather_code=1,
                weather_label="Partly cloudy",
                weather_icon_code="partly_cloudy",
            )
        ]

    monkeypatch.setattr(service, "_fetch_forecast_rows", fake_fetch)
    summary = service.refresh_upcoming_forecasts()

    assert summary["status"] == "completed"
    assert summary["venues_refreshed"] == 2
    assert summary["forecast_rows_written"] == 2
    with connection(settings=imported_settings) as conn:
        assert (
            fetch_value(
                conn,
                """
                SELECT COUNT(*)
                FROM weather_forecasts
                WHERE venue IN ('Adelaide Oval', 'Marvel Stadium')
                  AND forecast_hour_utc = ?
                """,
                [forecast_hour],
            )
            == 2
        )
