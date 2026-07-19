from __future__ import annotations

import logging
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import asdict
from dataclasses import dataclass
from datetime import UTC, datetime, timedelta
from typing import Any

import httpx
import pandas as pd  # type: ignore[import-untyped]

from app.config import Settings
from app.db.duckdb import connection, fetch_all
from app.services.weather_venues import VENUE_COORDINATES, VenueCoordinate
from app.utils.time import utc_now


LOGGER = logging.getLogger(__name__)


@dataclass(frozen=True, slots=True)
class WeatherForecastRow:
    venue: str
    forecast_hour_utc: datetime
    temperature_c: float | None
    wind_kph: float | None
    precipitation_probability: float | None
    precipitation_mm: float | None
    weather_code: int | None
    weather_label: str | None
    weather_icon_code: str | None


class WeatherService:
    def __init__(self, settings: Settings):
        self.settings = settings

    def refresh_upcoming_forecasts(self) -> dict[str, Any]:
        venues = self._load_upcoming_venues()
        fetched_at = utc_now().replace(tzinfo=None)
        expires_at = (utc_now() + timedelta(seconds=self.settings.weather_cache_ttl_seconds)).replace(tzinfo=None)
        window_start = (utc_now() - timedelta(hours=self.settings.weather_refresh_hours_before_event)).replace(
            tzinfo=None
        )
        window_end = (utc_now() + timedelta(days=self.settings.weather_forecast_days)).replace(tzinfo=None)

        errors: list[str] = []
        forecast_rows_written = 0
        refreshed_venues = 0
        resolved_venues = [venue for venue in venues if venue in VENUE_COORDINATES]
        unresolved_venues = sorted(set(venues) - set(resolved_venues))

        if unresolved_venues:
            errors.extend(f"unmapped venue: {venue}" for venue in unresolved_venues)

        fetched_rows_by_venue: dict[str, list[WeatherForecastRow]] = {}
        failed_venues: dict[str, Exception] = {}
        with httpx.Client(timeout=self.settings.weather_request_timeout_seconds) as client:
            with ThreadPoolExecutor(max_workers=min(2, len(resolved_venues) or 1)) as executor:
                futures = {
                    executor.submit(
                        self._fetch_forecast_rows,
                        client=client,
                        venue=VENUE_COORDINATES[venue_name],
                    ): venue_name
                    for venue_name in resolved_venues
                }
                for future in as_completed(futures):
                    venue_name = futures[future]
                    try:
                        fetched_rows_by_venue[venue_name] = future.result()
                    except Exception as exc:  # pragma: no cover - defensive network handling
                        failed_venues[venue_name] = exc

            # Public forecast APIs may briefly rate-limit concurrent requests.
            # Retry only failed venues sequentially before retaining the last good cache.
            for venue_name, initial_error in failed_venues.items():
                try:
                    time.sleep(0.35)
                    fetched_rows_by_venue[venue_name] = self._fetch_forecast_rows(
                        client=client,
                        venue=VENUE_COORDINATES[venue_name],
                    )
                except Exception as exc:  # pragma: no cover - defensive network handling
                    LOGGER.exception("Failed to refresh weather for %s", venue_name)
                    errors.append(f"{venue_name}: {exc or initial_error}")

        if fetched_rows_by_venue:
            all_rows = [
                {
                    **asdict(row),
                    "fetched_at": fetched_at,
                    "expires_at": expires_at,
                }
                for venue_name in sorted(fetched_rows_by_venue)
                for row in fetched_rows_by_venue[venue_name]
            ]
            with connection(write=True, settings=self.settings) as conn:
                for venue_name in fetched_rows_by_venue:
                    self._replace_venue_window(
                        conn=conn,
                        venue_name=venue_name,
                        window_start=window_start,
                        window_end=window_end,
                    )
                if all_rows:
                    relation_name = "_weather_forecast_batch"
                    conn.register(relation_name, pd.DataFrame.from_records(all_rows))
                    try:
                        conn.execute(
                            f"""
                            INSERT INTO weather_forecasts BY NAME
                            SELECT * FROM {relation_name}
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
                            """
                        )
                    finally:
                        conn.unregister(relation_name)
                self._cleanup_expired_rows(conn=conn, cutoff=fetched_at - timedelta(days=1))

            for venue_name, forecast_rows in fetched_rows_by_venue.items():
                refreshed_venues += 1
                forecast_rows_written += len(forecast_rows)

        status = "completed_with_errors" if errors else "completed"
        return {
            "status": status,
            "venues_considered": len(venues),
            "venues_refreshed": refreshed_venues,
            "forecast_rows_written": forecast_rows_written,
            "unmapped_venues": unresolved_venues,
            "errors": errors,
            "fetched_at": fetched_at,
            "expires_at": expires_at,
        }

    def _load_upcoming_venues(self) -> list[str]:
        window_start = (utc_now() - timedelta(hours=self.settings.weather_refresh_hours_before_event)).replace(
            tzinfo=None
        )
        window_end = (utc_now() + timedelta(days=self.settings.weather_forecast_days)).replace(tzinfo=None)
        with connection(settings=self.settings) as conn:
            rows = fetch_all(
                conn,
                """
                SELECT DISTINCT venue
                FROM events
                WHERE venue IS NOT NULL
                  AND start_time_utc IS NOT NULL
                  AND start_time_utc BETWEEN ? AND ?
                ORDER BY venue
                """,
                [window_start, window_end],
            )
        return [row["venue"] for row in rows]

    def _fetch_forecast_rows(
        self,
        *,
        client: httpx.Client,
        venue: VenueCoordinate,
    ) -> list[WeatherForecastRow]:
        response = client.get(
            self.settings.weather_api_url,
            params={
                "latitude": venue.latitude,
                "longitude": venue.longitude,
                "hourly": "temperature_2m,wind_speed_10m,precipitation_probability,precipitation,weather_code",
                "forecast_days": self.settings.weather_forecast_days,
                "timezone": "GMT",
                "timeformat": "unixtime",
            },
        )
        response.raise_for_status()
        payload = response.json()
        hourly = payload.get("hourly") or {}

        times = hourly.get("time") or []
        temperatures = hourly.get("temperature_2m") or []
        winds = hourly.get("wind_speed_10m") or []
        precipitation = hourly.get("precipitation_probability") or []
        precipitation_amounts = hourly.get("precipitation") or []
        weather_codes = hourly.get("weather_code") or []

        rows: list[WeatherForecastRow] = []
        for timestamp, temperature_c, wind_kph, precip_probability, precipitation_mm, weather_code in zip(
            times,
            temperatures,
            winds,
            precipitation,
            precipitation_amounts,
            weather_codes,
            strict=False,
        ):
            label, icon_code = interpret_weather_code(weather_code)
            if venue.indoor:
                label, icon_code = "Clear", "clear"
                precip_probability = 0.0
                precipitation_mm = 0.0
            rows.append(
                WeatherForecastRow(
                    venue=venue.venue_name,
                    forecast_hour_utc=datetime.fromtimestamp(int(timestamp), tz=UTC).replace(tzinfo=None),
                    temperature_c=_coerce_float(temperature_c),
                    wind_kph=_coerce_float(wind_kph),
                    precipitation_probability=_coerce_float(precip_probability),
                    precipitation_mm=_coerce_float(precipitation_mm),
                    weather_code=_coerce_int(weather_code),
                    weather_label=label,
                    weather_icon_code=icon_code,
                )
            )
        return rows

    def _replace_venue_window(
        self,
        *,
        conn: Any,
        venue_name: str,
        window_start: datetime,
        window_end: datetime,
    ) -> None:
        conn.execute(
            """
            DELETE FROM weather_forecasts
            WHERE venue = ?
              AND forecast_hour_utc BETWEEN ? AND ?
            """,
            [venue_name, window_start, window_end],
        )

    def _upsert_forecast_row(
        self,
        *,
        conn: Any,
        row: WeatherForecastRow,
        fetched_at: datetime,
        expires_at: datetime,
    ) -> None:
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
                row.venue,
                row.forecast_hour_utc,
                row.temperature_c,
                row.wind_kph,
                row.precipitation_probability,
                row.precipitation_mm,
                row.weather_code,
                row.weather_label,
                row.weather_icon_code,
                fetched_at,
                expires_at,
            ],
        )

    def _cleanup_expired_rows(self, *, conn: Any, cutoff: datetime) -> None:
        conn.execute(
            """
            DELETE FROM weather_forecasts
            WHERE forecast_hour_utc < ? OR expires_at < ?
            """,
            [cutoff, utc_now().replace(tzinfo=None)],
        )


def interpret_weather_code(weather_code: int | float | None) -> tuple[str | None, str | None]:
    code = _coerce_int(weather_code)
    if code is None:
        return None, None
    if code == 0:
        return "Clear", "clear"
    if code in {1, 2}:
        return "Partly cloudy", "partly_cloudy"
    if code == 3:
        return "Cloudy", "cloudy"
    if code in {45, 48}:
        return "Fog", "fog"
    if code in {51, 53, 55, 56, 57}:
        return "Drizzle", "drizzle"
    if code in {61, 63, 65, 66, 67, 80, 81, 82}:
        return "Rain", "rain"
    if code in {71, 73, 75, 77, 85, 86}:
        return "Snow", "snow"
    if code in {95, 96, 99}:
        return "Storm", "storm"
    return "Cloudy", "cloudy"


def _coerce_float(value: Any) -> float | None:
    if value is None:
        return None
    return float(value)


def _coerce_int(value: Any) -> int | None:
    if value is None:
        return None
    return int(value)
