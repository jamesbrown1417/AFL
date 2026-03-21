#!/usr/bin/env python3
from __future__ import annotations

import json

from app.config import get_settings
from app.db.duckdb import initialize_database
from app.services.weather_service import WeatherService


def main() -> None:
    settings = get_settings()
    initialize_database(settings)
    summary = WeatherService(settings).refresh_upcoming_forecasts()
    print(json.dumps(summary, indent=2, default=str))


if __name__ == "__main__":
    main()
