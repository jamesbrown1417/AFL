#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
from pathlib import Path

from app.config import get_settings
from app.services.weather_service import WeatherService
from ingest.import_csvs import run_import


def _remove_if_exists(path: Path) -> None:
    if path.exists():
        path.unlink()


def reset_duckdb_files() -> None:
    settings = get_settings()
    settings.ensure_runtime_dirs()
    duckdb_path = settings.duckdb_path
    sidecar_paths = [
        duckdb_path,
        Path(f"{duckdb_path}.wal"),
        Path(f"{duckdb_path}.tmp"),
    ]
    for path in sidecar_paths:
        _remove_if_exists(path)


def main() -> None:
    parser = argparse.ArgumentParser(description="Run a one-off backend import.")
    parser.add_argument(
        "--reset",
        dest="reset",
        action="store_true",
        default=True,
        help="Delete the DuckDB file and rebuild from source artifacts before importing (default).",
    )
    parser.add_argument(
        "--no-reset",
        dest="reset",
        action="store_false",
        help="Keep the existing DuckDB file and run an incremental import instead.",
    )
    args = parser.parse_args()

    if args.reset:
        reset_duckdb_files()

    settings = get_settings()
    summary = run_import(
        settings,
        triggered_by="manual_full_reset" if args.reset else "manual",
    )
    weather_summary = WeatherService(settings).refresh_upcoming_forecasts()
    payload = dict(summary)
    payload["weather_refresh"] = weather_summary
    print(json.dumps(payload, indent=2, default=str))


if __name__ == "__main__":
    main()
