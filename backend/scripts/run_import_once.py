#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import os
import shutil
from pathlib import Path

from app.config import get_settings
from app.services.weather_service import WeatherService
from ingest.import_csvs import run_import


def _remove_if_exists(path: Path) -> None:
    if path.exists():
        path.unlink()


def reset_duckdb_files(duckdb_path: Path) -> None:
    sidecar_paths = [
        duckdb_path,
        Path(f"{duckdb_path}.wal"),
        Path(f"{duckdb_path}.tmp"),
    ]
    for path in sidecar_paths:
        _remove_if_exists(path)


def _publish_staged_database(staged_path: Path, live_path: Path) -> None:
    backup_path = live_path.with_suffix(f"{live_path.suffix}.previous")
    _remove_if_exists(Path(f"{staged_path}.wal"))
    if live_path.exists():
        shutil.copy2(live_path, backup_path)
    os.replace(staged_path, live_path)


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

    settings = get_settings()
    staged_path = settings.duckdb_path.with_suffix(f"{settings.duckdb_path.suffix}.staging")
    reset_duckdb_files(staged_path)
    if not args.reset and settings.duckdb_path.exists():
        shutil.copy2(settings.duckdb_path, staged_path)
    staged_settings = settings.model_copy(update={"duckdb_path": staged_path})
    summary = run_import(
        staged_settings,
        triggered_by="manual_full_reset" if args.reset else "manual",
    )
    if summary["status"] not in {"completed", "completed_with_errors"}:
        raise RuntimeError(f"Staged import did not complete: {summary['status']}")
    weather_summary = WeatherService(staged_settings).refresh_upcoming_forecasts()
    _publish_staged_database(staged_path, settings.duckdb_path)
    payload = dict(summary)
    payload["weather_refresh"] = weather_summary
    print(json.dumps(payload, indent=2, default=str))


if __name__ == "__main__":
    main()
