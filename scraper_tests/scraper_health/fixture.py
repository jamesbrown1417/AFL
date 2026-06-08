from __future__ import annotations

from datetime import datetime
from pathlib import Path
from typing import Any
from zoneinfo import ZoneInfo

import pandas as pd


DEFAULT_TIMEZONE = "Australia/Adelaide"


def select_target_round(
    fixture_path: Path,
    *,
    now: datetime | None = None,
    timezone_name: str = DEFAULT_TIMEZONE,
) -> dict[str, Any]:
    timezone = ZoneInfo(timezone_name)
    generated_at = now or datetime.now(timezone)
    if generated_at.tzinfo is None:
        generated_at = generated_at.replace(tzinfo=timezone)
    generated_at_local = generated_at.astimezone(timezone)
    generated_at_utc = generated_at_local.astimezone(ZoneInfo("UTC"))

    fixture = pd.read_csv(fixture_path)
    required_columns = {"match", "round", "home_team", "away_team", "start_time", "venue"}
    missing = sorted(required_columns - set(fixture.columns))
    if missing:
        raise ValueError(f"Fixture file is missing required columns: {', '.join(missing)}")

    fixture = fixture.copy()
    fixture["start_time_utc"] = pd.to_datetime(fixture["start_time"], utc=True)
    fixture["start_time_local"] = fixture["start_time_utc"].dt.tz_convert(timezone_name)
    future = fixture[fixture["start_time_utc"] > pd.Timestamp(generated_at_utc)].copy()
    future = future.sort_values("start_time_utc")

    if future.empty:
        return {
            "timezone": timezone_name,
            "generated_at": generated_at_local.isoformat(),
            "target_round": None,
            "fixtures": [],
            "future_fixture_count": 0,
            "fixture_path": str(fixture_path),
        }

    target_round = str(future.iloc[0]["round"])
    target_fixtures = future[future["round"] == target_round].copy()

    records = []
    for _, row in target_fixtures.iterrows():
        records.append(
            {
                "match": row["match"],
                "round": row["round"],
                "home_team": row["home_team"],
                "away_team": row["away_team"],
                "start_time_utc": row["start_time_utc"].isoformat(),
                "start_time_local": row["start_time_local"].isoformat(),
                "venue": row["venue"],
            }
        )

    return {
        "timezone": timezone_name,
        "generated_at": generated_at_local.isoformat(),
        "target_round": target_round,
        "fixtures": records,
        "future_fixture_count": int(len(future)),
        "fixture_path": str(fixture_path),
    }

