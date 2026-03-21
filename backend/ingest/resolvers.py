from __future__ import annotations

import csv
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Any

from app.utils.hashing import sha256_text
from app.utils.time import parse_iso_datetime
from ingest.normalizers import clean_text, normalize_team_name


LEAGUE_CODE = "afl"


@dataclass(frozen=True, slots=True)
class EventContext:
    event_key: str
    match_name: str
    home_team_name: str
    away_team_name: str
    start_time_utc: Any
    round_label: str | None
    venue: str | None
    status: str = "scheduled"


def build_match_name(home_team_name: str, away_team_name: str) -> str:
    return f"{home_team_name} v {away_team_name}"


def build_event_key(home_team_name: str, away_team_name: str, start_time_iso: str | None) -> str:
    key_input = f"{LEAGUE_CODE}|{home_team_name}|{away_team_name}|{start_time_iso or 'unknown'}"
    return sha256_text(key_input)


def _naive_utc(value: datetime | None) -> datetime | None:
    if value is None:
        return None
    return value.replace(tzinfo=None)


def load_fixture_index(fixture_path: Path) -> dict[tuple[str, str], EventContext]:
    if not fixture_path.exists():
        return {}

    index: dict[tuple[str, str], EventContext] = {}
    with fixture_path.open("r", encoding="utf-8-sig", newline="") as handle:
        reader = csv.DictReader(handle)
        for row in reader:
            home_team_name = normalize_team_name(row.get("home_team"))
            away_team_name = normalize_team_name(row.get("away_team"))
            if not home_team_name or not away_team_name:
                continue
            start_time = _naive_utc(parse_iso_datetime(row.get("start_time")))
            start_time_iso = start_time.isoformat() if start_time else None
            index[(home_team_name, away_team_name)] = EventContext(
                event_key=build_event_key(home_team_name, away_team_name, start_time_iso),
                match_name=clean_text(row.get("match")) or build_match_name(home_team_name, away_team_name),
                home_team_name=home_team_name,
                away_team_name=away_team_name,
                start_time_utc=start_time,
                round_label=clean_text(row.get("round")),
                venue=clean_text(row.get("venue")),
            )
    return index


def resolve_event_context(row: dict[str, str], fixture_index: dict[tuple[str, str], EventContext]) -> EventContext:
    home_team_name = normalize_team_name(row.get("home_team"))
    away_team_name = normalize_team_name(row.get("away_team"))
    if not home_team_name or not away_team_name:
        raise ValueError("Row is missing home/away team fields.")

    if fixture_match := fixture_index.get((home_team_name, away_team_name)):
        return fixture_match

    row_start_time = _naive_utc(parse_iso_datetime(row.get("start_time")))
    start_time_iso = row_start_time.isoformat() if row_start_time else None
    return EventContext(
        event_key=build_event_key(home_team_name, away_team_name, start_time_iso),
        match_name=clean_text(row.get("match")) or build_match_name(home_team_name, away_team_name),
        home_team_name=home_team_name,
        away_team_name=away_team_name,
        start_time_utc=row_start_time,
        round_label=None,
        venue=None,
    )
