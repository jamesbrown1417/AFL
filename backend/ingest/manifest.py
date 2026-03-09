from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True, slots=True)
class ManifestSpec:
    name: str
    pattern: str
    file_kind: str
    required_columns: tuple[str, ...]
    optional_columns: tuple[str, ...] = ()

    def iter_paths(self, source_dir: Path) -> list[Path]:
        return sorted(source_dir.glob(self.pattern))


MANIFEST: tuple[ManifestSpec, ...] = (
    ManifestSpec(
        name="h2h",
        pattern="*_h2h.csv",
        file_kind="h2h",
        required_columns=("match", "market_name", "home_team", "away_team", "home_win", "away_win", "agency"),
        optional_columns=("start_time", "margin"),
    ),
    ManifestSpec(
        name="line",
        pattern="*line*.csv",
        file_kind="line",
        required_columns=(
            "match",
            "market_name",
            "home_team",
            "away_team",
            "home_line",
            "away_line",
            "home_win",
            "away_win",
            "agency",
        ),
        optional_columns=("margin",),
    ),
    ManifestSpec(
        name="totals",
        pattern="*total*.csv",
        file_kind="totals",
        required_columns=(
            "match",
            "market_name",
            "home_team",
            "away_team",
            "line",
            "over_price",
            "under_price",
            "agency",
        ),
        optional_columns=("start_time", "margin"),
    ),
    ManifestSpec(
        name="player_props",
        pattern="*_player_*.csv",
        file_kind="player_props",
        required_columns=(
            "match",
            "home_team",
            "away_team",
            "market_name",
            "player_name",
            "line",
            "agency",
        ),
        optional_columns=(
            "player_team",
            "opposition_team",
            "over_price",
            "under_price",
            "start_time",
            "margin",
        ),
    ),
)
