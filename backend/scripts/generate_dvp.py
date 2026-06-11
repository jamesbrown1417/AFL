#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
from pathlib import Path

from app.config import REPO_ROOT
from app.services.dvp import (
    DEFAULT_BOOTSTRAP_ITERATIONS,
    DEFAULT_MIN_TOG_PERCENTAGE,
    DEFAULT_SHRINKAGE_PRIOR_SAMPLES,
    DEFAULT_TEAM_WINDOW_GAMES,
    generate_dvp_artifacts,
    refresh_detailed_positions,
    refresh_fantasy_start_positions,
)


def main() -> None:
    parser = argparse.ArgumentParser(description="Generate backend-owned AFL DVP artifacts.")
    parser.add_argument("--season", type=int, default=2026)
    parser.add_argument(
        "--refresh-fantasy-positions",
        action="store_true",
        help="Run Scripts/scrape-fantasy-positions-official-site.R before generating DVP.",
    )
    parser.add_argument(
        "--refresh-detailed-positions",
        action=argparse.BooleanOptionalAction,
        default=True,
        help="Refresh detailed fitzRoy player positions before generating DVP.",
    )
    parser.add_argument(
        "--team-window-games",
        type=int,
        default=DEFAULT_TEAM_WINDOW_GAMES,
        help="Number of recent team games used in the DVP window.",
    )
    parser.add_argument(
        "--min-tog-percentage",
        type=float,
        default=DEFAULT_MIN_TOG_PERCENTAGE,
        help="Minimum player time-on-ground percentage for game rows.",
    )
    parser.add_argument(
        "--shrinkage-prior-samples",
        type=float,
        default=DEFAULT_SHRINKAGE_PRIOR_SAMPLES,
        help="Prior sample count used to shrink noisy DVP effects toward zero.",
    )
    parser.add_argument(
        "--bootstrap-iterations",
        type=int,
        default=DEFAULT_BOOTSTRAP_ITERATIONS,
        help="Bootstrap iterations per DVP cell for median-effect confidence intervals.",
    )
    parser.add_argument(
        "--history-stats-path",
        type=Path,
        default=REPO_ROOT / "Data" / "afl_fantasy_2015_2025_data.rds",
    )
    parser.add_argument(
        "--current-stats-path",
        type=Path,
        default=REPO_ROOT / "Data" / "afl_fantasy_2026_data.rds",
    )
    parser.add_argument(
        "--fantasy-start-positions-path",
        type=Path,
        default=REPO_ROOT / "Data" / "2026_start_positions_and_prices.rds",
    )
    parser.add_argument(
        "--detailed-positions-path",
        type=Path,
        default=REPO_ROOT / "DVP" / "AFL-Players-Positions-2026.csv",
    )
    parser.add_argument(
        "--output-path",
        type=Path,
        default=REPO_ROOT / "DVP" / "dvp_data.csv",
    )
    parser.add_argument(
        "--metadata-path",
        type=Path,
        default=REPO_ROOT / "DVP" / "dvp_metadata.json",
    )
    args = parser.parse_args()

    if args.refresh_fantasy_positions:
        refresh_fantasy_start_positions(
            REPO_ROOT / "Scripts" / "scrape-fantasy-positions-official-site.R"
        )

    if args.refresh_detailed_positions:
        refresh_detailed_positions(season=args.season, output_path=args.detailed_positions_path)

    metadata = generate_dvp_artifacts(
        history_stats_path=args.history_stats_path,
        current_stats_path=args.current_stats_path,
        detailed_positions_path=args.detailed_positions_path,
        fantasy_start_positions_path=args.fantasy_start_positions_path,
        output_path=args.output_path,
        metadata_path=args.metadata_path,
        team_window_games=args.team_window_games,
        min_tog_percentage=args.min_tog_percentage,
        shrinkage_prior_samples=args.shrinkage_prior_samples,
        bootstrap_iterations=args.bootstrap_iterations,
    )
    print(json.dumps(metadata, indent=2, sort_keys=True))


if __name__ == "__main__":
    main()
