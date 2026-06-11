#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import sys
from datetime import UTC, datetime
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd


REPO_ROOT = Path(__file__).resolve().parents[2]
BACKEND_ROOT = REPO_ROOT / "backend"
if str(BACKEND_ROOT) not in sys.path:
    sys.path.insert(0, str(BACKEND_ROOT))

from app.services.dvp import (  # noqa: E402
    DEFAULT_MIN_TOG_PERCENTAGE,
    DEFAULT_TEAM_WINDOW_GAMES,
    build_player_effect_rows,
    load_player_positions,
    load_player_stats,
    prepare_player_stats,
)


DEFAULT_ITERATIONS = 2000
DEFAULT_RANDOM_SEED = 20260612
FAVORABLE_LABELS = {"Good", "Excellent"}
DIFFICULT_LABELS = {"Bad", "Terrible"}


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Run resampling checks for DVP favorable-vs-difficult separation."
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
        "--detailed-positions-path",
        type=Path,
        default=REPO_ROOT / "DVP" / "AFL-Players-Positions-2026.csv",
    )
    parser.add_argument(
        "--fantasy-start-positions-path",
        type=Path,
        default=REPO_ROOT / "Data" / "2026_start_positions_and_prices.rds",
    )
    parser.add_argument("--dvp-data-path", type=Path, default=REPO_ROOT / "DVP" / "dvp_data.csv")
    parser.add_argument("--output-dir", type=Path, default=REPO_ROOT / "DVP" / "validation")
    parser.add_argument("--iterations", type=int, default=DEFAULT_ITERATIONS)
    parser.add_argument("--random-seed", type=int, default=DEFAULT_RANDOM_SEED)
    parser.add_argument("--team-window-games", type=int, default=DEFAULT_TEAM_WINDOW_GAMES)
    parser.add_argument("--min-tog-percentage", type=float, default=DEFAULT_MIN_TOG_PERCENTAGE)
    args = parser.parse_args()

    stats = load_player_stats(args.history_stats_path, args.current_stats_path)
    positions = load_player_positions(
        detailed_positions_path=args.detailed_positions_path,
        fantasy_start_positions_path=args.fantasy_start_positions_path,
    )
    prepared = prepare_player_stats(
        stats=stats,
        positions=positions,
        team_window_games=args.team_window_games,
        min_tog_percentage=args.min_tog_percentage,
    )
    effects = build_player_effect_rows(prepared)
    dvp = pd.read_csv(args.dvp_data_path)
    effects = effects.merge(
        dvp.loc[:, ["market_name", "Pos", "Opponent", "over_matchup_difficulty", "dvp"]],
        how="inner",
        on=["market_name", "Pos", "Opponent"],
        validate="many_to_one",
    )

    summaries = []
    for market_name, market_effects in effects.groupby("market_name", sort=True):
        summaries.append(
            _summarise_market(
                market_name=market_name,
                effects=market_effects,
                iterations=args.iterations,
                seed=args.random_seed,
            )
        )

    summary_df = pd.DataFrame(summaries)
    args.output_dir.mkdir(parents=True, exist_ok=True)
    summary_path = args.output_dir / "dvp_resampling_summary.csv"
    metadata_path = args.output_dir / "dvp_resampling_metadata.json"
    summary_df.to_csv(summary_path, index=False)

    metadata = {
        "generated_at": datetime.now(UTC).isoformat(),
        "method": (
            "For each market, bootstrap favorable player effects "
            "(Good/Excellent over matchup) against difficult effects "
            "(Bad/Terrible over matchup). Positive differences mean favorable "
            "labels produced higher player-vs-baseline effects in the model window."
        ),
        "iterations": args.iterations,
        "random_seed": args.random_seed,
        "inputs": {
            "dvp_data_path": str(args.dvp_data_path),
            "history_stats_path": str(args.history_stats_path),
            "current_stats_path": str(args.current_stats_path),
            "detailed_positions_path": str(args.detailed_positions_path),
            "fantasy_start_positions_path": str(args.fantasy_start_positions_path),
        },
        "outputs": {"summary_path": str(summary_path)},
    }
    metadata_path.write_text(json.dumps(metadata, indent=2, sort_keys=True), encoding="utf-8")
    print(json.dumps({"summary_path": str(summary_path), "metadata_path": str(metadata_path)}, indent=2))


def _summarise_market(
    *,
    market_name: str,
    effects: pd.DataFrame,
    iterations: int,
    seed: int,
) -> dict[str, Any]:
    favorable = effects.loc[effects["over_matchup_difficulty"].isin(FAVORABLE_LABELS), "effect"].dropna()
    difficult = effects.loc[effects["over_matchup_difficulty"].isin(DIFFICULT_LABELS), "effect"].dropna()
    base: dict[str, Any] = {
        "market_name": market_name,
        "favorable_count": int(len(favorable)),
        "difficult_count": int(len(difficult)),
        "favorable_mean": _safe_mean(favorable),
        "difficult_mean": _safe_mean(difficult),
        "favorable_median": _safe_median(favorable),
        "difficult_median": _safe_median(difficult),
        "observed_mean_diff": None,
        "observed_median_diff": None,
        "bootstrap_mean_diff_ci_low": None,
        "bootstrap_mean_diff_ci_high": None,
        "bootstrap_median_diff_ci_low": None,
        "bootstrap_median_diff_ci_high": None,
        "bootstrap_p_directional_mean": None,
        "bootstrap_p_directional_median": None,
        "standardized_mean_diff": None,
        "status": "insufficient_samples",
    }
    if len(favorable) < 2 or len(difficult) < 2:
        return base

    favorable_values = favorable.to_numpy(dtype=float)
    difficult_values = difficult.to_numpy(dtype=float)
    base["observed_mean_diff"] = float(favorable_values.mean() - difficult_values.mean())
    base["observed_median_diff"] = float(np.median(favorable_values) - np.median(difficult_values))
    pooled_sd = _pooled_sd(favorable_values, difficult_values)
    if pooled_sd and pooled_sd > 0:
        base["standardized_mean_diff"] = float(base["observed_mean_diff"] / pooled_sd)

    rng = np.random.default_rng((seed + zlib_crc32(market_name)) % (2**32))
    favorable_samples = rng.choice(
        favorable_values, size=(iterations, len(favorable_values)), replace=True
    )
    difficult_samples = rng.choice(
        difficult_values, size=(iterations, len(difficult_values)), replace=True
    )
    mean_diffs = favorable_samples.mean(axis=1) - difficult_samples.mean(axis=1)
    median_diffs = np.median(favorable_samples, axis=1) - np.median(difficult_samples, axis=1)

    base["bootstrap_mean_diff_ci_low"] = float(np.percentile(mean_diffs, 2.5))
    base["bootstrap_mean_diff_ci_high"] = float(np.percentile(mean_diffs, 97.5))
    base["bootstrap_median_diff_ci_low"] = float(np.percentile(median_diffs, 2.5))
    base["bootstrap_median_diff_ci_high"] = float(np.percentile(median_diffs, 97.5))
    base["bootstrap_p_directional_mean"] = _directional_p_value(mean_diffs)
    base["bootstrap_p_directional_median"] = _directional_p_value(median_diffs)
    base["status"] = "ok"
    return base


def zlib_crc32(value: str) -> int:
    import zlib

    return zlib.crc32(value.encode("utf-8"))


def _safe_mean(values: pd.Series) -> float | None:
    return None if values.empty else float(values.mean())


def _safe_median(values: pd.Series) -> float | None:
    return None if values.empty else float(values.median())


def _pooled_sd(left: np.ndarray, right: np.ndarray) -> float | None:
    if len(left) < 2 or len(right) < 2:
        return None
    numerator = ((len(left) - 1) * np.var(left, ddof=1)) + ((len(right) - 1) * np.var(right, ddof=1))
    denominator = len(left) + len(right) - 2
    return float(np.sqrt(numerator / denominator)) if denominator > 0 else None


def _directional_p_value(values: np.ndarray) -> float:
    le_zero = float(np.mean(values <= 0))
    ge_zero = float(np.mean(values >= 0))
    return min(1.0, 2.0 * min(le_zero, ge_zero))


if __name__ == "__main__":
    main()
