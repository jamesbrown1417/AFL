from __future__ import annotations

import json
import subprocess
import zlib
from dataclasses import dataclass
from datetime import UTC, datetime
from pathlib import Path
from typing import Any

import numpy as np
import pandas as pd
import pyreadr

from app.utils.hashing import sha256_file
from ingest.normalizers import normalize_player_name, normalize_team_name


MODEL_VERSION = "dvp_v2_2026_positions_shrunk_per100tog"
DEFAULT_TEAM_WINDOW_GAMES = 10
DEFAULT_MIN_TOG_PERCENTAGE = 30.0
DEFAULT_SHRINKAGE_PRIOR_SAMPLES = 12.0
DEFAULT_BOOTSTRAP_ITERATIONS = 400
DEFAULT_RANDOM_SEED = 20260612

MATCHUP_LABELS = ("Terrible", "Bad", "Neutral", "Good", "Excellent")
UNDER_LABEL_MAP = {
    "Terrible": "Excellent",
    "Bad": "Good",
    "Neutral": "Neutral",
    "Good": "Bad",
    "Excellent": "Terrible",
}


@dataclass(frozen=True, slots=True)
class DvpMarketSpec:
    market_name: str
    stat_column: str
    effect_aggregation: str = "median"


MARKET_SPECS = (
    DvpMarketSpec("Player Disposals", "disposals"),
    DvpMarketSpec("Player Kicks", "kicks"),
    DvpMarketSpec("Player Handballs", "handballs"),
    DvpMarketSpec("Player Marks", "marks"),
    DvpMarketSpec("Player Tackles", "tackles"),
    DvpMarketSpec("Player Fantasy Points", "fantasy_points"),
    DvpMarketSpec("Player Goals", "goals", effect_aggregation="mean"),
    DvpMarketSpec("Player Hitouts", "hitouts"),
    DvpMarketSpec("Player Clearances", "total_clearances"),
)

DVP_OUTPUT_COLUMNS = [
    "Pos",
    "Opponent",
    "market_name",
    "dvp",
    "raw_dvp",
    "over_matchup_difficulty",
    "under_matchup_difficulty",
    "matchup_difficulty",
    "games",
    "match_count",
    "sample_count",
    "player_sample_count",
    "observation_count",
    "effect_sd",
    "standard_error",
    "bootstrap_ci_low",
    "bootstrap_ci_high",
    "shrinkage_weight",
    "generated_at",
    "model_version",
    "team_window_games",
    "min_tog_percentage",
]


def refresh_fantasy_start_positions(script_path: Path) -> None:
    subprocess.run(["Rscript", str(script_path)], check=True)


def refresh_detailed_positions(*, season: int, output_path: Path) -> None:
    output_path.parent.mkdir(parents=True, exist_ok=True)
    r_code = f"""
    suppressPackageStartupMessages(library(dplyr))
    suppressPackageStartupMessages(library(fitzRoy))
    positions <- fitzRoy::fetch_player_details_afl(season = {season}) |>
      transmute(
        player_full_name = paste(firstName, surname),
        position = as.character(position)
      ) |>
      filter(!is.na(player_full_name), !is.na(position), position != "") |>
      distinct(player_full_name, .keep_all = TRUE) |>
      arrange(player_full_name)
    write.csv(positions, "{output_path.as_posix()}", row.names = FALSE)
    """
    subprocess.run(["Rscript", "-e", r_code], check=True)


def read_rds_dataframe(path: Path) -> pd.DataFrame:
    result = pyreadr.read_r(str(path))
    if not result:
        raise ValueError(f"RDS file did not contain a dataframe: {path}")
    return next(iter(result.values())).copy()


def load_player_stats(history_path: Path, current_path: Path) -> pd.DataFrame:
    return pd.concat(
        [read_rds_dataframe(history_path), read_rds_dataframe(current_path)],
        ignore_index=True,
        sort=False,
    )


def load_player_positions(
    *,
    detailed_positions_path: Path,
    fantasy_start_positions_path: Path | None = None,
) -> pd.DataFrame:
    if not detailed_positions_path.exists():
        raise FileNotFoundError(f"Detailed player positions file not found: {detailed_positions_path}")

    detailed_positions = pd.read_csv(detailed_positions_path)
    required_columns = {"player_full_name", "position"}
    missing_columns = required_columns.difference(detailed_positions.columns)
    if missing_columns:
        raise ValueError(
            f"{detailed_positions_path} is missing required columns: {sorted(missing_columns)}"
        )

    positions = detailed_positions.loc[:, ["player_full_name", "position"]].copy()
    if "position_source" in detailed_positions.columns:
        positions["position_source"] = detailed_positions["position_source"]
    else:
        positions["position_source"] = "fitzroy_player_details"
    positions = _clean_positions(positions)

    if fantasy_start_positions_path and fantasy_start_positions_path.exists():
        fantasy_positions = _load_fantasy_position_fallback(fantasy_start_positions_path)
        positions = pd.concat([positions, fantasy_positions], ignore_index=True)
        positions = positions.drop_duplicates("player_name_key", keep="first")

    return positions.loc[:, ["player_full_name", "position", "position_source", "player_name_key"]]


def generate_dvp_artifacts(
    *,
    history_stats_path: Path,
    current_stats_path: Path,
    detailed_positions_path: Path,
    fantasy_start_positions_path: Path | None,
    output_path: Path,
    metadata_path: Path,
    generated_at: datetime | None = None,
    team_window_games: int = DEFAULT_TEAM_WINDOW_GAMES,
    min_tog_percentage: float = DEFAULT_MIN_TOG_PERCENTAGE,
    shrinkage_prior_samples: float = DEFAULT_SHRINKAGE_PRIOR_SAMPLES,
    bootstrap_iterations: int = DEFAULT_BOOTSTRAP_ITERATIONS,
    random_seed: int = DEFAULT_RANDOM_SEED,
) -> dict[str, Any]:
    generated_at = generated_at or datetime.now(UTC)
    stats = load_player_stats(history_stats_path, current_stats_path)
    positions = load_player_positions(
        detailed_positions_path=detailed_positions_path,
        fantasy_start_positions_path=fantasy_start_positions_path,
    )
    positions.loc[:, ["player_full_name", "position", "position_source"]].to_csv(
        detailed_positions_path,
        index=False,
    )
    prepared_stats = prepare_player_stats(
        stats=stats,
        positions=positions,
        team_window_games=team_window_games,
        min_tog_percentage=min_tog_percentage,
    )
    effects = build_player_effect_rows(prepared_stats)
    dvp_data = aggregate_dvp(
        effects=effects,
        prepared_stats=prepared_stats,
        generated_at=generated_at,
        team_window_games=team_window_games,
        min_tog_percentage=min_tog_percentage,
        shrinkage_prior_samples=shrinkage_prior_samples,
        bootstrap_iterations=bootstrap_iterations,
        random_seed=random_seed,
    )

    output_path.parent.mkdir(parents=True, exist_ok=True)
    metadata_path.parent.mkdir(parents=True, exist_ok=True)
    dvp_data.to_csv(output_path, index=False, columns=DVP_OUTPUT_COLUMNS)

    metadata = {
        "model_version": MODEL_VERSION,
        "generated_at": generated_at.isoformat(),
        "parameters": {
            "team_window_games": team_window_games,
            "min_tog_percentage": min_tog_percentage,
            "shrinkage_prior_samples": shrinkage_prior_samples,
            "bootstrap_iterations": bootstrap_iterations,
            "random_seed": random_seed,
        },
        "inputs": {
            "history_stats_path": str(history_stats_path),
            "history_stats_sha256": sha256_file(history_stats_path),
            "current_stats_path": str(current_stats_path),
            "current_stats_sha256": sha256_file(current_stats_path),
            "detailed_positions_path": str(detailed_positions_path),
            "detailed_positions_sha256": sha256_file(detailed_positions_path),
            "fantasy_start_positions_path": (
                str(fantasy_start_positions_path) if fantasy_start_positions_path else None
            ),
            "fantasy_start_positions_sha256": (
                sha256_file(fantasy_start_positions_path)
                if fantasy_start_positions_path and fantasy_start_positions_path.exists()
                else None
            ),
        },
        "outputs": {
            "dvp_data_path": str(output_path),
            "row_count": int(len(dvp_data)),
            "markets": sorted(dvp_data["market_name"].unique().tolist()),
            "positions": sorted(dvp_data["Pos"].unique().tolist()),
            "opponents": sorted(dvp_data["Opponent"].unique().tolist()),
        },
        "coverage": {
            "source_stat_rows": int(len(stats)),
            "model_stat_rows": int(len(prepared_stats)),
            "player_effect_rows": int(len(effects)),
            "positioned_players": int(positions["player_name_key"].nunique()),
            "unpositioned_source_rows": int(stats["player_full_name"].isna().sum())
            if "player_full_name" in stats.columns
            else None,
        },
    }
    metadata_path.write_text(json.dumps(metadata, indent=2, sort_keys=True), encoding="utf-8")
    return metadata


def prepare_player_stats(
    *,
    stats: pd.DataFrame,
    positions: pd.DataFrame,
    team_window_games: int = DEFAULT_TEAM_WINDOW_GAMES,
    min_tog_percentage: float = DEFAULT_MIN_TOG_PERCENTAGE,
) -> pd.DataFrame:
    required_columns = {
        "player_full_name",
        "player_team",
        "season_name",
        "round",
        "match_name",
        "home_team",
        "away_team",
        "start_time_utc",
        "opposition_team",
        "tog_percentage",
    }
    stat_columns = {spec.stat_column for spec in MARKET_SPECS}
    missing_columns = required_columns.union(stat_columns).difference(stats.columns)
    if missing_columns:
        raise ValueError(f"Player stats are missing required columns: {sorted(missing_columns)}")

    selected_columns = list(required_columns.union(stat_columns))
    prepared = stats.loc[:, selected_columns].copy()
    prepared["start_time_utc"] = pd.to_datetime(prepared["start_time_utc"], utc=True, errors="coerce")
    prepared["round"] = prepared["round"].astype(str)
    prepared = prepared[~prepared["round"].str.contains("Final", case=False, na=False)]
    prepared["tog_percentage"] = pd.to_numeric(prepared["tog_percentage"], errors="coerce")
    prepared = prepared[prepared["tog_percentage"] >= min_tog_percentage]
    prepared = prepared.dropna(subset=["start_time_utc", "match_name", "season_name"])

    for column in ("player_team", "opposition_team", "home_team", "away_team"):
        prepared[column] = prepared[column].map(normalize_team_name)
    prepared["player_name_key"] = prepared["player_full_name"].map(normalize_player_name)
    prepared["match_id"] = _match_id(prepared)

    home_games = (
        prepared.loc[:, ["match_id", "match_name", "round", "season_name", "start_time_utc", "home_team"]]
        .drop_duplicates()
        .rename(columns={"home_team": "team"})
    )
    away_games = (
        prepared.loc[:, ["match_id", "match_name", "round", "season_name", "start_time_utc", "away_team"]]
        .drop_duplicates()
        .rename(columns={"away_team": "team"})
    )
    last_team_games = pd.concat([home_games, away_games], ignore_index=True)
    last_team_games = last_team_games.dropna(subset=["team"])
    last_team_games = last_team_games.sort_values(["team", "start_time_utc"], ascending=[True, False])
    last_team_games = last_team_games.groupby("team", group_keys=False).head(team_window_games)
    last_match_ids = set(last_team_games["match_id"].tolist())

    prepared = prepared[prepared["match_id"].isin(last_match_ids)].copy()
    positioned = prepared.merge(
        positions.loc[:, ["player_name_key", "position"]],
        how="left",
        on="player_name_key",
        validate="many_to_one",
    )
    positioned = positioned.dropna(subset=["position", "opposition_team"])
    positioned = positioned.rename(columns={"position": "Pos"})
    return positioned


def build_player_effect_rows(prepared_stats: pd.DataFrame) -> pd.DataFrame:
    effect_frames: list[pd.DataFrame] = []
    opponents = sorted(prepared_stats["opposition_team"].dropna().unique().tolist())
    base_columns = [
        "player_full_name",
        "player_team",
        "opposition_team",
        "start_time_utc",
        "tog_percentage",
        "match_id",
        "Pos",
    ]

    for spec in MARKET_SPECS:
        stats_table = prepared_stats.loc[:, [*base_columns, spec.stat_column]].copy()
        stats_table[spec.stat_column] = pd.to_numeric(stats_table[spec.stat_column], errors="coerce")
        stats_table = stats_table.dropna(subset=[spec.stat_column, "tog_percentage"])
        stats_table = stats_table[stats_table["tog_percentage"] > 0]
        stats_table["per100_tog"] = stats_table[spec.stat_column] / (
            stats_table["tog_percentage"] / 100.0
        )
        for opponent in opponents:
            vs_team = stats_table[stats_table["opposition_team"] == opponent]
            vs_others = stats_table[stats_table["opposition_team"] != opponent]
            if vs_team.empty or vs_others.empty:
                continue

            agg = "mean" if spec.effect_aggregation == "mean" else "median"
            vs_player = (
                vs_team.groupby(["player_full_name", "Pos", "player_team", "opposition_team"], dropna=False)
                .agg(
                    value_vs=("per100_tog", agg),
                    player_vs_observations=("per100_tog", "size"),
                    player_vs_matches=("match_id", "nunique"),
                )
                .reset_index()
            )
            others_player = (
                vs_others.groupby(["player_full_name", "Pos", "player_team"], dropna=False)
                .agg(value_others=("per100_tog", agg), player_other_observations=("per100_tog", "size"))
                .reset_index()
            )
            effects = vs_player.merge(
                others_player,
                how="inner",
                on=["player_full_name", "Pos", "player_team"],
                validate="many_to_one",
            )
            if effects.empty:
                continue
            effects["market_name"] = spec.market_name
            effects["effect"] = effects["value_vs"] - effects["value_others"]
            effects = effects.rename(
                columns={
                    "player_full_name": "Player",
                    "player_team": "Team",
                    "opposition_team": "Opponent",
                }
            )
            effect_frames.append(effects)

    if not effect_frames:
        return pd.DataFrame(
            columns=[
                "market_name",
                "Player",
                "Pos",
                "Team",
                "Opponent",
                "effect",
                "player_vs_observations",
                "player_vs_matches",
                "player_other_observations",
            ]
        )
    return pd.concat(effect_frames, ignore_index=True, sort=False)


def aggregate_dvp(
    *,
    effects: pd.DataFrame,
    prepared_stats: pd.DataFrame,
    generated_at: datetime,
    team_window_games: int,
    min_tog_percentage: float,
    shrinkage_prior_samples: float,
    bootstrap_iterations: int,
    random_seed: int,
) -> pd.DataFrame:
    if effects.empty:
        raise ValueError("No DVP player effects were generated.")

    match_counts = (
        prepared_stats.groupby(["Pos", "opposition_team"], dropna=False)
        .agg(match_count=("match_id", "nunique"), observation_count=("match_id", "size"))
        .reset_index()
        .rename(columns={"opposition_team": "Opponent"})
    )

    grouped = (
        effects.groupby(["market_name", "Pos", "Opponent"], dropna=False)
        .agg(
            raw_dvp=("effect", "median"),
            sample_count=("effect", "size"),
            player_sample_count=("Player", "nunique"),
            effect_sd=("effect", _sample_std),
        )
        .reset_index()
    )
    grouped = grouped.merge(match_counts, how="left", on=["Pos", "Opponent"], validate="many_to_one")
    grouped["standard_error"] = grouped.apply(
        lambda row: _standard_error(row["effect_sd"], row["sample_count"]),
        axis=1,
    )
    grouped[["bootstrap_ci_low", "bootstrap_ci_high"]] = grouped.apply(
        lambda row: pd.Series(
            _bootstrap_median_ci(
                effects.loc[
                    (effects["market_name"] == row["market_name"])
                    & (effects["Pos"] == row["Pos"])
                    & (effects["Opponent"] == row["Opponent"]),
                    "effect",
                ].to_numpy(dtype=float),
                iterations=bootstrap_iterations,
                seed=_stable_group_seed(
                    f"{row['market_name']}|{row['Pos']}|{row['Opponent']}", random_seed
                ),
            )
        ),
        axis=1,
    )
    grouped["shrinkage_weight"] = grouped["sample_count"] / (
        grouped["sample_count"] + shrinkage_prior_samples
    )
    grouped["dvp"] = grouped["raw_dvp"] * grouped["shrinkage_weight"]
    grouped = _assign_matchup_labels(grouped)
    grouped["games"] = grouped["match_count"]
    grouped["generated_at"] = generated_at.isoformat()
    grouped["model_version"] = MODEL_VERSION
    grouped["team_window_games"] = team_window_games
    grouped["min_tog_percentage"] = min_tog_percentage
    grouped = grouped.sort_values(["market_name", "Pos", "Opponent"]).reset_index(drop=True)
    return grouped


def _assign_matchup_labels(dvp_data: pd.DataFrame) -> pd.DataFrame:
    labeled_frames = []
    for market_name, market_rows in dvp_data.groupby("market_name", sort=False):
        market_rows = market_rows.copy()
        values = market_rows["dvp"].astype(float)
        unique_values = values.dropna().unique()
        if len(unique_values) < 2 or np.isclose(values.max(), values.min()):
            market_rows["over_matchup_difficulty"] = "Neutral"
        else:
            q1, q2, q3, q4 = values.quantile([0.2, 0.4, 0.6, 0.8]).tolist()
            market_rows["over_matchup_difficulty"] = values.map(
                lambda value: _label_from_thresholds(value, q1, q2, q3, q4)
            )
        market_rows["under_matchup_difficulty"] = market_rows["over_matchup_difficulty"].map(
            UNDER_LABEL_MAP
        )
        # Backward-compatible raw DVP label. Selection-aware labels are assigned during import.
        market_rows["matchup_difficulty"] = market_rows["over_matchup_difficulty"]
        labeled_frames.append(market_rows)
    return pd.concat(labeled_frames, ignore_index=True, sort=False)


def _clean_positions(positions: pd.DataFrame) -> pd.DataFrame:
    cleaned = positions.dropna(subset=["player_full_name", "position"]).copy()
    cleaned["player_full_name"] = cleaned["player_full_name"].astype(str).str.strip()
    cleaned["position"] = cleaned["position"].astype(str).str.strip()
    cleaned = cleaned[(cleaned["player_full_name"] != "") & (cleaned["position"] != "")]
    cleaned["player_name_key"] = cleaned["player_full_name"].map(normalize_player_name)
    cleaned = cleaned.dropna(subset=["player_name_key"])
    return cleaned.drop_duplicates("player_name_key", keep="first")


def _load_fantasy_position_fallback(path: Path) -> pd.DataFrame:
    fantasy = read_rds_dataframe(path)
    required_columns = {
        "player_full_name",
        "forward_status",
        "defender_status",
        "ruck_status",
        "midfield_status",
    }
    missing_columns = required_columns.difference(fantasy.columns)
    if missing_columns:
        raise ValueError(f"{path} is missing required columns: {sorted(missing_columns)}")

    fallback = fantasy.loc[:, list(required_columns)].copy()
    fallback["position"] = fallback.apply(_fantasy_position_to_detailed_fallback, axis=1)
    fallback["position_source"] = "afl_fantasy_fallback"
    fallback = fallback.rename(columns={"player_full_name": "player_full_name"})
    return _clean_positions(fallback.loc[:, ["player_full_name", "position", "position_source"]])


def _fantasy_position_to_detailed_fallback(row: pd.Series) -> str | None:
    if bool(row.get("forward_status")):
        return "MEDIUM_FORWARD"
    if bool(row.get("defender_status")):
        return "MEDIUM_DEFENDER"
    if bool(row.get("ruck_status")):
        return "RUCK"
    if bool(row.get("midfield_status")):
        return "MIDFIELDER"
    return None


def _match_id(df: pd.DataFrame) -> pd.Series:
    return (
        df["match_name"].astype(str)
        + "_"
        + df["round"].astype(str)
        + "_"
        + df["season_name"].astype(str)
    )


def _sample_std(values: pd.Series) -> float | None:
    if len(values) < 2:
        return None
    return float(values.std(ddof=1))


def _standard_error(effect_sd: float | None, sample_count: int) -> float | None:
    if effect_sd is None or sample_count < 2 or pd.isna(effect_sd):
        return None
    return float(effect_sd / np.sqrt(sample_count))


def _bootstrap_median_ci(
    values: np.ndarray,
    *,
    iterations: int,
    seed: int,
) -> tuple[float | None, float | None]:
    clean_values = values[~np.isnan(values)]
    if len(clean_values) == 0:
        return None, None
    if len(clean_values) == 1 or iterations <= 0:
        value = float(clean_values[0])
        return value, value
    rng = np.random.default_rng(seed)
    samples = rng.choice(clean_values, size=(iterations, len(clean_values)), replace=True)
    medians = np.median(samples, axis=1)
    return float(np.percentile(medians, 2.5)), float(np.percentile(medians, 97.5))


def _stable_group_seed(value: str, base_seed: int) -> int:
    return (zlib.crc32(value.encode("utf-8")) + base_seed) % (2**32)


def _label_from_thresholds(value: float, q1: float, q2: float, q3: float, q4: float) -> str:
    if pd.isna(value):
        return "Neutral"
    if value < q1:
        return "Terrible"
    if value < q2:
        return "Bad"
    if value <= q3:
        return "Neutral"
    if value <= q4:
        return "Good"
    return "Excellent"
