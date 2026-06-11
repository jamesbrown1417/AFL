from __future__ import annotations

from datetime import UTC, datetime

import pandas as pd

from app.services.dvp import aggregate_dvp


def test_aggregate_dvp_uses_deterministic_safe_labels_for_duplicate_thresholds() -> None:
    effects = pd.DataFrame(
        [
            _effect("Team A", 0.0),
            _effect("Team A", 0.0),
            _effect("Team B", 0.0),
            _effect("Team B", 0.0),
            _effect("Team C", 0.12),
            _effect("Team C", 0.20),
            _effect("Team D", -0.10),
            _effect("Team D", -0.20),
            _effect("Team E", 0.0),
            _effect("Team E", 0.0),
        ]
    )
    prepared_stats = pd.DataFrame(
        [
            {"Pos": "MIDFIELDER", "opposition_team": opponent, "match_id": f"match-{idx}"}
            for idx, opponent in enumerate(["Team A", "Team B", "Team C", "Team D", "Team E"])
        ]
    )

    dvp = aggregate_dvp(
        effects=effects,
        prepared_stats=prepared_stats,
        generated_at=datetime(2026, 6, 12, tzinfo=UTC),
        team_window_games=10,
        min_tog_percentage=30,
        shrinkage_prior_samples=1,
        bootstrap_iterations=20,
        random_seed=1,
    )

    assert len(dvp) == 5
    assert dvp["over_matchup_difficulty"].isna().sum() == 0
    assert dvp["under_matchup_difficulty"].isna().sum() == 0
    assert set(dvp["under_matchup_difficulty"]).issubset(
        {"Terrible", "Bad", "Neutral", "Good", "Excellent"}
    )
    team_c = dvp.loc[dvp["Opponent"] == "Team C"].iloc[0]
    assert team_c["over_matchup_difficulty"] == "Excellent"
    assert team_c["under_matchup_difficulty"] == "Terrible"
    assert team_c["match_count"] == 1
    assert team_c["sample_count"] == 2


def _effect(opponent: str, effect: float) -> dict[str, object]:
    return {
        "market_name": "Player Goals",
        "Player": f"{opponent} Player {effect}",
        "Pos": "MIDFIELDER",
        "Team": "Sample Team",
        "Opponent": opponent,
        "effect": effect,
    }
