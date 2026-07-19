#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Any

import duckdb


PARITY_QUERIES = {
    "events": """
        SELECT e.event_key, e.match_name, home.normalized_name, away.normalized_name,
               e.start_time_utc, e.round_label, e.venue, e.status
        FROM {db}.events e
        JOIN {db}.teams home ON home.team_id = e.home_team_id
        JOIN {db}.teams away ON away.team_id = e.away_team_id
    """,
    "markets": """
        SELECT m.market_key, e.event_key, m.market_type_code, m.market_name_raw,
               p.normalized_name, m.line_value, m.stat_side_scope
        FROM {db}.markets m
        JOIN {db}.events e USING (event_id)
        LEFT JOIN {db}.players p USING (player_id)
    """,
    "selections": """
        SELECT s.selection_key, m.market_key, s.selection_type, s.label, s.sort_order
        FROM {db}.selections s
        JOIN {db}.markets m USING (market_id)
    """,
    "selection_bookmaker_meta": """
        SELECT s.selection_key, b.code, meta.external_market_id,
               meta.external_selection_id, meta.sgm_eligible,
               CAST(meta.payload_meta_json AS VARCHAR)
        FROM {db}.selection_bookmaker_meta meta
        JOIN {db}.selections s USING (selection_id)
        JOIN {db}.bookmakers b USING (bookmaker_id)
    """,
    "all_prices": """
        SELECT s.selection_key, b.code, prices.decimal_price,
               prices.implied_prob, prices.margin
        FROM {db}.outcome_prices prices
        JOIN {db}.selections s USING (selection_id)
        JOIN {db}.bookmakers b USING (bookmaker_id)
    """,
    "current_prices": """
        SELECT s.selection_key, b.code, prices.decimal_price,
               prices.implied_prob, prices.margin
        FROM {db}.current_outcome_prices_v prices
        JOIN {db}.selections s USING (selection_id)
        JOIN {db}.bookmakers b USING (bookmaker_id)
    """,
    "player_game_logs": """
        SELECT p.normalized_name, logs.* EXCLUDE (player_game_log_id, player_id)
        FROM {db}.player_game_logs logs
        JOIN {db}.players p USING (player_id)
    """,
    "metric_values": """
        SELECT s.selection_key, b.code, metrics.fair_prob, metrics.fair_price,
               metrics.edge_pct,
               json_extract(metrics.metrics_json, '$.diff_2025'),
               json_extract(metrics.metrics_json, '$.diff_last_10'),
               json_extract(metrics.metrics_json, '$.home_away_diff'),
               json_extract(metrics.metrics_json, '$.win_loss_diff'),
               json_extract(metrics.metrics_json, '$.prob_2025'),
               json_extract(metrics.metrics_json, '$.prob_last_10'),
               json_extract(metrics.metrics_json, '$.player_position'),
               json_extract(metrics.metrics_json, '$.matchup_difficulty'),
               json_extract(metrics.metrics_json, '$.over_matchup_difficulty'),
               json_extract(metrics.metrics_json, '$.under_matchup_difficulty'),
               json_extract(metrics.metrics_json, '$.dvp'),
               json_extract(metrics.metrics_json, '$.raw_dvp'),
               json_extract(metrics.metrics_json, '$.dvp_standard_error'),
               json_extract(metrics.metrics_json, '$.dvp_bootstrap_ci_low'),
               json_extract(metrics.metrics_json, '$.dvp_bootstrap_ci_high'),
               json_extract(metrics.metrics_json, '$.dvp_sample_count'),
               json_extract(metrics.metrics_json, '$.dvp_match_count'),
               json_extract(metrics.metrics_json, '$.dvp_observation_count'),
               json_extract(metrics.metrics_json, '$.dvp_model_version'),
               json_extract(metrics.metrics_json, '$.dvp_generated_at'),
               json_extract(metrics.metrics_json, '$.variation')
        FROM {db}.selection_metrics metrics
        JOIN {db}.selections s USING (selection_id)
        LEFT JOIN {db}.bookmakers b USING (bookmaker_id)
    """,
}


def compare_databases(baseline: Path, candidate: Path) -> dict[str, Any]:
    conn = duckdb.connect(str(baseline), read_only=False)
    try:
        escaped_candidate = str(candidate).replace("'", "''")
        conn.execute(f"ATTACH '{escaped_candidate}' AS candidate (READ_ONLY)")
        results: dict[str, Any] = {}
        for name, query in PARITY_QUERIES.items():
            baseline_query = query.format(db="main")
            candidate_query = query.format(db="candidate")
            counts = conn.execute(
                f"""
                SELECT
                  (SELECT COUNT(*) FROM ({baseline_query})),
                  (SELECT COUNT(*) FROM ({candidate_query})),
                  (SELECT COUNT(*) FROM (({baseline_query}) EXCEPT ALL ({candidate_query}))),
                  (SELECT COUNT(*) FROM (({candidate_query}) EXCEPT ALL ({baseline_query})))
                """
            ).fetchone()
            if counts is None:
                raise RuntimeError(f"Parity query returned no result for {name}.")
            baseline_count, candidate_count, baseline_only, candidate_only = counts
            results[name] = {
                "baseline_count": baseline_count,
                "candidate_count": candidate_count,
                "baseline_only": baseline_only,
                "candidate_only": candidate_only,
                "matches": baseline_only == 0 and candidate_only == 0,
            }
        return {
            "matches": all(result["matches"] for result in results.values()),
            "tables": results,
        }
    finally:
        conn.close()


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Compare a legacy full rebuild with an incremental candidate."
    )
    parser.add_argument("baseline", type=Path)
    parser.add_argument("candidate", type=Path)
    args = parser.parse_args()
    result = compare_databases(args.baseline, args.candidate)
    print(json.dumps(result, indent=2))
    if not result["matches"]:
        raise SystemExit(1)


if __name__ == "__main__":
    main()
