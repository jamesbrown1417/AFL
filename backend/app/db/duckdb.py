from __future__ import annotations

import threading
from contextlib import contextmanager
from pathlib import Path
from typing import Any, Iterator

import duckdb

from app.config import Settings, get_settings


_write_lock = threading.Lock()


@contextmanager
def connection(
    *,
    write: bool = False,
    settings: Settings | None = None,
    transaction: bool = True,
) -> Iterator[duckdb.DuckDBPyConnection]:
    resolved_settings = settings or get_settings()
    resolved_settings.ensure_runtime_dirs()
    conn = duckdb.connect(str(resolved_settings.duckdb_path), read_only=not write)
    try:
        if write:
            with _write_lock:
                if transaction:
                    conn.execute("BEGIN TRANSACTION")
                    try:
                        yield conn
                    except Exception:
                        conn.rollback()
                        raise
                    else:
                        conn.commit()
                else:
                    yield conn
        else:
            yield conn
    finally:
        conn.close()


def initialize_database(settings: Settings | None = None) -> None:
    resolved_settings = settings or get_settings()
    schema_path = Path(__file__).with_name("schema.sql")
    schema_sql = schema_path.read_text(encoding="utf-8")
    with connection(write=True, settings=resolved_settings) as conn:
        conn.execute(schema_sql)
        refresh_serving_tables(conn)


def refresh_serving_tables(conn: duckdb.DuckDBPyConnection) -> None:
    """Atomically rebuild current-state read models without collapsing selections."""
    conn.execute("DELETE FROM serving_selection_data")
    conn.execute(
        """
        INSERT INTO serving_selection_data
        SELECT
          sbm.selection_id, sbm.bookmaker_id,
          cop.decimal_price, cop.implied_prob, cop.margin, cop.observed_at,
          lm.edge_pct,
          TRY_CAST(json_extract(lm.metrics_json, '$.diff_2025') AS DOUBLE),
          TRY_CAST(json_extract(lm.metrics_json, '$.diff_last_10') AS DOUBLE),
          TRY_CAST(json_extract(lm.metrics_json, '$.home_away_diff') AS DOUBLE),
          TRY_CAST(json_extract(lm.metrics_json, '$.win_loss_diff') AS DOUBLE),
          json_extract_string(lm.metrics_json, '$.player_position'),
          json_extract_string(lm.metrics_json, '$.matchup_difficulty'),
          json_extract_string(lm.metrics_json, '$.over_matchup_difficulty'),
          json_extract_string(lm.metrics_json, '$.under_matchup_difficulty'),
          TRY_CAST(json_extract(lm.metrics_json, '$.dvp') AS DOUBLE),
          TRY_CAST(json_extract(lm.metrics_json, '$.raw_dvp') AS DOUBLE),
          TRY_CAST(json_extract(lm.metrics_json, '$.dvp_standard_error') AS DOUBLE),
          TRY_CAST(json_extract(lm.metrics_json, '$.dvp_bootstrap_ci_low') AS DOUBLE),
          TRY_CAST(json_extract(lm.metrics_json, '$.dvp_bootstrap_ci_high') AS DOUBLE),
          TRY_CAST(json_extract(lm.metrics_json, '$.dvp_sample_count') AS BIGINT),
          TRY_CAST(json_extract(lm.metrics_json, '$.dvp_match_count') AS BIGINT),
          TRY_CAST(json_extract(lm.metrics_json, '$.dvp_observation_count') AS BIGINT),
          json_extract_string(lm.metrics_json, '$.dvp_model_version'),
          json_extract_string(lm.metrics_json, '$.dvp_generated_at')
        FROM selection_bookmaker_meta sbm
        LEFT JOIN current_outcome_prices_v cop
          ON cop.selection_id = sbm.selection_id AND cop.bookmaker_id = sbm.bookmaker_id
        LEFT JOIN latest_selection_metrics_v lm
          ON lm.selection_id = sbm.selection_id AND lm.bookmaker_id = sbm.bookmaker_id
        """
    )
    expected = fetch_value(conn, "SELECT COUNT(*) FROM selection_bookmaker_meta")
    actual = fetch_value(conn, "SELECT COUNT(*) FROM serving_selection_data")
    if actual != expected:
        raise RuntimeError(
            f"Serving odds parity failed: expected {expected} selection/bookmaker rows, got {actual}."
        )
    expected_prices = fetch_value(conn, "SELECT COUNT(*) FROM current_outcome_prices_v")
    actual_prices = fetch_value(
        conn, "SELECT COUNT(*) FROM serving_selection_data WHERE decimal_price IS NOT NULL"
    )
    if actual_prices != expected_prices:
        raise RuntimeError(
            f"Serving price parity failed: expected {expected_prices} current prices, got {actual_prices}."
        )
    mismatched_prices = fetch_value(
        conn,
        """
        SELECT COUNT(*)
        FROM current_outcome_prices_v source
        JOIN serving_selection_data serving
          ON serving.selection_id = source.selection_id
         AND serving.bookmaker_id = source.bookmaker_id
        WHERE serving.decimal_price IS DISTINCT FROM source.decimal_price
           OR serving.implied_prob IS DISTINCT FROM source.implied_prob
        """,
    )
    if mismatched_prices:
        raise RuntimeError(f"Serving price parity failed for {mismatched_prices} rows.")

    conn.execute("DELETE FROM serving_latest_player_team")
    conn.execute(
        """
        INSERT INTO serving_latest_player_team
        SELECT player_id, player_team
        FROM (
          SELECT player_id, player_team,
            ROW_NUMBER() OVER (
              PARTITION BY player_id
              ORDER BY start_time_utc DESC, player_game_log_id DESC
            ) AS row_num
          FROM player_game_logs
          WHERE player_team IS NOT NULL
        ) ranked
        WHERE row_num = 1
        """
    )


def fetch_all(
    conn: duckdb.DuckDBPyConnection, query: str, params: list[Any] | None = None
) -> list[dict[str, Any]]:
    cursor = conn.execute(query, params or [])
    rows = cursor.fetchall()
    columns = [description[0] for description in cursor.description]
    return [dict(zip(columns, row, strict=True)) for row in rows]


def fetch_one(
    conn: duckdb.DuckDBPyConnection, query: str, params: list[Any] | None = None
) -> dict[str, Any] | None:
    rows = fetch_all(conn, query, params)
    return rows[0] if rows else None


def fetch_value(
    conn: duckdb.DuckDBPyConnection, query: str, params: list[Any] | None = None
) -> Any:
    cursor = conn.execute(query, params or [])
    row = cursor.fetchone()
    return row[0] if row else None
