from __future__ import annotations

import logging
from dataclasses import asdict
from datetime import datetime
from pathlib import Path
from typing import Any

import pandas as pd  # type: ignore[import-untyped]

from app.config import Settings, get_settings
from app.db.duckdb import connection, fetch_value, initialize_database, refresh_serving_tables
from app.utils.hashing import sha256_file, sha256_text, stable_json_dumps
from app.utils.time import utc_now
from ingest.import_csvs import ImportSummary, Importer
from ingest.manifest import MANIFEST, ManifestSpec
from ingest.resolvers import LEAGUE_CODE, EventContext, load_fixture_index, resolve_event_context


LOGGER = logging.getLogger(__name__)

FIXTURE_IMPORTER_VERSION = "fixture-v2"
ODDS_IMPORTER_VERSION = "odds-normalizer-v2"
METRIC_IMPORTER_VERSION = "processed-metrics-v2"
GAME_LOG_IMPORTER_VERSION = "game-logs-v2"


class IncrementalImporter(Importer):
    """Source-partitioned importer with set-based DuckDB materialization.

    A source partition is replaced only when its bytes, dependencies, or
    normalizer version change. Canonical tables retain stable IDs for unchanged
    natural keys, while rows removed from a changed/deleted source are removed.
    """

    def run(self, triggered_by: str = "incremental") -> ImportSummary:
        initialize_database(self.settings, refresh_read_models=False)
        fixture_index = load_fixture_index(self.settings.fixture_path)
        self._affected_game_log_keys: set[str] = set()
        self._affected_metric_keys: set[tuple[str, str]] = set()
        summary: ImportSummary = {
            "files_scanned": 0,
            "files_imported": 0,
            "error_count": 0,
            "errors": [],
            "import_run_id": 0,
            "status": "running",
        }
        with connection(write=True, settings=self.settings, transaction=False) as conn:
            conn.execute("SET preserve_insertion_order = false")
            self._abandon_stale_import_runs(conn)
            observed_at = utc_now().replace(tzinfo=None)
            import_run_id = self._start_import_run(conn, observed_at, triggered_by)
            summary["import_run_id"] = import_run_id
            try:
                current_paths: dict[str, set[str]] = {
                    "fixture": set(),
                    "odds": set(),
                    "processed_player_props": set(),
                    "player_game_logs": set(),
                }
                odds_changed: set[str] = set()
                metrics_changed: set[str] = set()
                game_logs_changed: set[str] = set()
                fixture_changed = False

                if self.settings.fixture_path.exists():
                    path = self.settings.fixture_path
                    current_paths["fixture"].add(str(path))
                    summary["files_scanned"] += 1
                    dependency = self._dependency_fingerprint(
                        path, FIXTURE_IMPORTER_VERSION, ()
                    )
                    if self._artifact_changed(conn, path, dependency, FIXTURE_IMPORTER_VERSION):
                        self._replace_fixture_source(
                            conn, path, fixture_index, import_run_id, observed_at, dependency
                        )
                        summary["files_imported"] += 1
                        fixture_changed = True

                fixture_dependency = self._optional_file_sha(self.settings.fixture_path)
                for spec in MANIFEST:
                    for path in spec.iter_paths(self.settings.scraped_odds_dir):
                        current_paths["odds"].add(str(path))
                        summary["files_scanned"] += 1
                        dependency = self._dependency_fingerprint(
                            path, ODDS_IMPORTER_VERSION, (fixture_dependency,)
                        )
                        if not self._artifact_changed(
                            conn, path, dependency, ODDS_IMPORTER_VERSION
                        ):
                            continue
                        try:
                            self._replace_odds_source(
                                conn,
                                path,
                                spec,
                                fixture_index,
                                import_run_id,
                                observed_at,
                                dependency,
                            )
                            summary["files_imported"] += 1
                            odds_changed.add(str(path))
                        except Exception as exc:
                            self._record_source_error(
                                conn, summary, import_run_id, path, spec.file_kind, exc
                            )

                metric_dependencies = (
                    fixture_dependency,
                    self._optional_file_sha(self.settings.dvp_data_path),
                    self._optional_file_sha(self.settings.player_positions_path),
                    self._optional_file_sha(self.settings.home_away_diff_path),
                    self._optional_file_sha(self.settings.win_loss_diff_path),
                )
                for path in sorted(self.settings.processed_odds_dir.glob("all_player_*.rds")):
                    current_paths["processed_player_props"].add(str(path))
                    summary["files_scanned"] += 1
                    dependency = self._dependency_fingerprint(
                        path, METRIC_IMPORTER_VERSION, metric_dependencies
                    )
                    if not self._artifact_changed(
                        conn, path, dependency, METRIC_IMPORTER_VERSION
                    ):
                        continue
                    try:
                        self._replace_metric_source(
                            conn,
                            path,
                            fixture_index,
                            import_run_id,
                            observed_at,
                            dependency,
                        )
                        summary["files_imported"] += 1
                        metrics_changed.add(str(path))
                    except Exception as exc:
                        self._record_source_error(
                            conn,
                            summary,
                            import_run_id,
                            path,
                            "processed_player_props",
                            exc,
                        )

                for path in sorted(self.settings.fixture_path.parent.glob("afl_fantasy*_data.rds")):
                    current_paths["player_game_logs"].add(str(path))
                    summary["files_scanned"] += 1
                    dependency = self._dependency_fingerprint(
                        path, GAME_LOG_IMPORTER_VERSION, ()
                    )
                    if not self._artifact_changed(
                        conn, path, dependency, GAME_LOG_IMPORTER_VERSION
                    ):
                        continue
                    try:
                        self._replace_game_log_source(
                            conn, path, import_run_id, observed_at, dependency
                        )
                        summary["files_imported"] += 1
                        game_logs_changed.add(str(path))
                    except Exception as exc:
                        self._record_source_error(
                            conn, summary, import_run_id, path, "player_game_logs", exc
                        )

                removed = self._remove_missing_sources(conn, current_paths)
                fixture_changed = fixture_changed or bool(removed["fixture"])
                odds_changed.update(removed["odds"])
                metrics_changed.update(removed["processed_player_props"])
                game_logs_changed.update(removed["player_game_logs"])

                if fixture_changed or odds_changed:
                    self._materialize_odds(
                        conn=conn,
                        import_run_id=import_run_id,
                        observed_at=observed_at,
                        changed_paths=odds_changed,
                    )
                if metrics_changed or odds_changed:
                    self._materialize_metrics(
                        conn=conn,
                        observed_at=observed_at,
                        rebuild_all=bool(odds_changed),
                    )
                if game_logs_changed:
                    self._materialize_game_logs(conn, rebuild_all=False)
                if fixture_changed or odds_changed or metrics_changed or game_logs_changed:
                    refresh_serving_tables(conn)

                status = "completed_with_errors" if summary["error_count"] else "completed"
                summary["status"] = status
                self._finish_import_run(conn, import_run_id, status, summary)
                return summary
            except Exception:
                summary["status"] = "failed"
                summary["error_count"] += 1
                self._finish_import_run(conn, import_run_id, "failed", summary)
                raise

    def _optional_file_sha(self, path: Path) -> str:
        return sha256_file(path) if path.exists() else f"missing:{path}"

    def _dependency_fingerprint(
        self, path: Path, importer_version: str, dependencies: tuple[str, ...]
    ) -> str:
        return sha256_text(
            stable_json_dumps(
                {
                    "content_sha256": sha256_file(path),
                    "dependencies": dependencies,
                    "importer_version": importer_version,
                }
            )
        )

    def _artifact_changed(
        self, conn: Any, path: Path, dependency: str, importer_version: str
    ) -> bool:
        existing = fetch_value(
            conn,
            """
            SELECT 1
            FROM source_artifacts
            WHERE source_path = ?
              AND content_sha256 = ?
              AND dependency_fingerprint = ?
              AND importer_version = ?
            """,
            [str(path), sha256_file(path), dependency, importer_version],
        )
        return not bool(existing)

    def _record_artifact(
        self,
        conn: Any,
        *,
        path: Path,
        file_kind: str,
        dependency: str,
        importer_version: str,
        imported_file_id: int,
        rows_read: int,
        rows_loaded: int,
        observed_at: datetime,
    ) -> None:
        conn.execute(
            """
            INSERT INTO source_artifacts BY NAME
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT (source_path) DO UPDATE SET
              file_kind = excluded.file_kind,
              content_sha256 = excluded.content_sha256,
              dependency_fingerprint = excluded.dependency_fingerprint,
              importer_version = excluded.importer_version,
              imported_file_id = excluded.imported_file_id,
              rows_read = excluded.rows_read,
              rows_loaded = excluded.rows_loaded,
              updated_at = excluded.updated_at
            """,
            [
                str(path),
                file_kind,
                sha256_file(path),
                dependency,
                importer_version,
                imported_file_id,
                rows_read,
                rows_loaded,
                observed_at,
            ],
        )

    def _register_frame(self, conn: Any, relation_name: str, rows: list[dict[str, Any]]) -> None:
        conn.register(relation_name, pd.DataFrame.from_records(rows))

    def _replace_fixture_source(
        self,
        conn: Any,
        path: Path,
        fixture_index: dict[tuple[str, str], EventContext],
        import_run_id: int,
        observed_at: datetime,
        dependency: str,
    ) -> None:
        rows = [
            {
                "source_path": str(path),
                "source_row_number": index,
                "event_key": context.event_key,
                "match_name": context.match_name,
                "home_team_name": context.home_team_name,
                "away_team_name": context.away_team_name,
                "start_time_utc": context.start_time_utc,
                "round_label": context.round_label,
                "venue": context.venue,
                "event_status": context.status,
            }
            for index, context in enumerate(fixture_index.values(), start=1)
        ]
        conn.execute("BEGIN TRANSACTION")
        try:
            conn.execute("DELETE FROM normalized_fixture_sources WHERE source_path = ?", [str(path)])
            if rows:
                self._register_frame(conn, "_fixture_source_batch", rows)
                try:
                    conn.execute(
                        "INSERT INTO normalized_fixture_sources BY NAME SELECT * FROM _fixture_source_batch"
                    )
                finally:
                    conn.unregister("_fixture_source_batch")
            file_id = self._record_imported_file(
                conn=conn,
                import_run_id=import_run_id,
                path=path,
                file_kind="fixture",
                bookmaker_code="system",
                rows_read=len(rows),
                rows_loaded=len(rows),
                status="imported",
                error_text=None,
                observed_at=observed_at,
            )
            self._record_artifact(
                conn,
                path=path,
                file_kind="fixture",
                dependency=dependency,
                importer_version=FIXTURE_IMPORTER_VERSION,
                imported_file_id=file_id,
                rows_read=len(rows),
                rows_loaded=len(rows),
                observed_at=observed_at,
            )
            conn.commit()
        except Exception:
            conn.rollback()
            raise

    def _replace_odds_source(
        self,
        conn: Any,
        path: Path,
        spec: ManifestSpec,
        fixture_index: dict[tuple[str, str], EventContext],
        import_run_id: int,
        observed_at: datetime,
        dependency: str,
    ) -> None:
        source_rows = self._read_csv_rows(path)
        self._validate_columns(spec, source_rows)
        records = []
        for source_row in source_rows:
            records.extend(
                self._normalize_row(
                    spec.file_kind,
                    source_row,
                    resolve_event_context(source_row, fixture_index),
                )
            )
        rows: list[dict[str, Any]] = []
        for index, record in enumerate(records, start=1):
            row = asdict(record)
            row["source_path"] = str(path)
            row["source_row_number"] = index
            row["event_payload_meta_json"] = stable_json_dumps(row.pop("event_payload_meta"))
            row["selection_payload_meta_json"] = stable_json_dumps(
                row.pop("selection_payload_meta")
            )
            rows.append(row)

        conn.execute("BEGIN TRANSACTION")
        try:
            conn.execute(
                """
                DELETE FROM outcome_prices
                WHERE source_file_id IN (
                  SELECT imported_file_id FROM imported_files WHERE source_path = ?
                )
                """,
                [str(path)],
            )
            conn.execute("DELETE FROM normalized_odds_sources WHERE source_path = ?", [str(path)])
            if rows:
                self._register_frame(conn, "_odds_source_batch", rows)
                try:
                    conn.execute(
                        "INSERT INTO normalized_odds_sources BY NAME SELECT * FROM _odds_source_batch"
                    )
                finally:
                    conn.unregister("_odds_source_batch")
            file_id = self._record_imported_file(
                conn=conn,
                import_run_id=import_run_id,
                path=path,
                file_kind=spec.file_kind,
                bookmaker_code=self._bookmaker_from_path(
                    path, source_rows[0] if source_rows else None
                ),
                rows_read=len(source_rows),
                rows_loaded=len(rows),
                status="imported",
                error_text=None,
                observed_at=observed_at,
            )
            self._record_artifact(
                conn,
                path=path,
                file_kind="odds",
                dependency=dependency,
                importer_version=ODDS_IMPORTER_VERSION,
                imported_file_id=file_id,
                rows_read=len(source_rows),
                rows_loaded=len(rows),
                observed_at=observed_at,
            )
            conn.commit()
        except Exception:
            conn.rollback()
            raise

    def _replace_metric_source(
        self,
        conn: Any,
        path: Path,
        fixture_index: dict[tuple[str, str], EventContext],
        import_run_id: int,
        observed_at: datetime,
        dependency: str,
    ) -> None:
        source_rows = self._read_rds_rows(path)
        metrics = []
        for source_row in source_rows:
            if not source_row.get("agency"):
                continue
            metrics.extend(
                self._build_processed_metric_records(
                    source_row, resolve_event_context(source_row, fixture_index)
                )
            )
        rows = []
        for index, metric in enumerate(metrics, start=1):
            row = asdict(metric)
            row["source_path"] = str(path)
            row["source_row_number"] = index
            rows.append(row)

        conn.execute("BEGIN TRANSACTION")
        try:
            self._affected_metric_keys.update(
                (str(selection_key), str(bookmaker_code))
                for selection_key, bookmaker_code in conn.execute(
                    """
                    SELECT selection_key, bookmaker_code
                    FROM normalized_metric_sources
                    WHERE source_path = ?
                    """,
                    [str(path)],
                ).fetchall()
            )
            conn.execute("DELETE FROM normalized_metric_sources WHERE source_path = ?", [str(path)])
            if rows:
                self._register_frame(conn, "_metric_source_batch", rows)
                try:
                    conn.execute(
                        "INSERT INTO normalized_metric_sources BY NAME SELECT * FROM _metric_source_batch"
                    )
                finally:
                    conn.unregister("_metric_source_batch")
            self._affected_metric_keys.update(
                (str(row["selection_key"]), str(row["bookmaker_code"])) for row in rows
            )
            file_id = self._record_imported_file(
                conn=conn,
                import_run_id=import_run_id,
                path=path,
                file_kind="processed_player_props",
                bookmaker_code="system",
                rows_read=len(source_rows),
                rows_loaded=len(rows),
                status="imported",
                error_text=None,
                observed_at=observed_at,
            )
            self._record_artifact(
                conn,
                path=path,
                file_kind="processed_player_props",
                dependency=dependency,
                importer_version=METRIC_IMPORTER_VERSION,
                imported_file_id=file_id,
                rows_read=len(source_rows),
                rows_loaded=len(rows),
                observed_at=observed_at,
            )
            conn.commit()
        except Exception:
            conn.rollback()
            raise

    def _replace_game_log_source(
        self,
        conn: Any,
        path: Path,
        import_run_id: int,
        observed_at: datetime,
        dependency: str,
    ) -> None:
        source_rows = self._read_rds_rows(path)
        records = [
            record
            for source_row in source_rows
            if (record := self._build_player_game_log(source_row)) is not None
        ]
        rows = []
        for index, record in enumerate(records, start=1):
            row = asdict(record)
            row["source_path"] = str(path)
            row["source_row_number"] = index
            rows.append(row)

        conn.execute("BEGIN TRANSACTION")
        try:
            self._affected_game_log_keys.update(
                str(row[0])
                for row in conn.execute(
                    """
                    SELECT game_log_key
                    FROM normalized_game_log_sources
                    WHERE source_path = ?
                    """,
                    [str(path)],
                ).fetchall()
            )
            conn.execute(
                "DELETE FROM normalized_game_log_sources WHERE source_path = ?", [str(path)]
            )
            if rows:
                self._register_frame(conn, "_game_log_source_batch", rows)
                try:
                    conn.execute(
                        "INSERT INTO normalized_game_log_sources BY NAME SELECT * FROM _game_log_source_batch"
                    )
                finally:
                    conn.unregister("_game_log_source_batch")
            self._affected_game_log_keys.update(record.game_log_key for record in records)
            file_id = self._record_imported_file(
                conn=conn,
                import_run_id=import_run_id,
                path=path,
                file_kind="player_game_logs",
                bookmaker_code="system",
                rows_read=len(source_rows),
                rows_loaded=len(rows),
                status="imported",
                error_text=None,
                observed_at=observed_at,
            )
            self._record_artifact(
                conn,
                path=path,
                file_kind="player_game_logs",
                dependency=dependency,
                importer_version=GAME_LOG_IMPORTER_VERSION,
                imported_file_id=file_id,
                rows_read=len(source_rows),
                rows_loaded=len(rows),
                observed_at=observed_at,
            )
            conn.commit()
        except Exception:
            conn.rollback()
            raise

    def _remove_missing_sources(
        self, conn: Any, current_paths: dict[str, set[str]]
    ) -> dict[str, set[str]]:
        removed: dict[str, set[str]] = {kind: set() for kind in current_paths}
        rows = conn.execute(
            "SELECT source_path, file_kind FROM source_artifacts"
        ).fetchall()
        table_by_kind = {
            "fixture": "normalized_fixture_sources",
            "odds": "normalized_odds_sources",
            "processed_player_props": "normalized_metric_sources",
            "player_game_logs": "normalized_game_log_sources",
        }
        for source_path, file_kind in rows:
            if file_kind not in current_paths or source_path in current_paths[file_kind]:
                continue
            removed[file_kind].add(source_path)
            conn.execute("BEGIN TRANSACTION")
            try:
                if file_kind == "odds":
                    conn.execute(
                        """
                        DELETE FROM outcome_prices
                        WHERE source_file_id IN (
                          SELECT imported_file_id FROM imported_files WHERE source_path = ?
                        )
                        """,
                        [source_path],
                    )
                elif file_kind == "processed_player_props":
                    self._affected_metric_keys.update(
                        (str(selection_key), str(bookmaker_code))
                        for selection_key, bookmaker_code in conn.execute(
                            """
                            SELECT selection_key, bookmaker_code
                            FROM normalized_metric_sources
                            WHERE source_path = ?
                            """,
                            [source_path],
                        ).fetchall()
                    )
                elif file_kind == "player_game_logs":
                    self._affected_game_log_keys.update(
                        str(row[0])
                        for row in conn.execute(
                            """
                            SELECT game_log_key
                            FROM normalized_game_log_sources
                            WHERE source_path = ?
                            """,
                            [source_path],
                        ).fetchall()
                    )
                conn.execute(
                    f"DELETE FROM {table_by_kind[file_kind]} WHERE source_path = ?",
                    [source_path],
                )
                conn.execute("DELETE FROM source_artifacts WHERE source_path = ?", [source_path])
                conn.commit()
            except Exception:
                conn.rollback()
                raise
        return removed

    def _materialize_odds(
        self,
        *,
        conn: Any,
        import_run_id: int,
        observed_at: datetime,
        changed_paths: set[str],
    ) -> None:
        conn.execute("BEGIN TRANSACTION")
        try:
            conn.execute(
                """
                INSERT INTO bookmakers (code, display_name)
                SELECT DISTINCT bookmaker_code, bookmaker_code
                FROM normalized_odds_sources source
                WHERE NOT EXISTS (
                  SELECT 1 FROM bookmakers current WHERE current.code = source.bookmaker_code
                )
                """
            )
            conn.execute(
                """
                INSERT INTO teams (league_code, name, normalized_name)
                SELECT ?, team_name, team_name
                FROM (
                  SELECT home_team_name AS team_name FROM normalized_fixture_sources
                  UNION
                  SELECT away_team_name FROM normalized_fixture_sources
                  UNION
                  SELECT home_team_name FROM normalized_odds_sources
                  UNION
                  SELECT away_team_name FROM normalized_odds_sources
                ) source
                WHERE NOT EXISTS (
                  SELECT 1 FROM teams current WHERE current.normalized_name = source.team_name
                )
                """,
                [LEAGUE_CODE],
            )
            conn.execute(
                """
                INSERT INTO players (full_name, normalized_name)
                SELECT DISTINCT player_name, player_name
                FROM normalized_odds_sources source
                WHERE player_name IS NOT NULL
                  AND NOT EXISTS (
                    SELECT 1 FROM players current
                    WHERE current.normalized_name = source.player_name
                  )
                """
            )
            conn.execute(
                """
                INSERT INTO events (
                  event_key, league_code, match_name, home_team_id, away_team_id,
                  start_time_utc, round_label, venue, status
                )
                SELECT event_key, ?, match_name, home.team_id, away.team_id,
                       start_time_utc, round_label, venue, event_status
                FROM (
                  SELECT *,
                    ROW_NUMBER() OVER (
                      PARTITION BY event_key ORDER BY source_priority DESC, source_path, source_row_number
                    ) AS choice
                  FROM (
                    SELECT *, 2 AS source_priority FROM normalized_fixture_sources
                    UNION ALL BY NAME
                    SELECT source_path, source_row_number, event_key, match_name,
                           home_team_name, away_team_name, start_time_utc, round_label,
                           venue, event_status, 1 AS source_priority
                    FROM normalized_odds_sources
                  ) candidates
                ) chosen
                JOIN teams home ON home.normalized_name = chosen.home_team_name
                JOIN teams away ON away.normalized_name = chosen.away_team_name
                WHERE choice = 1
                ON CONFLICT (event_key) DO UPDATE SET
                  match_name = excluded.match_name,
                  home_team_id = excluded.home_team_id,
                  away_team_id = excluded.away_team_id,
                  start_time_utc = excluded.start_time_utc,
                  round_label = excluded.round_label,
                  venue = excluded.venue,
                  status = excluded.status
                """,
                [LEAGUE_CODE],
            )
            conn.execute(
                """
                INSERT INTO markets (
                  market_key, event_id, market_type_code, market_name_raw,
                  player_id, line_value, stat_side_scope
                )
                SELECT market_key, event_id, market_type_code, market_name_raw,
                       player_id, line_value, stat_side_scope
                FROM (
                  SELECT source.*, events.event_id, players.player_id,
                    ROW_NUMBER() OVER (
                      PARTITION BY market_key ORDER BY source_path, source_row_number
                    ) AS choice
                  FROM normalized_odds_sources source
                  JOIN events USING (event_key)
                  LEFT JOIN players ON players.normalized_name = source.player_name
                ) chosen
                WHERE choice = 1
                ON CONFLICT (market_key) DO UPDATE SET
                  event_id = excluded.event_id,
                  market_type_code = excluded.market_type_code,
                  market_name_raw = excluded.market_name_raw,
                  player_id = excluded.player_id,
                  line_value = excluded.line_value,
                  stat_side_scope = excluded.stat_side_scope
                """
            )
            conn.execute(
                """
                INSERT INTO selections (
                  selection_key, market_id, selection_type, label, sort_order
                )
                SELECT selection_key, market_id, selection_type, selection_label, sort_order
                FROM (
                  SELECT source.*, markets.market_id,
                    ROW_NUMBER() OVER (
                      PARTITION BY selection_key ORDER BY source_path, source_row_number
                    ) AS choice
                  FROM normalized_odds_sources source
                  JOIN markets USING (market_key)
                ) chosen
                WHERE choice = 1
                ON CONFLICT (selection_key) DO UPDATE SET
                  market_id = excluded.market_id,
                  selection_type = excluded.selection_type,
                  label = excluded.label,
                  sort_order = excluded.sort_order
                """
            )

            conn.execute("DELETE FROM event_bookmaker_map")
            conn.execute(
                """
                INSERT INTO event_bookmaker_map
                SELECT event_id, bookmaker_id,
                       first(external_event_id ORDER BY external_event_id IS NULL,
                             CASE file_kind WHEN 'h2h' THEN 1 WHEN 'line' THEN 2
                               WHEN 'totals' THEN 3 ELSE 4 END DESC,
                             source_path DESC, source_row_number DESC),
                       first(external_competition_id ORDER BY external_competition_id IS NULL,
                             CASE file_kind WHEN 'h2h' THEN 1 WHEN 'line' THEN 2
                               WHEN 'totals' THEN 3 ELSE 4 END DESC,
                             source_path DESC, source_row_number DESC),
                       first(event_payload_meta_json ORDER BY
                             CASE file_kind WHEN 'h2h' THEN 1 WHEN 'line' THEN 2
                               WHEN 'totals' THEN 3 ELSE 4 END DESC,
                             source_path DESC, source_row_number DESC),
                       ?
                FROM normalized_odds_sources source
                JOIN events USING (event_key)
                JOIN bookmakers ON bookmakers.code = source.bookmaker_code
                GROUP BY event_id, bookmaker_id
                """,
                [observed_at],
            )
            conn.execute("DELETE FROM selection_bookmaker_meta")
            conn.execute(
                """
                INSERT INTO selection_bookmaker_meta
                SELECT selection_id, bookmaker_id,
                       first(external_market_id ORDER BY external_market_id IS NULL,
                             CASE file_kind WHEN 'h2h' THEN 1 WHEN 'line' THEN 2
                               WHEN 'totals' THEN 3 ELSE 4 END DESC,
                             source_path DESC, source_row_number DESC),
                       first(external_selection_id ORDER BY external_selection_id IS NULL,
                             CASE file_kind WHEN 'h2h' THEN 1 WHEN 'line' THEN 2
                               WHEN 'totals' THEN 3 ELSE 4 END DESC,
                             source_path DESC, source_row_number DESC),
                       first(sgm_eligible ORDER BY
                             CASE file_kind WHEN 'h2h' THEN 1 WHEN 'line' THEN 2
                               WHEN 'totals' THEN 3 ELSE 4 END DESC,
                             source_path DESC, source_row_number DESC),
                       first(selection_payload_meta_json ORDER BY
                             CASE file_kind WHEN 'h2h' THEN 1 WHEN 'line' THEN 2
                               WHEN 'totals' THEN 3 ELSE 4 END DESC,
                             source_path DESC, source_row_number DESC),
                       ?
                FROM normalized_odds_sources source
                JOIN selections USING (selection_key)
                JOIN bookmakers ON bookmakers.code = source.bookmaker_code
                GROUP BY selection_id, bookmaker_id
                """,
                [observed_at],
            )

            if changed_paths:
                changed_frame = pd.DataFrame({"source_path": sorted(changed_paths)})
                conn.register("_changed_odds_paths", changed_frame)
                try:
                    conn.execute(
                        """
                        INSERT INTO outcome_prices (
                          selection_id, bookmaker_id, import_run_id, decimal_price,
                          implied_prob, margin, observed_at, source_file_id
                        )
                        SELECT selections.selection_id, bookmakers.bookmaker_id, ?,
                               source.decimal_price, source.implied_prob, source.margin, ?,
                               artifacts.imported_file_id
                        FROM normalized_odds_sources source
                        JOIN _changed_odds_paths changed USING (source_path)
                        JOIN source_artifacts artifacts USING (source_path)
                        JOIN selections USING (selection_key)
                        JOIN bookmakers ON bookmakers.code = source.bookmaker_code
                        ORDER BY CASE source.file_kind
                                   WHEN 'h2h' THEN 1 WHEN 'line' THEN 2
                                   WHEN 'totals' THEN 3 ELSE 4
                                 END,
                                 source.source_path,
                                 source.source_row_number
                        """,
                        [import_run_id, observed_at],
                    )
                finally:
                    conn.unregister("_changed_odds_paths")

            conn.execute("DELETE FROM quote_legs")
            conn.execute("DELETE FROM quote_cache")
            conn.execute(
                """
                DELETE FROM selections
                WHERE selection_key NOT IN (
                  SELECT DISTINCT selection_key FROM normalized_odds_sources
                )
                """
            )
            conn.execute(
                """
                DELETE FROM markets
                WHERE market_key NOT IN (
                  SELECT DISTINCT market_key FROM normalized_odds_sources
                )
                """
            )
            conn.execute(
                """
                DELETE FROM events
                WHERE event_key NOT IN (
                  SELECT event_key FROM normalized_fixture_sources
                  UNION
                  SELECT event_key FROM normalized_odds_sources
                )
                """
            )
            conn.commit()
        except Exception:
            conn.rollback()
            raise

    def _materialize_metrics(
        self, conn: Any, observed_at: datetime, *, rebuild_all: bool
    ) -> None:
        conn.execute("BEGIN TRANSACTION")
        try:
            if rebuild_all:
                conn.execute(
                    "DELETE FROM selection_metrics WHERE metric_source = 'processed_odds_v1'"
                )
                affected_join = ""
            else:
                affected = pd.DataFrame.from_records(
                    sorted(self._affected_metric_keys),
                    columns=["selection_key", "bookmaker_code"],
                )
                conn.register("_affected_metric_keys", affected)
                conn.execute(
                    """
                    DELETE FROM selection_metrics
                    WHERE metric_source = 'processed_odds_v1'
                      AND (selection_id, bookmaker_id) IN (
                        SELECT selections.selection_id, bookmakers.bookmaker_id
                        FROM _affected_metric_keys affected
                        JOIN selections USING (selection_key)
                        JOIN bookmakers ON bookmakers.code = affected.bookmaker_code
                      )
                    """
                )
                affected_join = """
                    JOIN _affected_metric_keys affected
                      ON affected.selection_key = source.selection_key
                     AND affected.bookmaker_code = source.bookmaker_code
                """
            conn.execute(
                f"""
                INSERT INTO selection_metrics (
                  selection_id, bookmaker_id, metric_source, fair_prob, fair_price,
                  edge_pct, computed_at, metrics_json, source_file_id
                )
                SELECT selections.selection_id, bookmakers.bookmaker_id,
                       'processed_odds_v1', source.prob_last_10,
                       CASE WHEN source.prob_last_10 > 0
                            THEN round(1 / source.prob_last_10, 4) END,
                       source.diff_last_10, ?,
                       json_object(
                         'diff_2025', source.diff_2025,
                         'diff_last_10', source.diff_last_10,
                         'home_away_diff', source.home_away_diff,
                         'win_loss_diff', source.win_loss_diff,
                         'prob_2025', source.prob_2025,
                         'prob_last_10', source.prob_last_10,
                         'player_position', source.player_position,
                         'matchup_difficulty', source.matchup_difficulty,
                         'over_matchup_difficulty', source.over_matchup_difficulty,
                         'under_matchup_difficulty', source.under_matchup_difficulty,
                         'dvp', source.dvp,
                         'raw_dvp', source.raw_dvp,
                         'dvp_standard_error', source.dvp_standard_error,
                         'dvp_bootstrap_ci_low', source.dvp_bootstrap_ci_low,
                         'dvp_bootstrap_ci_high', source.dvp_bootstrap_ci_high,
                         'dvp_sample_count', source.dvp_sample_count,
                         'dvp_match_count', source.dvp_match_count,
                         'dvp_observation_count', source.dvp_observation_count,
                         'dvp_model_version', source.dvp_model_version,
                         'dvp_generated_at', source.dvp_generated_at,
                         'variation', source.variation
                       ),
                       artifacts.imported_file_id
                FROM normalized_metric_sources source
                {affected_join}
                JOIN source_artifacts artifacts USING (source_path)
                JOIN selections USING (selection_key)
                JOIN bookmakers ON bookmakers.code = source.bookmaker_code
                ORDER BY source.source_path, source.source_row_number
                """,
                [observed_at],
            )
            if not rebuild_all:
                conn.unregister("_affected_metric_keys")
            conn.commit()
        except Exception:
            if not rebuild_all:
                try:
                    conn.unregister("_affected_metric_keys")
                except Exception:
                    pass
            conn.rollback()
            raise

    def _materialize_game_logs(self, conn: Any, *, rebuild_all: bool) -> None:
        conn.execute("BEGIN TRANSACTION")
        try:
            conn.execute(
                """
                INSERT INTO players (full_name, normalized_name)
                SELECT DISTINCT player_name, player_name
                FROM normalized_game_log_sources source
                WHERE NOT EXISTS (
                  SELECT 1 FROM players current
                  WHERE current.normalized_name = source.player_name
                )
                """
            )
            if rebuild_all:
                conn.execute("DELETE FROM player_game_logs")
                affected_join = ""
            else:
                affected = pd.DataFrame(
                    {"game_log_key": sorted(self._affected_game_log_keys)}
                )
                conn.register("_affected_game_log_keys", affected)
                conn.execute(
                    """
                    DELETE FROM player_game_logs
                    WHERE game_log_key IN (
                      SELECT game_log_key FROM _affected_game_log_keys
                    )
                    """
                )
                affected_join = "JOIN _affected_game_log_keys USING (game_log_key)"
            conn.execute(
                f"""
                INSERT INTO player_game_logs BY NAME
                SELECT source.* EXCLUDE (
                         source_path, source_row_number, player_name, source_choice
                       ),
                       players.player_id
                FROM (
                  SELECT *,
                    ROW_NUMBER() OVER (
                      PARTITION BY game_log_key ORDER BY source_path DESC, source_row_number DESC
                    ) AS source_choice
                  FROM normalized_game_log_sources
                ) source
                {affected_join}
                JOIN players ON players.normalized_name = source.player_name
                WHERE source_choice = 1
                """
            )
            if not rebuild_all:
                conn.unregister("_affected_game_log_keys")
            conn.commit()
        except Exception:
            if not rebuild_all:
                try:
                    conn.unregister("_affected_game_log_keys")
                except Exception:
                    pass
            conn.rollback()
            raise

    def _record_source_error(
        self,
        conn: Any,
        summary: ImportSummary,
        import_run_id: int,
        path: Path,
        file_kind: str,
        exc: Exception,
    ) -> None:
        summary["error_count"] += 1
        summary["errors"].append(f"{path.name}: {exc}")
        LOGGER.exception("Failed to import %s", path)
        self._record_imported_file(
            conn=conn,
            import_run_id=import_run_id,
            path=path,
            file_kind=file_kind,
            bookmaker_code="system",
            rows_read=0,
            rows_loaded=0,
            status="failed",
            error_text=str(exc),
        )


def run_incremental_import(
    settings: Settings | None = None, *, triggered_by: str = "incremental"
) -> ImportSummary:
    return IncrementalImporter(settings or get_settings()).run(triggered_by=triggered_by)
