from __future__ import annotations

import csv
import logging
import math
from dataclasses import dataclass
from datetime import UTC, datetime
from pathlib import Path
from typing import Any, Callable, TypedDict

from app.config import Settings, get_settings
from app.db.duckdb import connection, fetch_value, initialize_database
from app.utils.hashing import sha256_file, sha256_text, stable_json_dumps
from app.utils.time import utc_now
from ingest.manifest import MANIFEST, ManifestSpec
from ingest.normalizers import (
    NormalizedSelectionRecord,
    build_market_key,
    build_selection_key,
    build_selection_label,
    clean_text,
    implied_probability,
    market_type_from_name,
    normalize_bookmaker_code,
    normalize_player_name,
    normalize_team_name,
    parse_float,
    resolve_bookmaker_meta,
    selection_sort_order,
    stat_side_scope,
)
from ingest.resolvers import EventContext, LEAGUE_CODE, load_fixture_index, resolve_event_context


LOGGER = logging.getLogger(__name__)

UNDER_MATCHUP_MAP = {
    "Terrible": "Excellent",
    "Bad": "Good",
    "Neutral": "Neutral",
    "Good": "Bad",
    "Excellent": "Terrible",
}


class ImportSummary(TypedDict):
    files_scanned: int
    files_imported: int
    error_count: int
    errors: list[str]
    import_run_id: int
    status: str


@dataclass(frozen=True, slots=True)
class ProcessedSelectionMetric:
    bookmaker_code: str
    selection_key: str
    prob_2025: float | None
    prob_last_10: float | None
    diff_2025: float | None
    diff_last_10: float | None
    variation: float | None
    player_position: str | None
    matchup_difficulty: str | None
    over_matchup_difficulty: str | None
    under_matchup_difficulty: str | None
    dvp: float | None
    raw_dvp: float | None
    dvp_standard_error: float | None
    dvp_bootstrap_ci_low: float | None
    dvp_bootstrap_ci_high: float | None
    dvp_sample_count: int | None
    dvp_match_count: int | None
    dvp_observation_count: int | None
    dvp_model_version: str | None
    dvp_generated_at: str | None


@dataclass(frozen=True, slots=True)
class DvpMatchupInfo:
    over_matchup_difficulty: str | None
    under_matchup_difficulty: str | None
    dvp: float | None
    raw_dvp: float | None
    standard_error: float | None
    bootstrap_ci_low: float | None
    bootstrap_ci_high: float | None
    sample_count: int | None
    match_count: int | None
    observation_count: int | None
    model_version: str | None
    generated_at: str | None


@dataclass(frozen=True, slots=True)
class PlayerGameLogRecord:
    game_log_key: str
    player_name: str
    source_player_id: str | None
    match_name: str
    season_name: str
    start_time_utc: datetime
    round_label: str | None
    venue: str | None
    weather_category: str | None
    weather_description: str | None
    home_team: str | None
    away_team: str | None
    player_team: str | None
    opposition_team: str | None
    home_away: str
    margin: int | None
    tog_percentage: float | None
    fantasy_points: float | None
    goals: float | None
    behinds: float | None
    disposals: float | None
    kicks: float | None
    handballs: float | None
    marks: float | None
    tackles: float | None
    hitouts: float | None
    frees_for: float | None
    frees_against: float | None
    total_clearances: float | None
    metres_gained: float | None
    goal_assists: float | None
    cba_percentage: float | None
    cbas: float | None
    kick_ins: float | None
    kick_in_percentage: float | None
    kick_ins_play_on: float | None
    kick_to_handball_ratio: float | None
    hitout_win_percentage: float | None


class Importer:
    def __init__(self, settings: Settings):
        self.settings = settings
        self.bookmaker_ids: dict[str, int] = {}
        self.team_ids: dict[str, int] = {}
        self.player_ids: dict[str, int] = {}
        self.event_ids: dict[str, int] = {}
        self.market_ids: dict[str, int] = {}
        self.selection_ids: dict[str, int] = {}
        self.player_positions_by_name = self._load_player_positions()
        self.matchup_difficulty_by_key = self._load_matchup_difficulty_map()

    def run(self, triggered_by: str = "manual") -> ImportSummary:
        initialize_database(self.settings)
        fixture_index = load_fixture_index(self.settings.fixture_path)
        summary: ImportSummary = {
            "files_scanned": 0,
            "files_imported": 0,
            "error_count": 0,
            "errors": [],
            "import_run_id": 0,
            "status": "running",
        }
        with connection(write=True, settings=self.settings, transaction=False) as conn:
            self._prepare_import_connection(conn)
            run_started_at = utc_now().replace(tzinfo=None)
            self._abandon_stale_import_runs(conn)
            import_run_id = self._start_import_run(conn, run_started_at, triggered_by)
            try:
                summary["files_scanned"] += 1
                if self.settings.fixture_path.exists() and not self._file_already_loaded(
                    conn, self.settings.fixture_path
                ):
                    self._execute_file_import(
                        conn,
                        lambda: self._import_fixture(conn, import_run_id, run_started_at, fixture_index),
                    )
                    summary["files_imported"] += 1

                for spec in MANIFEST:
                    for path in spec.iter_paths(self.settings.scraped_odds_dir):
                        summary["files_scanned"] += 1
                        if self._file_already_loaded(conn, path):
                            continue
                        try:
                            def import_odds_file(
                                current_path: Path = path,
                                current_spec: ManifestSpec = spec,
                            ) -> int:
                                return self._import_odds_file(
                                    conn=conn,
                                    import_run_id=import_run_id,
                                    observed_at=run_started_at,
                                    path=current_path,
                                    spec=current_spec,
                                    fixture_index=fixture_index,
                                )

                            loaded_rows = self._execute_file_import(
                                conn,
                                import_odds_file,
                            )
                            summary["files_imported"] += 1
                            LOGGER.info("Imported %s rows from %s", loaded_rows, path.name)
                        except Exception as exc:  # pragma: no cover - defensive logging path
                            summary["error_count"] += 1
                            summary["errors"].append(f"{path.name}: {exc}")
                            LOGGER.exception("Failed to import %s", path)
                            self._record_imported_file(
                                conn=conn,
                                import_run_id=import_run_id,
                                path=path,
                                file_kind=spec.file_kind,
                                bookmaker_code=self._bookmaker_from_path(path),
                                rows_read=0,
                                rows_loaded=0,
                                status="failed",
                                error_text=str(exc),
                            )

                for path in sorted(self.settings.processed_odds_dir.glob("all_player_*.rds")):
                    summary["files_scanned"] += 1
                    if self._file_already_loaded(conn, path):
                        continue
                    try:
                        def import_processed_file(current_path: Path = path) -> int:
                            return self._import_processed_player_file(
                                conn=conn,
                                import_run_id=import_run_id,
                                observed_at=run_started_at,
                                path=current_path,
                                fixture_index=fixture_index,
                            )

                        loaded_rows = self._execute_file_import(
                            conn,
                            import_processed_file,
                        )
                        summary["files_imported"] += 1
                        LOGGER.info("Imported %s processed metric rows from %s", loaded_rows, path.name)
                    except Exception as exc:  # pragma: no cover - defensive logging path
                        summary["error_count"] += 1
                        summary["errors"].append(f"{path.name}: {exc}")
                        LOGGER.exception("Failed to import processed odds %s", path)
                        self._record_imported_file(
                            conn=conn,
                            import_run_id=import_run_id,
                            path=path,
                            file_kind="processed_player_props",
                            bookmaker_code="system",
                            rows_read=0,
                            rows_loaded=0,
                            status="failed",
                            error_text=str(exc),
                        )

                for path in sorted(self.settings.fixture_path.parent.glob("afl_fantasy*_data.rds")):
                    summary["files_scanned"] += 1
                    if self._file_already_loaded(conn, path):
                        continue
                    try:
                        loaded_rows = self._import_player_stats_file(
                            conn=conn,
                            import_run_id=import_run_id,
                            observed_at=run_started_at,
                            path=path,
                        )
                        summary["files_imported"] += 1
                        LOGGER.info("Imported %s player game log rows from %s", loaded_rows, path.name)
                    except Exception as exc:  # pragma: no cover - defensive logging path
                        summary["error_count"] += 1
                        summary["errors"].append(f"{path.name}: {exc}")
                        LOGGER.exception("Failed to import player stats %s", path)
                        self._record_imported_file(
                            conn=conn,
                            import_run_id=import_run_id,
                            path=path,
                            file_kind="player_game_logs",
                            bookmaker_code="system",
                            rows_read=0,
                            rows_loaded=0,
                            status="failed",
                            error_text=str(exc),
                        )

                status = "completed_with_errors" if summary["error_count"] else "completed"
                self._finish_import_run(conn, import_run_id, status, summary)
                summary["import_run_id"] = import_run_id
                summary["status"] = status
                return summary
            except Exception:
                self._finish_import_run(
                    conn,
                    import_run_id,
                    "failed",
                    {
                        "files_scanned": summary["files_scanned"],
                        "files_imported": summary["files_imported"],
                        "error_count": summary["error_count"] + 1,
                        "errors": summary["errors"],
                        "import_run_id": import_run_id,
                        "status": "failed",
                    },
                )
                raise

    def _prepare_import_connection(self, conn: Any) -> None:
        conn.execute("SET preserve_insertion_order = false")
        conn.execute("SET threads = 1")

    def _abandon_stale_import_runs(self, conn: Any) -> None:
        conn.execute(
            """
            UPDATE import_runs
            SET finished_at = ?, status = 'abandoned', notes = COALESCE(notes || '\n', '') || 'Marked abandoned on next importer startup.'
            WHERE status = 'running'
            """,
            [utc_now().replace(tzinfo=None)],
        )

    def _execute_file_import(self, conn: Any, operation: Callable[[], int]) -> int:
        conn.execute("BEGIN TRANSACTION")
        try:
            rows_loaded = operation()
        except Exception:
            conn.rollback()
            raise
        conn.commit()
        return rows_loaded

    def _start_import_run(
        self, conn: Any, started_at: datetime, triggered_by: str
    ) -> int:
        conn.execute(
            """
            INSERT INTO import_runs (started_at, status, triggered_by)
            VALUES (?, 'running', ?)
            """,
            [started_at, triggered_by],
        )
        import_run_id = fetch_value(conn, "SELECT MAX(import_run_id) FROM import_runs")
        if import_run_id is None:
            raise RuntimeError("Failed to create import run.")
        return int(import_run_id)

    def _finish_import_run(self, conn: Any, import_run_id: int, status: str, summary: ImportSummary) -> None:
        conn.execute(
            """
            UPDATE import_runs
            SET finished_at = ?, status = ?, files_scanned = ?, files_imported = ?, error_count = ?, notes = ?
            WHERE import_run_id = ?
            """,
            [
                utc_now().replace(tzinfo=None),
                status,
                summary["files_scanned"],
                summary["files_imported"],
                summary["error_count"],
                "\n".join(summary["errors"]) if summary["errors"] else None,
                import_run_id,
            ],
        )

    def _file_already_loaded(self, conn: Any, path: Path) -> bool:
        sha = sha256_file(path)
        existing = fetch_value(
            conn,
            """
            SELECT 1
            FROM imported_files
            WHERE source_path = ? AND sha256 = ? AND status = 'imported'
            LIMIT 1
            """,
            [str(path), sha],
        )
        return bool(existing)

    def _import_fixture(
        self,
        conn: Any,
        import_run_id: int,
        observed_at: datetime,
        fixture_index: dict[tuple[str, str], EventContext],
    ) -> int:
        rows_loaded = 0
        for context in fixture_index.values():
            home_team_id = self._upsert_team(conn, context.home_team_name)
            away_team_id = self._upsert_team(conn, context.away_team_name)
            self._upsert_event(
                conn=conn,
                event_key=context.event_key,
                match_name=context.match_name,
                home_team_id=home_team_id,
                away_team_id=away_team_id,
                start_time_utc=context.start_time_utc,
                round_label=context.round_label,
                venue=context.venue,
                status=context.status,
            )
            rows_loaded += 1
        self._record_imported_file(
            conn=conn,
            import_run_id=import_run_id,
            path=self.settings.fixture_path,
            file_kind="fixture",
            bookmaker_code="system",
            rows_read=len(fixture_index),
            rows_loaded=rows_loaded,
            status="imported",
            error_text=None,
            observed_at=observed_at,
        )
        return rows_loaded

    def _import_odds_file(
        self,
        *,
        conn: Any,
        import_run_id: int,
        observed_at: datetime,
        path: Path,
        spec: ManifestSpec,
        fixture_index: dict[tuple[str, str], EventContext],
    ) -> int:
        rows = self._read_csv_rows(path)
        self._validate_columns(spec, rows)
        normalized_records: list[NormalizedSelectionRecord] = []
        for row in rows:
            event_context = resolve_event_context(row, fixture_index)
            normalized_records.extend(
                self._normalize_row(spec.file_kind, row, event_context)
            )

        source_file_id = self._record_imported_file(
            conn=conn,
            import_run_id=import_run_id,
            path=path,
            file_kind=spec.file_kind,
            bookmaker_code=self._bookmaker_from_path(path, rows[0] if rows else None),
            rows_read=len(rows),
            rows_loaded=len(normalized_records),
            status="imported",
            error_text=None,
            observed_at=observed_at,
        )

        for record in normalized_records:
            self._upsert_normalized_record(
                conn=conn,
                import_run_id=import_run_id,
                source_file_id=source_file_id,
                observed_at=observed_at,
                record=record,
            )
        return len(normalized_records)

    def _read_csv_rows(self, path: Path) -> list[dict[str, str]]:
        with path.open("r", encoding="utf-8-sig", newline="") as handle:
            return list(csv.DictReader(handle))

    def _read_rds_rows(self, path: Path) -> list[dict[str, str]]:
        import pyreadr  # type: ignore[import-untyped]

        result = pyreadr.read_r(str(path))
        if not result:
            return []
        frame = next(iter(result.values()))
        if frame is None:
            return []
        return [self._coerce_rds_row(row) for row in frame.to_dict(orient="records")]

    def _coerce_rds_row(self, row: dict[str, Any]) -> dict[str, str]:
        coerced: dict[str, str] = {}
        for key, value in row.items():
            if value is None:
                coerced[key] = ""
                continue
            if isinstance(value, float) and math.isnan(value):
                coerced[key] = ""
                continue
            text = str(value)
            coerced[key] = "" if text.lower() in {"nan", "na", "<na>", "nat"} else text
        return coerced

    def _validate_columns(self, spec: ManifestSpec, rows: list[dict[str, str]]) -> None:
        if not rows:
            return
        columns = set(rows[0].keys())
        missing = [column for column in spec.required_columns if column not in columns]
        if missing:
            raise ValueError(f"Missing required columns: {', '.join(missing)}")

    def _normalize_row(
        self, file_kind: str, row: dict[str, str], event_context: EventContext
    ) -> list[NormalizedSelectionRecord]:
        bookmaker_code = normalize_bookmaker_code(row.get("agency") or self._bookmaker_from_path(None, row))
        market_name_raw = clean_text(row.get("market_name"))
        if not market_name_raw:
            raise ValueError("market_name is missing")
        market_type_code = market_type_from_name(market_name_raw)
        scope = stat_side_scope(market_type_code)

        if file_kind == "h2h":
            return self._build_h2h_records(row, event_context, bookmaker_code, market_name_raw, market_type_code, scope)
        if file_kind == "line":
            return self._build_line_records(row, event_context, bookmaker_code, market_name_raw, market_type_code, scope)
        if file_kind == "totals":
            return self._build_totals_records(row, event_context, bookmaker_code, market_name_raw, market_type_code, scope)
        if file_kind == "player_props":
            return self._build_player_prop_records(row, event_context, bookmaker_code, market_name_raw, market_type_code, scope)
        raise ValueError(f"Unsupported file kind: {file_kind}")

    def _import_processed_player_file(
        self,
        *,
        conn: Any,
        import_run_id: int,
        observed_at: datetime,
        path: Path,
        fixture_index: dict[tuple[str, str], EventContext],
    ) -> int:
        rows = self._read_rds_rows(path)
        processed_metrics: list[ProcessedSelectionMetric] = []
        for row in rows:
            if not clean_text(row.get("agency")):
                continue
            event_context = resolve_event_context(row, fixture_index)
            processed_metrics.extend(self._build_processed_metric_records(row, event_context))

        self._record_imported_file(
            conn=conn,
            import_run_id=import_run_id,
            path=path,
            file_kind="processed_player_props",
            bookmaker_code="system",
            rows_read=len(rows),
            rows_loaded=len(processed_metrics),
            status="imported",
            error_text=None,
            observed_at=observed_at,
        )

        for metric in processed_metrics:
            self._upsert_processed_metric(
                conn=conn,
                metric=metric,
                observed_at=observed_at,
            )
        return len(processed_metrics)

    def _build_processed_metric_records(
        self,
        row: dict[str, str],
        event_context: EventContext,
    ) -> list[ProcessedSelectionMetric]:
        bookmaker_code = normalize_bookmaker_code(row.get("agency"))
        market_name_raw = clean_text(row.get("market_name"))
        player_name = normalize_player_name(row.get("player_name"))
        if not market_name_raw or not player_name:
            return []
        market_type_code = market_type_from_name(market_name_raw)
        if not market_type_code.startswith("player_"):
            return []
        player_position = self.player_positions_by_name.get(player_name)
        matchup_info = self._resolve_matchup_info(
            opposition_team=row.get("opposition_team"),
            player_position=player_position,
            market_name_raw=market_name_raw,
        )
        line_value = parse_float(row.get("line"))
        market_key = build_market_key(event_context.event_key, market_type_code, player_name, line_value)
        variation = parse_float(row.get("variation"))

        metric_specs = (
            (
                "over",
                "over_price",
                "empirical_prob_over_2025",
                "emp_prob_last_10",
                "diff_over_2025",
                "diff_over_last_10",
            ),
            (
                "under",
                "under_price",
                "empirical_prob_under_2025",
                "empirical_prop_under_last_10",
                "diff_under_2025",
                "diff_under_last_10",
            ),
        )
        metrics: list[ProcessedSelectionMetric] = []
        for (
            selection_type,
            price_column,
            prob_2025_column,
            prob_last_10_column,
            diff_2025_column,
            diff_last_10_column,
        ) in metric_specs:
            if parse_float(row.get(price_column)) is None:
                continue
            matchup_difficulty = None
            if matchup_info is not None:
                matchup_difficulty = (
                    matchup_info.under_matchup_difficulty
                    if selection_type == "under"
                    else matchup_info.over_matchup_difficulty
                )
            metrics.append(
                ProcessedSelectionMetric(
                    bookmaker_code=bookmaker_code,
                    selection_key=build_selection_key(market_key, selection_type),
                    prob_2025=parse_float(row.get(prob_2025_column)),
                    prob_last_10=parse_float(row.get(prob_last_10_column)),
                    diff_2025=parse_float(row.get(diff_2025_column)),
                    diff_last_10=parse_float(row.get(diff_last_10_column)),
                    variation=variation,
                    player_position=player_position,
                    matchup_difficulty=matchup_difficulty,
                    over_matchup_difficulty=(
                        matchup_info.over_matchup_difficulty if matchup_info else None
                    ),
                    under_matchup_difficulty=(
                        matchup_info.under_matchup_difficulty if matchup_info else None
                    ),
                    dvp=matchup_info.dvp if matchup_info else None,
                    raw_dvp=matchup_info.raw_dvp if matchup_info else None,
                    dvp_standard_error=matchup_info.standard_error if matchup_info else None,
                    dvp_bootstrap_ci_low=matchup_info.bootstrap_ci_low if matchup_info else None,
                    dvp_bootstrap_ci_high=matchup_info.bootstrap_ci_high if matchup_info else None,
                    dvp_sample_count=matchup_info.sample_count if matchup_info else None,
                    dvp_match_count=matchup_info.match_count if matchup_info else None,
                    dvp_observation_count=matchup_info.observation_count if matchup_info else None,
                    dvp_model_version=matchup_info.model_version if matchup_info else None,
                    dvp_generated_at=matchup_info.generated_at if matchup_info else None,
                )
            )
        return metrics

    def _load_player_positions(self) -> dict[str, str]:
        path = self.settings.player_positions_path
        if not path.exists():
            LOGGER.warning("Player positions file not found: %s", path)
            return {}

        positions: dict[str, str] = {}
        with path.open(newline="", encoding="utf-8") as handle:
            for row in csv.DictReader(handle):
                player_name = normalize_player_name(row.get("player_full_name"))
                position = clean_text(row.get("position"))
                if player_name and position:
                    positions[player_name] = position
        return positions

    def _load_matchup_difficulty_map(self) -> dict[tuple[str, str, str], DvpMatchupInfo]:
        path = self.settings.dvp_data_path
        if not path.exists():
            LOGGER.warning("DVP file not found: %s", path)
            return {}

        rows_by_market: dict[str, list[dict[str, Any]]] = {}
        with path.open(newline="", encoding="utf-8") as handle:
            for row in csv.DictReader(handle):
                market_name = clean_text(row.get("market_name"))
                position = clean_text(row.get("Pos"))
                opposition_team = normalize_team_name(row.get("Opponent"))
                dvp_value = parse_float(row.get("dvp"))
                if not market_name or not position or not opposition_team:
                    continue
                rows_by_market.setdefault(market_name, []).append(
                    {
                        "market_name": market_name,
                        "position": position,
                        "opposition_team": opposition_team,
                        "dvp": dvp_value,
                        "raw_dvp": parse_float(row.get("raw_dvp")),
                        "over_matchup_difficulty": clean_text(
                            row.get("over_matchup_difficulty") or row.get("matchup_difficulty")
                        ),
                        "under_matchup_difficulty": clean_text(row.get("under_matchup_difficulty")),
                        "standard_error": parse_float(row.get("standard_error")),
                        "bootstrap_ci_low": parse_float(row.get("bootstrap_ci_low")),
                        "bootstrap_ci_high": parse_float(row.get("bootstrap_ci_high")),
                        "sample_count": _parse_optional_int(row.get("sample_count")),
                        "match_count": _parse_optional_int(row.get("match_count") or row.get("games")),
                        "observation_count": _parse_optional_int(row.get("observation_count")),
                        "model_version": clean_text(row.get("model_version")),
                        "generated_at": clean_text(row.get("generated_at")),
                    }
                )

        matchup_map: dict[tuple[str, str, str], DvpMatchupInfo] = {}
        for market_name, rows in rows_by_market.items():
            threshold_values = [
                float(row["dvp"]) for row in rows if row.get("dvp") is not None
            ]
            thresholds = None
            if threshold_values:
                sorted_values = sorted(threshold_values)
                thresholds = [
                    _percentile(sorted_values, percentile)
                    for percentile in (0.2, 0.4, 0.6, 0.8)
                ]
            for row in rows:
                opposition_team = str(row["opposition_team"])
                position = str(row["position"])
                over_matchup = clean_text(row.get("over_matchup_difficulty"))
                if not over_matchup and row.get("dvp") is not None and thresholds is not None:
                    over_matchup = _label_from_thresholds(float(row["dvp"]), thresholds)
                over_matchup = over_matchup or "Neutral"
                under_matchup = (
                    clean_text(row.get("under_matchup_difficulty"))
                    or UNDER_MATCHUP_MAP.get(over_matchup)
                    or "Neutral"
                )
                matchup_map[(opposition_team, position, market_name)] = DvpMatchupInfo(
                    over_matchup_difficulty=over_matchup,
                    under_matchup_difficulty=under_matchup,
                    dvp=row.get("dvp"),
                    raw_dvp=row.get("raw_dvp"),
                    standard_error=row.get("standard_error"),
                    bootstrap_ci_low=row.get("bootstrap_ci_low"),
                    bootstrap_ci_high=row.get("bootstrap_ci_high"),
                    sample_count=row.get("sample_count"),
                    match_count=row.get("match_count"),
                    observation_count=row.get("observation_count"),
                    model_version=row.get("model_version"),
                    generated_at=row.get("generated_at"),
                )
        return matchup_map

    def _resolve_matchup_info(
        self,
        *,
        opposition_team: str | None,
        player_position: str | None,
        market_name_raw: str,
    ) -> DvpMatchupInfo | None:
        normalized_opposition = normalize_team_name(opposition_team)
        if not normalized_opposition or not player_position:
            return None
        return self.matchup_difficulty_by_key.get((normalized_opposition, player_position, market_name_raw))

    def _import_player_stats_file(
        self,
        *,
        conn: Any,
        import_run_id: int,
        observed_at: datetime,
        path: Path,
    ) -> int:
        rows = self._read_rds_rows(path)
        rows_loaded = 0
        batch: list[PlayerGameLogRecord] = []
        batch_size = 500

        for row in rows:
            record = self._build_player_game_log(row)
            if record is None:
                continue
            batch.append(record)
            if len(batch) >= batch_size:
                self._upsert_player_game_log_batch(conn=conn, batch=batch)
                rows_loaded += len(batch)
                batch.clear()

        if batch:
            self._upsert_player_game_log_batch(conn=conn, batch=batch)
            rows_loaded += len(batch)

        self._execute_file_import(
            conn,
            lambda: self._record_imported_file(
                conn=conn,
                import_run_id=import_run_id,
                path=path,
                file_kind="player_game_logs",
                bookmaker_code="system",
                rows_read=len(rows),
                rows_loaded=rows_loaded,
                status="imported",
                error_text=None,
                observed_at=observed_at,
            ),
        )
        return rows_loaded

    def _upsert_player_game_log_batch(self, *, conn: Any, batch: list[PlayerGameLogRecord]) -> None:
        self._execute_file_import(
            conn,
            lambda: self._write_player_game_log_batch(conn=conn, batch=batch),
        )

    def _write_player_game_log_batch(self, *, conn: Any, batch: list[PlayerGameLogRecord]) -> int:
        for record in batch:
            self._upsert_player_game_log(conn=conn, record=record)
        return len(batch)

    def _build_player_game_log(self, row: dict[str, str]) -> PlayerGameLogRecord | None:
        player_name = normalize_player_name(row.get("player_full_name"))
        start_time_utc = self._parse_rds_datetime(row.get("start_time_utc"))
        match_name = clean_text(row.get("match")) or clean_text(row.get("match_name"))
        season_name = clean_text(row.get("season_name"))
        if not player_name or start_time_utc is None or not match_name or not season_name:
            return None

        player_team = clean_text(row.get("player_team"))
        home_team = clean_text(row.get("home_team"))
        away_team = clean_text(row.get("away_team"))
        opposition_team = clean_text(row.get("opposition_team"))
        home_away = "Home" if player_team and home_team and player_team == home_team else "Away"
        margin_value = parse_float(row.get("margin"))
        signed_margin = self._signed_margin(
            margin_value=margin_value,
            match_result=clean_text(row.get("match_result")),
            home_away=home_away,
        )
        source_player_id = clean_text(row.get("player_id"))
        game_log_key = sha256_text(
            f"{source_player_id or player_name}|{match_name}|{start_time_utc.isoformat()}"
        )

        return PlayerGameLogRecord(
            game_log_key=game_log_key,
            player_name=player_name,
            source_player_id=source_player_id,
            match_name=match_name,
            season_name=season_name,
            start_time_utc=start_time_utc.replace(tzinfo=None),
            round_label=clean_text(row.get("round")),
            venue=clean_text(row.get("venue")),
            weather_category=clean_text(row.get("weather_category")),
            weather_description=clean_text(row.get("weather_description")),
            home_team=home_team,
            away_team=away_team,
            player_team=player_team,
            opposition_team=opposition_team,
            home_away=home_away,
            margin=signed_margin,
            tog_percentage=parse_float(row.get("tog_percentage")),
            fantasy_points=parse_float(row.get("fantasy_points")),
            goals=parse_float(row.get("goals")),
            behinds=parse_float(row.get("behinds")),
            disposals=parse_float(row.get("disposals")),
            kicks=parse_float(row.get("kicks")),
            handballs=parse_float(row.get("handballs")),
            marks=parse_float(row.get("marks")),
            tackles=parse_float(row.get("tackles")),
            hitouts=parse_float(row.get("hitouts")),
            frees_for=parse_float(row.get("frees_for")),
            frees_against=parse_float(row.get("frees_against")),
            total_clearances=parse_float(row.get("total_clearances")),
            metres_gained=parse_float(row.get("metres_gained")),
            goal_assists=parse_float(row.get("goal_assists")),
            cba_percentage=parse_float(row.get("cba_percentage")),
            cbas=parse_float(row.get("cbas")),
            kick_ins=parse_float(row.get("kick_ins")),
            kick_in_percentage=parse_float(row.get("kick_in_percentage")),
            kick_ins_play_on=parse_float(row.get("kick_ins_play_on")),
            kick_to_handball_ratio=parse_float(row.get("kick_to_handball_ratio")),
            hitout_win_percentage=parse_float(row.get("hitout_win_percentage")),
        )

    def _parse_rds_datetime(self, value: str | None) -> datetime | None:
        cleaned = clean_text(value)
        if cleaned is None:
            return None
        normalized = cleaned.replace(" ", "T")
        parsed = datetime.fromisoformat(normalized)
        if parsed.tzinfo is None:
            return parsed.replace(tzinfo=UTC)
        return parsed.astimezone(UTC)

    def _signed_margin(
        self,
        *,
        margin_value: float | None,
        match_result: str | None,
        home_away: str,
    ) -> int | None:
        if margin_value is None:
            return None
        signed = int(round(margin_value))
        if match_result == "Away Win" and home_away == "Home":
            return -signed
        if match_result == "Home Win" and home_away == "Away":
            return -signed
        return signed

    def _build_h2h_records(
        self,
        row: dict[str, str],
        event_context: EventContext,
        bookmaker_code: str,
        market_name_raw: str,
        market_type_code: str,
        scope: str,
    ) -> list[NormalizedSelectionRecord]:
        market_key = build_market_key(event_context.event_key, market_type_code, None, None)
        margin = parse_float(row.get("margin"))
        records: list[NormalizedSelectionRecord] = []
        for selection_type, price_value in (("home", row.get("home_win")), ("away", row.get("away_win"))):
            decimal_price = parse_float(price_value)
            if decimal_price is None:
                continue
            selection_key = build_selection_key(market_key, selection_type)
            selection_label = build_selection_label(
                market_type_code=market_type_code,
                selection_type=selection_type,
                line_value=None,
                player_name=None,
                home_team_name=event_context.home_team_name,
                away_team_name=event_context.away_team_name,
            )
            records.append(
                NormalizedSelectionRecord(
                    bookmaker_code=bookmaker_code,
                    file_kind="h2h",
                    event_key=event_context.event_key,
                    match_name=event_context.match_name,
                    home_team_name=event_context.home_team_name,
                    away_team_name=event_context.away_team_name,
                    start_time_utc=event_context.start_time_utc,
                    round_label=event_context.round_label,
                    venue=event_context.venue,
                    event_status=event_context.status,
                    external_event_id=None,
                    external_competition_id=None,
                    event_payload_meta={},
                    market_key=market_key,
                    market_type_code=market_type_code,
                    market_name_raw=market_name_raw,
                    player_name=None,
                    line_value=None,
                    stat_side_scope=scope,
                    selection_key=selection_key,
                    selection_type=selection_type,
                    selection_label=selection_label,
                    sort_order=selection_sort_order(selection_type),
                    external_market_id=None,
                    external_selection_id=None,
                    selection_payload_meta={"bookmaker_code": bookmaker_code, "selection_type": selection_type},
                    sgm_eligible=False,
                    decimal_price=decimal_price,
                    implied_prob=implied_probability(decimal_price),
                    margin=margin,
                )
            )
        return records

    def _build_line_records(
        self,
        row: dict[str, str],
        event_context: EventContext,
        bookmaker_code: str,
        market_name_raw: str,
        market_type_code: str,
        scope: str,
    ) -> list[NormalizedSelectionRecord]:
        home_line = parse_float(row.get("home_line"))
        away_line = parse_float(row.get("away_line"))
        market_key = build_market_key(event_context.event_key, market_type_code, None, home_line)
        margin = parse_float(row.get("margin"))
        line_values = {"home": home_line, "away": away_line}
        prices = {"home": row.get("home_win"), "away": row.get("away_win")}
        records: list[NormalizedSelectionRecord] = []
        for selection_type in ("home", "away"):
            decimal_price = parse_float(prices[selection_type])
            if decimal_price is None:
                continue
            selection_key = build_selection_key(market_key, selection_type)
            selection_label = build_selection_label(
                market_type_code=market_type_code,
                selection_type=selection_type,
                line_value=line_values[selection_type],
                player_name=None,
                home_team_name=event_context.home_team_name,
                away_team_name=event_context.away_team_name,
            )
            records.append(
                NormalizedSelectionRecord(
                    bookmaker_code=bookmaker_code,
                    file_kind="line",
                    event_key=event_context.event_key,
                    match_name=event_context.match_name,
                    home_team_name=event_context.home_team_name,
                    away_team_name=event_context.away_team_name,
                    start_time_utc=event_context.start_time_utc,
                    round_label=event_context.round_label,
                    venue=event_context.venue,
                    event_status=event_context.status,
                    external_event_id=None,
                    external_competition_id=None,
                    event_payload_meta={},
                    market_key=market_key,
                    market_type_code=market_type_code,
                    market_name_raw=market_name_raw,
                    player_name=None,
                    line_value=home_line,
                    stat_side_scope=scope,
                    selection_key=selection_key,
                    selection_type=selection_type,
                    selection_label=selection_label,
                    sort_order=selection_sort_order(selection_type),
                    external_market_id=None,
                    external_selection_id=None,
                    selection_payload_meta={"bookmaker_code": bookmaker_code, "selection_type": selection_type},
                    sgm_eligible=False,
                    decimal_price=decimal_price,
                    implied_prob=implied_probability(decimal_price),
                    margin=margin,
                )
            )
        return records

    def _build_totals_records(
        self,
        row: dict[str, str],
        event_context: EventContext,
        bookmaker_code: str,
        market_name_raw: str,
        market_type_code: str,
        scope: str,
    ) -> list[NormalizedSelectionRecord]:
        line_value = parse_float(row.get("line"))
        market_key = build_market_key(event_context.event_key, market_type_code, None, line_value)
        margin = parse_float(row.get("margin"))
        records: list[NormalizedSelectionRecord] = []
        for selection_type, price_value in (("over", row.get("over_price")), ("under", row.get("under_price"))):
            decimal_price = parse_float(price_value)
            if decimal_price is None:
                continue
            selection_key = build_selection_key(market_key, selection_type)
            selection_label = build_selection_label(
                market_type_code=market_type_code,
                selection_type=selection_type,
                line_value=line_value,
                player_name=None,
                home_team_name=event_context.home_team_name,
                away_team_name=event_context.away_team_name,
            )
            records.append(
                NormalizedSelectionRecord(
                    bookmaker_code=bookmaker_code,
                    file_kind="totals",
                    event_key=event_context.event_key,
                    match_name=event_context.match_name,
                    home_team_name=event_context.home_team_name,
                    away_team_name=event_context.away_team_name,
                    start_time_utc=event_context.start_time_utc,
                    round_label=event_context.round_label,
                    venue=event_context.venue,
                    event_status=event_context.status,
                    external_event_id=None,
                    external_competition_id=None,
                    event_payload_meta={},
                    market_key=market_key,
                    market_type_code=market_type_code,
                    market_name_raw=market_name_raw,
                    player_name=None,
                    line_value=line_value,
                    stat_side_scope=scope,
                    selection_key=selection_key,
                    selection_type=selection_type,
                    selection_label=selection_label,
                    sort_order=selection_sort_order(selection_type),
                    external_market_id=None,
                    external_selection_id=None,
                    selection_payload_meta={"bookmaker_code": bookmaker_code, "selection_type": selection_type},
                    sgm_eligible=False,
                    decimal_price=decimal_price,
                    implied_prob=implied_probability(decimal_price),
                    margin=margin,
                )
            )
        return records

    def _build_player_prop_records(
        self,
        row: dict[str, str],
        event_context: EventContext,
        bookmaker_code: str,
        market_name_raw: str,
        market_type_code: str,
        scope: str,
    ) -> list[NormalizedSelectionRecord]:
        player_name = normalize_player_name(row.get("player_name"))
        line_value = parse_float(row.get("line"))
        if not player_name:
            raise ValueError("player_name is missing")
        market_key = build_market_key(event_context.event_key, market_type_code, player_name, line_value)
        margin = parse_float(row.get("margin"))
        records: list[NormalizedSelectionRecord] = []
        for selection_type, price_value in (("over", row.get("over_price")), ("under", row.get("under_price"))):
            decimal_price = parse_float(price_value)
            if decimal_price is None:
                continue
            (
                external_event_id,
                external_competition_id,
                external_market_id,
                external_selection_id,
                payload_meta,
                sgm_eligible,
            ) = resolve_bookmaker_meta(bookmaker_code, "player_props", row, selection_type)
            selection_key = build_selection_key(market_key, selection_type)
            selection_label = build_selection_label(
                market_type_code=market_type_code,
                selection_type=selection_type,
                line_value=line_value,
                player_name=player_name,
                home_team_name=event_context.home_team_name,
                away_team_name=event_context.away_team_name,
            )
            records.append(
                NormalizedSelectionRecord(
                    bookmaker_code=bookmaker_code,
                    file_kind="player_props",
                    event_key=event_context.event_key,
                    match_name=event_context.match_name,
                    home_team_name=event_context.home_team_name,
                    away_team_name=event_context.away_team_name,
                    start_time_utc=event_context.start_time_utc,
                    round_label=event_context.round_label,
                    venue=event_context.venue,
                    event_status=event_context.status,
                    external_event_id=external_event_id,
                    external_competition_id=external_competition_id,
                    event_payload_meta={
                        key: value
                        for key, value in payload_meta.items()
                        if key in {"external_event_id", "external_competition_id", "class_external_id"}
                    },
                    market_key=market_key,
                    market_type_code=market_type_code,
                    market_name_raw=market_name_raw,
                    player_name=player_name,
                    line_value=line_value,
                    stat_side_scope=scope,
                    selection_key=selection_key,
                    selection_type=selection_type,
                    selection_label=selection_label,
                    sort_order=selection_sort_order(selection_type),
                    external_market_id=external_market_id,
                    external_selection_id=external_selection_id,
                    selection_payload_meta=payload_meta,
                    sgm_eligible=sgm_eligible,
                    decimal_price=decimal_price,
                    implied_prob=implied_probability(decimal_price),
                    margin=margin,
                )
            )
        return records

    def _record_imported_file(
        self,
        *,
        conn: Any,
        import_run_id: int,
        path: Path,
        file_kind: str,
        bookmaker_code: str,
        rows_read: int,
        rows_loaded: int,
        status: str,
        error_text: str | None,
        observed_at: datetime | None = None,
    ) -> int:
        modified_at = datetime.fromtimestamp(path.stat().st_mtime, UTC).replace(tzinfo=None)
        sha = sha256_file(path)
        conn.execute(
            """
            INSERT INTO imported_files (
              import_run_id, source_path, sha256, size_bytes, modified_at, file_kind,
              bookmaker_code, rows_read, rows_loaded, status, error_text, created_at
            )
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT (source_path, sha256) DO UPDATE SET
              import_run_id = excluded.import_run_id,
              size_bytes = excluded.size_bytes,
              modified_at = excluded.modified_at,
              file_kind = excluded.file_kind,
              bookmaker_code = excluded.bookmaker_code,
              rows_read = excluded.rows_read,
              rows_loaded = excluded.rows_loaded,
              status = excluded.status,
              error_text = excluded.error_text,
              created_at = excluded.created_at
            """,
            [
                import_run_id,
                str(path),
                sha,
                path.stat().st_size,
                modified_at,
                file_kind,
                bookmaker_code,
                rows_read,
                rows_loaded,
                status,
                error_text,
                observed_at or utc_now().replace(tzinfo=None),
            ],
        )
        file_id = fetch_value(
            conn,
            "SELECT imported_file_id FROM imported_files WHERE source_path = ? AND sha256 = ?",
            [str(path), sha],
        )
        if file_id is None:
            raise RuntimeError("Failed to create imported_files row.")
        return int(file_id)

    def _bookmaker_from_path(self, path: Path | None, row: dict[str, str] | None = None) -> str:
        if row and clean_text(row.get("agency")):
            return normalize_bookmaker_code(row.get("agency"))
        if path is None:
            raise ValueError("Cannot infer bookmaker without a path or row.")
        return normalize_bookmaker_code(path.name.split("_", 1)[0])

    def _upsert_normalized_record(
        self,
        *,
        conn: Any,
        import_run_id: int,
        source_file_id: int,
        observed_at: datetime,
        record: NormalizedSelectionRecord,
    ) -> None:
        bookmaker_id = self._get_bookmaker_id(conn, record.bookmaker_code)
        home_team_id = self._upsert_team(conn, record.home_team_name)
        away_team_id = self._upsert_team(conn, record.away_team_name)
        player_id = self._upsert_player(conn, record.player_name) if record.player_name else None
        event_id = self._upsert_event(
            conn=conn,
            event_key=record.event_key,
            match_name=record.match_name,
            home_team_id=home_team_id,
            away_team_id=away_team_id,
            start_time_utc=record.start_time_utc,
            round_label=record.round_label,
            venue=record.venue,
            status=record.event_status,
        )
        market_id = self._upsert_market(
            conn=conn,
            market_key=record.market_key,
            event_id=event_id,
            market_type_code=record.market_type_code,
            market_name_raw=record.market_name_raw,
            player_id=player_id,
            line_value=record.line_value,
            stat_scope=record.stat_side_scope,
        )
        selection_id = self._upsert_selection(
            conn=conn,
            selection_key=record.selection_key,
            market_id=market_id,
            selection_type=record.selection_type,
            label=record.selection_label,
            sort_order=record.sort_order,
        )
        self._upsert_event_bookmaker_map(
            conn=conn,
            event_id=event_id,
            bookmaker_id=bookmaker_id,
            external_event_id=record.external_event_id,
            external_competition_id=record.external_competition_id,
            payload_meta=record.event_payload_meta,
            last_seen_at=observed_at,
        )
        self._upsert_selection_meta(
            conn=conn,
            selection_id=selection_id,
            bookmaker_id=bookmaker_id,
            external_market_id=record.external_market_id,
            external_selection_id=record.external_selection_id,
            sgm_eligible=record.sgm_eligible,
            payload_meta=record.selection_payload_meta,
            last_seen_at=observed_at,
        )
        conn.execute(
            """
            INSERT INTO outcome_prices (
              selection_id, bookmaker_id, import_run_id, decimal_price,
              implied_prob, margin, observed_at, source_file_id
            )
            VALUES (?, ?, ?, ?, ?, ?, ?, ?)
            """,
            [
                selection_id,
                bookmaker_id,
                import_run_id,
                record.decimal_price,
                record.implied_prob,
                record.margin,
                observed_at,
                source_file_id,
            ],
        )

    def _get_bookmaker_id(self, conn: Any, code: str) -> int:
        if code in self.bookmaker_ids:
            return self.bookmaker_ids[code]
        bookmaker_id = fetch_value(conn, "SELECT bookmaker_id FROM bookmakers WHERE code = ?", [code])
        if bookmaker_id is None:
            conn.execute(
                "INSERT INTO bookmakers (code, display_name) VALUES (?, ?)",
                [code, code.title()],
            )
            bookmaker_id = fetch_value(conn, "SELECT bookmaker_id FROM bookmakers WHERE code = ?", [code])
        if bookmaker_id is None:
            raise RuntimeError(f"Failed to resolve bookmaker_id for {code}")
        self.bookmaker_ids[code] = int(bookmaker_id)
        return int(bookmaker_id)

    def _upsert_team(self, conn: Any, team_name: str) -> int:
        if team_name in self.team_ids:
            return self.team_ids[team_name]
        conn.execute(
            """
            INSERT INTO teams (league_code, name, normalized_name)
            VALUES (?, ?, ?)
            ON CONFLICT (normalized_name) DO UPDATE SET name = excluded.name
            """,
            [LEAGUE_CODE, team_name, team_name],
        )
        team_id = fetch_value(conn, "SELECT team_id FROM teams WHERE normalized_name = ?", [team_name])
        if team_id is None:
            raise RuntimeError(f"Failed to resolve team_id for {team_name}")
        self.team_ids[team_name] = int(team_id)
        return int(team_id)

    def _upsert_player(self, conn: Any, player_name: str | None) -> int | None:
        if player_name is None:
            return None
        if player_name in self.player_ids:
            return self.player_ids[player_name]
        conn.execute(
            """
            INSERT INTO players (full_name, normalized_name)
            VALUES (?, ?)
            ON CONFLICT (normalized_name) DO UPDATE SET full_name = excluded.full_name
            """,
            [player_name, player_name],
        )
        player_id = fetch_value(conn, "SELECT player_id FROM players WHERE normalized_name = ?", [player_name])
        if player_id is None:
            raise RuntimeError(f"Failed to resolve player_id for {player_name}")
        self.player_ids[player_name] = int(player_id)
        return int(player_id)

    def _upsert_player_game_log(self, *, conn: Any, record: PlayerGameLogRecord) -> None:
        player_id = self._upsert_player(conn, record.player_name)
        if player_id is None:
            raise RuntimeError(f"Failed to resolve player_id for {record.player_name}")
        conn.execute(
            """
            INSERT INTO player_game_logs (
              game_log_key, player_id, source_player_id, match_name, season_name, start_time_utc,
              round_label, venue, weather_category, weather_description, home_team, away_team,
              player_team, opposition_team, home_away, margin, tog_percentage, fantasy_points,
              goals, behinds, disposals, kicks, handballs, marks, tackles, hitouts, frees_for,
              frees_against, total_clearances, metres_gained, goal_assists, cba_percentage, cbas,
              kick_ins, kick_in_percentage, kick_ins_play_on, kick_to_handball_ratio,
              hitout_win_percentage
            )
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT (game_log_key) DO UPDATE SET
              player_id = excluded.player_id,
              source_player_id = excluded.source_player_id,
              match_name = excluded.match_name,
              season_name = excluded.season_name,
              start_time_utc = excluded.start_time_utc,
              round_label = excluded.round_label,
              venue = excluded.venue,
              weather_category = excluded.weather_category,
              weather_description = excluded.weather_description,
              home_team = excluded.home_team,
              away_team = excluded.away_team,
              player_team = excluded.player_team,
              opposition_team = excluded.opposition_team,
              home_away = excluded.home_away,
              margin = excluded.margin,
              tog_percentage = excluded.tog_percentage,
              fantasy_points = excluded.fantasy_points,
              goals = excluded.goals,
              behinds = excluded.behinds,
              disposals = excluded.disposals,
              kicks = excluded.kicks,
              handballs = excluded.handballs,
              marks = excluded.marks,
              tackles = excluded.tackles,
              hitouts = excluded.hitouts,
              frees_for = excluded.frees_for,
              frees_against = excluded.frees_against,
              total_clearances = excluded.total_clearances,
              metres_gained = excluded.metres_gained,
              goal_assists = excluded.goal_assists,
              cba_percentage = excluded.cba_percentage,
              cbas = excluded.cbas,
              kick_ins = excluded.kick_ins,
              kick_in_percentage = excluded.kick_in_percentage,
              kick_ins_play_on = excluded.kick_ins_play_on,
              kick_to_handball_ratio = excluded.kick_to_handball_ratio,
              hitout_win_percentage = excluded.hitout_win_percentage
            """,
            [
                record.game_log_key,
                player_id,
                record.source_player_id,
                record.match_name,
                record.season_name,
                record.start_time_utc,
                record.round_label,
                record.venue,
                record.weather_category,
                record.weather_description,
                record.home_team,
                record.away_team,
                record.player_team,
                record.opposition_team,
                record.home_away,
                record.margin,
                record.tog_percentage,
                record.fantasy_points,
                record.goals,
                record.behinds,
                record.disposals,
                record.kicks,
                record.handballs,
                record.marks,
                record.tackles,
                record.hitouts,
                record.frees_for,
                record.frees_against,
                record.total_clearances,
                record.metres_gained,
                record.goal_assists,
                record.cba_percentage,
                record.cbas,
                record.kick_ins,
                record.kick_in_percentage,
                record.kick_ins_play_on,
                record.kick_to_handball_ratio,
                record.hitout_win_percentage,
            ],
        )

    def _upsert_event(
        self,
        *,
        conn: Any,
        event_key: str,
        match_name: str,
        home_team_id: int,
        away_team_id: int,
        start_time_utc: datetime | None,
        round_label: str | None,
        venue: str | None,
        status: str,
    ) -> int:
        if event_key in self.event_ids:
            return self.event_ids[event_key]
        conn.execute(
            """
            INSERT INTO events (
              event_key, league_code, match_name, home_team_id, away_team_id,
              start_time_utc, round_label, venue, status
            )
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT (event_key) DO UPDATE SET
              match_name = excluded.match_name,
              home_team_id = excluded.home_team_id,
              away_team_id = excluded.away_team_id,
              start_time_utc = COALESCE(excluded.start_time_utc, events.start_time_utc),
              round_label = COALESCE(excluded.round_label, events.round_label),
              venue = COALESCE(excluded.venue, events.venue),
              status = excluded.status
            """,
            [
                event_key,
                LEAGUE_CODE,
                match_name,
                home_team_id,
                away_team_id,
                start_time_utc,
                round_label,
                venue,
                status,
            ],
        )
        event_id = fetch_value(conn, "SELECT event_id FROM events WHERE event_key = ?", [event_key])
        if event_id is None:
            raise RuntimeError(f"Failed to resolve event_id for {event_key}")
        self.event_ids[event_key] = int(event_id)
        return int(event_id)

    def _upsert_market(
        self,
        *,
        conn: Any,
        market_key: str,
        event_id: int,
        market_type_code: str,
        market_name_raw: str,
        player_id: int | None,
        line_value: float | None,
        stat_scope: str,
    ) -> int:
        if market_key in self.market_ids:
            return self.market_ids[market_key]
        conn.execute(
            """
            INSERT INTO markets (
              market_key, event_id, market_type_code, market_name_raw, player_id, line_value, stat_side_scope
            )
            VALUES (?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT (market_key) DO UPDATE SET
              market_name_raw = excluded.market_name_raw,
              line_value = COALESCE(excluded.line_value, markets.line_value)
            """,
            [market_key, event_id, market_type_code, market_name_raw, player_id, line_value, stat_scope],
        )
        market_id = fetch_value(conn, "SELECT market_id FROM markets WHERE market_key = ?", [market_key])
        if market_id is None:
            raise RuntimeError(f"Failed to resolve market_id for {market_key}")
        self.market_ids[market_key] = int(market_id)
        return int(market_id)

    def _upsert_selection(
        self,
        *,
        conn: Any,
        selection_key: str,
        market_id: int,
        selection_type: str,
        label: str,
        sort_order: int,
    ) -> int:
        if selection_key in self.selection_ids:
            return self.selection_ids[selection_key]
        conn.execute(
            """
            INSERT INTO selections (selection_key, market_id, selection_type, label, sort_order)
            VALUES (?, ?, ?, ?, ?)
            ON CONFLICT (selection_key) DO UPDATE SET
              label = excluded.label,
              sort_order = excluded.sort_order
            """,
            [selection_key, market_id, selection_type, label, sort_order],
        )
        selection_id = fetch_value(conn, "SELECT selection_id FROM selections WHERE selection_key = ?", [selection_key])
        if selection_id is None:
            raise RuntimeError(f"Failed to resolve selection_id for {selection_key}")
        self.selection_ids[selection_key] = int(selection_id)
        return int(selection_id)

    def _upsert_event_bookmaker_map(
        self,
        *,
        conn: Any,
        event_id: int,
        bookmaker_id: int,
        external_event_id: str | None,
        external_competition_id: str | None,
        payload_meta: dict[str, Any],
        last_seen_at: datetime,
    ) -> None:
        conn.execute(
            """
            INSERT INTO event_bookmaker_map (
              event_id, bookmaker_id, external_event_id, external_competition_id, payload_meta_json, last_seen_at
            )
            VALUES (?, ?, ?, ?, ?, ?)
            ON CONFLICT (event_id, bookmaker_id) DO UPDATE SET
              external_event_id = COALESCE(excluded.external_event_id, event_bookmaker_map.external_event_id),
              external_competition_id = COALESCE(excluded.external_competition_id, event_bookmaker_map.external_competition_id),
              payload_meta_json = excluded.payload_meta_json,
              last_seen_at = excluded.last_seen_at
            """,
            [
                event_id,
                bookmaker_id,
                external_event_id,
                external_competition_id,
                stable_json_dumps(payload_meta),
                last_seen_at,
            ],
        )

    def _upsert_selection_meta(
        self,
        *,
        conn: Any,
        selection_id: int,
        bookmaker_id: int,
        external_market_id: str | None,
        external_selection_id: str | None,
        sgm_eligible: bool,
        payload_meta: dict[str, Any],
        last_seen_at: datetime,
    ) -> None:
        conn.execute(
            """
            INSERT INTO selection_bookmaker_meta (
              selection_id, bookmaker_id, external_market_id, external_selection_id,
              sgm_eligible, payload_meta_json, last_seen_at
            )
            VALUES (?, ?, ?, ?, ?, ?, ?)
            ON CONFLICT (selection_id, bookmaker_id) DO UPDATE SET
              external_market_id = COALESCE(excluded.external_market_id, selection_bookmaker_meta.external_market_id),
              external_selection_id = COALESCE(excluded.external_selection_id, selection_bookmaker_meta.external_selection_id),
              sgm_eligible = excluded.sgm_eligible,
              payload_meta_json = excluded.payload_meta_json,
              last_seen_at = excluded.last_seen_at
            """,
            [
                selection_id,
                bookmaker_id,
                external_market_id,
                external_selection_id,
                sgm_eligible,
                stable_json_dumps(payload_meta),
                last_seen_at,
            ],
        )

    def _upsert_processed_metric(
        self,
        *,
        conn: Any,
        metric: ProcessedSelectionMetric,
        observed_at: datetime,
    ) -> None:
        bookmaker_id = self._get_bookmaker_id(conn, metric.bookmaker_code)
        selection_id = fetch_value(
            conn,
            "SELECT selection_id FROM selections WHERE selection_key = ?",
            [metric.selection_key],
        )
        if selection_id is None:
            LOGGER.warning(
                "Skipping processed metric for unresolved selection_key=%s bookmaker=%s",
                metric.selection_key,
                metric.bookmaker_code,
            )
            return

        fair_prob = metric.prob_last_10
        fair_price = None if fair_prob is None or fair_prob <= 0 else round(1 / fair_prob, 4)
        conn.execute(
            """
            INSERT INTO selection_metrics (
              selection_id,
              bookmaker_id,
              metric_source,
              fair_prob,
              fair_price,
              edge_pct,
              computed_at,
              metrics_json
            )
            VALUES (?, ?, ?, ?, ?, ?, ?, ?)
            """,
            [
                int(selection_id),
                bookmaker_id,
                "processed_odds_v1",
                fair_prob,
                fair_price,
                metric.diff_last_10,
                observed_at,
                stable_json_dumps(
                    {
                        "diff_2025": metric.diff_2025,
                        "diff_last_10": metric.diff_last_10,
                        "prob_2025": metric.prob_2025,
                        "prob_last_10": metric.prob_last_10,
                        "player_position": metric.player_position,
                        "matchup_difficulty": metric.matchup_difficulty,
                        "over_matchup_difficulty": metric.over_matchup_difficulty,
                        "under_matchup_difficulty": metric.under_matchup_difficulty,
                        "dvp": metric.dvp,
                        "raw_dvp": metric.raw_dvp,
                        "dvp_standard_error": metric.dvp_standard_error,
                        "dvp_bootstrap_ci_low": metric.dvp_bootstrap_ci_low,
                        "dvp_bootstrap_ci_high": metric.dvp_bootstrap_ci_high,
                        "dvp_sample_count": metric.dvp_sample_count,
                        "dvp_match_count": metric.dvp_match_count,
                        "dvp_observation_count": metric.dvp_observation_count,
                        "dvp_model_version": metric.dvp_model_version,
                        "dvp_generated_at": metric.dvp_generated_at,
                        "variation": metric.variation,
                    }
                ),
            ],
        )


def run_import(
    settings: Settings | None = None, *, triggered_by: str = "manual"
) -> ImportSummary:
    importer = Importer(settings or get_settings())
    return importer.run(triggered_by=triggered_by)


def _parse_optional_int(value: Any) -> int | None:
    parsed = parse_float(value)
    if parsed is None:
        return None
    return int(parsed)


def _percentile(sorted_values: list[float], percentile: float) -> float:
    if not sorted_values:
        return 0.0
    if len(sorted_values) == 1:
        return sorted_values[0]
    index = percentile * (len(sorted_values) - 1)
    lower = math.floor(index)
    upper = math.ceil(index)
    if lower == upper:
        return sorted_values[int(index)]
    weight = index - lower
    return sorted_values[lower] * (1 - weight) + sorted_values[upper] * weight


def _label_from_thresholds(value: float, thresholds: list[float]) -> str:
    q1, q2, q3, q4 = thresholds
    if value < q1:
        return "Terrible"
    if value < q2:
        return "Bad"
    if value <= q3:
        return "Neutral"
    if value <= q4:
        return "Good"
    return "Excellent"
