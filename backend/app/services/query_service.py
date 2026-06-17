from __future__ import annotations

import csv
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

from app.config import Settings
from app.db.duckdb import connection, fetch_all, fetch_one
from app.utils.hashing import sha256_text
from app.utils.time import utc_now


PLAYER_STAT_COLUMN_MAP = {
    "disposals": ("Disposals", "disposals"),
    "fantasy": ("Fantasy", "fantasy_points"),
    "fantasy_points": ("Fantasy", "fantasy_points"),
    "tackles": ("Tackles", "tackles"),
    "marks": ("Marks", "marks"),
    "goals": ("Goals", "goals"),
    "kicks": ("Kicks", "kicks"),
    "handballs": ("Handballs", "handballs"),
    "hitouts": ("Hitouts", "hitouts"),
    "clearances": ("Clearances", "total_clearances"),
}

ODDS_SORT_COLUMNS = {
    "diff_last_10": "diff_last_10",
    "diff_2025": "diff_2025",
    "home_away_diff": "home_away_diff",
    "win_loss_diff": "win_loss_diff",
    "next_best_prob_diff": "next_best_prob_diff",
    "price": "decimal_price",
    "edge": "edge_pct",
    "player": "player_name",
    "match": "match_name",
    "market": "market_display_name",
    "start_time": "start_time",
}


class QueryService:
    def __init__(self, settings: Settings):
        self.settings = settings

    @staticmethod
    def _derived_opposition_sql(table_alias: str | None = None) -> str:
        prefix = f"{table_alias}." if table_alias else ""
        player_team = f"{prefix}player_team"
        home_team = f"{prefix}home_team"
        away_team = f"{prefix}away_team"
        opposition_team = f"{prefix}opposition_team"
        return f"""
        CASE
          WHEN {player_team} IS NOT NULL
            AND {home_team} IS NOT NULL
            AND LOWER(TRIM({player_team})) = LOWER(TRIM({home_team}))
            THEN {away_team}
          WHEN {player_team} IS NOT NULL
            AND {away_team} IS NOT NULL
            AND LOWER(TRIM({player_team})) = LOWER(TRIM({away_team}))
            THEN {home_team}
          ELSE {opposition_team}
        END
        """

    @staticmethod
    def _derived_home_away_sql(table_alias: str | None = None) -> str:
        prefix = f"{table_alias}." if table_alias else ""
        player_team = f"{prefix}player_team"
        home_team = f"{prefix}home_team"
        away_team = f"{prefix}away_team"
        home_away = f"{prefix}home_away"
        return f"""
        CASE
          WHEN {player_team} IS NOT NULL
            AND {home_team} IS NOT NULL
            AND LOWER(TRIM({player_team})) = LOWER(TRIM({home_team}))
            THEN 'Home'
          WHEN {player_team} IS NOT NULL
            AND {away_team} IS NOT NULL
            AND LOWER(TRIM({player_team})) = LOWER(TRIM({away_team}))
            THEN 'Away'
          ELSE {home_away}
        END
        """

    def get_health(self) -> dict[str, Any]:
        with connection(settings=self.settings) as conn:
            last_run = fetch_one(
                conn,
                "SELECT finished_at FROM latest_successful_import_run_v LIMIT 1",
            )
        return {
            "status": "ok",
            "database_ok": True,
            "last_successful_import_at": last_run["finished_at"] if last_run else None,
        }

    def get_data_status(self) -> dict[str, Any]:
        sections: list[dict[str, Any]] = []

        processed_files = [
            self._file_status(path, base_dir=self.settings.processed_odds_dir)
            for path in sorted(self.settings.processed_odds_dir.glob("*"))
            if path.is_file()
        ]
        if processed_files:
            sections.append(
                {
                    "code": "processed",
                    "title": "Processed",
                    "category": "processed",
                    "files": processed_files,
                }
            )

        scraped_by_agency: dict[str, list[dict[str, Any]]] = {}
        for path in sorted(self.settings.scraped_odds_dir.glob("*")):
            if not path.is_file():
                continue
            agency_code = path.name.split("_", maxsplit=1)[0].lower()
            scraped_by_agency.setdefault(agency_code, []).append(
                self._file_status(path, base_dir=self.settings.scraped_odds_dir)
            )

        for agency_code in sorted(scraped_by_agency):
            sections.append(
                {
                    "code": agency_code,
                    "title": self._agency_title(agency_code),
                    "category": "scraped",
                    "files": scraped_by_agency[agency_code],
                }
            )

        return {
            "generated_at": datetime.now(timezone.utc),
            "sections": sections,
        }

    def list_bookmakers(self, live_pricing_codes: set[str]) -> list[dict[str, Any]]:
        with connection(settings=self.settings) as conn:
            rows = fetch_all(
                conn,
                """
                SELECT
                  b.bookmaker_id AS id,
                  b.code,
                  b.display_name,
                  b.enabled,
                  COALESCE(s.sgm_eligible_count, 0) AS sgm_eligible_count
                FROM bookmakers b
                LEFT JOIN (
                  SELECT bookmaker_id, COUNT(*) AS sgm_eligible_count
                  FROM selection_bookmaker_meta
                  WHERE sgm_eligible = TRUE
                  GROUP BY bookmaker_id
                ) s ON s.bookmaker_id = b.bookmaker_id
                ORDER BY b.display_name
                """,
            )
        for row in rows:
            row["live_pricing_enabled"] = row["code"] in live_pricing_codes
        return rows

    def _file_status(self, path: Path, *, base_dir: Path) -> dict[str, Any]:
        modified_at = datetime.fromtimestamp(path.stat().st_mtime, tz=timezone.utc)
        return {
            "file_name": path.name,
            "relative_path": str(path.relative_to(base_dir.parent)),
            "modified_at": modified_at,
        }

    def _agency_title(self, agency_code: str) -> str:
        return {
            "bet365": "Bet365",
            "betfair": "Betfair",
            "betright": "Betright",
            "dabble": "Dabble",
            "neds": "Neds",
            "pointsbet": "PointsBet",
            "sportsbet": "Sportsbet",
            "tab": "TAB",
        }.get(agency_code, agency_code.replace("_", " ").title())

    def list_events(
        self,
        *,
        date_from: str | None,
        date_to: str | None,
        query: str | None,
        bookmaker: str | None,
        limit: int,
        offset: int,
    ) -> list[dict[str, Any]]:
        conditions: list[str] = []
        params: list[Any] = []
        if date_from:
            conditions.append("e.start_time_utc >= ?")
            params.append(date_from)
        if date_to:
            conditions.append("e.start_time_utc <= ?")
            params.append(date_to)
        if query:
            conditions.append("LOWER(e.match_name) LIKE ?")
            params.append(f"%{query.lower()}%")
        if bookmaker:
            conditions.append(
                """
                EXISTS (
                  SELECT 1
                  FROM event_bookmaker_map ebm
                  JOIN bookmakers b2 ON b2.bookmaker_id = ebm.bookmaker_id
                  WHERE ebm.event_id = e.event_id AND b2.code = ?
                )
                """
            )
            params.append(bookmaker)

        where_clause = f"WHERE {' AND '.join(conditions)}" if conditions else ""
        params.extend([limit, offset])

        with connection(settings=self.settings) as conn:
            rows = fetch_all(
                conn,
                f"""
                SELECT
                  e.event_id AS id,
                  e.match_name,
                  e.start_time_utc AS start_time,
                  e.round_label,
                  e.venue,
                  home_t.team_id AS home_team_id,
                  home_t.name AS home_team_name,
                  away_t.team_id AS away_team_id,
                  away_t.name AS away_team_name,
                  COALESCE(STRING_AGG(DISTINCT b.code, ','), '') AS available_bookmakers
                FROM events e
                JOIN teams home_t ON home_t.team_id = e.home_team_id
                JOIN teams away_t ON away_t.team_id = e.away_team_id
                LEFT JOIN event_bookmaker_map ebm ON ebm.event_id = e.event_id
                LEFT JOIN bookmakers b ON b.bookmaker_id = ebm.bookmaker_id
                {where_clause}
                GROUP BY e.event_id, e.match_name, e.start_time_utc, e.round_label, e.venue,
                         home_t.team_id, home_t.name, away_t.team_id, away_t.name
                ORDER BY e.start_time_utc NULLS LAST, e.event_id
                LIMIT ? OFFSET ?
                """,
                params,
            )
        return [self._shape_event(row) for row in rows]

    def get_event(self, event_id: int) -> dict[str, Any] | None:
        with connection(settings=self.settings) as conn:
            row = fetch_one(
                conn,
                """
                SELECT
                  e.event_id AS id,
                  e.match_name,
                  e.start_time_utc AS start_time,
                  e.round_label,
                  e.venue,
                  home_t.team_id AS home_team_id,
                  home_t.name AS home_team_name,
                  away_t.team_id AS away_team_id,
                  away_t.name AS away_team_name,
                  COALESCE(STRING_AGG(DISTINCT b.code, ','), '') AS available_bookmakers
                FROM events e
                JOIN teams home_t ON home_t.team_id = e.home_team_id
                JOIN teams away_t ON away_t.team_id = e.away_team_id
                LEFT JOIN event_bookmaker_map ebm ON ebm.event_id = e.event_id
                LEFT JOIN bookmakers b ON b.bookmaker_id = ebm.bookmaker_id
                WHERE e.event_id = ?
                GROUP BY e.event_id, e.match_name, e.start_time_utc, e.round_label, e.venue,
                         home_t.team_id, home_t.name, away_t.team_id, away_t.name
                """,
                [event_id],
            )
        return self._shape_event(row) if row else None

    def list_markets(
        self,
        *,
        event_id: int,
        bookmaker: str,
        market_type: str | None,
        player_query: str | None,
        limit: int,
        offset: int,
    ) -> list[dict[str, Any]]:
        conditions = ["m.event_id = ?", "b.code = ?"]
        params: list[Any] = [event_id, bookmaker]
        if market_type:
            conditions.append("m.market_type_code = ?")
            params.append(market_type)
        if player_query:
            conditions.append("LOWER(COALESCE(p.full_name, '')) LIKE ?")
            params.append(f"%{player_query.lower()}%")
        params.extend([limit, offset])

        with connection(settings=self.settings) as conn:
            rows = fetch_all(
                conn,
                f"""
                SELECT
                  m.market_id AS id,
                  m.event_id,
                  m.market_type_code,
                  m.market_name_raw AS display_name,
                  p.player_id,
                  p.full_name AS player_name,
                  m.line_value,
                  b.code AS bookmaker,
                  STRING_AGG(DISTINCT s.selection_type, ',') AS available_selection_types
                FROM markets m
                LEFT JOIN players p ON p.player_id = m.player_id
                JOIN selections s ON s.market_id = m.market_id
                JOIN selection_bookmaker_meta sbm ON sbm.selection_id = s.selection_id
                JOIN bookmakers b ON b.bookmaker_id = sbm.bookmaker_id
                JOIN current_outcome_prices_v cop
                  ON cop.selection_id = s.selection_id AND cop.bookmaker_id = sbm.bookmaker_id
                WHERE {' AND '.join(conditions)}
                GROUP BY m.market_id, m.event_id, m.market_type_code, m.market_name_raw,
                         p.player_id, p.full_name, m.line_value, b.code
                ORDER BY COALESCE(p.full_name, ''), m.market_name_raw, m.line_value
                LIMIT ? OFFSET ?
                """,
                params,
            )
        return [self._shape_market(row) for row in rows]

    def list_market_selections(self, *, market_id: int, bookmaker: str) -> list[dict[str, Any]]:
        with connection(settings=self.settings) as conn:
            rows = fetch_all(
                conn,
                """
                SELECT
                  s.selection_id AS id,
                  s.market_id,
                  s.selection_type,
                  s.label,
                  cop.decimal_price,
                  cop.implied_prob,
                  b.code AS bookmaker,
                  sbm.sgm_eligible,
                  lm.edge_pct
                FROM selections s
                JOIN selection_bookmaker_meta sbm ON sbm.selection_id = s.selection_id
                JOIN bookmakers b ON b.bookmaker_id = sbm.bookmaker_id
                LEFT JOIN current_outcome_prices_v cop
                  ON cop.selection_id = s.selection_id AND cop.bookmaker_id = sbm.bookmaker_id
                LEFT JOIN latest_selection_metrics_v lm
                  ON lm.selection_id = s.selection_id AND lm.bookmaker_id = sbm.bookmaker_id
                WHERE s.market_id = ? AND b.code = ?
                ORDER BY s.sort_order, s.selection_id
                """,
                [market_id, bookmaker],
            )
        return rows

    def search_players(self, *, query: str, limit: int) -> list[dict[str, Any]]:
        with connection(settings=self.settings) as conn:
            rows = fetch_all(
                conn,
                """
                SELECT player_id AS id, full_name
                FROM players
                WHERE LOWER(full_name) LIKE ?
                ORDER BY full_name
                LIMIT ?
                """,
                [f"%{query.lower()}%", limit],
            )
        return rows

    def search_stat_players(
        self,
        *,
        query: str,
        limit: int,
        stat: str = "disposals",
        seasons: list[str] | None = None,
        oppositions: list[str] | None = None,
        venues: list[str] | None = None,
        weather_categories: list[str] | None = None,
        home_away: list[str] | None = None,
        margin_min: int = -200,
        margin_max: int = 200,
        last_games: int | None = None,
        minutes_minimum: float = 0,
    ) -> list[dict[str, Any]] | None:
        resolved = self._resolve_player_stat(stat)
        if resolved is None:
            return None
        conditions = [
            "LOWER(p.full_name) LIKE ?",
            "pgl.tog_percentage >= ?",
            "COALESCE(pgl.margin, 0) >= ?",
            "COALESCE(pgl.margin, 0) <= ?",
        ]
        params: list[Any] = [f"%{query.lower()}%", minutes_minimum, margin_min, margin_max]
        if seasons:
            placeholders = ", ".join("?" for _ in seasons)
            conditions.append(f"pgl.season_name IN ({placeholders})")
            params.extend(seasons)
        if home_away:
            placeholders = ", ".join("?" for _ in home_away)
            conditions.append(f"{self._derived_home_away_sql('pgl')} IN ({placeholders})")
            params.extend(home_away)

        post_conditions = [f"{resolved['column']} IS NOT NULL"]
        if last_games is not None:
            post_conditions.append("recent_rank <= ?")
            params.append(last_games)
        if oppositions:
            placeholders = ", ".join("?" for _ in oppositions)
            post_conditions.append(f"derived_opposition_team IN ({placeholders})")
            params.extend(oppositions)
        if weather_categories:
            placeholders = ", ".join("?" for _ in weather_categories)
            post_conditions.append(f"weather_category IN ({placeholders})")
            params.extend(weather_categories)
        if venues:
            placeholders = ", ".join("?" for _ in venues)
            post_conditions.append(f"venue IN ({placeholders})")
            params.extend(venues)

        params.append(limit)
        with connection(settings=self.settings) as conn:
            rows = fetch_all(
                conn,
                f"""
                WITH ranked AS (
                  SELECT
                    p.player_id AS id,
                    p.full_name,
                    {self._derived_opposition_sql("pgl")} AS derived_opposition_team,
                    pgl.weather_category,
                    pgl.venue,
                    pgl.{resolved["column"]},
                    ROW_NUMBER() OVER (
                      PARTITION BY p.player_id
                      ORDER BY pgl.start_time_utc DESC
                    ) AS recent_rank
                  FROM players p
                  JOIN player_game_logs pgl ON pgl.player_id = p.player_id
                  WHERE {' AND '.join(conditions)}
                )
                SELECT DISTINCT id, full_name
                FROM ranked
                WHERE {' AND '.join(post_conditions)}
                ORDER BY full_name
                LIMIT ?
                """,
                params,
            )
        return rows

    def get_player_available_venues(
        self,
        *,
        player_id: int,
        seasons: list[str] | None,
        oppositions: list[str] | None,
        weather_categories: list[str] | None,
        home_away: list[str] | None,
        margin_min: int,
        margin_max: int,
        last_games: int | None,
        minutes_minimum: float,
    ) -> list[str]:
        """Venues the player actually has games at under the given filters.

        Reuses the exact same filtering pipeline as the history endpoint
        (including the last-N-games ordering), so the option list can never
        drift from what the history query would return. The venue filter
        itself is deliberately excluded so it doesn't constrain its own
        option list.
        """
        rows = self._load_filtered_player_stats(
            player_id=player_id,
            seasons=seasons,
            oppositions=oppositions,
            venues=None,
            weather_categories=weather_categories,
            home_away=home_away,
            margin_min=margin_min,
            margin_max=margin_max,
            last_games=last_games,
            minutes_minimum=minutes_minimum,
            stat_column="disposals",
            stat_label="Disposals",
            line_mode=None,
            reference_line=None,
            lower_bound=None,
            upper_bound=None,
        )
        if not rows:
            return []
        return sorted({row["venue"] for row in rows if row["venue"] is not None})

    def get_player_stat_filter_options(self, *, player_id: int) -> dict[str, Any] | None:
        with connection(settings=self.settings) as conn:
            exists = fetch_one(
                conn,
                "SELECT player_id, full_name FROM players WHERE player_id = ?",
                [player_id],
            )
            if exists is None:
                return None
            seasons = fetch_all(
                conn,
                """
                SELECT DISTINCT season_name
                FROM player_game_logs
                WHERE player_id = ?
                ORDER BY season_name DESC
                """,
                [player_id],
            )
            oppositions = fetch_all(
                conn,
                f"""
                SELECT DISTINCT {self._derived_opposition_sql()} AS opposition_team
                FROM player_game_logs
                WHERE
                  player_id = ?
                  AND {self._derived_opposition_sql()} IS NOT NULL
                ORDER BY opposition_team
                """,
                [player_id],
            )
            venues = fetch_all(
                conn,
                """
                SELECT DISTINCT venue
                FROM player_game_logs
                WHERE player_id = ? AND venue IS NOT NULL
                ORDER BY venue
                """,
                [player_id],
            )
            weather = fetch_all(
                conn,
                """
                SELECT DISTINCT weather_category
                FROM player_game_logs
                WHERE player_id = ? AND weather_category IS NOT NULL
                ORDER BY weather_category
                """,
                [player_id],
            )
        return {
            "player_id": int(exists["player_id"]),
            "player_name": exists["full_name"],
            "stats": [{"code": code, "label": label} for code, (label, _) in PLAYER_STAT_COLUMN_MAP.items()],
            "seasons": [row["season_name"] for row in seasons],
            "oppositions": [row["opposition_team"] for row in oppositions],
            "venues": [row["venue"] for row in venues],
            "weather_categories": [row["weather_category"] for row in weather],
            "home_away_options": ["Home", "Away"],
        }

    def get_player_stat_history(
        self,
        *,
        player_id: int,
        stat: str,
        seasons: list[str] | None,
        oppositions: list[str] | None,
        venues: list[str] | None,
        weather_categories: list[str] | None,
        home_away: list[str] | None,
        margin_min: int,
        margin_max: int,
        last_games: int | None,
        minutes_minimum: float,
        line_mode: str | None,
        reference_line: float | None,
        lower_bound: float | None,
        upper_bound: float | None,
    ) -> list[dict[str, Any]] | None:
        resolved = self._resolve_player_stat(stat)
        if resolved is None:
            return None
        rows = self._load_filtered_player_stats(
            player_id=player_id,
            seasons=seasons,
            oppositions=oppositions,
            venues=venues,
            weather_categories=weather_categories,
            home_away=home_away,
            margin_min=margin_min,
            margin_max=margin_max,
            last_games=last_games,
            minutes_minimum=minutes_minimum,
            stat_column=resolved["column"],
            stat_label=resolved["label"],
            line_mode=line_mode,
            reference_line=reference_line,
            lower_bound=lower_bound,
            upper_bound=upper_bound,
        )
        return rows

    def get_player_stat_summary(
        self,
        *,
        player_id: int,
        stat: str,
        seasons: list[str] | None,
        oppositions: list[str] | None,
        venues: list[str] | None,
        weather_categories: list[str] | None,
        home_away: list[str] | None,
        margin_min: int,
        margin_max: int,
        last_games: int | None,
        minutes_minimum: float,
        line_mode: str,
        reference_line: float | None,
        lower_bound: float | None,
        upper_bound: float | None,
    ) -> dict[str, Any] | None:
        resolved = self._resolve_player_stat(stat)
        if resolved is None:
            return None
        rows = self._load_filtered_player_stats(
            player_id=player_id,
            seasons=seasons,
            oppositions=oppositions,
            venues=venues,
            weather_categories=weather_categories,
            home_away=home_away,
            margin_min=margin_min,
            margin_max=margin_max,
            last_games=last_games,
            minutes_minimum=minutes_minimum,
            stat_column=resolved["column"],
            stat_label=resolved["label"],
            line_mode=None,
            reference_line=None,
            lower_bound=None,
            upper_bound=None,
        )
        if rows is None:
            return None
        sample_size = len(rows)
        values = [row["selected_value"] for row in rows if row["selected_value"] is not None]
        if sample_size == 0:
            return self._empty_player_stat_summary(
                player_id=player_id,
                stat_code=resolved["code"],
                stat_label=resolved["label"],
                line_mode=line_mode,
                reference_line=reference_line,
                lower_bound=lower_bound,
                upper_bound=upper_bound,
            )

        if line_mode == "interval":
            proportion_within = (
                sum(
                    1
                    for value in values
                    if lower_bound is not None and upper_bound is not None and lower_bound < value < upper_bound
                )
                / sample_size
            )
            implied_within = self._implied_odds(proportion_within)
            implied_outside = self._implied_odds(1 - proportion_within)
            return {
                "player_id": player_id,
                "stat_code": resolved["code"],
                "stat_label": resolved["label"],
                "line_mode": "interval",
                "reference_line": None,
                "lower_bound": lower_bound,
                "upper_bound": upper_bound,
                "sample_size": sample_size,
                "proportion_over": None,
                "proportion_under": None,
                "implied_odds_over": None,
                "implied_odds_under": None,
                "proportion_within_interval": proportion_within,
                "proportion_outside_interval": 1 - proportion_within,
                "implied_odds_within_interval": implied_within,
                "implied_odds_outside_interval": implied_outside,
            }

        if reference_line is None:
            return None
        proportion_over = sum(1 for value in values if value >= reference_line) / sample_size
        proportion_under = 1 - proportion_over
        return {
            "player_id": player_id,
            "stat_code": resolved["code"],
            "stat_label": resolved["label"],
            "line_mode": "single",
            "reference_line": reference_line,
            "lower_bound": None,
            "upper_bound": None,
            "sample_size": sample_size,
            "proportion_over": proportion_over,
            "proportion_under": proportion_under,
            "implied_odds_over": self._implied_odds(proportion_over),
            "implied_odds_under": self._implied_odds(proportion_under),
            "proportion_within_interval": None,
            "proportion_outside_interval": None,
            "implied_odds_within_interval": None,
            "implied_odds_outside_interval": None,
        }

    def search_props(
        self,
        *,
        bookmaker: str,
        query: str | None,
        market_type: str | None,
        event_id: int | None,
        player_id: int | None,
        date_from: str | None,
        date_to: str | None,
        min_edge: float | None,
        limit: int,
        offset: int,
    ) -> list[dict[str, Any]]:
        conditions = ["b.code = ?", "m.player_id IS NOT NULL"]
        params: list[Any] = [bookmaker]
        if query:
            conditions.append(
                "(LOWER(p.full_name) LIKE ? OR LOWER(e.match_name) LIKE ? OR LOWER(s.label) LIKE ?)"
            )
            q = f"%{query.lower()}%"
            params.extend([q, q, q])
        if market_type:
            conditions.append("m.market_type_code = ?")
            params.append(market_type)
        if event_id:
            conditions.append("m.event_id = ?")
            params.append(event_id)
        if player_id:
            conditions.append("m.player_id = ?")
            params.append(player_id)
        if date_from:
            conditions.append("e.start_time_utc >= ?")
            params.append(date_from)
        if date_to:
            conditions.append("e.start_time_utc <= ?")
            params.append(date_to)
        if min_edge is not None:
            conditions.append("COALESCE(lm.edge_pct, -1000000) >= ?")
            params.append(min_edge)
        params.extend([limit, offset])

        with connection(settings=self.settings) as conn:
            rows = fetch_all(
                conn,
                f"""
                SELECT
                  s.selection_id,
                  e.event_id,
                  e.match_name,
                  e.start_time_utc AS start_time,
                  b.code AS bookmaker,
                  m.market_type_code,
                  p.player_id,
                  p.full_name AS player_name,
                  s.selection_type,
                  s.label,
                  m.line_value,
                  cop.decimal_price,
                  cop.implied_prob,
                  lm.edge_pct,
                  sbm.sgm_eligible
                FROM selections s
                JOIN markets m ON m.market_id = s.market_id
                JOIN events e ON e.event_id = m.event_id
                LEFT JOIN players p ON p.player_id = m.player_id
                JOIN selection_bookmaker_meta sbm ON sbm.selection_id = s.selection_id
                JOIN bookmakers b ON b.bookmaker_id = sbm.bookmaker_id
                LEFT JOIN current_outcome_prices_v cop
                  ON cop.selection_id = s.selection_id AND cop.bookmaker_id = sbm.bookmaker_id
                LEFT JOIN latest_selection_metrics_v lm
                  ON lm.selection_id = s.selection_id AND lm.bookmaker_id = sbm.bookmaker_id
                WHERE {' AND '.join(conditions)}
                ORDER BY e.start_time_utc NULLS LAST, e.event_id, p.full_name, m.market_type_code, s.sort_order
                LIMIT ? OFFSET ?
                """,
                params,
            )
        return [self._shape_prop_result(row) for row in rows]

    def search_odds(
        self,
        *,
        bookmakers: list[str],
        scope: str,
        query: str | None,
        market_type: str | None,
        event_ids: list[int],
        include_player_ids: list[int],
        exclude_player_ids: list[int],
        selection_type: str | None,
        matchup_difficulties: list[str],
        date_from: str | None,
        date_to: str | None,
        min_edge: float | None,
        min_price: float | None,
        max_price: float | None,
        min_diff_2025: float | None,
        max_diff_2025: float | None,
        min_diff_last_10: float | None,
        max_diff_last_10: float | None,
        min_home_away_diff: float | None,
        max_home_away_diff: float | None,
        min_win_loss_diff: float | None,
        max_win_loss_diff: float | None,
        min_next_best_prob_diff: float | None,
        max_next_best_prob_diff: float | None,
        sgm_only: bool,
        best_only: bool,
        sort_by: str,
        sort_dir: str,
        limit: int,
        offset: int,
    ) -> list[dict[str, Any]]:
        universe_conditions: list[str] = []
        row_conditions: list[str] = []
        if scope == "player":
            universe_conditions.append("m.player_id IS NOT NULL")
        elif scope == "match":
            universe_conditions.append("m.player_id IS NULL")
        universe_params: list[Any] = []
        row_params: list[Any] = []
        if query:
            universe_conditions.append(
                """
                (
                  LOWER(COALESCE(p.full_name, '')) LIKE ?
                  OR LOWER(e.match_name) LIKE ?
                  OR LOWER(s.label) LIKE ?
                  OR LOWER(m.market_name_raw) LIKE ?
                )
                """
            )
            q = f"%{query.lower()}%"
            universe_params.extend([q, q, q, q])
        if market_type:
            universe_conditions.append("m.market_type_code = ?")
            universe_params.append(market_type)
        if event_ids:
            placeholders = ", ".join("?" for _ in event_ids)
            universe_conditions.append(f"m.event_id IN ({placeholders})")
            universe_params.extend(event_ids)
        if include_player_ids:
            placeholders = ", ".join("?" for _ in include_player_ids)
            universe_conditions.append(f"m.player_id IN ({placeholders})")
            universe_params.extend(include_player_ids)
        if exclude_player_ids:
            placeholders = ", ".join("?" for _ in exclude_player_ids)
            universe_conditions.append(f"m.player_id NOT IN ({placeholders})")
            universe_params.extend(exclude_player_ids)
        if selection_type:
            universe_conditions.append("s.selection_type = ?")
            universe_params.append(selection_type)
        normalized_matchup_difficulties = [difficulty.lower() for difficulty in matchup_difficulties if difficulty.strip()]
        if normalized_matchup_difficulties:
            placeholders = ", ".join("?" for _ in normalized_matchup_difficulties)
            row_conditions.append(
                f"LOWER(COALESCE(matchup_difficulty, '')) IN ({placeholders})"
            )
            row_params.extend(normalized_matchup_difficulties)
        if date_from:
            universe_conditions.append("e.start_time_utc >= ?")
            universe_params.append(date_from)
        if date_to:
            universe_conditions.append("e.start_time_utc <= ?")
            universe_params.append(date_to)
        if bookmakers:
            placeholders = ", ".join("?" for _ in bookmakers)
            row_conditions.append(f"bookmaker IN ({placeholders})")
            row_params.extend(bookmakers)
        if min_edge is not None:
            row_conditions.append("COALESCE(edge_pct, -1000000) >= ?")
            row_params.append(min_edge)
        if min_price is not None:
            row_conditions.append("decimal_price >= ?")
            row_params.append(min_price)
        if max_price is not None:
            row_conditions.append("decimal_price <= ?")
            row_params.append(max_price)
        if min_diff_2025 is not None:
            row_conditions.append("diff_2025 >= ?")
            row_params.append(min_diff_2025)
        if max_diff_2025 is not None:
            row_conditions.append("diff_2025 <= ?")
            row_params.append(max_diff_2025)
        if min_diff_last_10 is not None:
            row_conditions.append("diff_last_10 >= ?")
            row_params.append(min_diff_last_10)
        if max_diff_last_10 is not None:
            row_conditions.append("diff_last_10 <= ?")
            row_params.append(max_diff_last_10)
        if min_home_away_diff is not None:
            row_conditions.append("home_away_diff >= ?")
            row_params.append(min_home_away_diff)
        if max_home_away_diff is not None:
            row_conditions.append("home_away_diff <= ?")
            row_params.append(max_home_away_diff)
        if min_win_loss_diff is not None:
            row_conditions.append("win_loss_diff >= ?")
            row_params.append(min_win_loss_diff)
        if max_win_loss_diff is not None:
            row_conditions.append("win_loss_diff <= ?")
            row_params.append(max_win_loss_diff)
        if min_next_best_prob_diff is not None:
            row_conditions.append("next_best_prob_diff >= ?")
            row_params.append(min_next_best_prob_diff)
        if max_next_best_prob_diff is not None:
            row_conditions.append("next_best_prob_diff <= ?")
            row_params.append(max_next_best_prob_diff)
        if sgm_only:
            row_conditions.append("sgm_eligible = TRUE")

        universe_where_clause = f"WHERE {' AND '.join(universe_conditions)}" if universe_conditions else ""
        row_where_conditions = list(row_conditions)
        if best_only:
            row_where_conditions.append("is_best_price = TRUE")
        row_where_clause = f"WHERE {' AND '.join(row_where_conditions)}" if row_where_conditions else ""
        order_clause = self._build_odds_order_clause(sort_by=sort_by, sort_dir=sort_dir)
        current_utc = utc_now().replace(tzinfo=None)
        params = [current_utc, *universe_params, *row_params, limit, offset]

        with connection(settings=self.settings) as conn:
            rows = fetch_all(
                conn,
                f"""
                WITH event_weather AS (
                  SELECT
                    event_id,
                    temperature_c,
                    wind_kph,
                    precipitation_probability,
                    precipitation_mm,
                    weather_label,
                    weather_icon_code
                  FROM (
                    SELECT
                      e.event_id,
                      wf.temperature_c,
                      wf.wind_kph,
                      wf.precipitation_probability,
                      wf.precipitation_mm,
                      wf.weather_label,
                      wf.weather_icon_code,
                      ROW_NUMBER() OVER (
                        PARTITION BY e.event_id
                        ORDER BY ABS(DATEDIFF('minute', wf.forecast_hour_utc, e.start_time_utc)) ASC
                      ) AS row_num
                    FROM events e
                    JOIN weather_forecasts wf
                      ON wf.venue = e.venue
                     AND wf.expires_at > ?
                     AND wf.forecast_hour_utc BETWEEN e.start_time_utc - INTERVAL 2 HOUR
                                                  AND e.start_time_utc + INTERVAL 2 HOUR
                    WHERE e.start_time_utc IS NOT NULL
                      AND e.venue IS NOT NULL
                  ) nearest
                  WHERE row_num = 1
                ),
                player_team_context AS (
                  SELECT
                    e.event_id,
                    latest_player_team.player_id,
                    latest_player_team.player_team,
                    CASE
                      WHEN LOWER(TRIM(latest_player_team.player_team)) = LOWER(TRIM(home_t.name)) THEN 'Home'
                      WHEN LOWER(TRIM(latest_player_team.player_team)) = LOWER(TRIM(away_t.name)) THEN 'Away'
                      ELSE NULL
                    END AS player_home_away
                  FROM (
                    SELECT
                      pgl.player_id,
                      pgl.player_team,
                      ROW_NUMBER() OVER (
                        PARTITION BY pgl.player_id
                        ORDER BY pgl.start_time_utc DESC
                      ) AS row_num
                    FROM player_game_logs pgl
                    WHERE pgl.player_team IS NOT NULL
                  ) latest_player_team
                  JOIN events e ON TRUE
                  JOIN teams home_t ON home_t.team_id = e.home_team_id
                  JOIN teams away_t ON away_t.team_id = e.away_team_id
                  WHERE latest_player_team.row_num = 1
                    AND (
                      LOWER(TRIM(latest_player_team.player_team)) = LOWER(TRIM(home_t.name))
                      OR LOWER(TRIM(latest_player_team.player_team)) = LOWER(TRIM(away_t.name))
                    )
                ),
                sportsbet_line_context AS (
                  SELECT
                    event_id,
                    home_line
                  FROM (
                    SELECT
                      line_market.event_id,
                      line_market.line_value AS home_line,
                      ROW_NUMBER() OVER (
                        PARTITION BY line_market.event_id
                        ORDER BY line_market.market_id DESC
                      ) AS row_num
                    FROM markets line_market
                    JOIN selections line_selection ON line_selection.market_id = line_market.market_id
                    JOIN selection_bookmaker_meta line_meta ON line_meta.selection_id = line_selection.selection_id
                    JOIN bookmakers b ON b.bookmaker_id = line_meta.bookmaker_id
                    WHERE line_market.market_type_code = 'line'
                      AND line_selection.selection_type = 'home'
                      AND b.code = 'sportsbet'
                  ) ranked_lines
                  WHERE row_num = 1
                ),
                base_odds AS (
                  SELECT
                    s.selection_id,
                    s.market_id,
                    e.event_id,
                    e.match_name,
                    e.start_time_utc AS start_time,
                    e.venue,
                    b.code AS bookmaker,
                    m.market_type_code,
                    m.market_name_raw AS market_display_name,
                    p.player_id,
                    p.full_name AS player_name,
                    ptc.player_team,
                    ptc.player_home_away,
                    CASE
                      WHEN ptc.player_home_away = 'Home' AND lc.home_line IS NOT NULL THEN lc.home_line * -1
                      WHEN ptc.player_home_away = 'Away' AND lc.home_line IS NOT NULL THEN lc.home_line
                      ELSE NULL
                    END AS player_team_line,
                    s.selection_type,
                    s.label,
                    m.line_value,
                    cop.decimal_price,
                    cop.implied_prob,
                    lm.edge_pct,
                    TRY_CAST(json_extract(lm.metrics_json, '$.diff_2025') AS DOUBLE) AS diff_2025,
                    TRY_CAST(json_extract(lm.metrics_json, '$.diff_last_10') AS DOUBLE) AS diff_last_10,
                    TRY_CAST(json_extract(lm.metrics_json, '$.home_away_diff') AS DOUBLE) AS home_away_diff,
                    TRY_CAST(json_extract(lm.metrics_json, '$.win_loss_diff') AS DOUBLE) AS win_loss_diff,
                    json_extract_string(lm.metrics_json, '$.player_position') AS player_position,
                    json_extract_string(lm.metrics_json, '$.matchup_difficulty') AS matchup_difficulty,
                    json_extract_string(lm.metrics_json, '$.over_matchup_difficulty') AS over_matchup_difficulty,
                    json_extract_string(lm.metrics_json, '$.under_matchup_difficulty') AS under_matchup_difficulty,
                    TRY_CAST(json_extract(lm.metrics_json, '$.dvp') AS DOUBLE) AS dvp,
                    TRY_CAST(json_extract(lm.metrics_json, '$.raw_dvp') AS DOUBLE) AS raw_dvp,
                    TRY_CAST(json_extract(lm.metrics_json, '$.dvp_standard_error') AS DOUBLE) AS dvp_standard_error,
                    TRY_CAST(json_extract(lm.metrics_json, '$.dvp_bootstrap_ci_low') AS DOUBLE) AS dvp_bootstrap_ci_low,
                    TRY_CAST(json_extract(lm.metrics_json, '$.dvp_bootstrap_ci_high') AS DOUBLE) AS dvp_bootstrap_ci_high,
                    TRY_CAST(json_extract(lm.metrics_json, '$.dvp_sample_count') AS BIGINT) AS dvp_sample_count,
                    TRY_CAST(json_extract(lm.metrics_json, '$.dvp_match_count') AS BIGINT) AS dvp_match_count,
                    TRY_CAST(json_extract(lm.metrics_json, '$.dvp_observation_count') AS BIGINT) AS dvp_observation_count,
                    json_extract_string(lm.metrics_json, '$.dvp_model_version') AS dvp_model_version,
                    json_extract_string(lm.metrics_json, '$.dvp_generated_at') AS dvp_generated_at,
                    ew.temperature_c AS weather_temperature_c,
                    ew.wind_kph AS weather_wind_kph,
                    ew.precipitation_probability AS weather_precip_probability,
                    ew.precipitation_mm AS weather_precip_mm,
                    ew.weather_label,
                    ew.weather_icon_code,
                    sbm.sgm_eligible
                  FROM selections s
                  JOIN markets m ON m.market_id = s.market_id
                  JOIN events e ON e.event_id = m.event_id
                  LEFT JOIN event_weather ew ON ew.event_id = e.event_id
                  LEFT JOIN players p ON p.player_id = m.player_id
                  LEFT JOIN player_team_context ptc
                    ON ptc.event_id = e.event_id AND ptc.player_id = p.player_id
                  JOIN selection_bookmaker_meta sbm ON sbm.selection_id = s.selection_id
                  JOIN bookmakers b ON b.bookmaker_id = sbm.bookmaker_id
                  LEFT JOIN sportsbet_line_context lc
                    ON lc.event_id = e.event_id
                  LEFT JOIN current_outcome_prices_v cop
                    ON cop.selection_id = s.selection_id AND cop.bookmaker_id = sbm.bookmaker_id
                  LEFT JOIN latest_selection_metrics_v lm
                    ON lm.selection_id = s.selection_id AND lm.bookmaker_id = sbm.bookmaker_id
                  {universe_where_clause}
                ),
                ranked_odds AS (
                  SELECT
                    base_odds.*,
                    DENSE_RANK() OVER (
                      PARTITION BY selection_id
                      ORDER BY decimal_price DESC NULLS LAST
                    ) AS market_price_rank,
                    ROW_NUMBER() OVER (
                      PARTITION BY selection_id
                      ORDER BY decimal_price DESC NULLS LAST, bookmaker ASC
                    ) AS market_price_row_number,
                    FIRST_VALUE(decimal_price) OVER (
                      PARTITION BY selection_id
                      ORDER BY decimal_price DESC NULLS LAST, bookmaker ASC
                    ) AS best_market_price,
                    LEAD(decimal_price) OVER (
                      PARTITION BY selection_id
                      ORDER BY decimal_price DESC NULLS LAST, bookmaker ASC
                    ) AS next_market_price
                  FROM base_odds
                ),
                scored_odds AS (
                  SELECT
                    selection_id,
                    market_id,
                    event_id,
                    match_name,
                    start_time,
                    venue,
                    bookmaker,
                    market_type_code,
                    market_display_name,
                    player_id,
                    player_name,
                    player_team,
                    player_home_away,
                    player_team_line,
                    selection_type,
                    label,
                    line_value,
                    decimal_price,
                    implied_prob,
                    edge_pct,
                    diff_2025,
                    diff_last_10,
                    home_away_diff,
                    win_loss_diff,
                    player_position,
                    matchup_difficulty,
                    over_matchup_difficulty,
                    under_matchup_difficulty,
                    dvp,
                    raw_dvp,
                    dvp_standard_error,
                    dvp_bootstrap_ci_low,
                    dvp_bootstrap_ci_high,
                    dvp_sample_count,
                    dvp_match_count,
                    dvp_observation_count,
                    dvp_model_version,
                    dvp_generated_at,
                    weather_temperature_c,
                    weather_wind_kph,
                    weather_precip_probability,
                    weather_precip_mm,
                    weather_label,
                    weather_icon_code,
                    market_price_rank = 1 AS is_best_price,
                    CASE
                      WHEN decimal_price IS NULL OR decimal_price <= 0 THEN NULL
                      WHEN market_price_row_number = 1 AND next_market_price IS NOT NULL AND next_market_price > 0
                        THEN ((1.0 / decimal_price) - (1.0 / next_market_price)) * -1
                      WHEN market_price_row_number > 1 AND best_market_price IS NOT NULL AND best_market_price > 0
                        THEN ((1.0 / decimal_price) - (1.0 / best_market_price)) * -1
                      ELSE NULL
                    END AS next_best_prob_diff,
                    sgm_eligible
                  FROM ranked_odds
                )
                SELECT
                  selection_id,
                  market_id,
                  event_id,
                  match_name,
                  start_time,
                  venue,
                  bookmaker,
                  market_type_code,
                  market_display_name,
                  player_id,
                  player_name,
                  player_team,
                  player_home_away,
                  player_team_line,
                  selection_type,
                  label,
                  line_value,
                  decimal_price,
                  implied_prob,
                  edge_pct,
                  diff_2025,
                  diff_last_10,
                  home_away_diff,
                  win_loss_diff,
                  player_position,
                  matchup_difficulty,
                  over_matchup_difficulty,
                  under_matchup_difficulty,
                  dvp,
                  raw_dvp,
                  dvp_standard_error,
                  dvp_bootstrap_ci_low,
                  dvp_bootstrap_ci_high,
                  dvp_sample_count,
                  dvp_match_count,
                  dvp_observation_count,
                  dvp_model_version,
                  dvp_generated_at,
                  weather_temperature_c,
                  weather_wind_kph,
                  weather_precip_probability,
                  weather_precip_mm,
                  weather_label,
                  weather_icon_code,
                  is_best_price,
                  next_best_prob_diff,
                  sgm_eligible
                FROM scored_odds
                {row_where_clause}
                ORDER BY
                  {order_clause}
                LIMIT ? OFFSET ?
                """,
                params,
            )
        return [self._shape_odds_result(row) for row in rows]

    def search_arbs(
        self,
        *,
        query: str | None,
        markets: list[str],
        agencies: list[str],
        min_margin: float,
        max_margin: float | None,
        limit: int,
        offset: int,
    ) -> list[dict[str, Any]]:
        rows = self._load_script_arbs()
        normalized_query = (query or "").strip().lower()
        market_set = {market.lower() for market in markets}
        agency_set = {agency.lower() for agency in agencies}

        filtered: list[dict[str, Any]] = []
        for row in rows:
            margin = row["margin"]
            if margin < min_margin:
                continue
            if max_margin is not None and margin > max_margin:
                continue
            if market_set and row["market_name"].lower() not in market_set:
                continue
            if agency_set and row["over_agency"].lower() not in agency_set and row["under_agency"].lower() not in agency_set:
                continue
            if normalized_query:
                searchable = " ".join(
                    str(value)
                    for value in (
                        row["match_name"],
                        row["market_name"],
                        row["player_name"],
                        row["player_team"],
                        row["opposition_team"],
                        row["over_agency"],
                        row["under_agency"],
                    )
                    if value is not None
                ).lower()
                if normalized_query not in searchable:
                    continue
            filtered.append(row)

        filtered.sort(key=lambda item: item["margin"], reverse=True)
        return filtered[offset : offset + limit]

    def _load_script_arbs(self) -> list[dict[str, Any]]:
        path = self.settings.arbs_path
        if not path.exists():
            return []
        source_modified_at = datetime.fromtimestamp(path.stat().st_mtime, timezone.utc)
        rows: list[dict[str, Any]] = []
        with path.open(newline="", encoding="utf-8-sig") as csv_file:
            reader = csv.DictReader(csv_file)
            for index, row in enumerate(reader, start=1):
                margin = self._parse_optional_float(row.get("margin"))
                over_price = self._parse_optional_float(row.get("over_price"))
                under_price = self._parse_optional_float(row.get("under_price"))
                if margin is None or over_price is None or under_price is None:
                    continue
                rows.append(
                    {
                        "id": self._arb_row_id(row, index),
                        "match_name": row.get("match") or "",
                        "market_name": row.get("market_name") or "",
                        "player_name": row.get("player_name") or "",
                        "player_team": row.get("player_team") or None,
                        "opposition_team": row.get("opposition_team") or None,
                        "over_line": self._parse_optional_float(row.get("over_line")),
                        "under_line": self._parse_optional_float(row.get("under_line")),
                        "over_price": over_price,
                        "over_agency": row.get("over_agency") or "",
                        "under_price": under_price,
                        "under_agency": row.get("under_agency") or "",
                        "margin": margin,
                        "implied_probability_sum": (1 / over_price) + (1 / under_price),
                        "status": "Arb" if margin > 0 else "Near",
                        "source_modified_at": source_modified_at,
                    }
                )
        return rows

    @staticmethod
    def _parse_optional_float(value: Any) -> float | None:
        if value is None or str(value).strip() == "":
            return None
        try:
            return float(value)
        except ValueError:
            return None

    @staticmethod
    def _arb_row_id(row: dict[str, str], index: int) -> str:
        key = "|".join(
            [
                row.get("match") or "",
                row.get("market_name") or "",
                row.get("player_name") or "",
                row.get("over_line") or "",
                row.get("under_line") or "",
                row.get("over_agency") or "",
                row.get("under_agency") or "",
                row.get("over_price") or "",
                row.get("under_price") or "",
                str(index),
            ]
        )
        return sha256_text(key)

    def _build_odds_order_clause(self, *, sort_by: str, sort_dir: str) -> str:
        sort_column = ODDS_SORT_COLUMNS.get(sort_by, "diff_last_10")
        direction = "ASC" if sort_dir.lower() == "asc" else "DESC"
        leading = f"{sort_column} {direction} NULLS LAST"
        tiebreakers = [
            "start_time NULLS LAST",
            "event_id",
            "COALESCE(player_name, '')",
            "market_display_name",
            "selection_type",
            "bookmaker",
        ]
        return ",\n                  ".join([leading, *tiebreakers])

    def _resolve_player_stat(self, stat: str) -> dict[str, str] | None:
        normalized = stat.strip().lower().replace(" ", "_")
        resolved = PLAYER_STAT_COLUMN_MAP.get(normalized)
        if resolved is None:
            return None
        label, column = resolved
        return {"code": normalized, "label": label, "column": column}

    def _load_filtered_player_stats(
        self,
        *,
        player_id: int,
        seasons: list[str] | None,
        oppositions: list[str] | None,
        venues: list[str] | None,
        weather_categories: list[str] | None,
        home_away: list[str] | None,
        margin_min: int,
        margin_max: int,
        last_games: int | None,
        minutes_minimum: float,
        stat_column: str,
        stat_label: str,
        line_mode: str | None,
        reference_line: float | None,
        lower_bound: float | None,
        upper_bound: float | None,
    ) -> list[dict[str, Any]] | None:
        conditions = [
            "player_id = ?",
            "tog_percentage >= ?",
            "COALESCE(margin, 0) >= ?",
            "COALESCE(margin, 0) <= ?",
        ]
        params: list[Any] = [player_id, minutes_minimum, margin_min, margin_max]
        if seasons:
            placeholders = ", ".join("?" for _ in seasons)
            conditions.append(f"season_name IN ({placeholders})")
            params.extend(seasons)
        if home_away:
            placeholders = ", ".join("?" for _ in home_away)
            conditions.append(f"{self._derived_home_away_sql()} IN ({placeholders})")
            params.extend(home_away)

        with connection(settings=self.settings) as conn:
            rows = fetch_all(
                conn,
                f"""
                SELECT
                  start_time_utc,
                  round_label,
                  home_team,
                  venue,
                  weather_category,
                  away_team,
                  player_team,
                  {self._derived_opposition_sql()} AS derived_opposition_team,
                  {self._derived_home_away_sql()} AS derived_home_away,
                  margin,
                  tog_percentage,
                  disposals,
                  kicks,
                  handballs,
                  marks,
                  goals,
                  behinds,
                  tackles,
                  hitouts,
                  frees_for,
                  frees_against,
                  fantasy_points,
                  cba_percentage,
                  {stat_column} AS selected_value
                FROM player_game_logs
                WHERE {' AND '.join(conditions)}
                ORDER BY start_time_utc ASC
                """,
                params,
            )
        if not rows:
            return []

        shaped_rows: list[dict[str, Any]] = []
        for index, row in enumerate(rows, start=1):
            shaped_rows.append(
                {
                    "date": row["start_time_utc"],
                    "round_label": row["round_label"],
                    "home": row["home_team"],
                    "venue": row["venue"],
                    "weather": row["weather_category"],
                    "away": row["away_team"],
                    "team": row["player_team"],
                    "opposition": row["derived_opposition_team"],
                    "home_away": row["derived_home_away"],
                    "margin": row["margin"],
                    "tog": row["tog_percentage"],
                    "disposals": row["disposals"],
                    "kicks": row["kicks"],
                    "handballs": row["handballs"],
                    "marks": row["marks"],
                    "goals": row["goals"],
                    "behinds": row["behinds"],
                    "tackles": row["tackles"],
                    "hitouts": row["hitouts"],
                    "frees_for": row["frees_for"],
                    "frees_against": row["frees_against"],
                    "fantasy": row["fantasy_points"],
                    "cba": row["cba_percentage"],
                    "game_number": index,
                    "selected_stat": stat_label,
                    "selected_value": row["selected_value"],
                    "hit": self._line_hit(
                        value=row["selected_value"],
                        line_mode=line_mode,
                        reference_line=reference_line,
                        lower_bound=lower_bound,
                        upper_bound=upper_bound,
                    ),
                }
            )

        shaped_rows.sort(key=lambda item: item["date"], reverse=True)
        if last_games is not None:
            shaped_rows = shaped_rows[:last_games]
        if oppositions:
            shaped_rows = [row for row in shaped_rows if row["opposition"] in oppositions]
        if weather_categories:
            shaped_rows = [row for row in shaped_rows if row["weather"] in weather_categories]
        if venues:
            shaped_rows = [row for row in shaped_rows if row["venue"] in venues]
        return shaped_rows

    def _line_hit(
        self,
        *,
        value: Any,
        line_mode: str | None,
        reference_line: float | None,
        lower_bound: float | None,
        upper_bound: float | None,
    ) -> bool | None:
        if value is None or line_mode is None:
            return None
        numeric_value = float(value)
        if line_mode == "interval":
            if lower_bound is None or upper_bound is None:
                return None
            return lower_bound < numeric_value < upper_bound
        if reference_line is None:
            return None
        return numeric_value >= reference_line

    def _implied_odds(self, probability: float) -> float | None:
        if probability <= 0:
            return None
        return 1 / probability

    def _empty_player_stat_summary(
        self,
        *,
        player_id: int,
        stat_code: str,
        stat_label: str,
        line_mode: str,
        reference_line: float | None,
        lower_bound: float | None,
        upper_bound: float | None,
    ) -> dict[str, Any]:
        return {
            "player_id": player_id,
            "stat_code": stat_code,
            "stat_label": stat_label,
            "line_mode": line_mode,
            "reference_line": reference_line,
            "lower_bound": lower_bound,
            "upper_bound": upper_bound,
            "sample_size": 0,
            "proportion_over": None,
            "proportion_under": None,
            "implied_odds_over": None,
            "implied_odds_under": None,
            "proportion_within_interval": None,
            "proportion_outside_interval": None,
            "implied_odds_within_interval": None,
            "implied_odds_outside_interval": None,
        }

    def _shape_event(self, row: dict[str, Any]) -> dict[str, Any]:
        return {
            "id": row["id"],
            "match_name": row["match_name"],
            "start_time": row["start_time"],
            "round_label": row["round_label"],
            "venue": row["venue"],
            "home_team": {"id": row["home_team_id"], "name": row["home_team_name"]},
            "away_team": {"id": row["away_team_id"], "name": row["away_team_name"]},
            "available_bookmakers": [code for code in (row["available_bookmakers"] or "").split(",") if code],
        }

    def _shape_market(self, row: dict[str, Any]) -> dict[str, Any]:
        player = None
        if row["player_id"] is not None:
            player = {"id": row["player_id"], "full_name": row["player_name"]}
        return {
            "id": row["id"],
            "event_id": row["event_id"],
            "market_type_code": row["market_type_code"],
            "display_name": row["display_name"],
            "player": player,
            "line_value": row["line_value"],
            "bookmaker": row["bookmaker"],
            "available_selection_types": [
                selection_type
                for selection_type in (row["available_selection_types"] or "").split(",")
                if selection_type
            ],
        }

    def _shape_prop_result(self, row: dict[str, Any]) -> dict[str, Any]:
        player = None
        if row["player_id"] is not None:
            player = {"id": row["player_id"], "full_name": row["player_name"]}
        return {
            "selection_id": row["selection_id"],
            "event_id": row["event_id"],
            "match_name": row["match_name"],
            "start_time": row["start_time"],
            "bookmaker": row["bookmaker"],
            "market_type_code": row["market_type_code"],
            "player": player,
            "selection_type": row["selection_type"],
            "label": row["label"],
            "line_value": row["line_value"],
            "decimal_price": row["decimal_price"],
            "implied_prob": row["implied_prob"],
            "edge_pct": row["edge_pct"],
            "sgm_eligible": row["sgm_eligible"],
        }

    def _shape_odds_result(self, row: dict[str, Any]) -> dict[str, Any]:
        player = None
        if row["player_id"] is not None:
            player = {"id": row["player_id"], "full_name": row["player_name"]}
        return {
            "selection_id": row["selection_id"],
            "market_id": row["market_id"],
            "event_id": row["event_id"],
            "match_name": row["match_name"],
            "start_time": row["start_time"],
            "venue": row["venue"],
            "bookmaker": row["bookmaker"],
            "market_type_code": row["market_type_code"],
            "market_display_name": row["market_display_name"],
            "player": player,
            "player_team": row["player_team"],
            "player_home_away": row["player_home_away"],
            "player_team_line": row["player_team_line"],
            "selection_type": row["selection_type"],
            "label": row["label"],
            "line_value": row["line_value"],
            "decimal_price": row["decimal_price"],
            "implied_prob": row["implied_prob"],
            "edge_pct": row["edge_pct"],
            "diff_2025": row["diff_2025"],
            "diff_last_10": row["diff_last_10"],
            "home_away_diff": row["home_away_diff"],
            "win_loss_diff": row["win_loss_diff"],
            "player_position": row["player_position"],
            "matchup_difficulty": row["matchup_difficulty"],
            "over_matchup_difficulty": row["over_matchup_difficulty"],
            "under_matchup_difficulty": row["under_matchup_difficulty"],
            "dvp": row["dvp"],
            "raw_dvp": row["raw_dvp"],
            "dvp_standard_error": row["dvp_standard_error"],
            "dvp_bootstrap_ci_low": row["dvp_bootstrap_ci_low"],
            "dvp_bootstrap_ci_high": row["dvp_bootstrap_ci_high"],
            "dvp_sample_count": row["dvp_sample_count"],
            "dvp_match_count": row["dvp_match_count"],
            "dvp_observation_count": row["dvp_observation_count"],
            "dvp_model_version": row["dvp_model_version"],
            "dvp_generated_at": row["dvp_generated_at"],
            "weather": (
                {
                    "temperature_c": row["weather_temperature_c"],
                    "wind_kph": row["weather_wind_kph"],
                    "precip_probability": row["weather_precip_probability"],
                    "precip_mm": row["weather_precip_mm"],
                    "label": row["weather_label"],
                    "icon_code": row["weather_icon_code"],
                }
                if any(
                    row.get(key) is not None
                    for key in (
                        "weather_temperature_c",
                        "weather_wind_kph",
                        "weather_precip_probability",
                        "weather_precip_mm",
                        "weather_label",
                        "weather_icon_code",
                    )
                )
                else None
            ),
            "is_best_price": row["is_best_price"],
            "next_best_prob_diff": row["next_best_prob_diff"],
            "sgm_eligible": row["sgm_eligible"],
        }
