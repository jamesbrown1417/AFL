from __future__ import annotations

from typing import Any

from app.config import Settings
from app.db.duckdb import connection, fetch_all, fetch_one


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


class QueryService:
    def __init__(self, settings: Settings):
        self.settings = settings

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
                """
                SELECT DISTINCT opposition_team
                FROM player_game_logs
                WHERE player_id = ? AND opposition_team IS NOT NULL
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
        query: str | None,
        market_type: str | None,
        event_id: int | None,
        selection_type: str | None,
        date_from: str | None,
        date_to: str | None,
        min_edge: float | None,
        min_price: float | None,
        max_price: float | None,
        sgm_only: bool,
        best_only: bool,
        limit: int,
        offset: int,
    ) -> list[dict[str, Any]]:
        conditions: list[str] = ["m.player_id IS NOT NULL"]
        params: list[Any] = []
        if bookmakers:
            placeholders = ", ".join("?" for _ in bookmakers)
            conditions.append(f"b.code IN ({placeholders})")
            params.extend(bookmakers)
        if query:
            conditions.append(
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
            params.extend([q, q, q, q])
        if market_type:
            conditions.append("m.market_type_code = ?")
            params.append(market_type)
        if event_id:
            conditions.append("m.event_id = ?")
            params.append(event_id)
        if selection_type:
            conditions.append("s.selection_type = ?")
            params.append(selection_type)
        if date_from:
            conditions.append("e.start_time_utc >= ?")
            params.append(date_from)
        if date_to:
            conditions.append("e.start_time_utc <= ?")
            params.append(date_to)
        if min_edge is not None:
            conditions.append("COALESCE(lm.edge_pct, -1000000) >= ?")
            params.append(min_edge)
        if min_price is not None:
            conditions.append("cop.decimal_price >= ?")
            params.append(min_price)
        if max_price is not None:
            conditions.append("cop.decimal_price <= ?")
            params.append(max_price)
        if sgm_only:
            conditions.append("sbm.sgm_eligible = TRUE")

        where_clause = f"WHERE {' AND '.join(conditions)}" if conditions else ""
        best_clause = "WHERE best_rank = 1" if best_only else ""
        params.extend([limit, offset])

        with connection(settings=self.settings) as conn:
            rows = fetch_all(
                conn,
                f"""
                WITH ranked_odds AS (
                  SELECT
                    s.selection_id,
                    s.market_id,
                    e.event_id,
                    e.match_name,
                    e.start_time_utc AS start_time,
                    b.code AS bookmaker,
                    m.market_type_code,
                    m.market_name_raw AS market_display_name,
                    p.player_id,
                    p.full_name AS player_name,
                    s.selection_type,
                    s.label,
                    m.line_value,
                    cop.decimal_price,
                    cop.implied_prob,
                    lm.edge_pct,
                    TRY_CAST(json_extract(lm.metrics_json, '$.diff_2025') AS DOUBLE) AS diff_2025,
                    TRY_CAST(json_extract(lm.metrics_json, '$.diff_last_10') AS DOUBLE) AS diff_last_10,
                    sbm.sgm_eligible,
                    ROW_NUMBER() OVER (
                      PARTITION BY s.selection_id
                      ORDER BY cop.decimal_price DESC NULLS LAST, b.code ASC
                    ) AS best_rank
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
                  {where_clause}
                )
                SELECT
                  selection_id,
                  market_id,
                  event_id,
                  match_name,
                  start_time,
                  bookmaker,
                  market_type_code,
                  market_display_name,
                  player_id,
                  player_name,
                  selection_type,
                  label,
                  line_value,
                  decimal_price,
                  implied_prob,
                  edge_pct,
                  diff_2025,
                  diff_last_10,
                  sgm_eligible
                FROM ranked_odds
                {best_clause}
                ORDER BY
                  start_time NULLS LAST,
                  event_id,
                  COALESCE(player_name, ''),
                  market_display_name,
                  selection_type,
                  bookmaker
                LIMIT ? OFFSET ?
                """,
                params,
            )
        return [self._shape_odds_result(row) for row in rows]

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
            conditions.append(f"home_away IN ({placeholders})")
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
                  opposition_team,
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
                    "opposition": row["opposition_team"],
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
            "diff_2025": row["diff_2025"],
            "diff_last_10": row["diff_last_10"],
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
            "bookmaker": row["bookmaker"],
            "market_type_code": row["market_type_code"],
            "market_display_name": row["market_display_name"],
            "player": player,
            "selection_type": row["selection_type"],
            "label": row["label"],
            "line_value": row["line_value"],
            "decimal_price": row["decimal_price"],
            "implied_prob": row["implied_prob"],
            "edge_pct": row["edge_pct"],
            "diff_2025": row["diff_2025"],
            "diff_last_10": row["diff_last_10"],
            "sgm_eligible": row["sgm_eligible"],
        }
