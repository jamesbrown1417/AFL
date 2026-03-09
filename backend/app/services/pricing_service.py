from __future__ import annotations

import asyncio
import json
from datetime import timedelta
from math import prod
from typing import Any, cast
from uuid import uuid4

import httpx

from app.bookmakers.bet365 import Bet365Adapter
from app.bookmakers.base import BookmakerAdapter, QuoteResult, ResolvedLeg
from app.bookmakers.pointsbet import PointsbetAdapter
from app.bookmakers.sportsbet import SportsbetAdapter
from app.bookmakers.tab import TabAdapter
from app.config import Settings
from app.db.duckdb import connection, fetch_all, fetch_one
from app.models.api import CgmCompareRequest, RequestedLeg, SgmCompareRequest, SgmQuoteRequest
from app.utils.errors import AppError
from app.utils.hashing import sha256_text, stable_json_dumps
from app.utils.time import utc_now


class PricingService:
    def __init__(self, settings: Settings):
        self.settings = settings
        self.adapters: dict[str, BookmakerAdapter] = {
            "bet365": Bet365Adapter(settings),
            "pointsbet": PointsbetAdapter(settings),
            "sportsbet": SportsbetAdapter(settings),
            "tab": TabAdapter(settings),
        }

    @property
    def live_pricing_codes(self) -> set[str]:
        return set(self.adapters)

    async def quote_sgm(self, request: SgmQuoteRequest) -> dict[str, Any]:
        adapter = self.adapters.get(request.bookmaker)
        if adapter is None:
            raise AppError(422, "bookmaker_not_supported", f"{request.bookmaker} is not enabled for live pricing.")

        resolved_legs = self._resolve_legs(request)
        if not resolved_legs:
            raise AppError(422, "no_resolved_legs", "No pricing legs could be resolved.")

        cache_key = self._build_cache_key(adapter, request.event_id, resolved_legs)
        if not request.force_refresh:
            cached = self._get_cached_quote(cache_key)
            if cached is not None:
                cached["from_cache"] = True
                return cached

        request_spec = adapter.build_request(resolved_legs)
        quote_result = await self._request_quote_with_retry(
            adapter=adapter,
            request_spec=request_spec,
            resolved_legs=resolved_legs,
        )
        response = self._store_quote(
            adapter=adapter,
            cache_key=cache_key,
            request=request,
            resolved_legs=resolved_legs,
            quote_result=quote_result,
        )
        return response

    async def compare_sgm(self, request: SgmCompareRequest) -> dict[str, Any]:
        selection_ids = self._validate_sgm_compare_request(request)
        comparison_requests = [
            SgmQuoteRequest(
                bookmaker=bookmaker_code,
                event_id=request.event_id,
                legs=[RequestedLeg(selection_id=selection_id) for selection_id in selection_ids],
                force_refresh=request.force_refresh,
            )
            for bookmaker_code in sorted(self.live_pricing_codes)
        ]

        results: list[dict[str, Any]] = []
        for comparison_request in comparison_requests:
            try:
                quote = await self.quote_sgm(comparison_request)
            except Exception:
                continue
            results.append(quote)

        results.sort(key=lambda result: float(result["quoted_price"]), reverse=True)
        return {
            "event_id": request.event_id,
            "selection_count": len(selection_ids),
            "results": results,
        }

    async def _request_quote_with_retry(
        self,
        *,
        adapter: BookmakerAdapter,
        request_spec: dict[str, Any],
        resolved_legs: list[ResolvedLeg],
    ) -> QuoteResult:
        attempts = max(1, self.settings.sgm_retry_attempts)
        last_error: AppError | None = None
        for attempt in range(1, attempts + 1):
            try:
                async with httpx.AsyncClient(timeout=self.settings.request_timeout_seconds) as client:
                    await adapter.ensure_session(client)
                    payload = await adapter.send(client, request_spec)
                return adapter.parse_response(payload, resolved_legs)
            except httpx.TimeoutException as exc:
                last_error = AppError(
                    502,
                    f"{adapter.code}_timeout",
                    f"{adapter.code.upper()} pricing timed out.",
                    retriable=True,
                )
                if attempt >= attempts:
                    raise last_error from exc
                await asyncio.sleep(self.settings.sgm_retry_delay_seconds)
            except httpx.HTTPError as exc:
                last_error = AppError(
                    502,
                    f"{adapter.code}_transport_error",
                    f"{adapter.code.upper()} pricing request failed.",
                    retriable=True,
                    details={"error": str(exc)},
                )
                if attempt >= attempts:
                    raise last_error from exc
                await asyncio.sleep(self.settings.sgm_retry_delay_seconds)
            except AppError as exc:
                last_error = exc
                if not exc.retriable or attempt >= attempts:
                    raise
                await asyncio.sleep(self.settings.sgm_retry_delay_seconds)
        if last_error is not None:
            raise last_error
        raise AppError(502, "pricing_failed", "Pricing failed unexpectedly.", retriable=True)

    def _validate_sgm_compare_request(self, request: SgmCompareRequest) -> list[int]:
        selection_ids = list(dict.fromkeys(request.selection_ids))
        if len(selection_ids) < 2:
            raise AppError(422, "too_few_legs", "An SGM comparison requires at least two legs.")

        placeholders = ", ".join(["?"] * len(selection_ids))
        with connection(settings=self.settings) as conn:
            rows = fetch_all(
                conn,
                f"""
                SELECT
                  s.selection_id,
                  m.event_id
                FROM selections s
                JOIN markets m ON m.market_id = s.market_id
                WHERE s.selection_id IN ({placeholders})
                """,
                selection_ids,
            )

        found_ids = {int(row["selection_id"]) for row in rows}
        missing_ids = [selection_id for selection_id in selection_ids if selection_id not in found_ids]
        if missing_ids:
            raise AppError(
                404,
                "selection_not_found",
                "One or more selections were not found for SGM comparison.",
                details={"missing_selection_ids": missing_ids},
            )

        wrong_event_ids = [
            int(row["selection_id"])
            for row in rows
            if int(row["event_id"]) != request.event_id
        ]
        if wrong_event_ids:
            raise AppError(
                422,
                "mixed_event_not_allowed",
                "All pricing legs must belong to the requested event.",
                details={"selection_ids": wrong_event_ids, "expected_event_id": request.event_id},
            )

        return selection_ids

    def compare_cgm(self, request: CgmCompareRequest) -> dict[str, Any]:
        selection_ids = list(dict.fromkeys(request.selection_ids))
        if len(selection_ids) < 2:
            raise AppError(422, "too_few_legs", "A CGM comparison requires at least two legs.")

        placeholders = ", ".join(["?"] * len(selection_ids))
        with connection(settings=self.settings) as conn:
            base_rows = fetch_all(
                conn,
                f"""
                SELECT
                  s.selection_id,
                  m.event_id,
                  e.match_name,
                  m.market_type_code,
                  s.selection_type,
                  s.label
                FROM selections s
                JOIN markets m ON m.market_id = s.market_id
                JOIN events e ON e.event_id = m.event_id
                WHERE s.selection_id IN ({placeholders})
                """,
                selection_ids,
            )
            price_rows = fetch_all(
                conn,
                f"""
                SELECT
                  b.code AS bookmaker,
                  s.selection_id,
                  e.match_name,
                  m.market_type_code,
                  s.selection_type,
                  s.label,
                  cop.decimal_price AS base_price
                FROM selections s
                JOIN markets m ON m.market_id = s.market_id
                JOIN events e ON e.event_id = m.event_id
                JOIN selection_bookmaker_meta sbm ON sbm.selection_id = s.selection_id
                JOIN bookmakers b ON b.bookmaker_id = sbm.bookmaker_id
                JOIN current_outcome_prices_v cop
                  ON cop.selection_id = s.selection_id AND cop.bookmaker_id = sbm.bookmaker_id
                WHERE s.selection_id IN ({placeholders})
                  AND b.enabled = TRUE
                  AND cop.decimal_price IS NOT NULL
                ORDER BY b.display_name, e.start_time_utc NULLS LAST, e.match_name, s.selection_id
                """,
                selection_ids,
            )

        found_ids = {int(row["selection_id"]) for row in base_rows}
        missing_ids = [selection_id for selection_id in selection_ids if selection_id not in found_ids]
        if missing_ids:
            raise AppError(
                404,
                "selection_not_found",
                "One or more selections were not found for CGM comparison.",
                details={"missing_selection_ids": missing_ids},
            )

        non_player_ids = [
            int(row["selection_id"])
            for row in base_rows
            if not str(row["market_type_code"]).startswith("player_")
        ]
        if non_player_ids:
            raise AppError(
                422,
                "invalid_cgm_leg",
                "CGM comparison currently supports player props only.",
                details={"selection_ids": non_player_ids},
            )

        event_ids = [int(row["event_id"]) for row in base_rows]
        if len(set(event_ids)) != len(event_ids):
            duplicate_event_ids = sorted({event_id for event_id in event_ids if event_ids.count(event_id) > 1})
            raise AppError(
                422,
                "duplicate_game_legs",
                "Cross-game multis allow only one leg per match.",
                details={"event_ids": duplicate_event_ids},
            )

        rows_by_bookmaker: dict[str, dict[int, dict[str, Any]]] = {}
        for row in price_rows:
            rows_by_bookmaker.setdefault(row["bookmaker"], {})[int(row["selection_id"])] = row

        results: list[dict[str, Any]] = []
        for bookmaker, bookmaker_rows in rows_by_bookmaker.items():
            if len(bookmaker_rows) != len(selection_ids):
                continue
            ordered_rows = [bookmaker_rows[selection_id] for selection_id in selection_ids]
            quoted_price = prod(float(row["base_price"]) for row in ordered_rows)
            results.append(
                {
                    "bookmaker": bookmaker,
                    "quoted_price": quoted_price,
                    "selection_count": len(selection_ids),
                    "legs": [
                        {
                            "selection_id": int(row["selection_id"]),
                            "match_name": row["match_name"],
                            "label": row["label"],
                            "market_type_code": row["market_type_code"],
                            "selection_type": row["selection_type"],
                            "base_price": float(row["base_price"]),
                        }
                        for row in ordered_rows
                    ],
                }
            )

        results.sort(key=lambda row: row["quoted_price"], reverse=True)
        return {
            "selection_count": len(selection_ids),
            "results": results,
        }

    def get_quote(self, quote_id: str) -> dict[str, Any]:
        with connection(write=True, settings=self.settings) as conn:
            row = fetch_one(
                conn,
                "SELECT response_json FROM quote_cache WHERE quote_id = ?",
                [quote_id],
            )
        if row is None:
            raise AppError(404, "quote_not_found", f"Quote {quote_id} was not found.")
        response = self._json_load(row["response_json"])
        response["from_cache"] = False
        return response

    def _resolve_legs(self, request: SgmQuoteRequest) -> list[ResolvedLeg]:
        selection_ids = [leg.selection_id for leg in request.legs]
        if not selection_ids:
            raise AppError(422, "no_legs", "At least one pricing leg is required.")
        placeholders = ", ".join(["?"] * len(selection_ids))
        params: list[Any] = selection_ids + [request.bookmaker]
        with connection(settings=self.settings) as conn:
            rows = fetch_all(
                conn,
                f"""
                SELECT
                  s.selection_id,
                  s.market_id,
                  m.event_id,
                  m.market_type_code,
                  s.selection_type,
                  s.label,
                  cop.decimal_price AS base_price,
                  ebm.external_event_id,
                  ebm.external_competition_id,
                  ebm.payload_meta_json AS event_payload_meta_json,
                  sbm.external_market_id,
                  sbm.external_selection_id,
                  sbm.payload_meta_json AS selection_payload_meta_json,
                  sbm.sgm_eligible,
                  b.code AS bookmaker_code
                FROM selections s
                JOIN markets m ON m.market_id = s.market_id
                JOIN selection_bookmaker_meta sbm ON sbm.selection_id = s.selection_id
                JOIN bookmakers b ON b.bookmaker_id = sbm.bookmaker_id
                LEFT JOIN event_bookmaker_map ebm
                  ON ebm.event_id = m.event_id AND ebm.bookmaker_id = sbm.bookmaker_id
                LEFT JOIN current_outcome_prices_v cop
                  ON cop.selection_id = s.selection_id AND cop.bookmaker_id = sbm.bookmaker_id
                WHERE s.selection_id IN ({placeholders}) AND b.code = ?
                """,
                params,
            )

        by_selection_id = {int(row["selection_id"]): row for row in rows}
        resolved_legs: list[ResolvedLeg] = []
        for leg in request.legs:
            row = by_selection_id.get(leg.selection_id)
            if row is None:
                raise AppError(
                    404,
                    "selection_not_found",
                    f"Selection {leg.selection_id} is not available for bookmaker {request.bookmaker}.",
                )
            if int(row["event_id"]) != request.event_id:
                raise AppError(
                    422,
                    "mixed_event_not_allowed",
                    "All pricing legs must belong to the requested event.",
                    details={"selection_id": leg.selection_id, "expected_event_id": request.event_id, "actual_event_id": row["event_id"]},
                )
            if not row["sgm_eligible"]:
                raise AppError(
                    409,
                    "selection_not_priceable",
                    f"Selection {leg.selection_id} is not marked as SGM-eligible.",
                )
            if row["base_price"] is None:
                raise AppError(
                    409,
                    "selection_missing_price",
                    f"Selection {leg.selection_id} does not have a current local price.",
                )
            resolved_legs.append(
                ResolvedLeg(
                    selection_id=int(row["selection_id"]),
                    event_id=int(row["event_id"]),
                    market_id=int(row["market_id"]),
                    bookmaker_code=row["bookmaker_code"],
                    market_type_code=row["market_type_code"],
                    selection_type=row["selection_type"],
                    label=row["label"],
                    base_price=float(row["base_price"]),
                    external_event_id=row["external_event_id"],
                    external_competition_id=row["external_competition_id"],
                    external_market_id=row["external_market_id"],
                    external_selection_id=row["external_selection_id"],
                    event_payload_meta=self._json_load(row["event_payload_meta_json"]),
                    selection_payload_meta=self._json_load(row["selection_payload_meta_json"]),
                )
            )
        return resolved_legs

    def _build_cache_key(
        self, adapter: BookmakerAdapter, event_id: int, resolved_legs: list[ResolvedLeg]
    ) -> str:
        payload = {
            "bookmaker": adapter.code,
            "adapter_version": adapter.adapter_version,
            "event_id": event_id,
            "legs": sorted(adapter.leg_fingerprint(leg) for leg in resolved_legs),
        }
        return sha256_text(stable_json_dumps(payload))

    def _get_cached_quote(self, cache_key: str) -> dict[str, Any] | None:
        now = utc_now().replace(tzinfo=None)
        with connection(write=True, settings=self.settings) as conn:
            row = fetch_one(
                conn,
                """
                SELECT quote_id, response_json
                FROM quote_cache
                WHERE cache_key = ? AND expires_at > ?
                """,
                [cache_key, now],
            )
            if row is None:
                return None
            conn.execute(
                "UPDATE quote_cache SET last_hit_at = ?, hit_count = hit_count + 1 WHERE quote_id = ?",
                [now, row["quote_id"]],
            )
        return self._json_load(row["response_json"])

    def _store_quote(
        self,
        *,
        adapter: BookmakerAdapter,
        cache_key: str,
        request: SgmQuoteRequest,
        resolved_legs: list[ResolvedLeg],
        quote_result: QuoteResult,
    ) -> dict[str, Any]:
        quote_id = uuid4().hex
        quoted_at = utc_now().replace(tzinfo=None)
        expires_at = quoted_at + timedelta(seconds=self.settings.quote_ttl_seconds)
        unadjusted_price = prod(leg.base_price for leg in resolved_legs)
        adjustment_factor = quote_result.quoted_price / unadjusted_price if unadjusted_price else 0.0
        response = {
            "quote_id": quote_id,
            "bookmaker": request.bookmaker,
            "event_id": request.event_id,
            "legs": [
                {
                    "selection_id": leg.selection_id,
                    "label": leg.label,
                    "market_type_code": leg.market_type_code,
                    "selection_type": leg.selection_type,
                    "base_price": leg.base_price,
                }
                for leg in resolved_legs
            ],
            "unadjusted_price": unadjusted_price,
            "quoted_price": quote_result.quoted_price,
            "adjustment_factor": adjustment_factor,
            "from_cache": False,
            "quoted_at": quoted_at,
            "expires_at": expires_at,
            "status": quote_result.status,
        }
        with connection(write=True, settings=self.settings) as conn:
            bookmaker_row = fetch_one(conn, "SELECT bookmaker_id FROM bookmakers WHERE code = ?", [request.bookmaker])
            if bookmaker_row is None:
                raise AppError(500, "bookmaker_missing", f"Bookmaker {request.bookmaker} is not in the database.")
            conn.execute("DELETE FROM quote_cache WHERE cache_key = ?", [cache_key])
            conn.execute(
                """
                INSERT INTO quote_cache (
                  quote_id, cache_key, bookmaker_id, event_id, request_hash,
                  quoted_price, unadjusted_price, adjustment_factor, status,
                  response_json, raw_response_json, created_at, expires_at,
                  last_hit_at, hit_count, adapter_version
                )
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """,
                [
                    quote_id,
                    cache_key,
                    bookmaker_row["bookmaker_id"],
                    request.event_id,
                    cache_key,
                    quote_result.quoted_price,
                    unadjusted_price,
                    adjustment_factor,
                    quote_result.status,
                    stable_json_dumps(response),
                    stable_json_dumps(quote_result.raw_response),
                    quoted_at,
                    expires_at,
                    quoted_at,
                    1,
                    adapter.adapter_version,
                ],
            )
            for index, leg in enumerate(resolved_legs, start=1):
                conn.execute(
                    """
                    INSERT INTO quote_legs (quote_id, selection_id, base_price, leg_order, resolved_meta_json)
                    VALUES (?, ?, ?, ?, ?)
                    """,
                    [
                        quote_id,
                        leg.selection_id,
                        leg.base_price,
                        index,
                        stable_json_dumps(
                            {
                                "external_event_id": leg.external_event_id,
                                "external_competition_id": leg.external_competition_id,
                                "external_market_id": leg.external_market_id,
                                "external_selection_id": leg.external_selection_id,
                                "event_payload_meta": leg.event_payload_meta,
                                "selection_payload_meta": leg.selection_payload_meta,
                            }
                        ),
                    ],
                )
        return response

    def _json_load(self, value: Any) -> dict[str, Any]:
        if value is None:
            return {}
        if isinstance(value, dict):
            return value
        loaded = json.loads(value)
        return cast(dict[str, Any], loaded if isinstance(loaded, dict) else {})
