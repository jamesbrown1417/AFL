from __future__ import annotations

from typing import Any

import httpx

from app.bookmakers.base import BookmakerAdapter, QuoteResult, ResolvedLeg
from app.config import Settings
from app.utils.errors import AppError


class PointsbetAdapter(BookmakerAdapter):
    code = "pointsbet"
    adapter_version = "v1"

    def __init__(self, settings: Settings):
        self.settings = settings

    def build_request(self, resolved_legs: list[ResolvedLeg]) -> dict[str, Any]:
        if not resolved_legs:
            raise AppError(422, "no_legs", "At least one selection is required for pricing.")

        event_key = resolved_legs[0].external_event_id or resolved_legs[0].selection_payload_meta.get("EventKey")
        if not event_key:
            raise AppError(
                409,
                "pointsbet_metadata_incomplete",
                "PointsBet selection metadata is incomplete for pricing.",
            )

        selected_outcomes = []
        for leg in resolved_legs:
            market_key = leg.external_market_id or leg.selection_payload_meta.get("MarketKey")
            outcome_key = leg.external_selection_id or leg.selection_payload_meta.get("OutcomeKey")
            if not market_key or not outcome_key:
                raise AppError(
                    409,
                    "pointsbet_leg_not_priceable",
                    f"Selection {leg.selection_id} is missing PointsBet market or outcome IDs.",
                )
            selected_outcomes.append(
                {
                    "MarketKey": self._coerce_numeric(str(market_key)),
                    "OutcomeKey": self._coerce_numeric(str(outcome_key)),
                }
            )

        return {
            "method": "POST",
            "url": self.settings.pointsbet_quote_url,
            "headers": {
                "User-Agent": self.settings.pointsbet_user_agent,
                "Content-Type": "application/json;charset=UTF-8",
                "Origin": self.settings.pointsbet_origin,
                "Referer": self.settings.pointsbet_referer,
            },
            "json": {
                "EventKey": self._coerce_numeric(str(event_key)),
                "SelectedOutcomes": selected_outcomes,
            },
        }

    async def send(self, client: httpx.AsyncClient, request_spec: dict[str, Any]) -> dict[str, Any]:
        response = await client.request(
            method=request_spec["method"],
            url=request_spec["url"],
            headers=request_spec["headers"],
            json=request_spec["json"],
        )
        try:
            response.raise_for_status()
        except httpx.HTTPStatusError as exc:
            raise AppError(
                502,
                "pointsbet_upstream_error",
                "PointsBet returned an error response.",
                retriable=response.status_code >= 500,
                details={"status_code": response.status_code, "body": response.text[:500]},
            ) from exc
        payload = response.json()
        if not isinstance(payload, dict):
            raise AppError(
                502,
                "pointsbet_invalid_payload",
                "PointsBet returned an unexpected payload shape.",
            )
        return payload

    def parse_response(self, payload: dict[str, Any], resolved_legs: list[ResolvedLeg]) -> QuoteResult:
        del resolved_legs
        price = payload.get("price")
        if price is None:
            raise AppError(
                502,
                "pointsbet_response_invalid",
                "PointsBet response did not contain a usable price.",
                details={"response_keys": sorted(payload.keys())},
            )
        try:
            quoted_price = float(price)
        except (TypeError, ValueError) as exc:
            raise AppError(
                502,
                "pointsbet_response_invalid",
                "PointsBet response did not contain a usable price.",
                details={"response_keys": sorted(payload.keys())},
            ) from exc
        return QuoteResult(quoted_price=quoted_price, status="accepted", raw_response=payload)

    def _coerce_numeric(self, value: str) -> int | str:
        return int(value) if value.isdigit() else value
