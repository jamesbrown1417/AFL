from __future__ import annotations

from typing import Any

import httpx

from app.bookmakers.base import BookmakerAdapter, QuoteResult, ResolvedLeg
from app.config import Settings
from app.utils.errors import AppError


class SportsbetAdapter(BookmakerAdapter):
    code = "sportsbet"
    adapter_version = "v1"

    def __init__(self, settings: Settings):
        self.settings = settings

    def build_request(self, resolved_legs: list[ResolvedLeg]) -> dict[str, Any]:
        if not resolved_legs:
            raise AppError(422, "no_legs", "At least one selection is required for pricing.")

        first_leg = resolved_legs[0]
        class_external_id = (
            first_leg.selection_payload_meta.get("class_external_id")
            or first_leg.event_payload_meta.get("class_external_id")
        )
        competition_external_id = (
            first_leg.external_competition_id
            or first_leg.selection_payload_meta.get("competition_external_id")
        )
        event_external_id = first_leg.external_event_id

        if not class_external_id or not competition_external_id or not event_external_id:
            raise AppError(
                409,
                "sportsbet_metadata_incomplete",
                "Sportsbet selection metadata is incomplete for pricing.",
            )

        outcomes = []
        for leg in resolved_legs:
            if not leg.external_market_id or not leg.external_selection_id:
                raise AppError(
                    409,
                    "sportsbet_leg_not_priceable",
                    f"Selection {leg.selection_id} is missing Sportsbet market or outcome IDs.",
                )
            outcomes.append(
                {
                    "marketExternalId": self._coerce_numeric(leg.external_market_id),
                    "outcomeExternalId": self._coerce_numeric(leg.external_selection_id),
                }
            )

        return {
            "method": "POST",
            "url": self.settings.sportsbet_quote_url,
            "headers": {
                "User-Agent": self.settings.sportsbet_user_agent,
                "Content-Type": "application/json;charset=UTF-8",
            },
            "json": {
                "classExternalId": self._coerce_numeric(class_external_id),
                "competitionExternalId": self._coerce_numeric(competition_external_id),
                "eventExternalId": self._coerce_numeric(event_external_id),
                "outcomesExternalIds": outcomes,
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
                "sportsbet_upstream_error",
                "Sportsbet returned an error response.",
                retriable=response.status_code >= 500,
                details={"status_code": response.status_code, "body": response.text[:500]},
            ) from exc
        payload = response.json()
        if not isinstance(payload, dict):
            raise AppError(
                502,
                "sportsbet_invalid_payload",
                "Sportsbet returned an unexpected payload shape.",
                retriable=False,
            )
        return payload

    def parse_response(self, payload: dict[str, Any], resolved_legs: list[ResolvedLeg]) -> QuoteResult:
        del resolved_legs
        price = payload.get("price")
        if isinstance(price, dict) and {"numerator", "denominator"} <= set(price):
            denominator = float(price["denominator"])
            numerator = float(price["numerator"])
            quoted_price = 1 + (numerator / denominator)
        elif isinstance(price, (int, float)):
            quoted_price = float(price)
        else:
            raise AppError(
                502,
                "sportsbet_response_invalid",
                "Sportsbet response did not contain a usable price.",
                details={"response_keys": sorted(payload.keys())},
            )
        return QuoteResult(quoted_price=quoted_price, status="accepted", raw_response=payload)

    def _coerce_numeric(self, value: str) -> int | str:
        return int(value) if value.isdigit() else value
