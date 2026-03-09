from __future__ import annotations

from math import prod
from typing import Any

import httpx

from app.bookmakers.base import BookmakerAdapter, QuoteResult, ResolvedLeg
from app.config import Settings
from app.utils.errors import AppError


class Bet365Adapter(BookmakerAdapter):
    code = "bet365"
    adapter_version = "v1"

    def __init__(self, settings: Settings):
        self.settings = settings

    def build_request(self, resolved_legs: list[ResolvedLeg]) -> dict[str, Any]:
        if not resolved_legs:
            raise AppError(422, "no_legs", "At least one selection is required for pricing.")
        return {
            "legs": [
                {
                    "selection_id": leg.selection_id,
                    "base_price": leg.base_price,
                }
                for leg in resolved_legs
            ]
        }

    async def send(self, client: httpx.AsyncClient, request_spec: dict[str, Any]) -> dict[str, Any]:
        del client
        base_prices = [float(leg["base_price"]) for leg in request_spec["legs"]]
        unadjusted_price = prod(base_prices)
        if unadjusted_price <= 0:
            raise AppError(
                409,
                "bet365_invalid_unadjusted_price",
                "Bet365 legs do not have a valid local unadjusted price.",
            )
        quoted_price = round(1 / (self.settings.bet365_sgm_margin + (1 / unadjusted_price)), 2)
        return {
            "pricing_mode": "formula",
            "unadjusted_price": unadjusted_price,
            "quoted_price": quoted_price,
            "margin": self.settings.bet365_sgm_margin,
        }

    def parse_response(self, payload: dict[str, Any], resolved_legs: list[ResolvedLeg]) -> QuoteResult:
        del resolved_legs
        try:
            quoted_price = float(payload["quoted_price"])
        except (KeyError, TypeError, ValueError) as exc:
            raise AppError(
                502,
                "bet365_response_invalid",
                "Bet365 pricing did not produce a usable price.",
                details={"response_keys": sorted(payload.keys())},
            ) from exc
        return QuoteResult(quoted_price=quoted_price, status="accepted", raw_response=payload)
