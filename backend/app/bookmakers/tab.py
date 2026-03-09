from __future__ import annotations

import json
from typing import Any

import httpx

from app.bookmakers.base import BookmakerAdapter, QuoteResult, ResolvedLeg
from app.config import Settings
from app.utils.errors import AppError


class TabAdapter(BookmakerAdapter):
    code = "tab"
    adapter_version = "v2"

    def __init__(self, settings: Settings):
        self.settings = settings

    def build_request(self, resolved_legs: list[ResolvedLeg]) -> dict[str, Any]:
        if not resolved_legs:
            raise AppError(422, "no_legs", "At least one selection is required for pricing.")

        propositions = []
        for leg in resolved_legs:
            proposition_id = leg.external_selection_id or leg.selection_payload_meta.get("prop_id")
            if not proposition_id:
                raise AppError(
                    409,
                    "tab_leg_not_priceable",
                    f"Selection {leg.selection_id} is missing a TAB proposition ID.",
                )
            propositions.append(
                {
                    "type": "WIN",
                    "propositionId": self._coerce_numeric(str(proposition_id)),
                }
            )

        return {
            "method": "POST",
            "url": self.settings.tab_quote_url,
            "headers": {
                "accept": "application/json, text/plain, */*",
                "accept-language": "en-US,en;q=0.9",
                "content-type": "application/json;charset=UTF-8",
                "origin": self.settings.tab_origin,
                "referer": self.settings.tab_referer,
                "user-agent": self.settings.tab_user_agent,
            },
            "body": json.dumps(
                {
                    "clientDetails": {
                        "jurisdiction": self.settings.tab_jurisdiction,
                        "channel": self.settings.tab_channel,
                    },
                    "bets": [
                        {
                            "type": "FIXED_ODDS",
                            "legs": [
                                {
                                    "type": "SAME_GAME_MULTI",
                                    "propositions": propositions,
                                }
                            ],
                        }
                    ],
                    "returnValidationMatrix": True,
                },
                separators=(",", ":"),
            ),
        }

    async def ensure_session(self, client: httpx.AsyncClient) -> None:
        if getattr(client, "_afl_tab_bootstrapped", False):
            return
        response = await client.get(
            self.settings.tab_bootstrap_url,
            headers={
                "user-agent": self.settings.tab_user_agent,
                "accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
                "accept-language": "en-US,en;q=0.9",
            },
            follow_redirects=True,
            timeout=self.settings.tab_request_timeout_seconds,
        )
        response.raise_for_status()
        setattr(client, "_afl_tab_bootstrapped", True)

    async def send(self, client: httpx.AsyncClient, request_spec: dict[str, Any]) -> dict[str, Any]:
        response = await client.request(
            method=request_spec["method"],
            url=request_spec["url"],
            headers=request_spec["headers"],
            content=request_spec["body"],
            timeout=self.settings.tab_request_timeout_seconds,
        )
        try:
            response.raise_for_status()
        except httpx.HTTPStatusError as exc:
            raise AppError(
                502,
                "tab_upstream_error",
                "TAB returned an error response.",
                retriable=response.status_code >= 500,
                details={"status_code": response.status_code, "body": response.text[:500]},
            ) from exc
        payload = response.json()
        if not isinstance(payload, dict):
            raise AppError(
                502,
                "tab_invalid_payload",
                "TAB returned an unexpected payload shape.",
            )
        return payload

    def parse_response(self, payload: dict[str, Any], resolved_legs: list[ResolvedLeg]) -> QuoteResult:
        del resolved_legs
        status = str(payload.get("bets", [{}])[0].get("status", "")).lower()
        if status and status != "ok":
            raise AppError(
                409,
                "tab_bet_rejected",
                "TAB did not accept the requested combination.",
                details={"status": status},
            )
        try:
            quoted_price = float(payload["bets"][0]["legs"][0]["odds"]["decimal"])
        except (KeyError, IndexError, TypeError, ValueError) as exc:
            raise AppError(
                502,
                "tab_response_invalid",
                "TAB response did not contain a usable price.",
                details={"response_keys": sorted(payload.keys())},
            ) from exc
        return QuoteResult(quoted_price=quoted_price, status="accepted", raw_response=payload)

    def _coerce_numeric(self, value: str) -> int | str:
        return int(value) if value.isdigit() else value
