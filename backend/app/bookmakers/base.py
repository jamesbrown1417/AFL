from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Any

import httpx

from app.utils.hashing import stable_json_dumps


@dataclass(frozen=True, slots=True)
class ResolvedLeg:
    selection_id: int
    event_id: int
    market_id: int
    bookmaker_code: str
    market_type_code: str
    selection_type: str
    label: str
    base_price: float
    external_event_id: str | None
    external_competition_id: str | None
    external_market_id: str | None
    external_selection_id: str | None
    event_payload_meta: dict[str, Any]
    selection_payload_meta: dict[str, Any]


@dataclass(frozen=True, slots=True)
class QuoteResult:
    quoted_price: float
    status: str
    raw_response: dict[str, Any]


class BookmakerAdapter(ABC):
    code: str
    adapter_version: str = "v1"

    @abstractmethod
    def build_request(self, resolved_legs: list[ResolvedLeg]) -> dict[str, Any]:
        raise NotImplementedError

    @abstractmethod
    async def send(self, client: httpx.AsyncClient, request_spec: dict[str, Any]) -> dict[str, Any]:
        raise NotImplementedError

    @abstractmethod
    def parse_response(self, payload: dict[str, Any], resolved_legs: list[ResolvedLeg]) -> QuoteResult:
        raise NotImplementedError

    async def ensure_session(self, client: httpx.AsyncClient) -> None:
        return None

    def leg_fingerprint(self, leg: ResolvedLeg) -> str:
        payload = {
            "bookmaker_code": leg.bookmaker_code,
            "event_id": leg.event_id,
            "selection_id": leg.selection_id,
            "external_event_id": leg.external_event_id,
            "external_market_id": leg.external_market_id,
            "external_selection_id": leg.external_selection_id,
            "selection_payload_meta": leg.selection_payload_meta,
        }
        return stable_json_dumps(payload)
