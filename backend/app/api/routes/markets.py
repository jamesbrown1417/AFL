from __future__ import annotations

from typing import Annotated

from fastapi import APIRouter, Depends, Query

from app.dependencies import get_query_service, require_auth
from app.models.api import MarketSummary, SelectionSummary
from app.services.query_service import QueryService


router = APIRouter(tags=["markets"], dependencies=[Depends(require_auth)])


@router.get("/events/{event_id}/markets", response_model=list[MarketSummary])
def list_markets(
    event_id: int,
    query_service: Annotated[QueryService, Depends(get_query_service)],
    bookmaker: str = Query(...),
    market_type: str | None = Query(default=None),
    player_q: str | None = Query(default=None),
    limit: int = Query(default=100, ge=1, le=500),
    offset: int = Query(default=0, ge=0),
) -> list[MarketSummary]:
    rows = query_service.list_markets(
        event_id=event_id,
        bookmaker=bookmaker,
        market_type=market_type,
        player_query=player_q,
        limit=limit,
        offset=offset,
    )
    return [MarketSummary.model_validate(row) for row in rows]


@router.get("/markets/{market_id}/selections", response_model=list[SelectionSummary])
def list_selections(
    market_id: int,
    query_service: Annotated[QueryService, Depends(get_query_service)],
    bookmaker: str = Query(...),
) -> list[SelectionSummary]:
    rows = query_service.list_market_selections(market_id=market_id, bookmaker=bookmaker)
    return [SelectionSummary.model_validate(row) for row in rows]
