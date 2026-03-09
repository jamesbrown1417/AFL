from __future__ import annotations

from typing import Annotated

from fastapi import APIRouter, Depends, Query

from app.dependencies import get_query_service, require_auth
from app.models.api import PropSearchResult
from app.services.query_service import QueryService


router = APIRouter(tags=["props"], dependencies=[Depends(require_auth)])


@router.get("/props/search", response_model=list[PropSearchResult])
def search_props(
    query_service: Annotated[QueryService, Depends(get_query_service)],
    bookmaker: str = Query(...),
    q: str | None = Query(default=None),
    market_type: str | None = Query(default=None),
    event_id: int | None = Query(default=None),
    player_id: int | None = Query(default=None),
    date_from: str | None = Query(default=None),
    date_to: str | None = Query(default=None),
    min_edge: float | None = Query(default=None),
    limit: int = Query(default=100, ge=1, le=500),
    offset: int = Query(default=0, ge=0),
) -> list[PropSearchResult]:
    rows = query_service.search_props(
        bookmaker=bookmaker,
        query=q,
        market_type=market_type,
        event_id=event_id,
        player_id=player_id,
        date_from=date_from,
        date_to=date_to,
        min_edge=min_edge,
        limit=limit,
        offset=offset,
    )
    return [PropSearchResult.model_validate(row) for row in rows]
