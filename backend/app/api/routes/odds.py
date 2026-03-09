from __future__ import annotations

from typing import Annotated, Literal

from fastapi import APIRouter, Depends, Query

from app.dependencies import get_query_service, require_auth
from app.models.api import OddsSearchResult
from app.services.query_service import QueryService


router = APIRouter(tags=["odds"], dependencies=[Depends(require_auth)])


@router.get("/odds/search", response_model=list[OddsSearchResult])
def search_odds(
    query_service: Annotated[QueryService, Depends(get_query_service)],
    bookmaker: Annotated[list[str] | None, Query()] = None,
    scope: Literal["player", "match", "all"] = Query(default="player"),
    q: str | None = Query(default=None),
    market_type: str | None = Query(default=None),
    event_id: int | None = Query(default=None),
    include_player_id: Annotated[list[int] | None, Query()] = None,
    exclude_player_id: Annotated[list[int] | None, Query()] = None,
    selection_type: str | None = Query(default=None),
    date_from: str | None = Query(default=None),
    date_to: str | None = Query(default=None),
    min_edge: float | None = Query(default=None),
    min_price: float | None = Query(default=None),
    max_price: float | None = Query(default=None),
    min_diff_2025: float | None = Query(default=None),
    max_diff_2025: float | None = Query(default=None),
    min_diff_last_10: float | None = Query(default=None),
    max_diff_last_10: float | None = Query(default=None),
    min_next_best_prob_diff: float | None = Query(default=None),
    max_next_best_prob_diff: float | None = Query(default=None),
    sgm_only: bool = Query(default=False),
    best_only: bool = Query(default=False),
    sort_by: str = Query(default="diff_last_10"),
    sort_dir: Literal["asc", "desc"] = Query(default="desc"),
    limit: int = Query(default=100, ge=1, le=5000),
    offset: int = Query(default=0, ge=0),
) -> list[OddsSearchResult]:
    rows = query_service.search_odds(
        bookmakers=bookmaker or [],
        scope=scope,
        query=q,
        market_type=market_type,
        event_id=event_id,
        include_player_ids=include_player_id or [],
        exclude_player_ids=exclude_player_id or [],
        selection_type=selection_type,
        date_from=date_from,
        date_to=date_to,
        min_edge=min_edge,
        min_price=min_price,
        max_price=max_price,
        min_diff_2025=min_diff_2025,
        max_diff_2025=max_diff_2025,
        min_diff_last_10=min_diff_last_10,
        max_diff_last_10=max_diff_last_10,
        min_next_best_prob_diff=min_next_best_prob_diff,
        max_next_best_prob_diff=max_next_best_prob_diff,
        sgm_only=sgm_only,
        best_only=best_only,
        sort_by=sort_by,
        sort_dir=sort_dir,
        limit=limit,
        offset=offset,
    )
    return [OddsSearchResult.model_validate(row) for row in rows]
