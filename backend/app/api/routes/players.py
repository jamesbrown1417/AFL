from __future__ import annotations

from typing import Annotated

from fastapi import APIRouter, Depends, Query

from app.dependencies import get_query_service, require_auth
from app.models.api import PlayerSummary
from app.services.query_service import QueryService


router = APIRouter(tags=["players"], dependencies=[Depends(require_auth)])


@router.get("/players/search", response_model=list[PlayerSummary])
def search_players(
    query_service: Annotated[QueryService, Depends(get_query_service)],
    q: str = Query(..., min_length=1),
    limit: int = Query(default=20, ge=1, le=100),
) -> list[PlayerSummary]:
    rows = query_service.search_players(query=q, limit=limit)
    return [PlayerSummary.model_validate(row) for row in rows]
