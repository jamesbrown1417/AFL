from __future__ import annotations

from typing import Annotated

from fastapi import APIRouter, Depends, Query

from app.dependencies import get_query_service, require_auth
from app.models.api import ArbSearchResult
from app.services.query_service import QueryService


router = APIRouter(tags=["arbs"], dependencies=[Depends(require_auth)])


@router.get("/arbs", response_model=list[ArbSearchResult])
def search_arbs(
    query_service: Annotated[QueryService, Depends(get_query_service)],
    q: str | None = Query(default=None),
    market: Annotated[list[str] | None, Query()] = None,
    agency: Annotated[list[str] | None, Query()] = None,
    min_margin: float = Query(default=-5),
    max_margin: float | None = Query(default=None),
    limit: int = Query(default=250, ge=1, le=5000),
    offset: int = Query(default=0, ge=0),
) -> list[ArbSearchResult]:
    rows = query_service.search_arbs(
        query=q,
        markets=market or [],
        agencies=agency or [],
        min_margin=min_margin,
        max_margin=max_margin,
        limit=limit,
        offset=offset,
    )
    return [ArbSearchResult.model_validate(row) for row in rows]
