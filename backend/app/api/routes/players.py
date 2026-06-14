from __future__ import annotations

from typing import Annotated

from fastapi import APIRouter, Depends, Query

from app.dependencies import get_query_service, require_auth
from app.models.api import PlayerSummary
from app.services.query_service import QueryService
from app.utils.errors import AppError


router = APIRouter(tags=["players"], dependencies=[Depends(require_auth)])


@router.get("/players/search", response_model=list[PlayerSummary])
def search_players(
    query_service: Annotated[QueryService, Depends(get_query_service)],
    q: str = Query(default=""),
    limit: int = Query(default=50, ge=1, le=5000),
) -> list[PlayerSummary]:
    rows = query_service.search_players(query=q, limit=limit)
    return [PlayerSummary.model_validate(row) for row in rows]


@router.get("/players/stats/search", response_model=list[PlayerSummary])
def search_stat_players(
    query_service: Annotated[QueryService, Depends(get_query_service)],
    q: str = Query(default=""),
    limit: int = Query(default=50, ge=1, le=5000),
    stat: str = Query(default="disposals"),
    seasons: Annotated[list[str] | None, Query()] = None,
    oppositions: Annotated[list[str] | None, Query()] = None,
    venues: Annotated[list[str] | None, Query()] = None,
    weather_categories: Annotated[list[str] | None, Query()] = None,
    home_away: Annotated[list[str] | None, Query()] = None,
    margin_min: int = Query(default=-200),
    margin_max: int = Query(default=200),
    last_games: int | None = Query(default=None, ge=1, le=100),
    minutes_minimum: float = Query(default=0, ge=0),
) -> list[PlayerSummary]:
    rows = query_service.search_stat_players(
        query=q,
        limit=limit,
        stat=stat,
        seasons=seasons,
        oppositions=oppositions,
        venues=venues,
        weather_categories=weather_categories,
        home_away=home_away,
        margin_min=margin_min,
        margin_max=margin_max,
        last_games=last_games,
        minutes_minimum=minutes_minimum,
    )
    if rows is None:
        raise AppError(422, "invalid_stat", f"Unsupported stat '{stat}'.")
    return [PlayerSummary.model_validate(row) for row in rows]
