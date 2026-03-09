from __future__ import annotations

from typing import Annotated, Literal

from fastapi import APIRouter, Depends, Query

from app.dependencies import get_query_service, require_auth
from app.models.api import PlayerGameLogEntry, PlayerStatFilterOptions, PlayerStatSummary
from app.services.query_service import QueryService
from app.utils.errors import AppError


router = APIRouter(tags=["player-stats"], dependencies=[Depends(require_auth)])


def _validate_line_inputs(
    *,
    line_mode: str,
    reference_line: float | None,
    lower_bound: float | None,
    upper_bound: float | None,
) -> None:
    if line_mode == "single" and reference_line is None:
        raise AppError(422, "reference_line_required", "reference_line is required for single mode.")
    if line_mode == "interval":
        if lower_bound is None or upper_bound is None:
            raise AppError(422, "interval_bounds_required", "lower_bound and upper_bound are required for interval mode.")
        if lower_bound >= upper_bound:
            raise AppError(422, "invalid_interval_bounds", "lower_bound must be less than upper_bound.")


@router.get("/players/{player_id}/stats/filters", response_model=PlayerStatFilterOptions)
def get_player_stat_filters(
    player_id: int,
    query_service: Annotated[QueryService, Depends(get_query_service)],
) -> PlayerStatFilterOptions:
    response = query_service.get_player_stat_filter_options(player_id=player_id)
    if response is None:
        raise AppError(404, "player_not_found", f"Player {player_id} was not found.")
    return PlayerStatFilterOptions.model_validate(response)


@router.get("/players/{player_id}/stats/history", response_model=list[PlayerGameLogEntry])
def get_player_stat_history(
    player_id: int,
    query_service: Annotated[QueryService, Depends(get_query_service)],
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
    line_mode: Literal["single", "interval"] | None = Query(default=None),
    reference_line: float | None = Query(default=None),
    lower_bound: float | None = Query(default=None),
    upper_bound: float | None = Query(default=None),
) -> list[PlayerGameLogEntry]:
    if line_mode is not None:
        _validate_line_inputs(
            line_mode=line_mode,
            reference_line=reference_line,
            lower_bound=lower_bound,
            upper_bound=upper_bound,
        )
    rows = query_service.get_player_stat_history(
        player_id=player_id,
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
        line_mode=line_mode,
        reference_line=reference_line,
        lower_bound=lower_bound,
        upper_bound=upper_bound,
    )
    if rows is None:
        raise AppError(422, "invalid_stat", f"Unsupported stat '{stat}'.")
    return [PlayerGameLogEntry.model_validate(row) for row in rows]


@router.get("/players/{player_id}/stats/summary", response_model=PlayerStatSummary)
def get_player_stat_summary(
    player_id: int,
    query_service: Annotated[QueryService, Depends(get_query_service)],
    stat: str = Query(default="disposals"),
    line_mode: Literal["single", "interval"] = Query(default="single"),
    reference_line: float | None = Query(default=None),
    lower_bound: float | None = Query(default=None),
    upper_bound: float | None = Query(default=None),
    seasons: Annotated[list[str] | None, Query()] = None,
    oppositions: Annotated[list[str] | None, Query()] = None,
    venues: Annotated[list[str] | None, Query()] = None,
    weather_categories: Annotated[list[str] | None, Query()] = None,
    home_away: Annotated[list[str] | None, Query()] = None,
    margin_min: int = Query(default=-200),
    margin_max: int = Query(default=200),
    last_games: int | None = Query(default=None, ge=1, le=100),
    minutes_minimum: float = Query(default=0, ge=0),
) -> PlayerStatSummary:
    _validate_line_inputs(
        line_mode=line_mode,
        reference_line=reference_line,
        lower_bound=lower_bound,
        upper_bound=upper_bound,
    )
    response = query_service.get_player_stat_summary(
        player_id=player_id,
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
        line_mode=line_mode,
        reference_line=reference_line,
        lower_bound=lower_bound,
        upper_bound=upper_bound,
    )
    if response is None:
        raise AppError(422, "invalid_stat", f"Unsupported stat '{stat}'.")
    return PlayerStatSummary.model_validate(response)
