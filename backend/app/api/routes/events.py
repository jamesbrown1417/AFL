from __future__ import annotations

from typing import Annotated

from fastapi import APIRouter, Depends, Query

from app.dependencies import get_query_service, require_auth
from app.models.api import EventDetail, EventSummary
from app.services.query_service import QueryService
from app.utils.errors import AppError


router = APIRouter(tags=["events"], dependencies=[Depends(require_auth)])


@router.get("/events", response_model=list[EventSummary])
def list_events(
    query_service: Annotated[QueryService, Depends(get_query_service)],
    date_from: str | None = Query(default=None),
    date_to: str | None = Query(default=None),
    q: str | None = Query(default=None),
    bookmaker: str | None = Query(default=None),
    limit: int = Query(default=50, ge=1, le=500),
    offset: int = Query(default=0, ge=0),
) -> list[EventSummary]:
    rows = query_service.list_events(
        date_from=date_from,
        date_to=date_to,
        query=q,
        bookmaker=bookmaker,
        limit=limit,
        offset=offset,
    )
    return [EventSummary.model_validate(row) for row in rows]


@router.get("/events/{event_id}", response_model=EventDetail)
def get_event(
    event_id: int,
    query_service: Annotated[QueryService, Depends(get_query_service)],
) -> EventDetail:
    row = query_service.get_event(event_id)
    if row is None:
        raise AppError(404, "event_not_found", f"Event {event_id} was not found.")
    return EventDetail.model_validate(row)
