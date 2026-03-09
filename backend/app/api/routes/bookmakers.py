from __future__ import annotations

from typing import Annotated

from fastapi import APIRouter, Depends

from app.dependencies import get_pricing_service, get_query_service, require_auth
from app.models.api import BookmakerSummary
from app.services.pricing_service import PricingService
from app.services.query_service import QueryService


router = APIRouter(tags=["bookmakers"], dependencies=[Depends(require_auth)])


@router.get("/bookmakers", response_model=list[BookmakerSummary])
def list_bookmakers(
    query_service: Annotated[QueryService, Depends(get_query_service)],
    pricing_service: Annotated[PricingService, Depends(get_pricing_service)],
) -> list[BookmakerSummary]:
    rows = query_service.list_bookmakers(pricing_service.live_pricing_codes)
    return [BookmakerSummary.model_validate(row) for row in rows]
