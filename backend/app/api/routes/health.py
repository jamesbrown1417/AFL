from __future__ import annotations

from typing import Annotated

from fastapi import APIRouter, Depends

from app.dependencies import get_query_service
from app.models.api import HealthResponse
from app.services.query_service import QueryService


router = APIRouter(tags=["health"])


@router.get("/health", response_model=HealthResponse)
def health(
    query_service: Annotated[QueryService, Depends(get_query_service)],
) -> HealthResponse:
    return HealthResponse.model_validate(query_service.get_health())
