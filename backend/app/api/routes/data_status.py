from __future__ import annotations

from typing import Annotated

from fastapi import APIRouter, Depends

from app.dependencies import get_query_service, require_auth
from app.models.api import DataStatusResponse
from app.services.query_service import QueryService


router = APIRouter(tags=["data-status"], dependencies=[Depends(require_auth)])


@router.get("/data/status", response_model=DataStatusResponse)
def get_data_status(
    query_service: Annotated[QueryService, Depends(get_query_service)],
) -> DataStatusResponse:
    return DataStatusResponse.model_validate(query_service.get_data_status())
