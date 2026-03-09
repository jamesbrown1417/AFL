from __future__ import annotations

from typing import Annotated

from fastapi import APIRouter, Depends

from app.dependencies import get_pricing_service, require_auth
from app.models.api import (
    CgmCompareRequest,
    CgmCompareResponse,
    SgmCompareRequest,
    SgmCompareResponse,
    SgmQuoteRequest,
    SgmQuoteResponse,
)
from app.services.pricing_service import PricingService


router = APIRouter(tags=["pricing"], dependencies=[Depends(require_auth)])


@router.post("/pricing/sgm", response_model=SgmQuoteResponse)
async def price_sgm(
    request: SgmQuoteRequest,
    pricing_service: Annotated[PricingService, Depends(get_pricing_service)],
) -> SgmQuoteResponse:
    response = await pricing_service.quote_sgm(request)
    return SgmQuoteResponse.model_validate(response)


@router.post("/pricing/sgm/compare", response_model=SgmCompareResponse)
async def compare_sgm(
    request: SgmCompareRequest,
    pricing_service: Annotated[PricingService, Depends(get_pricing_service)],
) -> SgmCompareResponse:
    response = await pricing_service.compare_sgm(request)
    return SgmCompareResponse.model_validate(response)


@router.post("/pricing/cgm", response_model=CgmCompareResponse)
def compare_cgm(
    request: CgmCompareRequest,
    pricing_service: Annotated[PricingService, Depends(get_pricing_service)],
) -> CgmCompareResponse:
    response = pricing_service.compare_cgm(request)
    return CgmCompareResponse.model_validate(response)


@router.get("/quotes/{quote_id}", response_model=SgmQuoteResponse)
def get_quote(
    quote_id: str,
    pricing_service: Annotated[PricingService, Depends(get_pricing_service)],
) -> SgmQuoteResponse:
    response = pricing_service.get_quote(quote_id)
    return SgmQuoteResponse.model_validate(response)
