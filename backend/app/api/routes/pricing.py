from __future__ import annotations

from typing import Annotated

from fastapi import APIRouter, Depends

from app.dependencies import get_pricing_service, require_auth
from app.models.api import SgmQuoteRequest, SgmQuoteResponse
from app.services.pricing_service import PricingService


router = APIRouter(tags=["pricing"], dependencies=[Depends(require_auth)])


@router.post("/pricing/sgm", response_model=SgmQuoteResponse)
async def price_sgm(
    request: SgmQuoteRequest,
    pricing_service: Annotated[PricingService, Depends(get_pricing_service)],
) -> SgmQuoteResponse:
    response = await pricing_service.quote_sgm(request)
    return SgmQuoteResponse.model_validate(response)


@router.get("/quotes/{quote_id}", response_model=SgmQuoteResponse)
def get_quote(
    quote_id: str,
    pricing_service: Annotated[PricingService, Depends(get_pricing_service)],
) -> SgmQuoteResponse:
    response = pricing_service.get_quote(quote_id)
    return SgmQuoteResponse.model_validate(response)
