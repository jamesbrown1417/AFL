from __future__ import annotations

from typing import Annotated

from fastapi import Depends
from fastapi.security import HTTPAuthorizationCredentials, HTTPBearer

from app.config import Settings, get_settings
from app.services.pricing_service import PricingService
from app.services.query_service import QueryService
from app.utils.errors import AppError


bearer_scheme = HTTPBearer(auto_error=False)


def get_settings_dependency() -> Settings:
    return get_settings()


def require_auth(
    credentials: Annotated[HTTPAuthorizationCredentials | None, Depends(bearer_scheme)],
    settings: Annotated[Settings, Depends(get_settings_dependency)],
) -> None:
    if not settings.enable_auth:
        return
    if not settings.auth_token:
        raise AppError(500, "auth_not_configured", "Auth is enabled but no token is configured.")
    if credentials is None or credentials.credentials != settings.auth_token:
        raise AppError(401, "unauthorized", "Missing or invalid bearer token.")


def get_query_service(
    settings: Annotated[Settings, Depends(get_settings_dependency)],
) -> QueryService:
    return QueryService(settings)


def get_pricing_service(
    settings: Annotated[Settings, Depends(get_settings_dependency)],
) -> PricingService:
    return PricingService(settings)
