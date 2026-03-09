from __future__ import annotations

from secrets import compare_digest
from typing import Annotated

from fastapi import Depends, Request
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
    request: Request,
) -> None:
    if not settings.enable_auth:
        return
    if not settings.auth_token:
        raise AppError(500, "auth_not_configured", "Auth is enabled but no token is configured.")
    if credentials is None or not compare_digest(credentials.credentials, settings.auth_token):
        raise AppError(401, "unauthorized", "Missing or invalid bearer token.")
    if not settings.require_tailscale_identity:
        return
    allowed_logins = settings.allowed_tailscale_user_logins_list
    if not allowed_logins:
        raise AppError(
            500,
            "tailscale_identity_not_configured",
            "Tailscale identity checks are enabled but no allowed logins are configured.",
        )
    login = request.headers.get(settings.tailscale_user_header_name)
    if not login:
        raise AppError(
            401,
            "tailscale_identity_missing",
            "Missing required Tailscale identity header.",
        )
    if login not in allowed_logins:
        raise AppError(
            403,
            "tailscale_identity_forbidden",
            "Tailscale identity is not allowed.",
            details={"login": login},
        )


def get_query_service(
    settings: Annotated[Settings, Depends(get_settings_dependency)],
) -> QueryService:
    return QueryService(settings)


def get_pricing_service(
    settings: Annotated[Settings, Depends(get_settings_dependency)],
) -> PricingService:
    return PricingService(settings)
