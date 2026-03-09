from __future__ import annotations

from fastapi.security import HTTPAuthorizationCredentials
from starlette.requests import Request

from app.config import Settings
from app.dependencies import require_auth
from app.utils.errors import AppError


def build_request(headers: dict[str, str] | None = None) -> Request:
    request_headers = [(key.lower().encode("latin-1"), value.encode("latin-1")) for key, value in (headers or {}).items()]
    return Request({"type": "http", "headers": request_headers})


def build_settings(**overrides: object) -> Settings:
    return Settings(_env_file=None, **overrides)


def test_require_auth_allows_valid_bearer_without_tailscale_identity() -> None:
    settings = build_settings(enable_auth=True, auth_token="secret-token")

    require_auth(
        credentials=HTTPAuthorizationCredentials(scheme="Bearer", credentials="secret-token"),
        settings=settings,
        request=build_request(),
    )


def test_require_auth_rejects_missing_tailscale_identity_header() -> None:
    settings = build_settings(
        enable_auth=True,
        auth_token="secret-token",
        require_tailscale_identity=True,
        allowed_tailscale_user_logins="james@example.com",
    )

    try:
        require_auth(
            credentials=HTTPAuthorizationCredentials(scheme="Bearer", credentials="secret-token"),
            settings=settings,
            request=build_request(),
        )
    except AppError as exc:
        assert exc.status_code == 401
        assert exc.code == "tailscale_identity_missing"
    else:
        raise AssertionError("Expected missing Tailscale identity header to be rejected.")


def test_require_auth_rejects_unlisted_tailscale_identity() -> None:
    settings = build_settings(
        enable_auth=True,
        auth_token="secret-token",
        require_tailscale_identity=True,
        allowed_tailscale_user_logins="james@example.com",
    )

    try:
        require_auth(
            credentials=HTTPAuthorizationCredentials(scheme="Bearer", credentials="secret-token"),
            settings=settings,
            request=build_request({"Tailscale-User-Login": "other@example.com"}),
        )
    except AppError as exc:
        assert exc.status_code == 403
        assert exc.code == "tailscale_identity_forbidden"
    else:
        raise AssertionError("Expected unlisted Tailscale identity to be rejected.")


def test_require_auth_allows_listed_tailscale_identity() -> None:
    settings = build_settings(
        enable_auth=True,
        auth_token="secret-token",
        require_tailscale_identity=True,
        allowed_tailscale_user_logins="james@example.com",
    )

    require_auth(
        credentials=HTTPAuthorizationCredentials(scheme="Bearer", credentials="secret-token"),
        settings=settings,
        request=build_request({"Tailscale-User-Login": "james@example.com"}),
    )
