from __future__ import annotations

from collections.abc import AsyncIterator
from contextlib import asynccontextmanager

from fastapi import FastAPI, Request
from fastapi.middleware.gzip import GZipMiddleware
from fastapi.responses import JSONResponse

from app.api.routes import arbs, bookmakers, data_status, events, health, markets, odds, player_stats, players, pricing, props
from app.config import get_settings
from app.db.duckdb import initialize_database
from app.utils.errors import AppError
from app.utils.logging import configure_logging


@asynccontextmanager
async def lifespan(_: FastAPI) -> AsyncIterator[None]:
    settings = get_settings()
    configure_logging(settings.log_path, debug=settings.debug)
    initialize_database(settings)
    yield


def create_app() -> FastAPI:
    settings = get_settings()
    app = FastAPI(title=settings.app_name, version="0.1.0", lifespan=lifespan)
    app.add_middleware(GZipMiddleware, minimum_size=1_000, compresslevel=5)
    for router in [
        health.router,
        arbs.router,
        data_status.router,
        bookmakers.router,
        events.router,
        markets.router,
        players.router,
        player_stats.router,
        odds.router,
        props.router,
        pricing.router,
    ]:
        app.include_router(router, prefix=settings.api_prefix)

    @app.exception_handler(AppError)
    async def handle_app_error(_: Request, exc: AppError) -> JSONResponse:
        return JSONResponse(status_code=exc.status_code, content=exc.to_payload())

    return app


app = create_app()
