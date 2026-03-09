from __future__ import annotations

from functools import lru_cache
from pathlib import Path

from pydantic import Field
from pydantic_settings import BaseSettings, SettingsConfigDict


REPO_ROOT = Path(__file__).resolve().parents[2]
BACKEND_ROOT = Path(__file__).resolve().parents[1]


class Settings(BaseSettings):
    model_config = SettingsConfigDict(
        env_prefix="AFL_",
        env_file=BACKEND_ROOT / ".env",
        extra="ignore",
    )

    app_name: str = "AFL Backend"
    api_prefix: str = "/api/v1"
    debug: bool = False
    enable_auth: bool = True
    auth_token: str | None = None
    scraped_odds_dir: Path = Field(default=REPO_ROOT / "Data" / "scraped_odds")
    processed_odds_dir: Path = Field(default=REPO_ROOT / "Data" / "processed_odds")
    fixture_path: Path = Field(default=REPO_ROOT / "Data" / "current_fixture.csv")
    runtime_dir: Path = Field(default=REPO_ROOT / "runtime")
    duckdb_path: Path = Field(default=REPO_ROOT / "runtime" / "duckdb" / "afl.duckdb")
    log_path: Path = Field(default=REPO_ROOT / "runtime" / "logs" / "backend.log")
    bind_host: str = "127.0.0.1"
    bind_port: int = 8000
    quote_ttl_seconds: int = 30
    request_timeout_seconds: float = 10.0
    sportsbet_quote_url: str = "https://www.sportsbet.com.au/apigw/multi-pricer/combinations/price"
    sportsbet_user_agent: str = (
        "Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) "
        "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Mobile Safari/537.36"
    )

    @property
    def runtime_duckdb_dir(self) -> Path:
        return self.runtime_dir / "duckdb"

    @property
    def runtime_log_dir(self) -> Path:
        return self.runtime_dir / "logs"

    @property
    def runtime_cache_dir(self) -> Path:
        return self.runtime_dir / "cache"

    def ensure_runtime_dirs(self) -> None:
        self.runtime_duckdb_dir.mkdir(parents=True, exist_ok=True)
        self.runtime_log_dir.mkdir(parents=True, exist_ok=True)
        self.runtime_cache_dir.mkdir(parents=True, exist_ok=True)


@lru_cache(maxsize=1)
def get_settings() -> Settings:
    settings = Settings()
    settings.ensure_runtime_dirs()
    return settings
