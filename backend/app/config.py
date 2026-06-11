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
    require_tailscale_identity: bool = False
    allowed_tailscale_user_logins: str = ""
    tailscale_user_header_name: str = "Tailscale-User-Login"
    scraped_odds_dir: Path = Field(default=REPO_ROOT / "Data" / "scraped_odds")
    processed_odds_dir: Path = Field(default=REPO_ROOT / "Data" / "processed_odds")
    fixture_path: Path = Field(default=REPO_ROOT / "Data" / "current_fixture.csv")
    dvp_data_path: Path = Field(default=REPO_ROOT / "DVP" / "dvp_data.csv")
    player_positions_path: Path = Field(default=REPO_ROOT / "DVP" / "AFL-Players-Positions-2026.csv")
    runtime_dir: Path = Field(default=REPO_ROOT / "runtime")
    duckdb_path: Path = Field(default=REPO_ROOT / "runtime" / "duckdb" / "afl.duckdb")
    log_path: Path = Field(default=REPO_ROOT / "runtime" / "logs" / "backend.log")
    bind_host: str = "127.0.0.1"
    bind_port: int = 8000
    quote_ttl_seconds: int = 30
    request_timeout_seconds: float = 10.0
    weather_request_timeout_seconds: float = 10.0
    weather_cache_ttl_seconds: int = 7200
    weather_forecast_days: int = 16
    weather_refresh_hours_before_event: int = 6
    weather_api_url: str = "https://api.open-meteo.com/v1/forecast"
    sgm_retry_attempts: int = 3
    sgm_retry_delay_seconds: float = 0.35
    sportsbet_quote_url: str = "https://www.sportsbet.com.au/apigw/multi-pricer/combinations/price"
    sportsbet_user_agent: str = (
        "Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) "
        "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Mobile Safari/537.36"
    )
    tab_bootstrap_url: str = "https://www.tab.com.au/"
    tab_quote_url: str = "https://api.beta.tab.com.au/v1/pricing-service/enquiry"
    tab_jurisdiction: str = "NSW"
    tab_channel: str = "web"
    tab_request_timeout_seconds: float = 30.0
    tab_origin: str = "https://www.tab.com.au"
    tab_referer: str = "https://www.tab.com.au/"
    tab_user_agent: str = (
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/136.0.0.0 Safari/537.36"
    )
    pointsbet_quote_url: str = "https://api.au.pointsbet.com/api/v2/sgm/price"
    pointsbet_user_agent: str = sportsbet_user_agent
    pointsbet_origin: str = "https://pointsbet.com.au"
    pointsbet_referer: str = "https://pointsbet.com.au/"
    bet365_sgm_margin: float = 0.004

    @property
    def allowed_tailscale_user_logins_list(self) -> tuple[str, ...]:
        return tuple(item.strip() for item in self.allowed_tailscale_user_logins.split(",") if item.strip())

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
