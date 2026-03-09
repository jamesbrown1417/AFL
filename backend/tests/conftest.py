from __future__ import annotations

from importlib import reload
from pathlib import Path

import pytest
from fastapi.testclient import TestClient

from app.config import get_settings
from ingest.import_csvs import run_import


@pytest.fixture()
def repo_root() -> Path:
    return Path(__file__).resolve().parents[2]


@pytest.fixture()
def test_settings(tmp_path: Path, monkeypatch: pytest.MonkeyPatch, repo_root: Path):
    runtime_dir = tmp_path / "runtime"
    monkeypatch.setenv("AFL_ENABLE_AUTH", "false")
    monkeypatch.setenv("AFL_SCRAPED_ODDS_DIR", str(repo_root / "Data" / "scraped_odds"))
    monkeypatch.setenv("AFL_FIXTURE_PATH", str(repo_root / "Data" / "current_fixture.csv"))
    monkeypatch.setenv("AFL_RUNTIME_DIR", str(runtime_dir))
    monkeypatch.setenv("AFL_DUCKDB_PATH", str(runtime_dir / "duckdb" / "afl.duckdb"))
    monkeypatch.setenv("AFL_LOG_PATH", str(runtime_dir / "logs" / "backend.log"))
    monkeypatch.setenv("AFL_AUTH_TOKEN", "test-token")
    get_settings.cache_clear()
    settings = get_settings()
    yield settings
    get_settings.cache_clear()


@pytest.fixture()
def imported_settings(test_settings):
    summary = run_import(test_settings, triggered_by="test")
    assert summary["status"] in {"completed", "completed_with_errors"}
    return test_settings


@pytest.fixture()
def client(imported_settings):
    import app.main as main_module

    reload(main_module)
    with TestClient(main_module.app) as test_client:
        yield test_client
