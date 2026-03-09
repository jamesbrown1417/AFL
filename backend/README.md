# AFL Backend

FastAPI + DuckDB backend for querying locally scraped AFL odds data and requesting live SGM pricing.

## Quick start

1. Create a virtualenv in `backend/.venv`.
2. Install the package with `pip install -e .[dev]`.
3. Copy `.env.example` to `.env` and adjust paths if needed.
4. Bootstrap the database with `python scripts/bootstrap_db.py`.
5. Run a one-off import with `python scripts/run_import_once.py`.
6. Start the API with `uvicorn app.main:app --host 127.0.0.1 --port 8000 --workers 1`.

The backend reads scraper output from the existing `Data/` directory and writes its own state into `runtime/`.

## launchd service

Install the backend as a per-user LaunchAgent:

```bash
./scripts/backend_service.sh install
```

Restart it after backend code changes:

```bash
./scripts/backend_service.sh restart
```

Useful checks:

```bash
./scripts/backend_service.sh status
./scripts/backend_service.sh logs
```
