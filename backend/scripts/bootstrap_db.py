#!/usr/bin/env python3
from __future__ import annotations

from app.config import get_settings
from app.db.duckdb import initialize_database


def main() -> None:
    settings = get_settings()
    initialize_database(settings)
    print(f"Initialized DuckDB at {settings.duckdb_path}")


if __name__ == "__main__":
    main()
