from __future__ import annotations

import threading
from contextlib import contextmanager
from pathlib import Path
from typing import Any, Iterator

import duckdb

from app.config import Settings, get_settings


_write_lock = threading.Lock()


@contextmanager
def connection(
    *,
    write: bool = False,
    settings: Settings | None = None,
    transaction: bool = True,
) -> Iterator[duckdb.DuckDBPyConnection]:
    resolved_settings = settings or get_settings()
    resolved_settings.ensure_runtime_dirs()
    conn = duckdb.connect(str(resolved_settings.duckdb_path), read_only=not write)
    try:
        if write:
            with _write_lock:
                if transaction:
                    conn.execute("BEGIN TRANSACTION")
                    try:
                        yield conn
                    except Exception:
                        conn.rollback()
                        raise
                    else:
                        conn.commit()
                else:
                    yield conn
        else:
            yield conn
    finally:
        conn.close()


def initialize_database(settings: Settings | None = None) -> None:
    resolved_settings = settings or get_settings()
    schema_path = Path(__file__).with_name("schema.sql")
    schema_sql = schema_path.read_text(encoding="utf-8")
    with connection(write=True, settings=resolved_settings) as conn:
        conn.execute(schema_sql)


def fetch_all(
    conn: duckdb.DuckDBPyConnection, query: str, params: list[Any] | None = None
) -> list[dict[str, Any]]:
    cursor = conn.execute(query, params or [])
    rows = cursor.fetchall()
    columns = [description[0] for description in cursor.description]
    return [dict(zip(columns, row, strict=True)) for row in rows]


def fetch_one(
    conn: duckdb.DuckDBPyConnection, query: str, params: list[Any] | None = None
) -> dict[str, Any] | None:
    rows = fetch_all(conn, query, params)
    return rows[0] if rows else None


def fetch_value(
    conn: duckdb.DuckDBPyConnection, query: str, params: list[Any] | None = None
) -> Any:
    cursor = conn.execute(query, params or [])
    row = cursor.fetchone()
    return row[0] if row else None
