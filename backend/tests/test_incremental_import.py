from __future__ import annotations

import shutil
from pathlib import Path

from app.db.duckdb import connection, fetch_value
from ingest.incremental_import import run_incremental_import


def _isolated_source_settings(test_settings, tmp_path: Path):
    source_root = tmp_path / "sources"
    odds_dir = source_root / "scraped_odds"
    processed_dir = source_root / "processed_odds"
    odds_dir.mkdir(parents=True)
    processed_dir.mkdir(parents=True)
    fixture_path = source_root / "current_fixture.csv"
    shutil.copy2(test_settings.fixture_path, fixture_path)
    for source_path in test_settings.scraped_odds_dir.glob("*.csv"):
        shutil.copy2(source_path, odds_dir / source_path.name)
    return test_settings.model_copy(
        update={
            "scraped_odds_dir": odds_dir,
            "processed_odds_dir": processed_dir,
            "fixture_path": fixture_path,
            "duckdb_path": tmp_path / "incremental.duckdb",
        }
    )


def test_incremental_import_skips_unchanged_and_replaces_changed_partition(
    test_settings, tmp_path: Path
) -> None:
    settings = _isolated_source_settings(test_settings, tmp_path)
    first = run_incremental_import(settings, triggered_by="test_initial")
    assert first["status"] == "completed"
    assert first["files_imported"] == first["files_scanned"]

    odds_paths = sorted(settings.scraped_odds_dir.glob("*.csv"))
    changed_path = odds_paths[0]
    unchanged_path = odds_paths[-1]
    with connection(settings=settings) as conn:
        unchanged_updated_at = fetch_value(
            conn,
            "SELECT updated_at FROM source_artifacts WHERE source_path = ?",
            [str(unchanged_path)],
        )
        old_partition_count = int(
            fetch_value(
                conn,
                "SELECT COUNT(*) FROM normalized_odds_sources WHERE source_path = ?",
                [str(changed_path)],
            )
            or 0
        )

    source_lines = changed_path.read_text(encoding="utf-8").splitlines()
    assert len(source_lines) > 1
    changed_path.write_text("\n".join(source_lines[:-1]) + "\n", encoding="utf-8")

    second = run_incremental_import(settings, triggered_by="test_changed")
    assert second["status"] == "completed"
    assert second["files_imported"] == 1

    with connection(settings=settings) as conn:
        new_partition_count = int(
            fetch_value(
                conn,
                "SELECT COUNT(*) FROM normalized_odds_sources WHERE source_path = ?",
                [str(changed_path)],
            )
            or 0
        )
        assert new_partition_count < old_partition_count
        assert (
            fetch_value(
                conn,
                "SELECT updated_at FROM source_artifacts WHERE source_path = ?",
                [str(unchanged_path)],
            )
            == unchanged_updated_at
        )
        assert fetch_value(conn, "SELECT COUNT(*) FROM current_outcome_prices_v") == fetch_value(
            conn,
            """
            SELECT COUNT(*)
            FROM (
              SELECT DISTINCT selection_key, bookmaker_code
              FROM normalized_odds_sources
            )
            """,
        )

    third = run_incremental_import(settings, triggered_by="test_unchanged")
    assert third["status"] == "completed"
    assert third["files_imported"] == 0

    changed_path.write_text("unexpected_column\nbad data\n", encoding="utf-8")
    failed = run_incremental_import(settings, triggered_by="test_failed_change")
    assert failed["status"] == "completed_with_errors"
    assert failed["error_count"] == 1
    with connection(settings=settings) as conn:
        assert (
            fetch_value(
                conn,
                "SELECT COUNT(*) FROM normalized_odds_sources WHERE source_path = ?",
                [str(changed_path)],
            )
            == new_partition_count
        )


def test_incremental_import_removes_deleted_source_rows(test_settings, tmp_path: Path) -> None:
    settings = _isolated_source_settings(test_settings, tmp_path)
    run_incremental_import(settings, triggered_by="test_initial")
    removed_path = sorted(settings.scraped_odds_dir.glob("*.csv"))[0]
    removed_path.unlink()

    summary = run_incremental_import(settings, triggered_by="test_deleted")

    assert summary["status"] == "completed"
    with connection(settings=settings) as conn:
        assert (
            fetch_value(
                conn,
                "SELECT COUNT(*) FROM source_artifacts WHERE source_path = ?",
                [str(removed_path)],
            )
            == 0
        )
        assert (
            fetch_value(
                conn,
                "SELECT COUNT(*) FROM normalized_odds_sources WHERE source_path = ?",
                [str(removed_path)],
            )
            == 0
        )
        assert fetch_value(conn, "SELECT COUNT(*) FROM current_outcome_prices_v") == fetch_value(
            conn,
            """
            SELECT COUNT(*)
            FROM (
              SELECT DISTINCT selection_key, bookmaker_code
              FROM normalized_odds_sources
            )
            """,
        )
