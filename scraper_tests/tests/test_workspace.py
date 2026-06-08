from __future__ import annotations

from pathlib import Path
import tempfile
import unittest

from scraper_tests.scraper_health.workspace import create_latest_run_dir, detect_repo_warnings


class WorkspaceTests(unittest.TestCase):
    def test_create_latest_run_dir_overwrites_previous_latest(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            latest = root / "scraper_tests" / "latest"
            latest.mkdir(parents=True)
            (latest / "old.txt").write_text("old", encoding="utf-8")

            created = create_latest_run_dir(root)

            self.assertEqual(created, latest)
            self.assertTrue(created.exists())
            self.assertFalse((created / "old.txt").exists())

    def test_detects_case_mismatch_in_master_script(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            scraper_dir = root / "OddsScraper"
            scraper_dir.mkdir()
            (scraper_dir / "scrape_Betr.R").write_text("", encoding="utf-8")
            (scraper_dir / "master_processing_script.R").write_text(
                'scraping_scripts <- c("OddsScraper/scrape_betr.R")',
                encoding="utf-8",
            )

            warnings = detect_repo_warnings(root)

            self.assertTrue(warnings)
            self.assertEqual(warnings[0]["severity"], "warning")
