from __future__ import annotations

from pathlib import Path
import tempfile
import unittest

from scraper_tests.scraper_health.workspace import (
    apply_production_cache_cleanup,
    create_latest_run_dir,
    detect_repo_warnings,
    detect_source_artifact_mutations,
    snapshot_source_artifacts,
)


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

    def test_production_cache_cleanup_only_removes_shadow_cache_files(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            neds = workspace / "OddsScraper" / "Neds"
            bet365 = workspace / "Data" / "BET365_HTML"
            neds.mkdir(parents=True)
            bet365.mkdir(parents=True)
            (neds / "data_1.json").write_text("{}", encoding="utf-8")
            (neds / "neds_response.json").write_text("{}", encoding="utf-8")
            (neds / "neds_afl_match_urls.csv").write_text("url\n", encoding="utf-8")
            (bet365 / "h2h_html.txt").write_text("html", encoding="utf-8")
            (bet365 / "urls.csv").write_text("url\n", encoding="utf-8")

            removed = apply_production_cache_cleanup(workspace)

            removed_paths = {item["path"] for item in removed}
            self.assertEqual(
                removed_paths,
                {
                    "OddsScraper/Neds/data_1.json",
                    "OddsScraper/Neds/neds_response.json",
                    "OddsScraper/Neds/neds_afl_match_urls.csv",
                    "Data/BET365_HTML/h2h_html.txt",
                },
            )
            self.assertFalse((neds / "neds_afl_match_urls.csv").exists())
            self.assertTrue((bet365 / "urls.csv").exists())

    def test_source_artifact_mutation_detection_flags_production_writes(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            root = Path(temp_dir)
            output = root / "Data" / "scraped_odds" / "tab_h2h.csv"
            output.parent.mkdir(parents=True)
            output.write_text("old\n", encoding="utf-8")
            manifest = {
                "prefetch": [],
                "bookmakers": [
                    {
                        "outputs": [
                            {
                                "path": "Data/scraped_odds/tab_h2h.csv",
                            }
                        ]
                    }
                ],
            }

            before = snapshot_source_artifacts(root, manifest)
            output.write_text("new content\n", encoding="utf-8")

            findings = detect_source_artifact_mutations(root, manifest, before)

            self.assertEqual(len(findings), 1)
            self.assertEqual(findings[0]["severity"], "error")
            self.assertEqual(findings[0]["file"], "Data/scraped_odds/tab_h2h.csv")
