from __future__ import annotations

import unittest

from pathlib import Path
import tempfile

from scraper_tests.scraper_health.runner import (
    PYTHON_EXECUTABLE,
    _resolve_command,
    build_report_only_bookmaker_results,
    inspect_prefetch_artifacts,
)


class RunnerTests(unittest.TestCase):
    def test_python_commands_use_production_pyenv_interpreter(self) -> None:
        self.assertEqual(
            _resolve_command(["python3", "script.py"]),
            [str(PYTHON_EXECUTABLE), "script.py"],
        )
        self.assertEqual(
            _resolve_command(["python", "script.py"]),
            [str(PYTHON_EXECUTABLE), "script.py"],
        )

    def test_non_python_commands_are_left_unchanged(self) -> None:
        self.assertEqual(_resolve_command(["Rscript", "script.R"]), ["Rscript", "script.R"])

    def test_report_only_bookmaker_results_are_skipped_and_not_started(self) -> None:
        manifest = {
            "bookmakers": [
                {
                    "code": "tab",
                    "name": "TAB",
                    "command": ["Rscript", "OddsScraper/scrape_tab.R"],
                }
            ]
        }

        results = build_report_only_bookmaker_results(manifest)

        self.assertEqual(len(results), 1)
        self.assertTrue(results[0]["skipped"])
        self.assertEqual(results[0]["status"], "pass")
        self.assertIsNone(results[0]["started_at_epoch"])
        self.assertIsNone(results[0]["exit_code"])

    def test_inspect_prefetch_artifacts_is_read_only_warning_for_missing_cache(self) -> None:
        manifest = {
            "prefetch": [
                {
                    "code": "tab_response",
                    "name": "TAB cached response",
                    "bookmaker": "tab",
                    "command": ["python3", "OddsScraper/TAB/get-TAB-response.py"],
                    "outputs": [{"path": "OddsScraper/TAB/tab_response.json"}],
                }
            ]
        }
        with tempfile.TemporaryDirectory() as temp_dir:
            results = inspect_prefetch_artifacts(manifest, workspace=Path(temp_dir))

        self.assertEqual(len(results), 1)
        self.assertTrue(results[0]["skipped"])
        self.assertEqual(results[0]["status"], "warning")
        self.assertEqual(results[0]["findings"][0]["severity"], "warning")
