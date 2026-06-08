from __future__ import annotations

import tempfile
import time
import unittest
from pathlib import Path

import pandas as pd

from scraper_tests.scraper_health.validators import validate_output


H2H_SPEC = {
    "path": "Data/scraped_odds/test_h2h.csv",
    "market": "h2h",
    "market_label": "H2H",
    "market_type": "h2h",
    "required_columns": [
        "match",
        "market_name",
        "home_team",
        "away_team",
        "home_win",
        "away_win",
        "agency",
    ],
}


def _script_result(status: str = "pass") -> dict[str, object]:
    return {
        "status": status,
        "exit_code": 0 if status == "pass" else None,
        "started_at_epoch": time.time() - 10,
    }


def _write_csv(workspace: Path, columns: dict[str, list[object]]) -> None:
    path = workspace / H2H_SPEC["path"]
    path.parent.mkdir(parents=True, exist_ok=True)
    pd.DataFrame(columns).to_csv(path, index=False)


class ValidatorTests(unittest.TestCase):
    def test_valid_h2h_output_passes(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            _write_csv(
                workspace,
                {
                    "match": ["A v B"],
                    "market_name": ["Head To Head"],
                    "home_team": ["A"],
                    "away_team": ["B"],
                    "home_win": [1.9],
                    "away_win": [1.9],
                    "agency": ["Test"],
                },
            )

            result = validate_output(
                H2H_SPEC,
                workspace=workspace,
                target_matches=["A v B"],
                script_result=_script_result(),
            )

            self.assertEqual(result["status"], "pass")
            self.assertEqual(result["matched_target_count"], 1)

    def test_missing_required_column_is_error(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            _write_csv(
                workspace,
                {
                    "match": ["A v B"],
                    "market_name": ["Head To Head"],
                    "home_team": ["A"],
                    "away_team": ["B"],
                    "home_win": [1.9],
                    "agency": ["Test"],
                },
            )

            result = validate_output(
                H2H_SPEC,
                workspace=workspace,
                target_matches=["A v B"],
                script_result=_script_result(),
            )

            self.assertEqual(result["status"], "error")
            self.assertTrue(any("Required columns" in item["title"] for item in result["findings"]))

    def test_invalid_odds_are_error(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            _write_csv(
                workspace,
                {
                    "match": ["A v B"],
                    "market_name": ["Head To Head"],
                    "home_team": ["A"],
                    "away_team": ["B"],
                    "home_win": [1.0],
                    "away_win": [1.9],
                    "agency": ["Test"],
                },
            )

            result = validate_output(
                H2H_SPEC,
                workspace=workspace,
                target_matches=["A v B"],
                script_result=_script_result(),
            )

            self.assertEqual(result["status"], "error")

    def test_target_coverage_gap_is_warning_not_error(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            _write_csv(
                workspace,
                {
                    "match": ["C v D"],
                    "market_name": ["Head To Head"],
                    "home_team": ["C"],
                    "away_team": ["D"],
                    "home_win": [1.9],
                    "away_win": [1.9],
                    "agency": ["Test"],
                },
            )

            result = validate_output(
                H2H_SPEC,
                workspace=workspace,
                target_matches=["A v B"],
                script_result=_script_result(),
            )

            self.assertEqual(result["status"], "warning")
            self.assertEqual(result["missing_target_matches"], ["A v B"])

    def test_blocked_script_marks_output_blocked(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            workspace = Path(temp_dir)
            result = validate_output(
                H2H_SPEC,
                workspace=workspace,
                target_matches=["A v B"],
                script_result=_script_result("blocked"),
            )

            self.assertEqual(result["status"], "blocked")
