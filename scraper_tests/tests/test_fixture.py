from __future__ import annotations

from datetime import datetime
from pathlib import Path
import tempfile
import unittest
from zoneinfo import ZoneInfo

import pandas as pd

from scraper_tests.scraper_health.fixture import select_target_round


class FixtureSelectionTests(unittest.TestCase):
    def test_selects_first_future_round_and_remaining_games(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            fixture_path = Path(temp_dir) / "fixture.csv"
            pd.DataFrame(
                [
                    {
                        "match": "Past v Game",
                        "round": "Round 12",
                        "home_team": "Past",
                        "away_team": "Game",
                        "start_time": "2026-06-07T05:00:00Z",
                        "venue": "Old Ground",
                    },
                    {
                        "match": "A v B",
                        "round": "Round 13",
                        "home_team": "A",
                        "away_team": "B",
                        "start_time": "2026-06-08T05:15:00Z",
                        "venue": "MCG",
                    },
                    {
                        "match": "C v D",
                        "round": "Round 13",
                        "home_team": "C",
                        "away_team": "D",
                        "start_time": "2026-06-09T05:15:00Z",
                        "venue": "Docklands",
                    },
                    {
                        "match": "E v F",
                        "round": "Round 14",
                        "home_team": "E",
                        "away_team": "F",
                        "start_time": "2026-06-11T09:30:00Z",
                        "venue": "Oval",
                    },
                ]
            ).to_csv(fixture_path, index=False)

            target = select_target_round(
                fixture_path,
                now=datetime(2026, 6, 8, 9, 0, tzinfo=ZoneInfo("Australia/Adelaide")),
            )

            self.assertEqual(target["target_round"], "Round 13")
            self.assertEqual(
                [fixture["match"] for fixture in target["fixtures"]],
                ["A v B", "C v D"],
            )
            self.assertEqual(target["timezone"], "Australia/Adelaide")

    def test_returns_empty_target_when_no_future_fixtures(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            fixture_path = Path(temp_dir) / "fixture.csv"
            pd.DataFrame(
                [
                    {
                        "match": "A v B",
                        "round": "Round 1",
                        "home_team": "A",
                        "away_team": "B",
                        "start_time": "2026-01-01T00:00:00Z",
                        "venue": "MCG",
                    }
                ]
            ).to_csv(fixture_path, index=False)

            target = select_target_round(
                fixture_path,
                now=datetime(2026, 6, 8, 9, 0, tzinfo=ZoneInfo("Australia/Adelaide")),
            )

            self.assertIsNone(target["target_round"])
            self.assertEqual(target["fixtures"], [])
