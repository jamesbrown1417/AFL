from __future__ import annotations

from pathlib import Path
import unittest

from scraper_tests.scraper_health.manifest import load_manifest


class ManifestTests(unittest.TestCase):
    def test_manifest_resolves_required_columns(self) -> None:
        manifest = load_manifest(Path("scraper_tests/manifest.yml"))
        codes = {bookmaker["code"] for bookmaker in manifest["bookmakers"]}

        self.assertEqual(
            codes,
            {
                "betright",
                "pointsbet",
                "sportsbet",
                "tab",
                "bet365",
                "neds",
                "betfair",
            },
        )
        for bookmaker in manifest["bookmakers"]:
            for output in bookmaker["outputs"]:
                self.assertTrue(output["required_columns"])
                self.assertTrue(output["market_label"])
