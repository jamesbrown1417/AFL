from __future__ import annotations

import unittest

from scraper_tests.scraper_health.models import worst_status


class ModelTests(unittest.TestCase):
    def test_worst_status_order(self) -> None:
        self.assertEqual(worst_status(["pass", "warning"]), "warning")
        self.assertEqual(worst_status(["warning", "blocked"]), "blocked")
        self.assertEqual(worst_status(["blocked", "error"]), "error")
