from __future__ import annotations

import unittest

from scraper_tests.scraper_health.report import render_html


class ReportTests(unittest.TestCase):
    def test_render_html_contains_core_sections_and_is_self_contained(self) -> None:
        summary = {
            "generated_at": "2026-06-08T00:00:00+00:00",
            "overall_status": "warning",
            "target": {
                "target_round": "Round 13",
                "timezone": "Australia/Adelaide",
                "fixtures": [
                    {
                        "match": "A v B",
                        "start_time_local": "2026-06-08T14:45:00+09:30",
                        "venue": "MCG",
                    }
                ],
            },
            "counters": {
                "scraper_count": 1,
                "output_count": 1,
                "prefetch_count": 0,
                "scrapers": {"pass": 0, "warning": 1, "blocked": 0, "error": 0},
                "outputs": {"pass": 0, "warning": 1, "blocked": 0, "error": 0},
                "prefetch": {"pass": 0, "warning": 0, "blocked": 0, "error": 0},
                "findings": {"pass": 0, "warning": 1, "blocked": 0, "error": 0},
            },
            "coverage": {
                "markets": [{"code": "h2h", "label": "H2H"}],
                "rows": [
                    {
                        "bookmaker": "Test",
                        "code": "test",
                        "status": "warning",
                        "cells": [
                            {
                                "market": "h2h",
                                "status": "warning",
                                "label": "Warning",
                                "row_count": 2,
                                "file_count": 1,
                                "matched_target_count": 1,
                                "issue_count": 1,
                                "issue_summary": "Coverage gap",
                                "issues": [
                                    {
                                        "severity": "warning",
                                        "title": "Coverage gap",
                                        "detail": "No rows were found for A v B.",
                                        "file": "Data/scraped_odds/test_h2h.csv",
                                    }
                                ],
                                "files": [
                                    {
                                        "path": "Data/scraped_odds/test_h2h.csv",
                                        "status": "warning",
                                        "row_count": 2,
                                        "updated_during_run": True,
                                    }
                                ],
                            }
                        ],
                    }
                ],
            },
            "bookmakers": [
                {
                    "name": "Test",
                    "status": "warning",
                    "duration_seconds": 1.2,
                    "exit_code": 0,
                    "command": ["Rscript", "test.R"],
                    "stdout_excerpt": "",
                    "stderr_excerpt": "",
                    "outputs": [
                        {
                            "market_label": "H2H",
                            "status": "warning",
                            "row_count": 2,
                            "updated_during_run": True,
                            "path": "Data/scraped_odds/test_h2h.csv",
                        }
                    ],
                }
            ],
            "findings": [
                {
                    "severity": "warning",
                    "title": "Coverage gap",
                    "detail": "No rows were found for A v B.",
                    "file": "Data/scraped_odds/test_h2h.csv",
                }
            ],
            "prefetch": [],
        }

        html = render_html(summary)

        self.assertIn("Coverage Matrix", html)
        self.assertIn("Artifact Inventory", html)
        self.assertIn("No rows were found for A v B.", html)
        self.assertNotIn("https://", html)
