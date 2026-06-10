from __future__ import annotations

import unittest

from scraper_tests.scraper_health.runner import PYTHON_EXECUTABLE, _resolve_command


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
