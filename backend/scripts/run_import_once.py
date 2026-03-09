#!/usr/bin/env python3
from __future__ import annotations

import json

from app.config import get_settings
from ingest.import_csvs import run_import


def main() -> None:
    settings = get_settings()
    summary = run_import(settings, triggered_by="manual")
    print(json.dumps(summary, indent=2, default=str))


if __name__ == "__main__":
    main()
