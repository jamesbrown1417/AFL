from __future__ import annotations

import re
import shutil
from pathlib import Path
from typing import Any

from .models import finding


COPY_IGNORE = shutil.ignore_patterns(
    "__pycache__",
    ".DS_Store",
    ".Rhistory",
    ".RData",
    ".RDataTmp*",
    "*.pyc",
)


def create_latest_run_dir(source_root: Path) -> Path:
    latest_dir = source_root / "scraper_tests" / "latest"
    if latest_dir.exists():
        shutil.rmtree(latest_dir)
    latest_dir.mkdir(parents=True)
    return latest_dir


def prepare_workspace(source_root: Path, run_dir: Path) -> tuple[Path, list[str]]:
    workspace = run_dir / "workspace"
    workspace.mkdir(parents=True, exist_ok=False)

    copied: list[str] = []
    for directory_name in ("OddsScraper", "Functions"):
        src = source_root / directory_name
        dest = workspace / directory_name
        if src.exists():
            shutil.copytree(src, dest, ignore=COPY_IGNORE)
            copied.append(directory_name)

    data_dir = workspace / "Data"
    data_dir.mkdir(parents=True, exist_ok=True)
    for filename in ("current_fixture.csv", "current_fixture.rds", "2026_start_positions_and_prices.rds"):
        src = source_root / "Data" / filename
        if src.exists():
            shutil.copy2(src, data_dir / filename)
            copied.append(f"Data/{filename}")

    for directory_name in ("BET365_HTML", "scraped_odds"):
        src = source_root / "Data" / directory_name
        dest = data_dir / directory_name
        if src.exists():
            shutil.copytree(src, dest, ignore=COPY_IGNORE)
            copied.append(f"Data/{directory_name}")
        else:
            dest.mkdir(parents=True, exist_ok=True)

    (data_dir / "scraped_odds").mkdir(parents=True, exist_ok=True)
    (data_dir / "BET365_HTML").mkdir(parents=True, exist_ok=True)
    return workspace, copied


def detect_repo_warnings(source_root: Path) -> list[dict[str, Any]]:
    findings: list[dict[str, Any]] = []
    exact_paths = {
        path.relative_to(source_root).as_posix()
        for path in source_root.rglob("*")
        if path.is_file()
    }
    lower_to_exact = {path.lower(): path for path in exact_paths}
    master_script = source_root / "OddsScraper" / "master_processing_script.R"
    if not master_script.exists():
        return findings

    text = master_script.read_text(encoding="utf-8", errors="ignore")
    script_refs = re.findall(r'"(OddsScraper/[^"]+\.R)"', text)
    for ref in script_refs:
        if ref in exact_paths:
            continue
        exact_match = lower_to_exact.get(ref.lower())
        if exact_match:
            findings.append(
                finding(
                    "warning",
                    "Case-sensitive script path mismatch",
                    f"`{ref}` is referenced by master_processing_script.R, but the file on disk is "
                    f"`{exact_match}`. This works on case-insensitive filesystems but is portable-risky.",
                    file="OddsScraper/master_processing_script.R",
                )
            )
    return findings
