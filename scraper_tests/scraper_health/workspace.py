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

PRODUCTION_CACHE_CLEANUP_GLOBS = (
    "OddsScraper/Neds/*.json",
    "Data/BET365_HTML/*.txt",
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


def apply_production_cache_cleanup(workspace: Path) -> list[dict[str, Any]]:
    """Mirror the cache cleanup at the top of afl_update_file.sh inside workspace."""
    removed: list[dict[str, Any]] = []
    for pattern in PRODUCTION_CACHE_CLEANUP_GLOBS:
        for path in sorted(workspace.glob(pattern)):
            if not path.is_file():
                continue
            relative = path.relative_to(workspace).as_posix()
            size = path.stat().st_size
            path.unlink()
            removed.append({"path": relative, "bytes": size})
    return removed


def snapshot_source_artifacts(source_root: Path, manifest: dict[str, Any]) -> dict[str, dict[str, Any]]:
    snapshot: dict[str, dict[str, Any]] = {}
    for pattern in _production_managed_patterns(manifest):
        for path in _expand_artifact_pattern(source_root, pattern):
            if path.is_file():
                snapshot[path.relative_to(source_root).as_posix()] = _file_fingerprint(path)
    return snapshot


def detect_source_artifact_mutations(
    source_root: Path,
    manifest: dict[str, Any],
    before: dict[str, dict[str, Any]],
) -> list[dict[str, Any]]:
    after = snapshot_source_artifacts(source_root, manifest)
    findings: list[dict[str, Any]] = []
    for relative in sorted(set(before) | set(after)):
        if before.get(relative) == after.get(relative):
            continue
        if relative.startswith("scraper_tests/"):
            continue
        findings.append(
            finding(
                "error",
                "Source artifact modified during isolated run",
                "The health suite should only write inside scraper_tests/latest/workspace.",
                file=relative,
                context={"before": before.get(relative), "after": after.get(relative)},
            )
        )
    return findings


def _production_managed_patterns(manifest: dict[str, Any]) -> set[str]:
    patterns = set(PRODUCTION_CACHE_CLEANUP_GLOBS)
    for entry in manifest.get("prefetch", []) or []:
        for spec in entry.get("outputs", []) or []:
            _add_artifact_pattern(patterns, spec)
    for entry in manifest.get("bookmakers", []) or []:
        for collection in ("cached_inputs", "outputs"):
            for spec in entry.get(collection, []) or []:
                _add_artifact_pattern(patterns, spec)
    return patterns


def _add_artifact_pattern(patterns: set[str], spec: dict[str, Any]) -> None:
    if "path" in spec:
        patterns.add(spec["path"])
    elif "glob" in spec:
        patterns.add(spec["glob"])


def _expand_artifact_pattern(root: Path, pattern: str) -> list[Path]:
    if any(char in pattern for char in "*?[]"):
        return sorted(root.glob(pattern))
    path = root / pattern
    return [path] if path.exists() else []


def _file_fingerprint(path: Path) -> dict[str, Any]:
    stat = path.stat()
    return {"mtime_ns": stat.st_mtime_ns, "size": stat.st_size}


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
