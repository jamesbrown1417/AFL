from __future__ import annotations

import argparse
import json
import os
import shutil
from pathlib import Path
from typing import Any

from .fixture import select_target_round
from .manifest import load_manifest
from .models import utc_now_iso, worst_status
from .report import build_counters, build_coverage_matrix, collect_findings, write_report
from .runner import (
    PrerequisiteChecker,
    build_report_only_bookmaker_results,
    inspect_prefetch_artifacts,
    run_bookmakers,
    run_prefetches,
)
from .validators import attach_output_statuses
from .workspace import (
    apply_production_cache_cleanup,
    create_latest_run_dir,
    detect_repo_warnings,
    detect_source_artifact_mutations,
    prepare_workspace,
    snapshot_source_artifacts,
)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Run AFL scraper health diagnostics.")
    parser.add_argument(
        "--mode",
        choices=("shadow", "production"),
        default="shadow",
        help=(
            "shadow runs scrapers in an isolated copied workspace; production validates "
            "the currently existing production output files without running scrapers."
        ),
    )
    parser.add_argument(
        "--production-outputs",
        action="store_const",
        dest="mode",
        const="production",
        help="Alias for --mode production.",
    )
    parser.add_argument(
        "--prefetch",
        choices=("auto", "cached", "off"),
        default="auto",
        help="How to handle scraper prefetch helpers.",
    )
    parser.add_argument(
        "--timeout-seconds",
        type=int,
        default=None,
        help="Override every script/helper timeout.",
    )
    parser.add_argument(
        "--report-path",
        type=Path,
        default=None,
        help="Optional additional path for the generated HTML report.",
    )
    parser.add_argument(
        "--no-fail",
        action="store_true",
        help="Always exit 0 after writing the report.",
    )
    args = parser.parse_args(argv)

    source_root = Path(__file__).resolve().parents[2]
    _load_dotenv(source_root)
    manifest_path = source_root / "scraper_tests" / "manifest.yml"
    manifest = load_manifest(manifest_path)
    source_artifact_snapshot = snapshot_source_artifacts(source_root, manifest)

    run_dir = create_latest_run_dir(source_root)
    logs_dir = run_dir / "logs"
    repo_warnings = detect_repo_warnings(source_root)

    if args.mode == "production":
        workspace = source_root
        copied_inputs: list[str] = []
        production_cache_cleanup: list[dict[str, Any]] = []
        target = select_target_round(source_root / "Data" / "current_fixture.csv")
        prefetch_results = inspect_prefetch_artifacts(manifest, workspace=workspace)
        raw_bookmaker_results = build_report_only_bookmaker_results(manifest)
        source_write_findings: list[dict[str, Any]] = []
    else:
        workspace, copied_inputs = prepare_workspace(source_root, run_dir)
        production_cache_cleanup = (
            apply_production_cache_cleanup(workspace) if args.prefetch == "auto" else []
        )
        target = select_target_round(workspace / "Data" / "current_fixture.csv")

        checker = PrerequisiteChecker()
        prefetch_results = run_prefetches(
            manifest,
            mode=args.prefetch,
            workspace=workspace,
            logs_dir=logs_dir,
            timeout_override=args.timeout_seconds,
            checker=checker,
        )
        raw_bookmaker_results = run_bookmakers(
            manifest,
            prefetch_mode=args.prefetch,
            workspace=workspace,
            logs_dir=logs_dir,
            timeout_override=args.timeout_seconds,
            checker=checker,
        )
        source_write_findings = detect_source_artifact_mutations(
            source_root,
            manifest,
            source_artifact_snapshot,
        )

    bookmaker_results = attach_output_statuses(
        manifest,
        raw_bookmaker_results,
        workspace=workspace,
        target_matches=[fixture["match"] for fixture in target["fixtures"]],
    )
    _attach_prefetch_statuses(bookmaker_results, prefetch_results)
    repo_findings = repo_warnings + source_write_findings

    coverage = build_coverage_matrix(manifest, bookmaker_results)
    counters = build_counters(prefetch_results, bookmaker_results, repo_findings)
    findings = collect_findings(prefetch_results, bookmaker_results, repo_findings)
    overall_status = worst_status(
        [result["status"] for result in prefetch_results]
        + [result["status"] for result in bookmaker_results]
        + [item["severity"] for item in repo_findings]
    )

    summary: dict[str, Any] = {
        "generated_at": utc_now_iso(),
        "overall_status": overall_status,
        "source_root": str(source_root),
        "run_dir": str(run_dir),
        "workspace": str(workspace),
        "validation_root": str(workspace),
        "execution_mode": args.mode,
        "manifest_path": str(manifest_path),
        "prefetch_mode": args.prefetch,
        "copied_inputs": copied_inputs,
        "production_cache_cleanup": production_cache_cleanup,
        "target": target,
        "repo_warnings": repo_warnings,
        "source_write_findings": source_write_findings,
        "prefetch": prefetch_results,
        "bookmakers": bookmaker_results,
        "coverage": coverage,
        "counters": counters,
        "findings": findings,
    }

    summary_path = run_dir / "summary.json"
    summary_path.write_text(json.dumps(summary, indent=2, default=str), encoding="utf-8")
    report_path = run_dir / "report.html"
    write_report(summary, report_path)
    if args.report_path:
        args.report_path.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(report_path, args.report_path)
        summary["report_path_override"] = str(args.report_path)
        summary_path.write_text(json.dumps(summary, indent=2, default=str), encoding="utf-8")

    print(f"Report: {report_path}")
    print(f"Summary: {summary_path}")
    print(f"Overall status: {overall_status}")

    if args.no_fail:
        return 0
    return 1 if overall_status in {"blocked", "error"} else 0


def _load_dotenv(source_root: Path) -> None:
    """Load credentials from the repo's .env (then `env`) and ~/.Renviron.

    Mirrors how the production scrapers load secrets: the Python scrapers call
    load_dotenv() (.env), and the R scrapers read ~/.Renviron automatically.
    Loading both here lets credential-gated scrapers like Bet365 (BET365USER/PW
    in .env) and Betfair (BETFAIR_USER/PASS/APP in ~/.Renviron) pass their
    prerequisite checks and inherit the vars in their subprocess env. Existing
    environment variables are never overridden.
    """
    try:
        from dotenv import load_dotenv
    except ImportError:
        load_dotenv = None
    if load_dotenv is not None:
        for candidate in (source_root / ".env", source_root / "env"):
            if candidate.exists():
                load_dotenv(candidate, override=False)
    _load_renviron(Path.home() / ".Renviron")


def _load_renviron(path: Path) -> None:
    """Load `KEY=VALUE` pairs from an R-style .Renviron into os.environ.

    Only sets keys that are not already present, so real environment variables
    take precedence. Best-effort: malformed lines and a missing file are ignored.
    """
    if not path.exists():
        return
    try:
        lines = path.read_text(encoding="utf-8", errors="ignore").splitlines()
    except OSError:
        return
    for raw in lines:
        line = raw.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        key, _, value = line.partition("=")
        key = key.strip()
        value = value.strip().strip('"').strip("'")
        if key and key not in os.environ:
            os.environ[key] = value


def _attach_prefetch_statuses(
    bookmaker_results: list[dict[str, Any]],
    prefetch_results: list[dict[str, Any]],
) -> None:
    by_bookmaker: dict[str, list[dict[str, Any]]] = {}
    for result in prefetch_results:
        bookmaker = result.get("bookmaker")
        if bookmaker:
            by_bookmaker.setdefault(bookmaker, []).append(result)

    for bookmaker_result in bookmaker_results:
        related = by_bookmaker.get(bookmaker_result["code"], [])
        bookmaker_result["prefetch_results"] = related
        if related:
            bookmaker_result["status"] = worst_status(
                [bookmaker_result["status"]] + [result["status"] for result in related]
            )
