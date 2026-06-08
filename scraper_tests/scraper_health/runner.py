from __future__ import annotations

import importlib.util
import os
import subprocess
import sys
import time
from pathlib import Path
from typing import Any

from .models import finding, status_from_findings, utc_now_iso, worst_status


class PrerequisiteChecker:
    def __init__(self) -> None:
        self._python_cache: dict[str, bool] = {}
        self._r_cache: dict[str, bool] = {}
        self._r_available: bool | None = None

    def check(self, prerequisites: dict[str, Any] | None) -> list[dict[str, Any]]:
        prerequisites = prerequisites or {}
        findings: list[dict[str, Any]] = []

        for env_key in prerequisites.get("env", []) or []:
            if not os.getenv(env_key):
                findings.append(
                    finding(
                        "blocked",
                        "Missing environment variable",
                        f"`{env_key}` is required before this workflow can run.",
                        context={"env": env_key},
                    )
                )

        for module in prerequisites.get("python_modules", []) or []:
            if not self._has_python_module(module):
                findings.append(
                    finding(
                        "blocked",
                        "Missing Python module",
                        f"`{module}` is not importable in the current Python environment.",
                        context={"python_module": module},
                    )
                )

        for package in prerequisites.get("r_packages", []) or []:
            if not self._has_r_package(package):
                findings.append(
                    finding(
                        "blocked",
                        "Missing R package",
                        f"`{package}` is not available to `Rscript`.",
                        context={"r_package": package},
                    )
                )

        return findings

    def _has_python_module(self, module: str) -> bool:
        if module not in self._python_cache:
            self._python_cache[module] = importlib.util.find_spec(module) is not None
        return self._python_cache[module]

    def _has_r_package(self, package: str) -> bool:
        if package in self._r_cache:
            return self._r_cache[package]
        if self._r_available is False:
            self._r_cache[package] = False
            return False

        expression = (
            "quit(status = ifelse(requireNamespace("
            f"'{package}', quietly = TRUE), 0, 1))"
        )
        try:
            completed = subprocess.run(
                ["Rscript", "-e", expression],
                text=True,
                capture_output=True,
                timeout=20,
            )
            self._r_available = True
            self._r_cache[package] = completed.returncode == 0
        except (FileNotFoundError, subprocess.TimeoutExpired):
            self._r_available = False
            self._r_cache[package] = False
        return self._r_cache[package]


def check_artifacts(
    workspace: Path,
    artifact_specs: list[dict[str, Any]] | None,
    *,
    missing_severity: str,
) -> tuple[list[dict[str, Any]], list[dict[str, Any]]]:
    artifact_specs = artifact_specs or []
    artifacts: list[dict[str, Any]] = []
    findings: list[dict[str, Any]] = []
    for spec in artifact_specs:
        if "path" in spec:
            relative = spec["path"]
            path = workspace / relative
            exists = path.exists()
            artifacts.append(
                {
                    "path": relative,
                    "kind": "path",
                    "exists": exists,
                    "count": 1 if exists else 0,
                    "bytes": path.stat().st_size if exists and path.is_file() else None,
                }
            )
            if not exists:
                findings.append(
                    finding(
                        missing_severity,
                        "Missing required artifact",
                        f"`{relative}` was not found.",
                        file=relative,
                    )
                )
        elif "glob" in spec:
            pattern = spec["glob"]
            matches = sorted(workspace.glob(pattern))
            artifacts.append(
                {
                    "path": pattern,
                    "kind": "glob",
                    "exists": bool(matches),
                    "count": len(matches),
                    "bytes": sum(path.stat().st_size for path in matches if path.is_file()),
                }
            )
            if not matches:
                findings.append(
                    finding(
                        missing_severity,
                        "Missing required artifact",
                        f"No files matched `{pattern}`.",
                        file=pattern,
                    )
                )
    return artifacts, findings


def run_prefetches(
    manifest: dict[str, Any],
    *,
    mode: str,
    workspace: Path,
    logs_dir: Path,
    timeout_override: int | None,
    checker: PrerequisiteChecker,
) -> list[dict[str, Any]]:
    results: list[dict[str, Any]] = []
    for entry in manifest.get("prefetch", []) or []:
        if mode == "off":
            results.append(
                {
                    "code": entry["code"],
                    "name": entry["name"],
                    "bookmaker": entry.get("bookmaker"),
                    "kind": "prefetch",
                    "status": "pass",
                    "skipped": True,
                    "message": "Prefetch mode is off; helper was not run and artifacts were not checked.",
                    "findings": [],
                    "artifacts": [],
                }
            )
            continue

        if mode == "cached":
            artifacts, artifact_findings = check_artifacts(
                workspace,
                entry.get("outputs", []),
                missing_severity="blocked",
            )
            results.append(
                {
                    "code": entry["code"],
                    "name": entry["name"],
                    "bookmaker": entry.get("bookmaker"),
                    "kind": "prefetch",
                    "status": status_from_findings(artifact_findings),
                    "skipped": True,
                    "message": "Prefetch mode is cached; helper was not run.",
                    "findings": artifact_findings,
                    "artifacts": artifacts,
                }
            )
            continue

        prerequisite_findings = checker.check(entry.get("prerequisites"))
        if prerequisite_findings:
            artifacts, artifact_findings = check_artifacts(
                workspace,
                entry.get("outputs", []),
                missing_severity="warning",
            )
            all_findings = prerequisite_findings + artifact_findings
            results.append(
                {
                    "code": entry["code"],
                    "name": entry["name"],
                    "bookmaker": entry.get("bookmaker"),
                    "kind": "prefetch",
                    "status": "blocked",
                    "skipped": True,
                    "message": "Helper was not run because prerequisites are missing.",
                    "findings": all_findings,
                    "artifacts": artifacts,
                }
            )
            continue

        command_result = run_command(
            entry,
            workspace=workspace,
            logs_dir=logs_dir,
            timeout_seconds=timeout_override or entry.get("timeout_seconds", 120),
            kind="prefetch",
        )
        artifacts, artifact_findings = check_artifacts(
            workspace,
            entry.get("outputs", []),
            missing_severity="error" if command_result["status"] == "pass" else "warning",
        )
        command_result["artifacts"] = artifacts
        command_result["findings"].extend(artifact_findings)
        command_result["status"] = worst_status(
            [command_result["status"], status_from_findings(command_result["findings"])]
        )
        results.append(command_result)
    return results


def run_bookmakers(
    manifest: dict[str, Any],
    *,
    prefetch_mode: str,
    workspace: Path,
    logs_dir: Path,
    timeout_override: int | None,
    checker: PrerequisiteChecker,
) -> list[dict[str, Any]]:
    results: list[dict[str, Any]] = []
    for entry in manifest.get("bookmakers", []):
        prerequisite_findings = checker.check(entry.get("prerequisites"))
        cached_artifacts: list[dict[str, Any]] = []
        cached_findings: list[dict[str, Any]] = []
        if prefetch_mode != "off":
            cached_artifacts, cached_findings = check_artifacts(
                workspace,
                entry.get("cached_inputs", []),
                missing_severity="blocked",
            )

        blocking_findings = prerequisite_findings + cached_findings
        if blocking_findings:
            results.append(
                {
                    "code": entry["code"],
                    "name": entry["name"],
                    "kind": "scraper",
                    "status": "blocked",
                    "skipped": True,
                    "command": entry.get("command", []),
                    "message": "Scraper was not run because prerequisites or cached inputs are missing.",
                    "findings": blocking_findings,
                    "cached_artifacts": cached_artifacts,
                    "started_at_epoch": None,
                    "finished_at_epoch": None,
                    "duration_seconds": None,
                    "exit_code": None,
                }
            )
            continue

        result = run_command(
            entry,
            workspace=workspace,
            logs_dir=logs_dir,
            timeout_seconds=timeout_override or entry.get("timeout_seconds", 120),
            kind="scraper",
        )
        result["cached_artifacts"] = cached_artifacts
        results.append(result)
    return results


def run_command(
    entry: dict[str, Any],
    *,
    workspace: Path,
    logs_dir: Path,
    timeout_seconds: int,
    kind: str,
) -> dict[str, Any]:
    logs_dir.mkdir(parents=True, exist_ok=True)
    code = entry["code"]
    command = _resolve_command(entry.get("command", []))
    stdout_log = logs_dir / f"{kind}_{code}.stdout.log"
    stderr_log = logs_dir / f"{kind}_{code}.stderr.log"
    started_epoch = time.time()
    started_at = utc_now_iso()
    findings: list[dict[str, Any]] = []
    exit_code: int | None = None
    timed_out = False
    stdout = ""
    stderr = ""

    try:
        completed = subprocess.run(
            command,
            cwd=workspace,
            text=True,
            capture_output=True,
            timeout=timeout_seconds,
            env=_command_env(),
        )
        stdout = completed.stdout or ""
        stderr = completed.stderr or ""
        exit_code = completed.returncode
        if completed.returncode != 0:
            findings.append(
                finding(
                    "error",
                    "Command exited non-zero",
                    f"`{' '.join(command)}` exited with status {completed.returncode}.",
                )
            )
    except subprocess.TimeoutExpired as exc:
        timed_out = True
        stdout = _coerce_output(exc.stdout)
        stderr = _coerce_output(exc.stderr)
        findings.append(
            finding(
                "error",
                "Command timed out",
                f"`{' '.join(command)}` did not complete within {timeout_seconds} seconds.",
            )
        )
    except FileNotFoundError as exc:
        findings.append(
            finding(
                "blocked",
                "Command executable not found",
                str(exc),
            )
        )

    finished_epoch = time.time()
    stdout_log.write_text(stdout, encoding="utf-8")
    stderr_log.write_text(stderr, encoding="utf-8")
    status = status_from_findings(findings)

    return {
        "code": code,
        "name": entry["name"],
        "bookmaker": entry.get("bookmaker", code),
        "kind": kind,
        "status": status,
        "skipped": False,
        "message": "Completed." if status == "pass" else "Completed with issues.",
        "command": command,
        "timeout_seconds": timeout_seconds,
        "started_at": started_at,
        "started_at_epoch": started_epoch,
        "finished_at": utc_now_iso(),
        "finished_at_epoch": finished_epoch,
        "duration_seconds": round(finished_epoch - started_epoch, 3),
        "exit_code": exit_code,
        "timed_out": timed_out,
        "stdout_log": str(stdout_log),
        "stderr_log": str(stderr_log),
        "stdout_excerpt": tail_text(stdout),
        "stderr_excerpt": tail_text(stderr),
        "findings": findings,
    }


def tail_text(text: str, *, max_chars: int = 5000) -> str:
    if len(text) <= max_chars:
        return text
    return text[-max_chars:]


def _coerce_output(value: str | bytes | None) -> str:
    if value is None:
        return ""
    if isinstance(value, bytes):
        return value.decode("utf-8", errors="replace")
    return value


def _resolve_command(command: list[str]) -> list[str]:
    if not command:
        raise ValueError("Command cannot be empty")
    resolved = list(command)
    if resolved[0] in {"python", "python3"}:
        resolved[0] = sys.executable
    return resolved


def _command_env() -> dict[str, str]:
    env = os.environ.copy()
    env.setdefault("PYTHONUNBUFFERED", "1")
    return env

