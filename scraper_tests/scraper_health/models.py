from __future__ import annotations

from collections.abc import Iterable
from datetime import datetime, timezone
from typing import Any


STATUS_RANK = {
    "pass": 0,
    "warning": 1,
    "blocked": 2,
    "error": 3,
}


def utc_now_iso() -> str:
    return datetime.now(timezone.utc).isoformat()


def finding(
    severity: str,
    title: str,
    detail: str,
    *,
    file: str | None = None,
    context: dict[str, Any] | None = None,
) -> dict[str, Any]:
    if severity not in STATUS_RANK:
        raise ValueError(f"Unknown severity: {severity}")
    payload: dict[str, Any] = {
        "severity": severity,
        "title": title,
        "detail": detail,
    }
    if file:
        payload["file"] = file
    if context:
        payload["context"] = context
    return payload


def worst_status(statuses: Iterable[str], *, default: str = "pass") -> str:
    worst = default
    for status in statuses:
        if STATUS_RANK.get(status, -1) > STATUS_RANK.get(worst, -1):
            worst = status
    return worst


def status_from_findings(findings: Iterable[dict[str, Any]], *, default: str = "pass") -> str:
    return worst_status((item.get("severity", default) for item in findings), default=default)


def count_statuses(items: Iterable[dict[str, Any]]) -> dict[str, int]:
    counts = {status: 0 for status in STATUS_RANK}
    for item in items:
        status = item.get("status", "pass")
        if status in counts:
            counts[status] += 1
    return counts

