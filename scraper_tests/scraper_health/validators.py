from __future__ import annotations

from pathlib import Path
from typing import Any

import pandas as pd

from .models import finding, status_from_findings, worst_status


ODDS_COLUMNS = ("home_win", "away_win", "over_price", "under_price")
LINE_COLUMNS = ("line", "home_line", "away_line")


def validate_bookmaker_outputs(
    bookmaker: dict[str, Any],
    script_result: dict[str, Any],
    *,
    workspace: Path,
    target_matches: list[str],
) -> list[dict[str, Any]]:
    output_results: list[dict[str, Any]] = []
    for spec in bookmaker.get("outputs", []):
        output_results.append(
            validate_output(
                spec,
                workspace=workspace,
                target_matches=target_matches,
                script_result=script_result,
            )
        )
    return output_results


def validate_output(
    spec: dict[str, Any],
    *,
    workspace: Path,
    target_matches: list[str],
    script_result: dict[str, Any],
) -> dict[str, Any]:
    relative_path = spec["path"]
    path = workspace / relative_path
    findings: list[dict[str, Any]] = []
    result: dict[str, Any] = {
        "path": relative_path,
        "market": spec["market"],
        "market_label": spec.get("market_label", spec["market"]),
        "market_type": spec["market_type"],
        "required_columns": spec.get("required_columns", []),
        "exists": path.exists(),
        "row_count": None,
        "column_count": None,
        "columns": [],
        "matched_target_count": 0,
        "missing_target_matches": [],
        "updated_during_run": None,
        "findings": findings,
        "status": "pass",
    }

    if script_result.get("status") == "blocked":
        findings.append(
            finding(
                "blocked",
                "Output not checked",
                "The scraper did not run, so this artifact was not validated.",
                file=relative_path,
            )
        )
        result["status"] = "blocked"
        return result

    if not path.exists():
        findings.append(
            finding(
                "error",
                "Expected output file missing",
                f"`{relative_path}` was not produced.",
                file=relative_path,
            )
        )
        result["status"] = status_from_findings(findings)
        return result

    started_at_epoch = script_result.get("started_at_epoch")
    if started_at_epoch is not None:
        updated = path.stat().st_mtime >= (float(started_at_epoch) - 1.0)
        result["updated_during_run"] = updated
        if script_result.get("exit_code") == 0 and not updated:
            findings.append(
                finding(
                    "error",
                    "Output was not refreshed",
                    "The scraper exited successfully, but this file timestamp predates the isolated run.",
                    file=relative_path,
                )
            )

    try:
        frame = pd.read_csv(path)
    except Exception as exc:  # pragma: no cover - pandas error types vary by parser
        findings.append(
            finding(
                "error",
                "CSV could not be read",
                f"{type(exc).__name__}: {exc}",
                file=relative_path,
            )
        )
        result["status"] = status_from_findings(findings)
        return result

    result["row_count"] = int(len(frame))
    result["column_count"] = int(len(frame.columns))
    result["columns"] = list(frame.columns)

    missing_columns = [col for col in spec.get("required_columns", []) if col not in frame.columns]
    if missing_columns:
        findings.append(
            finding(
                "error",
                "Required columns missing",
                f"Missing columns: {', '.join(missing_columns)}.",
                file=relative_path,
            )
        )

    if frame.empty:
        findings.append(
            finding(
                "warning",
                "Output contains zero rows",
                "This may be legitimate if the bookmaker has not released the market yet.",
                file=relative_path,
            )
        )
        result["status"] = status_from_findings(findings)
        return result

    _validate_identity_columns(frame, relative_path, findings)
    _validate_numeric_columns(frame, relative_path, findings)
    _validate_market_type(frame, spec["market_type"], relative_path, findings)
    _validate_target_coverage(frame, target_matches, relative_path, result, findings)
    result["status"] = status_from_findings(findings)
    return result


def attach_output_statuses(
    manifest: dict[str, Any],
    script_results: list[dict[str, Any]],
    *,
    workspace: Path,
    target_matches: list[str],
) -> list[dict[str, Any]]:
    by_code = {result["code"]: result for result in script_results}
    hydrated: list[dict[str, Any]] = []
    for bookmaker in manifest.get("bookmakers", []):
        result = dict(by_code[bookmaker["code"]])
        outputs = validate_bookmaker_outputs(
            bookmaker,
            result,
            workspace=workspace,
            target_matches=target_matches,
        )
        result["outputs"] = outputs
        output_status = worst_status(output["status"] for output in outputs)
        result["status"] = worst_status([result["status"], output_status])
        result["findings"] = list(result.get("findings", []))
        hydrated.append(result)
    return hydrated


def _validate_identity_columns(
    frame: pd.DataFrame,
    relative_path: str,
    findings: list[dict[str, Any]],
) -> None:
    for column in ("match", "home_team", "away_team", "market_name", "agency"):
        if column not in frame.columns:
            continue
        missing_count = int(frame[column].isna().sum() + (frame[column].astype(str).str.strip() == "").sum())
        if missing_count:
            findings.append(
                finding(
                    "error",
                    "Required identity values are blank",
                    f"`{column}` has {missing_count} blank values.",
                    file=relative_path,
                    context={"column": column, "blank_count": missing_count},
                )
            )
    if "match" in frame.columns:
        odd_match_names = frame["match"].dropna().astype(str)
        malformed = int((~odd_match_names.str.contains(" v ", regex=False)).sum())
        if malformed:
            findings.append(
                finding(
                    "warning",
                    "Unexpected match name shape",
                    f"{malformed} rows do not contain ` v ` in the match name.",
                    file=relative_path,
                )
            )


def _validate_numeric_columns(
    frame: pd.DataFrame,
    relative_path: str,
    findings: list[dict[str, Any]],
) -> None:
    for column in ODDS_COLUMNS:
        if column not in frame.columns:
            continue
        values = pd.to_numeric(frame[column], errors="coerce")
        present = frame[column].notna() & (frame[column].astype(str).str.strip() != "")
        invalid_numeric = int((present & values.isna()).sum())
        invalid_range = int((values.notna() & (values <= 1)).sum())
        if invalid_numeric or invalid_range:
            findings.append(
                finding(
                    "error",
                    "Invalid decimal odds",
                    f"`{column}` has {invalid_numeric} non-numeric and {invalid_range} <= 1 values.",
                    file=relative_path,
                    context={"column": column},
                )
            )

    for column in LINE_COLUMNS:
        if column not in frame.columns:
            continue
        values = pd.to_numeric(frame[column], errors="coerce")
        present = frame[column].notna() & (frame[column].astype(str).str.strip() != "")
        invalid_numeric = int((present & values.isna()).sum())
        if invalid_numeric:
            findings.append(
                finding(
                    "error",
                    "Invalid line values",
                    f"`{column}` has {invalid_numeric} non-numeric values.",
                    file=relative_path,
                    context={"column": column},
                )
            )

    if "margin" in frame.columns:
        values = pd.to_numeric(frame["margin"], errors="coerce")
        suspicious = int((values.notna() & ((values <= 0) | (values > 200))).sum())
        if suspicious:
            findings.append(
                finding(
                    "warning",
                    "Suspicious margin values",
                    f"`margin` has {suspicious} values outside the broad expected range.",
                    file=relative_path,
                )
            )


def _validate_market_type(
    frame: pd.DataFrame,
    market_type: str,
    relative_path: str,
    findings: list[dict[str, Any]],
) -> None:
    if market_type == "player_props":
        if "player_name" in frame.columns:
            blank_players = int(
                frame["player_name"].isna().sum()
                + (frame["player_name"].astype(str).str.strip() == "").sum()
            )
            if blank_players:
                findings.append(
                    finding(
                        "error",
                        "Blank player names",
                        f"`player_name` has {blank_players} blank values.",
                        file=relative_path,
                    )
                )
        for column in ("player_team", "opposition_team"):
            if column in frame.columns:
                blank_ratio = float(
                    (
                        frame[column].isna()
                        | (frame[column].astype(str).str.strip() == "")
                    ).mean()
                )
                if blank_ratio >= 0.25:
                    findings.append(
                        finding(
                            "warning",
                            "High player team mapping gap",
                            f"`{column}` is blank in {blank_ratio:.0%} of rows.",
                            file=relative_path,
                        )
                    )

    if market_type == "line" and {"home_line", "away_line"}.issubset(frame.columns):
        home_lines = pd.to_numeric(frame["home_line"], errors="coerce")
        away_lines = pd.to_numeric(frame["away_line"], errors="coerce")
        same_sign = int(((home_lines * away_lines) > 0).sum())
        if same_sign:
            findings.append(
                finding(
                    "warning",
                    "Line sides have same sign",
                    f"{same_sign} line rows have home and away lines with the same sign.",
                    file=relative_path,
                )
            )


def _validate_target_coverage(
    frame: pd.DataFrame,
    target_matches: list[str],
    relative_path: str,
    result: dict[str, Any],
    findings: list[dict[str, Any]],
) -> None:
    if not target_matches or "match" not in frame.columns:
        return
    present_matches = set(frame["match"].dropna().astype(str).str.strip())
    target_set = set(target_matches)
    matched = sorted(target_set & present_matches)
    missing = sorted(target_set - present_matches)
    result["matched_target_count"] = len(matched)
    result["missing_target_matches"] = missing
    if missing:
        findings.append(
            finding(
                "warning",
                "Target-round coverage gap",
                "No rows were found for: " + "; ".join(missing),
                file=relative_path,
                context={"missing_target_matches": missing},
            )
        )
    if present_matches and not matched:
        findings.append(
            finding(
                "warning",
                "No target-round rows found",
                "The file has rows, but none match the current target round fixtures.",
                file=relative_path,
            )
        )
