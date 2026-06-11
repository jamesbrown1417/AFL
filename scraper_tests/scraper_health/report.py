from __future__ import annotations

from pathlib import Path
from typing import Any

from jinja2 import Environment, select_autoescape

from .manifest import expected_market_order
from .models import STATUS_RANK, count_statuses, worst_status


def build_coverage_matrix(
    manifest: dict[str, Any],
    bookmaker_results: list[dict[str, Any]],
) -> dict[str, Any]:
    markets = expected_market_order(manifest)
    by_code = {result["code"]: result for result in bookmaker_results}
    rows: list[dict[str, Any]] = []

    for bookmaker in manifest.get("bookmakers", []):
        result = by_code[bookmaker["code"]]
        outputs = result.get("outputs", [])
        cells = []
        for market in markets:
            relevant = [output for output in outputs if output["market"] == market["code"]]
            if not relevant:
                cells.append(
                    {
                        "market": market["code"],
                        "status": "na",
                        "label": "Not scoped",
                        "row_count": None,
                        "matched_target_count": None,
                        "file_count": 0,
                        "files": [],
                        "issues": [],
                        "issue_count": 0,
                        "issue_summary": "Not in scope for this bookmaker.",
                    }
                )
                continue
            status = worst_status(output["status"] for output in relevant)
            issues = [
                {
                    "severity": issue.get("severity", "warning"),
                    "title": issue.get("title", "Finding"),
                    "detail": issue.get("detail", ""),
                    "file": issue.get("file", output["path"]),
                }
                for output in relevant
                for issue in output.get("findings", [])
            ]
            files = [
                {
                    "path": output["path"],
                    "status": output["status"],
                    "row_count": output.get("row_count"),
                    "updated_during_run": output.get("updated_during_run"),
                }
                for output in relevant
            ]
            cells.append(
                {
                    "market": market["code"],
                    "status": status,
                    "label": status.title(),
                    "row_count": sum(output.get("row_count") or 0 for output in relevant),
                    "matched_target_count": sum(
                        output.get("matched_target_count") or 0 for output in relevant
                    ),
                    "file_count": len(relevant),
                    "files": files,
                    "issues": issues,
                    "issue_count": len(issues),
                    "issue_summary": summarize_cell_issues(issues, files),
                }
            )
        rows.append(
            {
                "bookmaker": result["name"],
                "code": result["code"],
                "status": result["status"],
                "cells": cells,
            }
        )
    return {"markets": markets, "rows": rows}


def summarize_cell_issues(issues: list[dict[str, Any]], files: list[dict[str, Any]]) -> str:
    if issues:
        first = issues[0]
        suffix = "" if len(issues) == 1 else f" + {len(issues) - 1} more"
        return f"{first['title']}{suffix}"
    if files:
        return "All expected files passed."
    return "Not in scope."


def build_counters(
    prefetch_results: list[dict[str, Any]],
    bookmaker_results: list[dict[str, Any]],
    repo_warnings: list[dict[str, Any]],
) -> dict[str, Any]:
    outputs = [output for result in bookmaker_results for output in result.get("outputs", [])]
    findings = collect_findings(prefetch_results, bookmaker_results, repo_warnings)
    finding_counts = {status: 0 for status in STATUS_RANK}
    for item in findings:
        severity = item.get("severity", "pass")
        if severity in finding_counts:
            finding_counts[severity] += 1
    return {
        "prefetch": count_statuses(prefetch_results),
        "scrapers": count_statuses(bookmaker_results),
        "outputs": count_statuses(outputs),
        "findings": finding_counts,
        "output_count": len(outputs),
        "scraper_count": len(bookmaker_results),
        "prefetch_count": len(prefetch_results),
    }


def collect_findings(
    prefetch_results: list[dict[str, Any]],
    bookmaker_results: list[dict[str, Any]],
    repo_warnings: list[dict[str, Any]],
) -> list[dict[str, Any]]:
    findings: list[dict[str, Any]] = list(repo_warnings)
    for result in prefetch_results:
        for item in result.get("findings", []):
            copied = dict(item)
            copied.setdefault("source", result["name"])
            findings.append(copied)
    for result in bookmaker_results:
        for item in result.get("findings", []):
            copied = dict(item)
            copied.setdefault("source", result["name"])
            findings.append(copied)
        for output in result.get("outputs", []):
            for item in output.get("findings", []):
                copied = dict(item)
                copied.setdefault("source", result["name"])
                findings.append(copied)
    return findings


def render_html(summary: dict[str, Any]) -> str:
    environment = Environment(autoescape=select_autoescape(default=True))
    return environment.from_string(REPORT_TEMPLATE).render(summary=summary)


def write_report(summary: dict[str, Any], path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(render_html(summary), encoding="utf-8")


REPORT_TEMPLATE = r"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>AFL Scraper Health Report</title>
  <style>
    :root {
      --bg: #f6f8fb;
      --surface: #ffffff;
      --ink: #18212f;
      --muted: #657184;
      --line: #dce3ed;
      --soft-line: #edf1f6;
      --pass: #16855e;
      --pass-bg: #e8f6ef;
      --warning: #a9650f;
      --warning-bg: #fff3db;
      --blocked: #6f5cc2;
      --blocked-bg: #f0edff;
      --error: #c3374a;
      --error-bg: #ffe8ec;
      --accent: #275ecf;
      --accent-2: #0f9f9a;
      --radius: 8px;
      --shadow: 0 18px 50px rgba(24, 33, 47, 0.08);
      color-scheme: light;
      font-family: Inter, ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
    }

    * { box-sizing: border-box; }
    body {
      margin: 0;
      background:
        linear-gradient(180deg, rgba(39, 94, 207, 0.08), rgba(39, 94, 207, 0) 360px),
        var(--bg);
      color: var(--ink);
      font-size: 14px;
      line-height: 1.45;
    }
    .page {
      width: min(1420px, calc(100vw - 48px));
      margin: 0 auto;
      padding: 36px 0 56px;
    }
    header {
      display: grid;
      grid-template-columns: minmax(0, 1fr) auto;
      gap: 28px;
      align-items: end;
      margin-bottom: 24px;
    }
    h1 {
      margin: 0 0 8px;
      font-size: 34px;
      line-height: 1.05;
      font-weight: 760;
      letter-spacing: 0;
    }
    h2 {
      margin: 0 0 14px;
      font-size: 18px;
      line-height: 1.2;
      font-weight: 720;
      letter-spacing: 0;
    }
    h3 {
      margin: 0;
      font-size: 15px;
      line-height: 1.2;
      font-weight: 700;
      letter-spacing: 0;
    }
    p { margin: 0; }
    .muted { color: var(--muted); }
    .mono {
      font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", monospace;
      font-size: 12px;
    }
    .hero-meta {
      display: grid;
      grid-template-columns: repeat(2, minmax(170px, 1fr));
      gap: 10px;
      min-width: 360px;
    }
    .meta-box, .panel {
      background: rgba(255, 255, 255, 0.92);
      border: 1px solid var(--line);
      border-radius: var(--radius);
      box-shadow: var(--shadow);
    }
    .meta-box {
      padding: 14px 16px;
    }
    .meta-label {
      color: var(--muted);
      font-size: 11px;
      font-weight: 700;
      text-transform: uppercase;
      letter-spacing: 0.08em;
      margin-bottom: 5px;
    }
    .meta-value {
      font-size: 16px;
      font-weight: 740;
    }
    .status-strip {
      display: grid;
      grid-template-columns: 1.1fr repeat(4, minmax(0, 1fr));
      gap: 12px;
      margin: 20px 0 18px;
    }
    .status-tile {
      border-radius: var(--radius);
      padding: 16px;
      border: 1px solid var(--line);
      background: var(--surface);
      min-height: 92px;
    }
    .status-tile strong {
      display: block;
      font-size: 28px;
      line-height: 1;
      margin-top: 8px;
    }
    .status-tile .small {
      color: var(--muted);
      font-size: 12px;
      font-weight: 650;
    }
    .overall {
      color: var(--surface);
      background: linear-gradient(135deg, #172033, #275ecf);
      border-color: rgba(255,255,255,0.18);
    }
    .overall.status-pass { background: linear-gradient(135deg, #103629, #16855e); }
    .overall.status-warning { background: linear-gradient(135deg, #493205, #b86d12); }
    .overall.status-blocked { background: linear-gradient(135deg, #30245a, #6f5cc2); }
    .overall.status-error { background: linear-gradient(135deg, #4b101a, #c3374a); }
    .overall .small { color: rgba(255,255,255,0.74); }
    .grid {
      display: grid;
      grid-template-columns: 1fr;
      gap: 18px;
    }
    .panel {
      padding: 18px;
      overflow: hidden;
    }
    .panel-header {
      display: flex;
      justify-content: space-between;
      gap: 16px;
      align-items: start;
      margin-bottom: 14px;
    }
    .timeline {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(260px, 1fr));
      gap: 12px;
    }
    .fixture {
      border: 1px solid var(--soft-line);
      border-radius: var(--radius);
      padding: 14px;
      background: #fbfcfe;
    }
    .fixture-title {
      font-size: 15px;
      font-weight: 720;
      margin-bottom: 8px;
    }
    table {
      width: 100%;
      border-collapse: collapse;
      table-layout: auto;
    }
    th, td {
      padding: 11px 10px;
      border-bottom: 1px solid var(--soft-line);
      vertical-align: top;
      text-align: left;
    }
    th {
      color: var(--muted);
      font-size: 11px;
      text-transform: uppercase;
      letter-spacing: 0.07em;
      font-weight: 760;
      background: #fbfcfe;
    }
    tbody tr:hover td { background: #fafcff; }
    .coverage-table th {
      text-align: left;
      white-space: nowrap;
    }
    .coverage-table td {
      text-align: left;
      white-space: normal;
      min-width: 190px;
    }
    .coverage-table th:first-child, .coverage-table td:first-child {
      text-align: left;
      position: sticky;
      left: 0;
      background: var(--surface);
      z-index: 1;
    }
    .badge {
      display: inline-flex;
      align-items: center;
      min-height: 24px;
      padding: 4px 9px;
      border-radius: 999px;
      font-size: 12px;
      font-weight: 720;
      border: 1px solid transparent;
    }
    .status-pass { color: var(--pass); background: var(--pass-bg); border-color: #bee7d2; }
    .status-warning { color: var(--warning); background: var(--warning-bg); border-color: #f3d89a; }
    .status-blocked { color: var(--blocked); background: var(--blocked-bg); border-color: #d8d1ff; }
    .status-error { color: var(--error); background: var(--error-bg); border-color: #f2bec6; }
    .status-na { color: #8792a4; background: #f1f4f8; border-color: #dce3ed; }
    .cell-note {
      display: block;
      color: var(--muted);
      font-size: 11px;
      font-weight: 600;
      margin-top: 4px;
    }
    .cell-reason {
      display: block;
      color: var(--ink);
      font-size: 12px;
      line-height: 1.3;
      margin-top: 7px;
      max-width: 260px;
      overflow-wrap: anywhere;
    }
    .cell-detail {
      box-shadow: none;
      margin: 8px 0 0;
      background: #fff;
    }
    .cell-detail summary {
      padding: 7px 8px;
      font-size: 11px;
      color: var(--muted);
      font-weight: 760;
    }
    .cell-detail-body {
      padding: 8px;
      border-top: 1px solid var(--soft-line);
    }
    .cell-file {
      padding-bottom: 7px;
      margin-bottom: 7px;
      border-bottom: 1px solid var(--soft-line);
    }
    .cell-file:last-child {
      border-bottom: 0;
      margin-bottom: 0;
      padding-bottom: 0;
    }
    .cell-issue {
      margin-top: 6px;
      color: var(--ink);
      font-size: 12px;
      line-height: 1.35;
    }
    .cell-issue-title {
      font-weight: 760;
    }
    .finding {
      display: grid;
      grid-template-columns: 96px minmax(0, 1fr);
      gap: 12px;
      padding: 12px 0;
      border-bottom: 1px solid var(--soft-line);
    }
    .finding:last-child { border-bottom: 0; }
    .file-path {
      color: var(--muted);
      margin-top: 4px;
      overflow-wrap: anywhere;
    }
    details {
      border: 1px solid var(--soft-line);
      border-radius: var(--radius);
      background: #fbfcfe;
      margin: 8px 0;
      overflow: hidden;
    }
    summary {
      cursor: pointer;
      padding: 10px 12px;
      font-weight: 700;
    }
    pre {
      margin: 0;
      padding: 12px;
      max-height: 320px;
      overflow: auto;
      background: #111827;
      color: #d7dde8;
      font-size: 12px;
      line-height: 1.45;
    }
    .scroll-x {
      overflow-x: auto;
      border: 1px solid var(--soft-line);
      border-radius: var(--radius);
    }
    .empty {
      padding: 20px;
      border: 1px dashed var(--line);
      border-radius: var(--radius);
      color: var(--muted);
      background: #fbfcfe;
    }
    @media (max-width: 900px) {
      .page { width: min(100vw - 24px, 1420px); padding-top: 24px; }
      header { grid-template-columns: 1fr; }
      .hero-meta { min-width: 0; grid-template-columns: 1fr; }
      .status-strip { grid-template-columns: repeat(2, minmax(0, 1fr)); }
      .overall { grid-column: 1 / -1; }
    }
    @media (max-width: 560px) {
      h1 { font-size: 28px; }
      .status-strip { grid-template-columns: 1fr; }
      th, td { padding: 9px 8px; }
      .finding { grid-template-columns: 1fr; gap: 6px; }
    }
  </style>
</head>
<body>
  <main class="page">
    <header>
      <div>
        <h1>AFL Scraper Health Report</h1>
        <p class="muted">
          Generated {{ summary.generated_at }}
          {% if summary.execution_mode == "production" %}
            from currently existing production output files.
          {% else %}
            from an isolated shadow workspace.
          {% endif %}
        </p>
      </div>
      <div class="hero-meta">
        <div class="meta-box">
          <div class="meta-label">Target Round</div>
          <div class="meta-value">{{ summary.target.target_round or "No future fixtures" }}</div>
        </div>
        <div class="meta-box">
          <div class="meta-label">Timezone</div>
          <div class="meta-value">{{ summary.target.timezone }}</div>
        </div>
      </div>
    </header>

    <section class="status-strip">
      <div class="status-tile overall status-{{ summary.overall_status }}">
        <div class="small">Overall status</div>
        <strong>{{ summary.overall_status|upper }}</strong>
      </div>
      <div class="status-tile">
        <div class="small">Scrapers</div>
        <strong>{{ summary.counters.scraper_count }}</strong>
        <span class="muted">{{ summary.counters.scrapers.error }} error, {{ summary.counters.scrapers.blocked }} blocked</span>
      </div>
      <div class="status-tile">
        <div class="small">Outputs</div>
        <strong>{{ summary.counters.output_count }}</strong>
        <span class="muted">{{ summary.counters.outputs.warning }} warning, {{ summary.counters.outputs.error }} error</span>
      </div>
      <div class="status-tile">
        <div class="small">Prefetch</div>
        <strong>{{ summary.counters.prefetch_count }}</strong>
        <span class="muted">{{ summary.counters.prefetch.blocked }} blocked</span>
      </div>
      <div class="status-tile">
        <div class="small">Findings</div>
        <strong>{{ summary.findings|length }}</strong>
        <span class="muted">{{ summary.counters.findings.warning }} warning, {{ summary.counters.findings.error }} error</span>
      </div>
    </section>

    <div class="grid">
      <section class="panel">
        <div class="panel-header">
          <div>
            <h2>{{ "Production Output Audit" if summary.execution_mode == "production" else "Production Workflow Parity" }}</h2>
            <p class="muted">
              {% if summary.execution_mode == "production" %}
                No scraper or prefetch commands were run; this report validates files already present under the project root.
              {% else %}
                The shadow run mirrors afl_update_file.sh prefetch cleanup and checks that production artifacts were not modified.
              {% endif %}
            </p>
          </div>
          <span class="badge status-{{ 'pass' if summary.execution_mode == 'production' or summary.source_write_findings|length == 0 else 'error' }}">
            {% if summary.execution_mode == "production" %}
              read-only
            {% else %}
              {{ 'isolated' if summary.source_write_findings|length == 0 else 'source writes detected' }}
            {% endif %}
          </span>
        </div>
        <div class="scroll-x">
          <table>
            <thead>
              <tr>
                <th>Check</th>
                <th>Status</th>
                <th>Detail</th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td>Execution mode</td>
                <td><span class="badge status-pass">{{ summary.execution_mode }}</span></td>
                <td>
                  {% if summary.execution_mode == "production" %}
                    Existing production outputs were validated in place; no scraper outputs were updated by this suite.
                  {% else %}
                    Scrapers ran in a copied workspace below scraper_tests/latest.
                  {% endif %}
                </td>
              </tr>
              <tr>
                <td>Production cache cleanup</td>
                <td><span class="badge status-pass">{{ "skipped" if summary.execution_mode == "production" else "mirrored" }}</span></td>
                <td>
                  {% if summary.execution_mode == "production" %}
                    Not run in production-output mode.
                  {% else %}
                    {{ summary.production_cache_cleanup|length }} stale workspace artifact{{ "" if summary.production_cache_cleanup|length == 1 else "s" }} removed before prefetch.
                  {% endif %}
                </td>
              </tr>
              <tr>
                <td>Source artifact writes</td>
                <td>
                  <span class="badge status-{{ 'pass' if summary.source_write_findings|length == 0 else 'error' }}">
                    {{ 'pass' if summary.source_write_findings|length == 0 else 'error' }}
                  </span>
                </td>
                <td>
                  {% if summary.execution_mode == "production" %}
                    No scraper commands were run by the suite.
                  {% else %}
                    {{ "No production Data/OddsScraper artifacts changed during the isolated run." if summary.source_write_findings|length == 0 else summary.source_write_findings|length ~ " production artifact mutation(s) detected." }}
                  {% endif %}
                </td>
              </tr>
              <tr>
                <td>Validation root</td>
                <td><span class="badge status-pass">{{ "production" if summary.execution_mode == "production" else "shadow" }}</span></td>
                <td class="mono">{{ summary.validation_root or summary.workspace }}</td>
              </tr>
            </tbody>
          </table>
        </div>
      </section>

      <section class="panel">
        <div class="panel-header">
          <div>
            <h2>Target Round Timeline</h2>
            <p class="muted">Future games selected from the first upcoming fixture's round.</p>
          </div>
          <span class="badge status-{{ summary.overall_status }}">{{ summary.target.fixtures|length }} games</span>
        </div>
        {% if summary.target.fixtures %}
        <div class="timeline">
          {% for fixture in summary.target.fixtures %}
          <article class="fixture">
            <div class="fixture-title">{{ fixture.match }}</div>
            <div class="muted">{{ fixture.start_time_local }}</div>
            <div class="muted">{{ fixture.venue }}</div>
          </article>
          {% endfor %}
        </div>
        {% else %}
        <div class="empty">No future fixtures were found in the fixture file.</div>
        {% endif %}
      </section>

      <section class="panel">
        <div class="panel-header">
          <div>
            <h2>Coverage Matrix</h2>
            <p class="muted">Each cell summarizes the exact file-level reason for its status. Coverage gaps are advisory warnings.</p>
          </div>
        </div>
        <div class="scroll-x">
          <table class="coverage-table">
            <thead>
              <tr>
                <th>Bookmaker</th>
                {% for market in summary.coverage.markets %}
                <th>{{ market.label }}</th>
                {% endfor %}
              </tr>
            </thead>
            <tbody>
              {% for row in summary.coverage.rows %}
              <tr>
                <td><span class="badge status-{{ row.status }}">{{ row.bookmaker }}</span></td>
                {% for cell in row.cells %}
                <td>
                  <span class="badge status-{{ cell.status }}">{{ cell.label }}</span>
                  {% if cell.row_count is not none %}
                    <span class="cell-note">{{ cell.row_count }} rows / {{ cell.file_count }} files</span>
                  {% endif %}
                  <span class="cell-reason">{{ cell.issue_summary }}</span>
                  {% if cell.files %}
                  <details class="cell-detail">
                    <summary>{{ cell.issue_count }} finding{{ "" if cell.issue_count == 1 else "s" }} · {{ cell.file_count }} file{{ "" if cell.file_count == 1 else "s" }}</summary>
                    <div class="cell-detail-body">
                      {% for file in cell.files %}
                      <div class="cell-file">
                        <div><span class="badge status-{{ file.status }}">{{ file.status }}</span></div>
                        <div class="mono file-path">{{ file.path }}</div>
                        <div class="cell-note">{{ file.row_count if file.row_count is not none else "-" }} rows{% if file.updated_during_run is not none %} · refreshed: {{ "yes" if file.updated_during_run else "no" }}{% endif %}</div>
                        {% set file_issues = cell.issues | selectattr("file", "equalto", file.path) | list %}
                        {% if file_issues %}
                          {% for issue in file_issues %}
                          <div class="cell-issue">
                            <span class="badge status-{{ issue.severity }}">{{ issue.severity }}</span>
                            <span class="cell-issue-title">{{ issue.title }}</span>
                            <div>{{ issue.detail }}</div>
                          </div>
                          {% endfor %}
                        {% else %}
                          <div class="cell-note">No findings for this file.</div>
                        {% endif %}
                      </div>
                      {% endfor %}
                    </div>
                  </details>
                  {% endif %}
                </td>
                {% endfor %}
              </tr>
              {% endfor %}
            </tbody>
          </table>
        </div>
      </section>

      <section class="panel">
        <div class="panel-header">
          <div>
            <h2>Scraper Runs</h2>
            <p class="muted">
              {% if summary.execution_mode == "production" %}
                Commands are listed for traceability but were not run; statuses come from validating existing output files.
              {% else %}
                Sequential isolated execution. Raw logs are shown only for command failures/timeouts; normal R package attach messages are stored in log files but hidden here.
              {% endif %}
            </p>
          </div>
        </div>
        <div class="scroll-x">
          <table>
            <thead>
              <tr>
                <th>Bookmaker</th>
                <th>Status</th>
                <th>Runtime</th>
                <th>Exit</th>
                <th>Command</th>
              </tr>
            </thead>
            <tbody>
            {% for result in summary.bookmakers %}
              <tr>
                <td><strong>{{ result.name }}</strong></td>
                <td><span class="badge status-{{ result.status }}">{{ result.status }}</span></td>
                <td>{{ "skipped" if result.skipped else result.duration_seconds ~ "s" }}</td>
                <td>{{ result.exit_code if result.exit_code is not none else "-" }}</td>
                <td class="mono">{{ result.command|join(" ") }}</td>
              </tr>
              {% if (result.exit_code is not none and result.exit_code != 0) or result.timed_out %}
              <tr>
                <td colspan="5">
                  {% if result.stderr_excerpt %}
                  <details>
                    <summary>{{ result.name }} failure stderr excerpt</summary>
                    <pre>{{ result.stderr_excerpt }}</pre>
                  </details>
                  {% endif %}
                  {% if result.stdout_excerpt %}
                  <details>
                    <summary>{{ result.name }} failure stdout excerpt</summary>
                    <pre>{{ result.stdout_excerpt }}</pre>
                  </details>
                  {% endif %}
                </td>
              </tr>
              {% endif %}
            {% endfor %}
            </tbody>
          </table>
        </div>
      </section>

      <section class="panel">
        <div class="panel-header">
          <div>
            <h2>Artifact Inventory</h2>
            <p class="muted">Expected CSVs, refresh state, row counts, and validation status.</p>
          </div>
        </div>
        <div class="scroll-x">
          <table>
            <thead>
              <tr>
                <th>Bookmaker</th>
                <th>Market</th>
                <th>Status</th>
                <th>Rows</th>
                <th>Refreshed</th>
                <th>Path</th>
              </tr>
            </thead>
            <tbody>
              {% for result in summary.bookmakers %}
                {% for output in result.outputs %}
                <tr>
                  <td>{{ result.name }}</td>
                  <td>{{ output.market_label }}</td>
                  <td><span class="badge status-{{ output.status }}">{{ output.status }}</span></td>
                  <td>{{ output.row_count if output.row_count is not none else "-" }}</td>
                  <td>
                    {% if output.updated_during_run is none %}
                      -
                    {% elif output.updated_during_run %}
                      yes
                    {% else %}
                      no
                    {% endif %}
                  </td>
                  <td class="mono">{{ output.path }}</td>
                </tr>
                {% endfor %}
              {% endfor %}
            </tbody>
          </table>
        </div>
      </section>

      <section class="panel">
        <div class="panel-header">
          <div>
            <h2>Findings</h2>
            <p class="muted">Errors and blocked items need action; warnings include advisory market availability gaps.</p>
          </div>
        </div>
        {% if summary.findings %}
          {% for item in summary.findings %}
          <div class="finding">
            <div><span class="badge status-{{ item.severity }}">{{ item.severity }}</span></div>
            <div>
              <h3>{{ item.title }}</h3>
              <p>{{ item.detail }}</p>
              {% if item.file %}
              <div class="file-path mono">{{ item.file }}</div>
              {% endif %}
            </div>
          </div>
          {% endfor %}
        {% else %}
          <div class="empty">No findings were generated.</div>
        {% endif %}
      </section>

      <section class="panel">
        <div class="panel-header">
          <div>
            <h2>Prefetch Helpers</h2>
            <p class="muted">
              {% if summary.execution_mode == "production" %}
                Helpers were not run; cached helper artifacts are inspected read-only where applicable.
              {% else %}
                Browser or cached-response steps that feed parser-style scrapers.
              {% endif %}
            </p>
          </div>
        </div>
        <div class="scroll-x">
          <table>
            <thead>
              <tr>
                <th>Helper</th>
                <th>Status</th>
                <th>Mode</th>
                <th>Artifacts</th>
              </tr>
            </thead>
            <tbody>
              {% for result in summary.prefetch %}
              <tr>
                <td>{{ result.name }}</td>
                <td><span class="badge status-{{ result.status }}">{{ result.status }}</span></td>
                <td>{{ "skipped" if result.skipped else "ran" }}</td>
                <td>
                  {% for artifact in result.artifacts %}
                    <div class="mono">{{ artifact.path }} · {{ artifact.count }} found</div>
                  {% endfor %}
                </td>
              </tr>
              {% endfor %}
            </tbody>
          </table>
        </div>
      </section>
    </div>
  </main>
</body>
</html>
"""
