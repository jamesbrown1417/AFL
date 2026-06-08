from __future__ import annotations

from copy import deepcopy
from pathlib import Path
from typing import Any

import yaml


def load_manifest(path: Path) -> dict[str, Any]:
    with path.open("r", encoding="utf-8") as handle:
        manifest = yaml.safe_load(handle)

    if not isinstance(manifest, dict):
        raise ValueError(f"Manifest must be a mapping: {path}")
    if manifest.get("version") != 1:
        raise ValueError(f"Unsupported manifest version in {path}: {manifest.get('version')}")

    market_types = manifest.get("market_types")
    bookmakers = manifest.get("bookmakers")
    if not isinstance(market_types, dict) or not isinstance(bookmakers, list):
        raise ValueError("Manifest requires 'market_types' mapping and 'bookmakers' list")

    resolved = deepcopy(manifest)
    for bookmaker in resolved["bookmakers"]:
        if "code" not in bookmaker or "outputs" not in bookmaker:
            raise ValueError("Every bookmaker requires 'code' and 'outputs'")
        for output in bookmaker["outputs"]:
            market_type = output.get("market_type")
            if market_type not in market_types:
                raise ValueError(
                    f"{bookmaker['code']} output {output.get('path')} has unknown market_type "
                    f"{market_type!r}"
                )
            output.setdefault(
                "required_columns",
                list(market_types[market_type]["required_columns"]),
            )
            output.setdefault("market_label", market_label(resolved, output["market"]))

    return resolved


def market_label(manifest: dict[str, Any], market: str) -> str:
    explicit = {
        "h2h": "H2H",
        "line": "Line",
        "totals": "Totals",
        "player_disposals": "Disposals",
        "player_goals": "Goals",
        "player_fantasy_points": "Fantasy",
        "player_marks": "Marks",
        "player_tackles": "Tackles",
        "player_kicks": "Kicks",
        "player_handballs": "Handballs",
        "player_hitouts": "Hitouts",
    }
    if market in explicit:
        return explicit[market]
    if market in manifest.get("market_types", {}):
        return str(manifest["market_types"][market].get("label", market))
    return market.replace("_", " ").title()


def expected_market_order(manifest: dict[str, Any]) -> list[dict[str, str]]:
    seen: set[str] = set()
    ordered: list[dict[str, str]] = []
    for bookmaker in manifest.get("bookmakers", []):
        for output in bookmaker.get("outputs", []):
            market = output["market"]
            if market in seen:
                continue
            seen.add(market)
            ordered.append({"code": market, "label": market_label(manifest, market)})
    return ordered

