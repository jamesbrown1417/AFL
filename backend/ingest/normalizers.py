from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Any

from app.utils.hashing import sha256_text


MISSING_VALUES = {"", "na", "n/a", "null", "none", "nan"}

COMMON_META_COLUMNS = {
    "match",
    "home_team",
    "away_team",
    "market_name",
    "player_name",
    "player_team",
    "opposition_team",
    "line",
    "over_price",
    "under_price",
    "home_win",
    "away_win",
    "home_line",
    "away_line",
    "margin",
    "agency",
    "start_time",
}

MARKET_TYPE_MAP = {
    "head to head": "h2h",
    "line": "line",
    "total points": "total_points",
    "player disposals": "player_disposals",
    "player goals": "player_goals",
    "player fantasy points": "player_fantasy_points",
    "player marks": "player_marks",
    "player tackles": "player_tackles",
    "player kicks": "player_kicks",
    "player handballs": "player_handballs",
    "player hitouts": "player_hitouts",
    "player clearances": "player_clearances",
}


@dataclass(frozen=True, slots=True)
class NormalizedSelectionRecord:
    bookmaker_code: str
    file_kind: str
    event_key: str
    match_name: str
    home_team_name: str
    away_team_name: str
    start_time_utc: Any
    round_label: str | None
    venue: str | None
    event_status: str
    external_event_id: str | None
    external_competition_id: str | None
    event_payload_meta: dict[str, Any]
    market_key: str
    market_type_code: str
    market_name_raw: str
    player_name: str | None
    line_value: float | None
    stat_side_scope: str
    selection_key: str
    selection_type: str
    selection_label: str
    sort_order: int
    external_market_id: str | None
    external_selection_id: str | None
    selection_payload_meta: dict[str, Any]
    sgm_eligible: bool
    decimal_price: float
    implied_prob: float | None
    margin: float | None


def clean_text(value: Any) -> str | None:
    if value is None:
        return None
    normalized = str(value).strip()
    if normalized.lower() in MISSING_VALUES:
        return None
    return normalized


def normalize_player_name(value: Any) -> str | None:
    cleaned = clean_text(value)
    if not cleaned:
        return None
    return re.sub(r"\s+", " ", cleaned)


def normalize_team_name(value: Any) -> str | None:
    cleaned = clean_text(value)
    if not cleaned:
        return None
    patterns = (
        (r"(^Adelaide)|(Kuwarna)", "Adelaide Crows"),
        (r"^Brisbane", "Brisbane Lions"),
        (r"^Carlton", "Carlton"),
        (r"^Collingwood", "Collingwood Magpies"),
        (r"^Essendon", "Essendon Bombers"),
        (r"(^Fremantle)|(Walyalup)", "Fremantle Dockers"),
        (r"^Geelong", "Geelong Cats"),
        (r"^Gold Coast", "Gold Coast Suns"),
        (r"(^GWS)|(^Greater)", "GWS Giants"),
        (r"^Hawthorn", "Hawthorn Hawks"),
        (r"(^Melbourne)|(Narrm)", "Melbourne Demons"),
        (r"^North Melbourne", "North Melbourne Kangaroos"),
        (r"(^Port Adelaide)|(Yartapuulti)", "Port Adelaide Power"),
        (r"^Richmond", "Richmond Tigers"),
        (r"(^St Kilda)|(Euro-Yroke)", "St Kilda Saints"),
        (r"^Sydney", "Sydney Swans"),
        (r"(^West Coast)|(Waalitj Marawar)", "West Coast Eagles"),
        (r"(^Western Bulldogs)|(Bulldogs)", "Western Bulldogs"),
    )
    for pattern, replacement in patterns:
        if re.search(pattern, cleaned, flags=re.IGNORECASE):
            return replacement
    return cleaned


def normalize_bookmaker_code(value: Any) -> str:
    cleaned = clean_text(value)
    if not cleaned:
        raise ValueError("Bookmaker code is missing.")
    return cleaned.lower().replace(" ", "")


def parse_float(value: Any) -> float | None:
    cleaned = clean_text(value)
    if cleaned is None:
        return None
    return float(cleaned)


def implied_probability(decimal_price: float | None) -> float | None:
    if decimal_price is None or decimal_price <= 0:
        return None
    return 1.0 / decimal_price


def market_type_from_name(value: Any) -> str:
    cleaned = (clean_text(value) or "").lower()
    if cleaned in MARKET_TYPE_MAP:
        return MARKET_TYPE_MAP[cleaned]
    return re.sub(r"[^a-z0-9]+", "_", cleaned).strip("_")


def stat_side_scope(market_type_code: str) -> str:
    if market_type_code.startswith("player_"):
        return "player"
    return "game"


def build_market_key(
    event_key: str, market_type_code: str, player_name: str | None, line_value: float | None
) -> str:
    key_input = f"{event_key}|{market_type_code}|{player_name or 'none'}|{line_value if line_value is not None else 'none'}"
    return sha256_text(key_input)


def build_selection_key(market_key: str, selection_type: str) -> str:
    return sha256_text(f"{market_key}|{selection_type}")


def selection_sort_order(selection_type: str) -> int:
    orders = {"over": 1, "home": 1, "yes": 1, "under": 2, "away": 2, "no": 2, "draw": 3}
    return orders.get(selection_type, 99)


def extract_raw_meta(row: dict[str, str]) -> dict[str, Any]:
    meta: dict[str, Any] = {}
    for key, value in row.items():
        if key in COMMON_META_COLUMNS:
            continue
        cleaned = clean_text(value)
        if cleaned is not None:
            meta[key] = cleaned
    return meta


def resolve_bookmaker_meta(
    bookmaker_code: str, file_kind: str, row: dict[str, str], selection_type: str
) -> tuple[str | None, str | None, str | None, str | None, dict[str, Any], bool]:
    raw_meta = extract_raw_meta(row)
    external_event_id: str | None = None
    external_competition_id: str | None = None
    external_market_id: str | None = None
    external_selection_id: str | None = None

    if bookmaker_code == "sportsbet" and file_kind == "player_props":
        external_event_id = clean_text(row.get("event_external_id"))
        external_competition_id = clean_text(row.get("competition_external_id"))
        external_market_id = clean_text(row.get("market_id"))
        external_selection_id = (
            clean_text(row.get("player_id"))
            if selection_type == "over"
            else clean_text(row.get("player_id_unders"))
        )
    elif bookmaker_code == "tab" and file_kind == "player_props":
        external_selection_id = (
            clean_text(row.get("prop_id"))
            if selection_type == "over"
            else clean_text(row.get("under_prop_id"))
        )
    elif bookmaker_code == "neds" and file_kind == "player_props":
        external_event_id = clean_text(row.get("event_id"))
        external_market_id = clean_text(row.get("market_id"))
        external_selection_id = clean_text(row.get("entrant_id"))
    elif bookmaker_code == "pointsbet" and file_kind == "player_props":
        external_event_id = clean_text(row.get("EventKey"))
        external_market_id = clean_text(row.get("MarketKey"))
        external_selection_id = (
            clean_text(row.get("OutcomeKey"))
            if selection_type == "over"
            else clean_text(row.get("OutcomeKey_unders"))
        )
    elif bookmaker_code == "betright" and file_kind == "player_props":
        external_event_id = clean_text(row.get("event_id"))
        external_market_id = (
            clean_text(row.get("fixed_market_id"))
            if selection_type == "over"
            else clean_text(row.get("fixed_market_id_under"))
        )
        external_selection_id = (
            clean_text(row.get("outcome_id"))
            if selection_type == "over"
            else clean_text(row.get("outcome_id_under"))
        )

    payload_meta = {
        **raw_meta,
        "bookmaker_code": bookmaker_code,
        "file_kind": file_kind,
        "selection_type": selection_type,
    }
    if external_event_id:
        payload_meta["external_event_id"] = external_event_id
    if external_competition_id:
        payload_meta["external_competition_id"] = external_competition_id
    if external_market_id:
        payload_meta["external_market_id"] = external_market_id
    if external_selection_id:
        payload_meta["external_selection_id"] = external_selection_id

    sgm_eligible = (
        file_kind == "player_props"
        and bookmaker_code in {"sportsbet", "tab", "neds", "pointsbet", "betright"}
        and external_selection_id is not None
    )
    return (
        external_event_id,
        external_competition_id,
        external_market_id,
        external_selection_id,
        payload_meta,
        sgm_eligible,
    )


def build_selection_label(
    market_type_code: str,
    selection_type: str,
    line_value: float | None,
    player_name: str | None,
    home_team_name: str,
    away_team_name: str,
    explicit_label: str | None = None,
) -> str:
    if explicit_label:
        return explicit_label
    if market_type_code.startswith("player_") and player_name:
        return f"{player_name} {selection_type.title()} {line_value}"
    if market_type_code == "h2h":
        return home_team_name if selection_type == "home" else away_team_name
    if market_type_code == "line":
        if selection_type == "home":
            return f"{home_team_name} {line_value}"
        return f"{away_team_name} {line_value}"
    if market_type_code == "total_points":
        return f"{selection_type.title()} {line_value}"
    return selection_type.title()
