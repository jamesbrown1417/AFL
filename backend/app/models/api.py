from __future__ import annotations

from datetime import datetime

from pydantic import BaseModel, ConfigDict


class ErrorInfo(BaseModel):
    code: str
    message: str
    retriable: bool
    details: dict[str, object]


class ErrorResponse(BaseModel):
    error: ErrorInfo


class HealthResponse(BaseModel):
    status: str
    database_ok: bool
    last_successful_import_at: datetime | None


class TeamSummary(BaseModel):
    id: int
    name: str


class PlayerSummary(BaseModel):
    id: int
    full_name: str


class PlayerStatOption(BaseModel):
    code: str
    label: str


class PlayerStatFilterOptions(BaseModel):
    player_id: int
    player_name: str
    stats: list[PlayerStatOption]
    seasons: list[str]
    oppositions: list[str]
    venues: list[str]
    weather_categories: list[str]
    home_away_options: list[str]


class PlayerGameLogEntry(BaseModel):
    date: datetime
    round_label: str | None
    home: str | None
    venue: str | None
    weather: str | None
    away: str | None
    team: str | None
    opposition: str | None
    margin: int | None
    tog: float | None
    disposals: float | None
    kicks: float | None
    handballs: float | None
    marks: float | None
    goals: float | None
    behinds: float | None
    tackles: float | None
    hitouts: float | None
    frees_for: float | None
    frees_against: float | None
    fantasy: float | None
    cba: float | None
    game_number: int
    selected_stat: str
    selected_value: float | None
    hit: bool | None


class PlayerStatSummary(BaseModel):
    player_id: int
    stat_code: str
    stat_label: str
    line_mode: str
    reference_line: float | None
    lower_bound: float | None
    upper_bound: float | None
    sample_size: int
    proportion_over: float | None
    proportion_under: float | None
    implied_odds_over: float | None
    implied_odds_under: float | None
    proportion_within_interval: float | None
    proportion_outside_interval: float | None
    implied_odds_within_interval: float | None
    implied_odds_outside_interval: float | None


class BookmakerSummary(BaseModel):
    id: int
    code: str
    display_name: str
    enabled: bool
    live_pricing_enabled: bool
    sgm_eligible_count: int


class EventSummary(BaseModel):
    id: int
    match_name: str
    start_time: datetime | None
    round_label: str | None
    venue: str | None
    home_team: TeamSummary
    away_team: TeamSummary
    available_bookmakers: list[str]


class EventDetail(EventSummary):
    pass


class MarketSummary(BaseModel):
    id: int
    event_id: int
    market_type_code: str
    display_name: str
    player: PlayerSummary | None
    line_value: float | None
    bookmaker: str
    available_selection_types: list[str]


class SelectionSummary(BaseModel):
    id: int
    market_id: int
    selection_type: str
    label: str
    decimal_price: float | None
    implied_prob: float | None
    bookmaker: str
    sgm_eligible: bool
    edge_pct: float | None


class OddsSearchResult(BaseModel):
    selection_id: int
    market_id: int
    event_id: int
    match_name: str
    start_time: datetime | None
    bookmaker: str
    market_type_code: str
    market_display_name: str
    player: PlayerSummary | None
    selection_type: str
    label: str
    line_value: float | None
    decimal_price: float | None
    implied_prob: float | None
    edge_pct: float | None
    diff_2025: float | None = None
    diff_last_10: float | None = None
    is_best_price: bool = False
    next_best_prob_diff: float | None = None
    sgm_eligible: bool


class PropSearchResult(BaseModel):
    selection_id: int
    event_id: int
    match_name: str
    start_time: datetime | None
    bookmaker: str
    market_type_code: str
    player: PlayerSummary | None
    selection_type: str
    label: str
    line_value: float | None
    decimal_price: float | None
    implied_prob: float | None
    edge_pct: float | None
    sgm_eligible: bool


class RequestedLeg(BaseModel):
    selection_id: int


class ResolvedLegResponse(BaseModel):
    selection_id: int
    label: str
    market_type_code: str
    selection_type: str
    base_price: float


class SgmQuoteRequest(BaseModel):
    bookmaker: str
    event_id: int
    legs: list[RequestedLeg]
    force_refresh: bool = False


class SgmQuoteResponse(BaseModel):
    model_config = ConfigDict(extra="ignore")

    quote_id: str
    bookmaker: str
    event_id: int
    legs: list[ResolvedLegResponse]
    unadjusted_price: float
    quoted_price: float
    adjustment_factor: float
    from_cache: bool
    quoted_at: datetime
    expires_at: datetime
    status: str


class SgmCompareRequest(BaseModel):
    event_id: int
    selection_ids: list[int]
    force_refresh: bool = False


class SgmAgencyComparison(BaseModel):
    model_config = ConfigDict(extra="ignore")

    quote_id: str
    bookmaker: str
    event_id: int
    legs: list[ResolvedLegResponse]
    unadjusted_price: float
    quoted_price: float
    adjustment_factor: float
    from_cache: bool
    quoted_at: datetime
    expires_at: datetime
    status: str


class SgmCompareResponse(BaseModel):
    event_id: int
    selection_count: int
    results: list[SgmAgencyComparison]


class CgmCompareRequest(BaseModel):
    selection_ids: list[int]


class CgmLegPriceResponse(BaseModel):
    selection_id: int
    match_name: str
    label: str
    market_type_code: str
    selection_type: str
    base_price: float


class CgmAgencyComparison(BaseModel):
    bookmaker: str
    quoted_price: float
    selection_count: int
    legs: list[CgmLegPriceResponse]


class CgmCompareResponse(BaseModel):
    selection_count: int
    results: list[CgmAgencyComparison]
