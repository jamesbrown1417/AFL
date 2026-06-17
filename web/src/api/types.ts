export type ThemeMode = 'light' | 'dark'
export type OddsScope = 'player' | 'match'
export type BuilderMode = 'sgm' | 'cgm'
export type DisplayMode = 'row' | 'tile'
export type SortField = 'player' | 'player_team_line' | 'line' | 'next_best' | 'price' | 'diff_last_10' | 'diff_2025' | 'home_away_diff' | 'win_loss_diff'

export interface HealthResponse {
  status: string
  database_ok: boolean
  last_successful_import_at: string | null
}

export interface DataFileStatus {
  file_name: string
  relative_path: string
  modified_at: string
}

export interface DataFileSection {
  code: string
  title: string
  category: string
  files: DataFileStatus[]
}

export interface DataStatusResponse {
  generated_at: string
  sections: DataFileSection[]
}

export interface TeamSummary {
  id: number
  name: string
}

export interface PlayerSummary {
  id: number
  full_name: string
}

export interface PlayerStatOption {
  code: string
  label: string
}

export interface PlayerStatFilterOptions {
  player_id: number
  player_name: string
  stats: PlayerStatOption[]
  seasons: string[]
  oppositions: string[]
  venues: string[]
  weather_categories: string[]
  home_away_options: string[]
}

export interface PlayerGameLogEntry {
  date: string
  round_label: string | null
  home: string | null
  venue: string | null
  weather: string | null
  away: string | null
  team: string | null
  opposition: string | null
  margin: number | null
  tog: number | null
  disposals: number | null
  kicks: number | null
  handballs: number | null
  marks: number | null
  goals: number | null
  behinds: number | null
  tackles: number | null
  hitouts: number | null
  frees_for: number | null
  frees_against: number | null
  fantasy: number | null
  cba: number | null
  game_number: number
  selected_stat: string
  selected_value: number | null
  hit: boolean | null
}

export interface PlayerStatSummary {
  player_id: number
  stat_code: string
  stat_label: string
  line_mode: string
  reference_line: number | null
  lower_bound: number | null
  upper_bound: number | null
  sample_size: number
  proportion_over: number | null
  proportion_under: number | null
  implied_odds_over: number | null
  implied_odds_under: number | null
  proportion_within_interval: number | null
  proportion_outside_interval: number | null
  implied_odds_within_interval: number | null
  implied_odds_outside_interval: number | null
}

export interface BookmakerSummary {
  id: number
  code: string
  display_name: string
  enabled: boolean
  live_pricing_enabled: boolean
  sgm_eligible_count: number
}

export interface EventSummary {
  id: number
  match_name: string
  start_time: string | null
  round_label: string | null
  venue: string | null
  home_team: TeamSummary
  away_team: TeamSummary
  available_bookmakers: string[]
}

export interface WeatherSummary {
  temperature_c: number | null
  wind_kph: number | null
  precip_probability: number | null
  precip_mm: number | null
  label: string | null
  icon_code: string | null
}

export interface OddsSearchResult {
  selection_id: number
  market_id: number
  event_id: number
  match_name: string
  start_time: string | null
  venue: string | null
  bookmaker: string
  market_type_code: string
  market_display_name: string
  player: PlayerSummary | null
  player_team: string | null
  player_home_away: string | null
  player_team_line: number | null
  selection_type: string
  label: string
  line_value: number | null
  decimal_price: number | null
  implied_prob: number | null
  edge_pct: number | null
  diff_2025: number | null
  diff_last_10: number | null
  home_away_diff: number | null
  win_loss_diff: number | null
  player_position: string | null
  matchup_difficulty: string | null
  over_matchup_difficulty?: string | null
  under_matchup_difficulty?: string | null
  dvp?: number | null
  raw_dvp?: number | null
  dvp_standard_error?: number | null
  dvp_bootstrap_ci_low?: number | null
  dvp_bootstrap_ci_high?: number | null
  dvp_sample_count?: number | null
  dvp_match_count?: number | null
  dvp_observation_count?: number | null
  weather: WeatherSummary | null
  is_best_price: boolean
  next_best_prob_diff: number | null
  sgm_eligible: boolean
}

export interface ArbSearchResult {
  id: string
  match_name: string
  market_name: string
  player_name: string
  player_team: string | null
  opposition_team: string | null
  over_line: number | null
  under_line: number | null
  over_price: number
  over_agency: string
  under_price: number
  under_agency: string
  margin: number
  implied_probability_sum: number
  status: 'Arb' | 'Near'
  source_modified_at: string | null
}

export interface ArbQuery {
  q?: string | null
  market?: string[]
  agency?: string[]
  min_margin?: number | null
  max_margin?: number | null
  limit?: number
  offset?: number
}

export interface ArbFilters {
  query: string
  markets: string[]
  agencies: string[]
  minMargin: string
  maxMargin: string
}

export interface DraftLeg {
  selection_id: number
  event_id: number
  event_label: string
  bookmaker: string
  label: string
  market_type_code: string
  selection_type: string
  base_price: number
  start_time: string | null
  venue: string | null
  player_team: string | null
  player_home_away: string | null
  player_team_line: number | null
  diff_2025: number | null
  diff_last_10: number | null
  home_away_diff: number | null
  win_loss_diff: number | null
  next_best_prob_diff: number | null
  player_position?: string | null
  matchup_difficulty?: string | null
  is_best_price: boolean
}

export interface QuoteLeg {
  selection_id: number
  label: string
  market_type_code: string
  selection_type: string
  base_price: number
}

export interface SgmCompareRequestPayload {
  event_id: number
  selection_ids: number[]
  force_refresh: boolean
}

export interface SgmAgencyComparison {
  quote_id: string
  bookmaker: string
  event_id: number
  legs: QuoteLeg[]
  unadjusted_price: number
  quoted_price: number
  adjustment_factor: number
  from_cache: boolean
  quoted_at: string
  expires_at: string
  status: string
}

export interface SgmCompareResponse {
  event_id: number
  selection_count: number
  results: SgmAgencyComparison[]
}

export interface CgmLegPrice {
  selection_id: number
  match_name: string
  label: string
  market_type_code: string
  selection_type: string
  base_price: number
}

export interface CgmAgencyComparison {
  bookmaker: string
  quoted_price: number
  selection_count: number
  legs: CgmLegPrice[]
}

export interface CgmCompareResponse {
  selection_count: number
  results: CgmAgencyComparison[]
}

export interface OddsQuery {
  bookmaker?: string[]
  scope?: OddsScope
  market_type?: string | null
  event_id?: number[]
  include_player_id?: number[]
  exclude_player_id?: number[]
  sort_by?: string
  sort_dir?: string
  selection_type?: string | null
  matchup_difficulty?: string[]
  min_price?: number | null
  max_price?: number | null
  min_diff_2025?: number | null
  max_diff_2025?: number | null
  min_diff_last_10?: number | null
  max_diff_last_10?: number | null
  min_home_away_diff?: number | null
  max_home_away_diff?: number | null
  min_win_loss_diff?: number | null
  max_win_loss_diff?: number | null
  min_next_best_prob_diff?: number | null
  max_next_best_prob_diff?: number | null
  sgm_only?: boolean
  best_only?: boolean
  limit?: number
  offset?: number
}

export interface OddsFilters {
  scope: OddsScope
  bookmakerCodes: string[]
  marketTypeCode: string | null
  eventId: number | null
  sortBy: string
  sortDirection: 'asc' | 'desc'
  selectionType: string | null
  matchupDifficulties: string[]
  minPrice: string
  maxPrice: string
  minDiffLast10: number
  minNextBestProbDiff: number
  bestOnly: boolean
  sgmOnly: boolean
}

export interface MetricFilters {
  matchupDifficulties: string[]
  minPrice: string
  maxPrice: string
  minDiff2025: number
  maxDiff2025: number
  minDiffLast10: number
  maxDiffLast10: number
  minHomeAwayDiff: number | null
  maxHomeAwayDiff: number | null
  minWinLossDiff: number | null
  maxWinLossDiff: number | null
  minNextBestProbDiff: number
  maxNextBestProbDiff: number
}

export interface PlayerStatsFilters {
  stat: string
  seasons: string[]
  oppositions: string[]
  venues: string[]
  weatherCategories: string[]
  homeAway: string[]
  marginMin: string
  marginMax: string
  lastGames: string
  minutesMinimum: string
  lineMode: 'single' | 'interval'
  referenceLine: string
  lowerBound: string
  upperBound: string
}

export interface ApiErrorEnvelope {
  error?: {
    code?: string
    message?: string
    retriable?: boolean
    details?: unknown
  }
}
