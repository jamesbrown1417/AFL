CREATE SEQUENCE IF NOT EXISTS import_runs_seq START 1;
CREATE SEQUENCE IF NOT EXISTS imported_files_seq START 1;
CREATE SEQUENCE IF NOT EXISTS bookmakers_seq START 1;
CREATE SEQUENCE IF NOT EXISTS teams_seq START 1;
CREATE SEQUENCE IF NOT EXISTS players_seq START 1;
CREATE SEQUENCE IF NOT EXISTS player_game_logs_seq START 1;
CREATE SEQUENCE IF NOT EXISTS events_seq START 1;
CREATE SEQUENCE IF NOT EXISTS markets_seq START 1;
CREATE SEQUENCE IF NOT EXISTS selections_seq START 1;
CREATE SEQUENCE IF NOT EXISTS outcome_prices_seq START 1;
CREATE SEQUENCE IF NOT EXISTS selection_metrics_seq START 1;

CREATE TABLE IF NOT EXISTS import_runs (
  import_run_id BIGINT PRIMARY KEY DEFAULT nextval('import_runs_seq'),
  started_at TIMESTAMP NOT NULL,
  finished_at TIMESTAMP,
  status TEXT NOT NULL,
  triggered_by TEXT NOT NULL,
  files_scanned INTEGER NOT NULL DEFAULT 0,
  files_imported INTEGER NOT NULL DEFAULT 0,
  error_count INTEGER NOT NULL DEFAULT 0,
  notes TEXT
);

CREATE TABLE IF NOT EXISTS imported_files (
  imported_file_id BIGINT PRIMARY KEY DEFAULT nextval('imported_files_seq'),
  import_run_id BIGINT NOT NULL,
  source_path TEXT NOT NULL,
  sha256 TEXT NOT NULL,
  size_bytes BIGINT NOT NULL,
  modified_at TIMESTAMP,
  file_kind TEXT NOT NULL,
  bookmaker_code TEXT NOT NULL,
  rows_read INTEGER NOT NULL DEFAULT 0,
  rows_loaded INTEGER NOT NULL DEFAULT 0,
  status TEXT NOT NULL,
  error_text TEXT,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  UNIQUE(source_path, sha256)
);

CREATE TABLE IF NOT EXISTS bookmakers (
  bookmaker_id BIGINT PRIMARY KEY DEFAULT nextval('bookmakers_seq'),
  code TEXT NOT NULL UNIQUE,
  display_name TEXT NOT NULL,
  enabled BOOLEAN NOT NULL DEFAULT TRUE
);

CREATE TABLE IF NOT EXISTS teams (
  team_id BIGINT PRIMARY KEY DEFAULT nextval('teams_seq'),
  league_code TEXT NOT NULL,
  name TEXT NOT NULL,
  normalized_name TEXT NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS players (
  player_id BIGINT PRIMARY KEY DEFAULT nextval('players_seq'),
  full_name TEXT NOT NULL,
  normalized_name TEXT NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS player_game_logs (
  player_game_log_id BIGINT PRIMARY KEY DEFAULT nextval('player_game_logs_seq'),
  game_log_key TEXT NOT NULL UNIQUE,
  player_id BIGINT NOT NULL,
  source_player_id TEXT,
  match_name TEXT NOT NULL,
  season_name TEXT NOT NULL,
  start_time_utc TIMESTAMP NOT NULL,
  round_label TEXT,
  venue TEXT,
  weather_category TEXT,
  weather_description TEXT,
  home_team TEXT,
  away_team TEXT,
  player_team TEXT,
  opposition_team TEXT,
  home_away TEXT NOT NULL,
  margin INTEGER,
  tog_percentage DOUBLE,
  fantasy_points DOUBLE,
  goals DOUBLE,
  behinds DOUBLE,
  disposals DOUBLE,
  kicks DOUBLE,
  handballs DOUBLE,
  marks DOUBLE,
  tackles DOUBLE,
  hitouts DOUBLE,
  frees_for DOUBLE,
  frees_against DOUBLE,
  total_clearances DOUBLE,
  metres_gained DOUBLE,
  goal_assists DOUBLE,
  cba_percentage DOUBLE,
  cbas DOUBLE,
  kick_ins DOUBLE,
  kick_in_percentage DOUBLE,
  kick_ins_play_on DOUBLE,
  kick_to_handball_ratio DOUBLE,
  hitout_win_percentage DOUBLE
);

CREATE TABLE IF NOT EXISTS events (
  event_id BIGINT PRIMARY KEY DEFAULT nextval('events_seq'),
  event_key TEXT NOT NULL UNIQUE,
  league_code TEXT NOT NULL,
  match_name TEXT NOT NULL,
  home_team_id BIGINT NOT NULL,
  away_team_id BIGINT NOT NULL,
  start_time_utc TIMESTAMP,
  round_label TEXT,
  venue TEXT,
  status TEXT NOT NULL DEFAULT 'scheduled'
);

CREATE TABLE IF NOT EXISTS markets (
  market_id BIGINT PRIMARY KEY DEFAULT nextval('markets_seq'),
  market_key TEXT NOT NULL UNIQUE,
  event_id BIGINT NOT NULL,
  market_type_code TEXT NOT NULL,
  market_name_raw TEXT NOT NULL,
  player_id BIGINT,
  line_value DOUBLE,
  stat_side_scope TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS selections (
  selection_id BIGINT PRIMARY KEY DEFAULT nextval('selections_seq'),
  selection_key TEXT NOT NULL UNIQUE,
  market_id BIGINT NOT NULL,
  selection_type TEXT NOT NULL,
  label TEXT NOT NULL,
  sort_order INTEGER NOT NULL
);

CREATE TABLE IF NOT EXISTS event_bookmaker_map (
  event_id BIGINT NOT NULL,
  bookmaker_id BIGINT NOT NULL,
  external_event_id TEXT,
  external_competition_id TEXT,
  payload_meta_json JSON,
  last_seen_at TIMESTAMP NOT NULL,
  PRIMARY KEY (event_id, bookmaker_id)
);

CREATE TABLE IF NOT EXISTS selection_bookmaker_meta (
  selection_id BIGINT NOT NULL,
  bookmaker_id BIGINT NOT NULL,
  external_market_id TEXT,
  external_selection_id TEXT,
  sgm_eligible BOOLEAN NOT NULL DEFAULT FALSE,
  payload_meta_json JSON,
  last_seen_at TIMESTAMP NOT NULL,
  PRIMARY KEY (selection_id, bookmaker_id)
);

CREATE TABLE IF NOT EXISTS outcome_prices (
  price_snapshot_id BIGINT PRIMARY KEY DEFAULT nextval('outcome_prices_seq'),
  selection_id BIGINT NOT NULL,
  bookmaker_id BIGINT NOT NULL,
  import_run_id BIGINT NOT NULL,
  decimal_price DOUBLE NOT NULL,
  implied_prob DOUBLE,
  margin DOUBLE,
  observed_at TIMESTAMP NOT NULL,
  source_file_id BIGINT NOT NULL
);

CREATE TABLE IF NOT EXISTS selection_metrics (
  metric_id BIGINT PRIMARY KEY DEFAULT nextval('selection_metrics_seq'),
  selection_id BIGINT NOT NULL,
  bookmaker_id BIGINT,
  metric_source TEXT NOT NULL,
  fair_prob DOUBLE,
  fair_price DOUBLE,
  edge_pct DOUBLE,
  computed_at TIMESTAMP NOT NULL,
  metrics_json JSON
);

ALTER TABLE selection_metrics ADD COLUMN IF NOT EXISTS bookmaker_id BIGINT;

CREATE TABLE IF NOT EXISTS weather_forecasts (
  venue TEXT NOT NULL,
  forecast_hour_utc TIMESTAMP NOT NULL,
  temperature_c DOUBLE,
  wind_kph DOUBLE,
  precipitation_probability DOUBLE,
  precipitation_mm DOUBLE,
  weather_code INTEGER,
  weather_label TEXT,
  weather_icon_code TEXT,
  fetched_at TIMESTAMP NOT NULL,
  expires_at TIMESTAMP NOT NULL,
  PRIMARY KEY (venue, forecast_hour_utc)
);

ALTER TABLE weather_forecasts ADD COLUMN IF NOT EXISTS precipitation_mm DOUBLE;

CREATE TABLE IF NOT EXISTS quote_cache (
  quote_id TEXT PRIMARY KEY,
  cache_key TEXT NOT NULL UNIQUE,
  bookmaker_id BIGINT NOT NULL,
  event_id BIGINT NOT NULL,
  request_hash TEXT NOT NULL,
  quoted_price DOUBLE NOT NULL,
  unadjusted_price DOUBLE NOT NULL,
  adjustment_factor DOUBLE NOT NULL,
  status TEXT NOT NULL,
  response_json JSON NOT NULL,
  raw_response_json JSON,
  created_at TIMESTAMP NOT NULL,
  expires_at TIMESTAMP NOT NULL,
  last_hit_at TIMESTAMP NOT NULL,
  hit_count INTEGER NOT NULL DEFAULT 1,
  adapter_version TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS quote_legs (
  quote_id TEXT NOT NULL,
  selection_id BIGINT NOT NULL,
  base_price DOUBLE NOT NULL,
  leg_order INTEGER NOT NULL,
  resolved_meta_json JSON NOT NULL,
  PRIMARY KEY (quote_id, selection_id)
);

CREATE OR REPLACE VIEW current_outcome_prices_v AS
SELECT price_snapshot_id,
       selection_id,
       bookmaker_id,
       import_run_id,
       decimal_price,
       implied_prob,
       margin,
       observed_at,
       source_file_id
FROM (
  SELECT *,
         ROW_NUMBER() OVER (
           PARTITION BY selection_id, bookmaker_id
           ORDER BY observed_at DESC, price_snapshot_id DESC
         ) AS row_num
  FROM outcome_prices
) snapshots
WHERE row_num = 1;

CREATE OR REPLACE VIEW latest_successful_import_run_v AS
SELECT *
FROM import_runs
WHERE status IN ('completed', 'completed_with_errors')
ORDER BY started_at DESC
LIMIT 1;

CREATE OR REPLACE VIEW latest_selection_metrics_v AS
SELECT metric_id,
       selection_id,
       bookmaker_id,
       metric_source,
       fair_prob,
       fair_price,
       edge_pct,
       computed_at,
       metrics_json
FROM (
  SELECT *,
         ROW_NUMBER() OVER (
           PARTITION BY selection_id, bookmaker_id
           ORDER BY computed_at DESC, metric_id DESC
         ) AS row_num
  FROM selection_metrics
) metrics
WHERE row_num = 1;

INSERT INTO bookmakers (code, display_name)
VALUES
  ('sportsbet', 'Sportsbet'),
  ('tab', 'TAB'),
  ('neds', 'Neds'),
  ('pointsbet', 'PointsBet'),
  ('betright', 'BetRight'),
  ('bet365', 'Bet365'),
  ('unibet', 'Unibet'),
  ('betr', 'Betr'),
  ('dabble', 'Dabble'),
  ('betfair', 'Betfair')
ON CONFLICT (code) DO NOTHING;
