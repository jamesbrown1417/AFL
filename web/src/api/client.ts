import type {
  ApiErrorEnvelope,
  ArbQuery,
  ArbSearchResult,
  BookmakerSummary,
  CgmCompareResponse,
  DataStatusResponse,
  EventSummary,
  HealthResponse,
  OddsQuery,
  OddsSearchResult,
  PlayerGameLogEntry,
  PlayerStatFilterOptions,
  PlayerStatsFilters,
  PlayerStatSummary,
  PlayerSummary,
  SgmCompareResponse,
} from './types'

export class BackendError extends Error {
  readonly status: number
  readonly code?: string

  constructor(status: number, message: string, code?: string) {
    super(message)
    this.name = 'BackendError'
    this.status = status
    this.code = code
  }
}

export interface ClientSettings {
  apiBaseUrl: string
  authToken: string
}

const defaultSettings: ClientSettings = {
  apiBaseUrl: '/api/v1/',
  authToken: '',
}

const SGM_COMPARE_TIMEOUT_MS = 20_000

function normalizedBaseUrl(baseUrl: string) {
  const trimmed = baseUrl.trim() || defaultSettings.apiBaseUrl
  return trimmed.endsWith('/') ? trimmed : `${trimmed}/`
}

function appendQuery(url: URL, query: Record<string, unknown>) {
  Object.entries(query).forEach(([key, rawValue]) => {
    if (rawValue === null || rawValue === undefined || rawValue === '') return
    if (Array.isArray(rawValue)) {
      rawValue.forEach((value) => {
        if (value !== null && value !== undefined && `${value}` !== '') {
          url.searchParams.append(key, `${value}`)
        }
      })
      return
    }
    if (typeof rawValue === 'boolean') {
      if (rawValue) url.searchParams.set(key, 'true')
      return
    }
    url.searchParams.set(key, `${rawValue}`)
  })
}

async function request<T>(
  settings: ClientSettings,
  path: string,
  options: RequestInit & { query?: object; timeoutMs?: number } = {},
): Promise<T> {
  const base = normalizedBaseUrl(settings.apiBaseUrl)
  const url = new URL(path.replace(/^\/+/, ''), base.startsWith('http') ? base : window.location.origin + base)
  appendQuery(url, (options.query ?? {}) as Record<string, unknown>)

  const headers = new Headers(options.headers)
  headers.set('Accept', 'application/json')
  if (options.body) headers.set('Content-Type', 'application/json')
  if (settings.authToken.trim()) headers.set('Authorization', `Bearer ${settings.authToken.trim()}`)

  const timeoutController = options.timeoutMs ? new AbortController() : null
  const timeoutId = timeoutController
    ? window.setTimeout(() => timeoutController.abort(), options.timeoutMs)
    : null
  const abortFromCaller = () => timeoutController?.abort(options.signal?.reason)
  options.signal?.addEventListener('abort', abortFromCaller, { once: true })

  try {
    const response = await fetch(url, {
      ...options,
      headers,
      signal: timeoutController?.signal ?? options.signal,
    })
    const text = await response.text()
    if (!response.ok) {
      let envelope: ApiErrorEnvelope | null
      try {
        envelope = text ? (JSON.parse(text) as ApiErrorEnvelope) : null
      } catch {
        envelope = null
      }
      throw new BackendError(
        response.status,
        envelope?.error?.message ?? `Backend request failed with ${response.status}.`,
        envelope?.error?.code,
      )
    }
    return text ? (JSON.parse(text) as T) : (undefined as T)
  } catch (error) {
    if (timeoutController?.signal.aborted && !options.signal?.aborted) {
      throw new BackendError(
        408,
        'The bookmaker comparison took too long. Please try again.',
        'comparison_timeout',
      )
    }
    throw error
  } finally {
    if (timeoutId != null) window.clearTimeout(timeoutId)
    options.signal?.removeEventListener('abort', abortFromCaller)
  }
}

export const api = {
  health: (settings: ClientSettings) => request<HealthResponse>(settings, 'health'),
  dataStatus: (settings: ClientSettings) => request<DataStatusResponse>(settings, 'data/status'),
  bookmakers: (settings: ClientSettings) => request<BookmakerSummary[]>(settings, 'bookmakers'),
  arbs: (settings: ClientSettings, query: ArbQuery) =>
    request<ArbSearchResult[]>(settings, 'arbs', { query }),
  events: (settings: ClientSettings, bookmaker?: string | null, q?: string | null, limit = 50) =>
    request<EventSummary[]>(settings, 'events', { query: { bookmaker, q, limit } }),
  searchPlayers: (settings: ClientSettings, q: string, limit = 50) =>
    request<PlayerSummary[]>(settings, 'players/search', { query: { q, limit } }),
  searchStatPlayers: (settings: ClientSettings, q: string, limit = 50, filters?: PlayerStatsFilters) =>
    request<PlayerSummary[]>(settings, 'players/stats/search', {
      query: {
        q,
        limit,
        ...(filters ? playerFiltersToQuery(filters, true) : {}),
      },
    }),
  playerStatFilters: (settings: ClientSettings, playerId: number, filters?: Partial<PlayerStatsFilters>) =>
    request<PlayerStatFilterOptions>(settings, `players/${playerId}/stats/filters`, {
      query: filters ? playerFiltersToQuery(filters) : {},
    }),
  playerHistory: (settings: ClientSettings, playerId: number, filters: PlayerStatsFilters) =>
    request<PlayerGameLogEntry[]>(settings, `players/${playerId}/stats/history`, {
      query: playerFiltersToQuery(filters, true),
    }),
  playerSummary: (settings: ClientSettings, playerId: number, filters: PlayerStatsFilters) =>
    request<PlayerStatSummary>(settings, `players/${playerId}/stats/summary`, {
      query: playerFiltersToQuery(filters, true),
    }),
  odds: (settings: ClientSettings, query: OddsQuery) =>
    request<OddsSearchResult[]>(settings, 'odds/search', { query }),
  compareSgm: (settings: ClientSettings, eventId: number, selectionIds: number[], forceRefresh: boolean) =>
    request<SgmCompareResponse>(settings, 'pricing/sgm/compare', {
      method: 'POST',
      body: JSON.stringify({ event_id: eventId, selection_ids: selectionIds, force_refresh: forceRefresh }),
      timeoutMs: SGM_COMPARE_TIMEOUT_MS,
    }),
  compareCgm: (settings: ClientSettings, selectionIds: number[]) =>
    request<CgmCompareResponse>(settings, 'pricing/cgm', {
      method: 'POST',
      body: JSON.stringify({ selection_ids: selectionIds }),
    }),
}

function playerFiltersToQuery(filters: Partial<PlayerStatsFilters>, includeStat = false) {
  return {
    stat: includeStat ? filters.stat : undefined,
    seasons: filters.seasons ?? [],
    oppositions: filters.oppositions ?? [],
    venues: filters.venues ?? [],
    weather_categories: filters.weatherCategories ?? [],
    home_away: filters.homeAway ?? [],
    margin_min: filters.marginMin ?? undefined,
    margin_max: filters.marginMax ?? undefined,
    last_games: filters.lastGames || undefined,
    minutes_minimum: filters.minutesMinimum ?? undefined,
    line_mode: includeStat ? filters.lineMode : undefined,
    reference_line: filters.lineMode !== 'interval' ? filters.referenceLine || undefined : undefined,
    lower_bound: filters.lineMode === 'interval' ? filters.lowerBound || undefined : undefined,
    upper_bound: filters.lineMode === 'interval' ? filters.upperBound || undefined : undefined,
  }
}
