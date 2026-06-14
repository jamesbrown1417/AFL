import { keepPreviousData, useMutation, useQuery, useQueryClient } from '@tanstack/react-query'
import { api, type ClientSettings } from './client'
import type { ArbFilters, ArbQuery, MetricFilters, OddsFilters, OddsQuery, OddsSearchResult, PlayerStatsFilters } from './types'

export const queryKeys = {
  health: (settings: ClientSettings) => ['health', settings] as const,
  dataStatus: (settings: ClientSettings) => ['data-status', settings] as const,
  bookmakers: (settings: ClientSettings) => ['bookmakers', settings] as const,
  arbs: (settings: ClientSettings, query: ArbQuery) => ['arbs', settings, query] as const,
  events: (settings: ClientSettings, bookmaker?: string | null) => ['events', settings, bookmaker ?? 'all'] as const,
  odds: (settings: ClientSettings, query: OddsQuery) => ['odds', settings, query] as const,
  statPlayers: (settings: ClientSettings, query: string, filters: PlayerStatsFilters) => ['stat-players', settings, query, filters] as const,
  playerFilters: (settings: ClientSettings, playerId: number | null) => ['player-filters', settings, playerId] as const,
  playerHistory: (settings: ClientSettings, playerId: number | null, filters: PlayerStatsFilters) =>
    ['player-history', settings, playerId, filters] as const,
}

export function useHealth(settings: ClientSettings) {
  return useQuery({
    queryKey: queryKeys.health(settings),
    queryFn: () => api.health(settings),
    refetchInterval: 60_000,
    retry: 1,
  })
}

export function useDataStatus(settings: ClientSettings) {
  return useQuery({
    queryKey: queryKeys.dataStatus(settings),
    queryFn: () => api.dataStatus(settings),
    staleTime: 60_000,
    retry: 1,
  })
}

export function useBookmakers(settings: ClientSettings) {
  return useQuery({
    queryKey: queryKeys.bookmakers(settings),
    queryFn: () => api.bookmakers(settings),
    staleTime: 5 * 60_000,
    retry: 1,
  })
}

export function useArbs(settings: ClientSettings, filters: ArbFilters, limit = 250) {
  const query = arbFiltersToQuery(filters, limit)
  return useQuery({
    queryKey: queryKeys.arbs(settings, query),
    queryFn: () => api.arbs(settings, query),
    placeholderData: keepPreviousData,
    retry: 1,
  })
}

export function useEvents(settings: ClientSettings, bookmaker?: string | null) {
  return useQuery({
    queryKey: queryKeys.events(settings, bookmaker),
    queryFn: () => api.events(settings, bookmaker),
    staleTime: 2 * 60_000,
    retry: 1,
  })
}

export function useOdds(settings: ClientSettings, filters: OddsFilters, limit = 100) {
  return useQuery({
    queryKey: queryKeys.odds(settings, oddsFiltersToQuery(filters, limit)),
    queryFn: () => api.odds(settings, oddsFiltersToQuery(filters, limit)),
    placeholderData: keepPreviousData,
    retry: 1,
  })
}

export function useBuilderOdds(
  settings: ClientSettings,
  bookmaker: string | null | undefined,
  eventIds: number[],
  metricFilters: MetricFilters,
  bestOnly: boolean,
  enabled = true,
) {
  const query: OddsQuery = {
    bookmaker: bookmaker ? [bookmaker] : [],
    scope: 'player',
    event_id: eventIds,
    sort_by: eventIds.length <= 1 ? 'market' : 'next_best_prob_diff',
    sort_dir: eventIds.length <= 1 ? 'asc' : 'desc',
    matchup_difficulty: metricFilters.matchupDifficulties,
    min_price: numberOrNull(metricFilters.minPrice),
    max_price: numberOrNull(metricFilters.maxPrice),
    min_diff_2025: metricFilters.minDiff2025,
    max_diff_2025: 1,
    min_diff_last_10: metricFilters.minDiffLast10,
    max_diff_last_10: 1,
    min_next_best_prob_diff: metricFilters.minNextBestProbDiff,
    max_next_best_prob_diff: 1,
    best_only: bestOnly,
    limit: 5000,
  }
  return useQuery({
    queryKey: queryKeys.odds(settings, query),
    queryFn: () => api.odds(settings, query).then((rows) => rows.filter((row) => row.market_type_code.startsWith('player_'))),
    enabled: enabled && Boolean(bookmaker),
    placeholderData: keepPreviousData,
    retry: 1,
  })
}

export function useSelectionAgencyPrices(settings: ClientSettings, selection: OddsSearchResult | null) {
  return useQuery({
    queryKey: ['selection-agency-prices', settings, selection?.event_id ?? null, selection?.market_type_code ?? null, selection?.player?.id ?? null, selection?.selection_type ?? null, selection?.line_value ?? null] as const,
    queryFn: () => {
      const target = selection as OddsSearchResult
      const query: OddsQuery = {
        scope: 'player',
        event_id: [target.event_id],
        market_type: target.market_type_code,
        include_player_id: target.player ? [target.player.id] : [],
        selection_type: target.selection_type,
        sort_by: 'price',
        sort_dir: 'desc',
        limit: 200,
      }
      return api.odds(settings, query).then((rows) =>
        rows
          .filter(
            (row) =>
              row.market_type_code === target.market_type_code &&
              row.selection_type === target.selection_type &&
              (row.line_value ?? null) === (target.line_value ?? null),
          )
          .toSorted((a, b) => (b.decimal_price ?? -Infinity) - (a.decimal_price ?? -Infinity)),
      )
    },
    enabled: selection != null,
    staleTime: 30_000,
    retry: 1,
  })
}

export function useStatPlayers(settings: ClientSettings, query: string, filters: PlayerStatsFilters) {
  return useQuery({
    queryKey: queryKeys.statPlayers(settings, query, filters),
    queryFn: () => api.searchStatPlayers(settings, query, 50, filters),
    staleTime: 0,
    retry: 1,
  })
}

export function usePlayerFilters(settings: ClientSettings, playerId: number | null) {
  return useQuery({
    queryKey: queryKeys.playerFilters(settings, playerId),
    queryFn: () => api.playerStatFilters(settings, playerId as number),
    enabled: playerId != null,
    staleTime: 5 * 60_000,
    retry: 1,
  })
}

export function usePlayerHistory(settings: ClientSettings, playerId: number | null, filters: PlayerStatsFilters) {
  return useQuery({
    queryKey: queryKeys.playerHistory(settings, playerId, filters),
    queryFn: async () => {
      const [history, summary] = await Promise.all([
        api.playerHistory(settings, playerId as number, filters),
        canRequestSummary(filters) ? api.playerSummary(settings, playerId as number, filters) : Promise.resolve(null),
      ])
      return { history, summary }
    },
    enabled: playerId != null,
    placeholderData: keepPreviousData,
    retry: 1,
  })
}

export function useCompareSgm(settings: ClientSettings) {
  const queryClient = useQueryClient()
  return useMutation({
    mutationFn: ({ eventId, selectionIds, forceRefresh }: { eventId: number; selectionIds: number[]; forceRefresh: boolean }) =>
      api.compareSgm(settings, eventId, selectionIds, forceRefresh),
    onSuccess: () => {
      void queryClient.invalidateQueries({ queryKey: ['health'] })
    },
  })
}

export function useCompareCgm(settings: ClientSettings) {
  return useMutation({
    mutationFn: ({ selectionIds }: { selectionIds: number[] }) => api.compareCgm(settings, selectionIds),
  })
}

export function arbFiltersToQuery(filters: ArbFilters, limit = 250): ArbQuery {
  return {
    q: filters.query.trim() || null,
    market: filters.markets,
    agency: filters.agencies,
    min_margin: numberOrNull(filters.minMargin),
    max_margin: numberOrNull(filters.maxMargin),
    limit,
    offset: 0,
  }
}

export function oddsFiltersToQuery(filters: OddsFilters, limit = 100): OddsQuery {
  const playerScoped = filters.scope === 'player'
  return {
    bookmaker: filters.bookmakerCodes,
    scope: filters.scope,
    market_type: filters.marketTypeCode,
    event_id: filters.eventId == null ? [] : [filters.eventId],
    sort_by: filters.sortBy,
    sort_dir: filters.sortDirection,
    selection_type: playerScoped ? filters.selectionType : null,
    matchup_difficulty: playerScoped ? filters.matchupDifficulties : [],
    min_price: playerScoped ? numberOrNull(filters.minPrice) : null,
    max_price: playerScoped ? numberOrNull(filters.maxPrice) : null,
    min_diff_2025: playerScoped ? -1 : null,
    max_diff_2025: playerScoped ? 1 : null,
    min_diff_last_10: playerScoped ? filters.minDiffLast10 : null,
    max_diff_last_10: playerScoped ? 1 : null,
    min_next_best_prob_diff: playerScoped ? filters.minNextBestProbDiff : null,
    max_next_best_prob_diff: playerScoped ? 1 : null,
    best_only: playerScoped ? filters.bestOnly : false,
    sgm_only: playerScoped ? filters.sgmOnly : false,
    limit,
    offset: 0,
  }
}

function numberOrNull(value: string) {
  if (value.trim() === '') return null
  const parsed = Number(value)
  return Number.isFinite(parsed) ? parsed : null
}

function canRequestSummary(filters: PlayerStatsFilters) {
  if (filters.lineMode === 'interval') {
    return filters.lowerBound.trim() !== '' && filters.upperBound.trim() !== ''
  }
  return filters.referenceLine.trim() !== ''
}
