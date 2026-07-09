import { useMemo } from 'react'
import { create } from 'zustand'
import { persist } from 'zustand/middleware'
import type { ArbFilters, BuilderMode, DisplayMode, DraftLeg, MetricFilters, OddsFilters, PlayerStatsFilters, ThemeMode } from '../api/types'

export interface SgmDraftContext {
  eventId: number
  eventLabel: string
  bookmaker: string
}

export interface SgmUndoSnapshot {
  legs: DraftLeg[]
  context: SgmDraftContext
  expiresAt: number
}

export const SGM_UNDO_DURATION_MS = 8_000

export function sgmContextFromLeg(leg: DraftLeg): SgmDraftContext {
  return {
    eventId: leg.event_id,
    eventLabel: leg.event_label,
    bookmaker: leg.bookmaker,
  }
}

export const defaultOddsFilters: OddsFilters = {
  scope: 'player',
  bookmakerCodes: [],
  marketTypeCode: null,
  eventId: null,
  sortBy: 'diff_last_10',
  sortDirection: 'desc',
  selectionType: null,
  matchupDifficulties: [],
  minPrice: '',
  maxPrice: '',
  minDiffLast10: -1,
  minNextBestProbDiff: -1,
  minHomeAwayDiff: null,
  maxHomeAwayDiff: null,
  minWinLossDiff: null,
  maxWinLossDiff: null,
  favorableHomeAway: false,
  favorableWinLoss: false,
  bestOnly: false,
  sgmOnly: false,
}

export const defaultArbFilters: ArbFilters = {
  query: '',
  markets: [],
  agencies: [],
  minMargin: '-5',
  maxMargin: '',
}

export const defaultMetricFilters: MetricFilters = {
  selectionType: null,
  matchupDifficulties: [],
  minPrice: '',
  maxPrice: '',
  minDiff2025: -1,
  maxDiff2025: 1,
  minDiffLast10: -1,
  maxDiffLast10: 1,
  minHomeAwayDiff: null,
  maxHomeAwayDiff: null,
  minWinLossDiff: null,
  maxWinLossDiff: null,
  minNextBestProbDiff: -1,
  maxNextBestProbDiff: 1,
  favorableHomeAway: false,
  favorableWinLoss: false,
}

export const defaultPlayerFilters: PlayerStatsFilters = {
  stat: 'disposals',
  seasons: ['2026'],
  oppositions: [],
  venues: [],
  weatherCategories: [],
  homeAway: ['Home', 'Away'],
  marginMin: '-200',
  marginMax: '200',
  lastGames: '',
  minutesMinimum: '0',
  lineMode: 'single',
  referenceLine: '19.5',
  lowerBound: '19.5',
  upperBound: '25.5',
}

export interface AppStore {
  apiBaseUrl: string
  authToken: string
  defaultBookmaker: string
  themeMode: ThemeMode
  activeView: 'odds' | 'arbs' | 'player' | 'sgm' | 'cgm' | 'settings'
  builderMode: BuilderMode
  displayMode: DisplayMode
  arbFilters: ArbFilters
  oddsFilters: OddsFilters
  metricFilters: MetricFilters
  playerFilters: PlayerStatsFilters
  selectedPlayerId: number | null
  selectedPlayerName: string
  sgmLegs: DraftLeg[]
  sgmContext: SgmDraftContext | null
  sgmUndo: SgmUndoSnapshot | null
  cgmLegs: DraftLeg[]
  sgmForceRefresh: boolean
  setSettings: (settings: Partial<Pick<AppStore, 'apiBaseUrl' | 'authToken' | 'defaultBookmaker' | 'themeMode'>>) => void
  setActiveView: (view: AppStore['activeView']) => void
  setBuilderMode: (mode: BuilderMode) => void
  setDisplayMode: (mode: DisplayMode) => void
  setArbFilters: (filters: ArbFilters) => void
  patchArbFilters: (filters: Partial<ArbFilters>) => void
  setOddsFilters: (filters: OddsFilters) => void
  patchOddsFilters: (filters: Partial<OddsFilters>) => void
  setMetricFilters: (filters: MetricFilters) => void
  setPlayerFilters: (filters: PlayerStatsFilters) => void
  setSelectedPlayer: (playerId: number | null, playerName?: string) => void
  addSgmLeg: (leg: DraftLeg) => void
  removeSgmLeg: (selectionId: number) => void
  clearSgm: () => void
  undoSgmReplacement: () => void
  dismissSgmUndo: () => void
  addCgmLeg: (leg: DraftLeg) => void
  removeCgmLeg: (selectionId: number) => void
  clearCgm: () => void
  setSgmForceRefresh: (forceRefresh: boolean) => void
}

export function migratePersistedAppState(persistedState: unknown) {
  if (!persistedState || typeof persistedState !== 'object') return persistedState
  const state = persistedState as Partial<AppStore>
  const playerFilters = state.playerFilters
  const nextState = {
    ...state,
    sgmContext: state.sgmContext ?? (state.sgmLegs?.[0] ? sgmContextFromLeg(state.sgmLegs[0]) : null),
    oddsFilters: {
      ...defaultOddsFilters,
      ...state.oddsFilters,
    },
    metricFilters: {
      ...defaultMetricFilters,
      ...state.metricFilters,
    },
    arbFilters: {
      ...defaultArbFilters,
      ...state.arbFilters,
    },
  }
  if (!playerFilters || playerFilters.seasons.length > 0) return nextState
  return {
    ...nextState,
    playerFilters: {
      ...defaultPlayerFilters,
      ...playerFilters,
      seasons: defaultPlayerFilters.seasons,
    },
  }
}

export const useAppStore = create<AppStore>()(
  persist(
    (set) => ({
      apiBaseUrl: '/api/v1/',
      authToken: '',
      defaultBookmaker: 'sportsbet',
      themeMode: 'light',
      activeView: 'odds',
      builderMode: 'sgm',
      displayMode: 'row',
      arbFilters: defaultArbFilters,
      oddsFilters: defaultOddsFilters,
      metricFilters: defaultMetricFilters,
      playerFilters: defaultPlayerFilters,
      selectedPlayerId: null,
      selectedPlayerName: '',
      sgmLegs: [],
      sgmContext: null,
      sgmUndo: null,
      cgmLegs: [],
      sgmForceRefresh: false,
      setSettings: (settings) => set(settings),
      setActiveView: (activeView) => set({ activeView }),
      setBuilderMode: (builderMode) => set({ builderMode }),
      setDisplayMode: (displayMode) => set({ displayMode }),
      setArbFilters: (arbFilters) => set({ arbFilters }),
      patchArbFilters: (filters) => set((state) => ({ arbFilters: { ...state.arbFilters, ...filters } })),
      setOddsFilters: (oddsFilters) => set({ oddsFilters }),
      patchOddsFilters: (filters) => set((state) => ({ oddsFilters: { ...state.oddsFilters, ...filters } })),
      setMetricFilters: (metricFilters) => set({ metricFilters }),
      setPlayerFilters: (playerFilters) => set({ playerFilters }),
      setSelectedPlayer: (selectedPlayerId, selectedPlayerName = '') => set({ selectedPlayerId, selectedPlayerName, activeView: 'player' }),
      addSgmLeg: (leg) =>
        set((state) => {
          const existing = state.sgmLegs.some((item) => item.selection_id === leg.selection_id)
          if (existing) {
            const nextLegs = state.sgmLegs.filter((item) => item.selection_id !== leg.selection_id)
            return { sgmLegs: nextLegs, sgmContext: nextLegs.length ? state.sgmContext : null, sgmUndo: null }
          }

          const currentContext = state.sgmContext ?? (state.sgmLegs[0] ? sgmContextFromLeg(state.sgmLegs[0]) : null)
          const incomingContext = sgmContextFromLeg(leg)
          const contextChanged = currentContext != null && (
            currentContext.eventId !== incomingContext.eventId || currentContext.bookmaker !== incomingContext.bookmaker
          )
          if (state.sgmLegs.length > 0 && currentContext && contextChanged) {
            return {
              sgmLegs: [leg],
              sgmContext: incomingContext,
              sgmUndo: {
                legs: state.sgmLegs,
                context: currentContext,
                expiresAt: Date.now() + SGM_UNDO_DURATION_MS,
              },
              builderMode: 'sgm' as const,
            }
          }
          return {
            sgmLegs: [...state.sgmLegs, leg],
            sgmContext: currentContext ?? incomingContext,
            sgmUndo: null,
            builderMode: 'sgm' as const,
          }
        }),
      removeSgmLeg: (selectionId) => set((state) => {
        const nextLegs = state.sgmLegs.filter((leg) => leg.selection_id !== selectionId)
        return { sgmLegs: nextLegs, sgmContext: nextLegs.length ? state.sgmContext : null, sgmUndo: null }
      }),
      clearSgm: () => set({ sgmLegs: [], sgmContext: null, sgmUndo: null }),
      undoSgmReplacement: () => set((state) => {
        if (!state.sgmUndo || state.sgmUndo.expiresAt <= Date.now()) return { sgmUndo: null }
        return {
          sgmLegs: state.sgmUndo.legs,
          sgmContext: state.sgmUndo.context,
          sgmUndo: null,
          builderMode: 'sgm' as const,
        }
      }),
      dismissSgmUndo: () => set({ sgmUndo: null }),
      addCgmLeg: (leg) =>
        set((state) => {
          const existing = state.cgmLegs.some((item) => item.selection_id === leg.selection_id)
          if (existing) return { cgmLegs: state.cgmLegs.filter((item) => item.selection_id !== leg.selection_id) }
          const sameGame = state.cgmLegs.some((item) => item.event_id === leg.event_id)
          if (sameGame) return state
          return { cgmLegs: [...state.cgmLegs, leg], builderMode: 'cgm' }
        }),
      removeCgmLeg: (selectionId) => set((state) => ({ cgmLegs: state.cgmLegs.filter((leg) => leg.selection_id !== selectionId) })),
      clearCgm: () => set({ cgmLegs: [] }),
      setSgmForceRefresh: (sgmForceRefresh) => set({ sgmForceRefresh }),
    }),
    {
      name: 'afl-edge-web-v1',
      version: 3,
      migrate: migratePersistedAppState,
      partialize: (state) => ({
        apiBaseUrl: state.apiBaseUrl,
        authToken: state.authToken,
        defaultBookmaker: state.defaultBookmaker,
        themeMode: state.themeMode,
        activeView: state.activeView,
        builderMode: state.builderMode,
        displayMode: state.displayMode,
        arbFilters: state.arbFilters,
        oddsFilters: state.oddsFilters,
        metricFilters: state.metricFilters,
        playerFilters: state.playerFilters,
        selectedPlayerId: state.selectedPlayerId,
        selectedPlayerName: state.selectedPlayerName,
        sgmLegs: state.sgmLegs,
        sgmContext: state.sgmContext,
        cgmLegs: state.cgmLegs,
        sgmForceRefresh: state.sgmForceRefresh,
      }),
    },
  ),
)

export const useClientSettings = () => {
  const apiBaseUrl = useAppStore((state) => state.apiBaseUrl)
  const authToken = useAppStore((state) => state.authToken)
  return useMemo(() => ({ apiBaseUrl, authToken }), [apiBaseUrl, authToken])
}
