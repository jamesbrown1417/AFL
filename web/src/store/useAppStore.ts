import { useMemo } from 'react'
import { create } from 'zustand'
import { persist } from 'zustand/middleware'
import type { ArbFilters, BuilderMode, DisplayMode, DraftLeg, MetricFilters, OddsFilters, PlayerStatsFilters, ThemeMode } from '../api/types'

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

interface AppStore {
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
  addCgmLeg: (leg: DraftLeg) => void
  removeCgmLeg: (selectionId: number) => void
  clearCgm: () => void
  setSgmForceRefresh: (forceRefresh: boolean) => void
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
          if (existing) return { sgmLegs: state.sgmLegs.filter((item) => item.selection_id !== leg.selection_id) }
          const resetForNewContext =
            state.sgmLegs.length > 0 &&
            (state.sgmLegs[0].event_id !== leg.event_id || state.sgmLegs[0].bookmaker !== leg.bookmaker)
          return { sgmLegs: [...(resetForNewContext ? [] : state.sgmLegs), leg], builderMode: 'sgm' }
        }),
      removeSgmLeg: (selectionId) => set((state) => ({ sgmLegs: state.sgmLegs.filter((leg) => leg.selection_id !== selectionId) })),
      clearSgm: () => set({ sgmLegs: [] }),
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
      version: 1,
      migrate: (persistedState) => {
        if (!persistedState || typeof persistedState !== 'object') return persistedState
        const state = persistedState as Partial<AppStore>
        const playerFilters = state.playerFilters
        const nextState = {
          ...state,
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
      },
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
