import { useEffect, useMemo, useRef, useState } from 'react'
import { Area, AreaChart, CartesianGrid, Line, LineChart, ReferenceLine, ResponsiveContainer, Tooltip, XAxis, YAxis } from 'recharts'
import { ArrowDown, ArrowDownUp, ArrowUp, RefreshCcw, Search, SlidersHorizontal, X } from 'lucide-react'
import type { PlayerStatFilterOptions, PlayerGameLogEntry, PlayerStatsFilters, PlayerStatSummary } from '../api/types'
import { usePlayerFilters, usePlayerHistory, useStatPlayers } from '../api/queries'
import { defaultPlayerFilters, useAppStore, useClientSettings } from '../store/useAppStore'
import { formatMatchDateTime, formatPercent, formatPrice, formatShortDate } from '../lib/formatters'
import { Button, Chip, EmptyState, ErrorBanner, Field, Panel, Segmented, Select, StatPill, TextInput } from '../components/ui'

export function PlayerLab() {
  const settings = useClientSettings()
  const selectedPlayerId = useAppStore((state) => state.selectedPlayerId)
  const selectedPlayerName = useAppStore((state) => state.selectedPlayerName)
  const setSelectedPlayer = useAppStore((state) => state.setSelectedPlayer)
  const filters = useAppStore((state) => state.playerFilters)
  const setFilters = useAppStore((state) => state.setPlayerFilters)
  const [query, setQuery] = useState(selectedPlayerName)
  const [searchOpen, setSearchOpen] = useState(false)
  const [activeOptionIndex, setActiveOptionIndex] = useState(0)
  const [playerMode, setPlayerMode] = useState<PlayerMode>('stats')
  const [comparisonFocus, setComparisonFocus] = useState<ScenarioId>('a')
  const [filterTarget, setFilterTarget] = useState<FilterTarget>(null)
  const [draftFilters, setDraftFilters] = useState<PlayerStatsFilters>(filters)
  const [scenarioAFilters, setScenarioAFilters] = useState<PlayerStatsFilters>(filters)
  const [scenarioBFilters, setScenarioBFilters] = useState<PlayerStatsFilters>(filters)
  const searchInputRef = useRef<HTMLInputElement | null>(null)
  const [historySort, setHistorySort] = useState<{ key: HistorySortKey; direction: 'asc' | 'desc' }>({
    key: 'date',
    direction: 'desc',
  })
  const playersQuery = useStatPlayers(settings, query, filters)
  const playerOptions = useMemo(() => {
    const normalizedQuery = query.trim().toLowerCase()
    return (playersQuery.data ?? [])
      .filter((player) => !normalizedQuery || player.full_name.toLowerCase().includes(normalizedQuery))
      .slice(0, 10)
  }, [playersQuery.data, query])
  const firstPlayer = playerOptions[0]
  const activePlayer = searchOpen && !playersQuery.isFetching ? playerOptions[activeOptionIndex] : undefined

  useEffect(() => {
    if (selectedPlayerId == null && firstPlayer) {
      setSelectedPlayer(firstPlayer.id, firstPlayer.full_name)
    }
  }, [firstPlayer, selectedPlayerId, setSelectedPlayer])

  useEffect(() => {
    if (!searchOpen || playerOptions.length === 0) {
      setActiveOptionIndex(0)
      return
    }
    setActiveOptionIndex((current) => Math.min(Math.max(current, 0), playerOptions.length - 1))
  }, [playerOptions.length, searchOpen])

  const filterOptions = usePlayerFilters(settings, selectedPlayerId)
  const historyQuery = usePlayerHistory(settings, selectedPlayerId, filters, playerMode === 'stats')
  const scenarioAQuery = usePlayerHistory(settings, selectedPlayerId, scenarioAFilters, playerMode === 'comparison')
  const scenarioBQuery = usePlayerHistory(settings, selectedPlayerId, scenarioBFilters, playerMode === 'comparison')
  const history = useMemo(() => historyQuery.data?.history ?? [], [historyQuery.data?.history])
  const summary = historyQuery.data?.summary ?? null
  const selectedStatKey = normalizeStatKey(filters.stat)
  const referenceLines = useMemo(() => referenceLinesFromFilters(filters.lineMode, filters.referenceLine, filters.lowerBound, filters.upperBound), [filters.lineMode, filters.referenceLine, filters.lowerBound, filters.upperBound])
  const chartData = useMemo(
    () => {
      const points = history.toReversed().map((entry, index) => ({
        x: index,
        date: formatShortDate(entry.date),
        value: entry.selected_value,
        hit: entry.hit,
        referenceResult: referenceResultForValue(entry.selected_value, filters.lineMode, filters.referenceLine, filters.lowerBound, filters.upperBound),
      }))
      return buildSegmentedChartData(points)
    },
    [history, filters.lineMode, filters.referenceLine, filters.lowerBound, filters.upperBound],
  )
  const chartDomain = useMemo(() => computeChartDomain(chartData.map((entry) => entry.value), referenceLines.map((line) => line.value)), [chartData, referenceLines])
  const chartXAxisTicks = useMemo(() => chartData.filter((entry) => !entry.isTransition).map((entry) => entry.x), [chartData])
  const chartTickLabels = useMemo(() => new Map(chartData.filter((entry) => !entry.isTransition).map((entry) => [entry.x, entry.date])), [chartData])
  const chartXAxisDomain = useMemo<[number, number]>(() => [0, Math.max(1, chartXAxisTicks.length - 1)], [chartXAxisTicks.length])
  const sortedHistory = useMemo(() => sortHistory(history, historySort.key, historySort.direction), [history, historySort])

  useEffect(() => {
    setScenarioAFilters(filters)
    setScenarioBFilters(filters)
    setComparisonFocus('a')
  }, [filters, selectedPlayerId])

  const selectPlayer = (playerId: number, playerName: string) => {
    setSelectedPlayer(playerId, playerName)
    setQuery(playerName)
    setSearchOpen(false)
  }

  const clearPlayerSearch = () => {
    setQuery('')
    setSearchOpen(true)
    setActiveOptionIndex(0)
    window.requestAnimationFrame(() => searchInputRef.current?.focus())
  }

  const handlePlayerSearchKeyDown = (event: React.KeyboardEvent<HTMLInputElement>) => {
    if (event.key === 'ArrowDown') {
      event.preventDefault()
      setSearchOpen(true)
      if (playerOptions.length > 0) {
        setActiveOptionIndex((current) => (current + 1) % playerOptions.length)
      }
      return
    }

    if (event.key === 'ArrowUp') {
      event.preventDefault()
      setSearchOpen(true)
      if (playerOptions.length > 0) {
        setActiveOptionIndex((current) => (current <= 0 ? playerOptions.length - 1 : current - 1))
      }
      return
    }

    if (event.key === 'Enter' && searchOpen && activePlayer) {
      event.preventDefault()
      selectPlayer(activePlayer.id, activePlayer.full_name)
      return
    }

    if (event.key === 'Escape') {
      setSearchOpen(false)
    }
  }

  const toggleHistorySort = (key: HistorySortKey) => {
    setHistorySort((current) => ({
      key,
      direction: current.key === key && current.direction === 'asc' ? 'desc' : 'asc',
    }))
  }

  const openFilters = (target: Exclude<FilterTarget, null> = 'stats') => {
    if (target === 'scenarioA') setDraftFilters(scenarioAFilters)
    else if (target === 'scenarioB') setDraftFilters(scenarioBFilters)
    else setDraftFilters(filters)
    setFilterTarget(target)
  }

  const applyFilters = () => {
    if (filterTarget === 'scenarioA') setScenarioAFilters(draftFilters)
    else if (filterTarget === 'scenarioB') setScenarioBFilters(draftFilters)
    else setFilters(draftFilters)
    setFilterTarget(null)
  }

  const resetFilters = () => {
    const stat = draftFilters.stat
    setDraftFilters({ ...defaultPlayerFilters, stat, ...defaultsForStat(stat) })
  }

  const activeFilterCount = useMemo(() => countActiveFilters(filters), [filters])
  const comparisonLoading = scenarioAQuery.isFetching || scenarioBQuery.isFetching
  const comparisonStatLabel = filterOptions.data?.stats.find((stat) => stat.code === scenarioAFilters.stat)?.label ?? scenarioAFilters.stat

  return (
    <main className="workspace player-workspace">
      <section className="workspace-main">
        <div className="page-title-row">
          <div>
            <h1>Player</h1>
            <p>{selectedPlayerName || 'Search player history and implied prices'}</p>
          </div>
          <div className="page-actions">
            <Segmented<PlayerMode>
              value={playerMode}
              ariaLabel="Player mode"
              options={[
                { value: 'stats', label: 'Stats' },
                { value: 'comparison', label: 'Comparison' },
              ]}
              onChange={setPlayerMode}
            />
            {playerMode === 'stats' ? (
              <Button variant="secondary" onClick={() => openFilters('stats')}>
                <SlidersHorizontal size={15} /> Filters
                {activeFilterCount > 0 ? <span className="filter-count-badge">{activeFilterCount}</span> : null}
              </Button>
            ) : (
              <Button variant="secondary" onClick={() => {
                void scenarioAQuery.refetch()
                void scenarioBQuery.refetch()
              }}>
                <RefreshCcw size={15} /> Refresh
              </Button>
            )}
          </div>
        </div>

        <Panel className="filters-panel">
          <div className="player-controls-bar">
            <div className="autocomplete">
              <div className="input-with-icon">
                <Search size={16} />
                <TextInput
                  ref={searchInputRef}
                  value={query}
                  onChange={(event) => {
                    setQuery(event.currentTarget.value)
                    setSearchOpen(true)
                    setActiveOptionIndex(0)
                  }}
                  onFocus={() => setSearchOpen(true)}
                  onBlur={() => window.setTimeout(() => setSearchOpen(false), 120)}
                  onKeyDown={handlePlayerSearchKeyDown}
                  placeholder="Search players"
                  aria-autocomplete="list"
                  aria-controls="player-search-options"
                  aria-activedescendant={activePlayer ? `player-search-option-${activePlayer.id}` : undefined}
                  aria-expanded={searchOpen && !playersQuery.isFetching && playerOptions.length > 0}
                />
                {query ? (
                  <button
                    type="button"
                    className="search-clear-button"
                    aria-label="Clear player search"
                    onMouseDown={(event) => event.preventDefault()}
                    onClick={clearPlayerSearch}
                  >
                    <X size={14} />
                  </button>
                ) : null}
              </div>
              {searchOpen && playersQuery.isFetching && (
                <div className="autocomplete-menu autocomplete-menu--status" role="status">
                  Searching...
                </div>
              )}
              {searchOpen && !playersQuery.isFetching && playerOptions.length > 0 && (
                <div className="autocomplete-menu" id="player-search-options" role="listbox" aria-label="Player suggestions">
                  {playerOptions.map((player, index) => (
                    <button
                      type="button"
                      role="option"
                      id={`player-search-option-${player.id}`}
                      key={player.id}
                      className={index === activeOptionIndex ? 'is-active' : undefined}
                      aria-selected={index === activeOptionIndex}
                      onMouseDown={(event) => event.preventDefault()}
                      onMouseEnter={() => setActiveOptionIndex(index)}
                      onClick={() => selectPlayer(player.id, player.full_name)}
                    >
                      {player.full_name}
                    </button>
                  ))}
                </div>
              )}
            </div>
            {playerMode === 'stats' ? <FilterSummary filters={filters} /> : <FilterSummary filters={scenarioAFilters} />}
          </div>
          {playerMode === 'stats' ? (
            <PlayerQuickActions filters={filters} filterOptions={filterOptions.data ?? null} onApply={setFilters} />
          ) : (
            <ComparisonSetup
              statLabel={comparisonStatLabel}
              filterOptions={filterOptions.data ?? null}
              scenarioAFilters={scenarioAFilters}
              scenarioBFilters={scenarioBFilters}
              onScenarioAChange={setScenarioAFilters}
              onScenarioBChange={setScenarioBFilters}
            />
          )}
        </Panel>

        {playerMode === 'stats' && historyQuery.error ? <ErrorBanner message={historyQuery.error instanceof Error ? historyQuery.error.message : 'Failed to load player history.'} /> : null}
        {playerMode === 'comparison' && (scenarioAQuery.error || scenarioBQuery.error) ? (
          <ErrorBanner message={
            scenarioAQuery.error instanceof Error
              ? scenarioAQuery.error.message
              : scenarioBQuery.error instanceof Error
                ? scenarioBQuery.error.message
                : 'Failed to load comparison history.'
          } />
        ) : null}

        {playerMode === 'stats' ? (
          <>
            <div className="summary-strip">
              <StatPill label="Sample" value={summary?.sample_size?.toString() ?? String(history.length)} />
              <StatPill label="Over" value={formatPercent(summary?.proportion_over)} tone="good" />
              <StatPill label="Under" value={formatPercent(summary?.proportion_under)} tone="warn" />
              <StatPill label="Implied over" value={formatPrice(summary?.implied_odds_over)} />
              <StatPill label="Implied under" value={formatPrice(summary?.implied_odds_under)} />
            </div>

            <Panel className="chart-panel">
              <div className="section-heading">
                <h2>{filterOptions.data?.stats.find((stat) => stat.code === filters.stat)?.label ?? filters.stat} history</h2>
                <span>{historyQuery.isFetching ? 'Refreshing' : `${history.length} games`}</span>
              </div>
              {history.length === 0 && !historyQuery.isFetching ? (
                <EmptyState title="No player history" body="Pick a player or relax the filters." />
              ) : (
                <div className="chart-frame">
                  <ResponsiveContainer width="100%" height={280}>
                    <AreaChart data={chartData}>
                      <defs>
                        <linearGradient id="historyFill" x1="0" x2="0" y1="0" y2="1">
                          <stop offset="0%" stopColor="#0f766e" stopOpacity={0.36} />
                          <stop offset="100%" stopColor="#0f766e" stopOpacity={0.02} />
                        </linearGradient>
                        <linearGradient id="historyMissFill" x1="0" x2="0" y1="0" y2="1">
                          <stop offset="0%" stopColor="#b42318" stopOpacity={0.36} />
                          <stop offset="100%" stopColor="#b42318" stopOpacity={0.02} />
                        </linearGradient>
                      </defs>
                      <CartesianGrid vertical={false} stroke="#dce7ec" />
                      <XAxis
                        dataKey="x"
                        type="number"
                        domain={chartXAxisDomain}
                        ticks={chartXAxisTicks}
                        tickFormatter={(value) => chartTickLabels.get(Number(value)) ?? ''}
                        tickLine={false}
                        axisLine={false}
                        minTickGap={24}
                      />
                      <YAxis tickLine={false} axisLine={false} width={42} domain={chartDomain} />
                      <Tooltip content={<PlayerChartTooltip />} />
                      <Area dataKey="hitValue" stroke="#0f766e" strokeWidth={2} fill="url(#historyFill)" dot={false} activeDot={false} isAnimationActive={false} />
                      <Area dataKey="missValue" stroke="#b42318" strokeWidth={2} fill="url(#historyMissFill)" dot={false} activeDot={false} isAnimationActive={false} />
                      {referenceLines.map((line) => (
                        <ReferenceLine
                          key={line.key}
                          y={line.value}
                          stroke="#2563eb"
                          strokeDasharray="7 5"
                          strokeWidth={1.5}
                          label={{ value: line.label, position: 'insideTopRight', fill: '#2563eb', fontSize: 11 }}
                        />
                      ))}
                      <Area dataKey="value" stroke="transparent" strokeWidth={0} fill="transparent" dot={<ReferenceResultDot />} activeDot={<ReferenceResultDot isActive />} isAnimationActive={false} />
                    </AreaChart>
                  </ResponsiveContainer>
                </div>
              )}
            </Panel>

            <Panel className="table-panel">
              <PlayerHistoryTable history={sortedHistory} selectedStatKey={selectedStatKey} sort={historySort} onSort={toggleHistorySort} />
            </Panel>
          </>
        ) : (
          <PlayerComparisonMode
            statLabel={comparisonStatLabel}
            scenarioAFilters={scenarioAFilters}
            scenarioBFilters={scenarioBFilters}
            scenarioAData={scenarioAQuery.data ?? null}
            scenarioBData={scenarioBQuery.data ?? null}
            comparisonFocus={comparisonFocus}
            isLoading={comparisonLoading}
            historySort={historySort}
            onSort={toggleHistorySort}
            onFocusChange={setComparisonFocus}
            onEditScenarioA={() => openFilters('scenarioA')}
            onEditScenarioB={() => openFilters('scenarioB')}
          />
        )}
      </section>

      {filterTarget ? (
        <PlayerFiltersDrawer
          title={filterTarget === 'scenarioA' ? 'Scenario A filters' : filterTarget === 'scenarioB' ? 'Scenario B filters' : 'Filters'}
          filterOptions={filterOptions.data ?? null}
          draft={draftFilters}
          onChange={setDraftFilters}
          onApply={applyFilters}
          onReset={resetFilters}
          onClose={() => setFilterTarget(null)}
          showStatControls={filterTarget === 'stats'}
        />
      ) : null}
    </main>
  )
}

function toggleArrayValue(values: string[], value: string) {
  return values.includes(value) ? values.filter((item) => item !== value) : [...values, value]
}

function countActiveFilters(filters: PlayerStatsFilters) {
  let count = 0
  if (filters.seasons.length > 0) count += 1
  if (filters.oppositions.length > 0) count += 1
  if (filters.venues.length > 0) count += 1
  if (filters.weatherCategories.length > 0) count += 1
  if (!(filters.homeAway.includes('Home') && filters.homeAway.includes('Away'))) count += 1
  if (filters.marginMin !== defaultPlayerFilters.marginMin || filters.marginMax !== defaultPlayerFilters.marginMax) count += 1
  if (filters.lastGames.trim() !== '') count += 1
  if (filters.minutesMinimum.trim() !== '' && filters.minutesMinimum !== defaultPlayerFilters.minutesMinimum) count += 1
  if (filters.lineMode === 'interval') count += 1
  return count
}

function FilterSummary({ filters }: { filters: PlayerStatsFilters }) {
  const parts: string[] = []
  if (filters.seasons.length > 0) parts.push(`Seasons: ${summarize(filters.seasons)}`)
  if (!(filters.homeAway.includes('Home') && filters.homeAway.includes('Away'))) parts.push(filters.homeAway.join(' / ') || 'No venue side')
  if (filters.oppositions.length > 0) parts.push(`Opp: ${summarize(filters.oppositions)}`)
  if (filters.venues.length > 0) parts.push(`Venue: ${summarize(filters.venues)}`)
  if (filters.weatherCategories.length > 0) parts.push(`Weather: ${summarize(filters.weatherCategories)}`)
  if (filters.marginMin !== defaultPlayerFilters.marginMin || filters.marginMax !== defaultPlayerFilters.marginMax) parts.push(`Margin ${filters.marginMin} to ${filters.marginMax}`)
  if (filters.lastGames.trim() !== '') parts.push(`Last ${filters.lastGames}`)
  if (parts.length === 0) return <span className="filter-summary filter-summary--empty">All games</span>
  return (
    <div className="filter-summary">
      {parts.map((part) => (
        <span className="chip" key={part}>{part}</span>
      ))}
    </div>
  )
}

function PlayerQuickActions({
  filters,
  filterOptions,
  onApply,
}: {
  filters: PlayerStatsFilters
  filterOptions: PlayerStatFilterOptions | null
  onApply: (filters: PlayerStatsFilters) => void
}) {
  const latestSeason = filterOptions?.seasons[0] ?? '2026'
  const homeAwayOptions = filterOptions?.home_away_options.length ? filterOptions.home_away_options : ['Home', 'Away']
  const defaultHomeAway = homeAwayOptions.length ? homeAwayOptions : defaultPlayerFilters.homeAway
  return (
    <div className="quick-filter-grid player-quick-actions" aria-label="Player quick filters">
      <Chip active={filters.lastGames === '5'} onClick={() => onApply({ ...filters, lastGames: filters.lastGames === '5' ? '' : '5' })}>Last 5</Chip>
      <Chip active={filters.lastGames === '10'} onClick={() => onApply({ ...filters, lastGames: filters.lastGames === '10' ? '' : '10' })}>Last 10</Chip>
      <Chip
        active={filters.seasons.length === 1 && filters.seasons[0] === latestSeason}
        onClick={() => onApply({ ...filters, seasons: filters.seasons.length === 1 && filters.seasons[0] === latestSeason ? defaultPlayerFilters.seasons : [latestSeason] })}
      >
        {latestSeason} only
      </Chip>
      {homeAwayOptions.includes('Home') ? (
        <Chip active={filters.homeAway.length === 1 && filters.homeAway[0] === 'Home'} onClick={() => onApply({ ...filters, homeAway: filters.homeAway.length === 1 && filters.homeAway[0] === 'Home' ? defaultHomeAway : ['Home'] })}>
          Home
        </Chip>
      ) : null}
      {homeAwayOptions.includes('Away') ? (
        <Chip active={filters.homeAway.length === 1 && filters.homeAway[0] === 'Away'} onClick={() => onApply({ ...filters, homeAway: filters.homeAway.length === 1 && filters.homeAway[0] === 'Away' ? defaultHomeAway : ['Away'] })}>
          Away
        </Chip>
      ) : null}
      {countActiveFilters(filters) > 0 ? <Button variant="ghost" onClick={() => onApply(defaultPlayerFilters)}>Reset</Button> : null}
    </div>
  )
}

function ComparisonSetup({
  statLabel,
  filterOptions,
  scenarioAFilters,
  scenarioBFilters,
  onScenarioAChange,
  onScenarioBChange,
}: {
  statLabel: string
  filterOptions: PlayerStatFilterOptions | null
  scenarioAFilters: PlayerStatsFilters
  scenarioBFilters: PlayerStatsFilters
  onScenarioAChange: (filters: PlayerStatsFilters) => void
  onScenarioBChange: (filters: PlayerStatsFilters) => void
}) {
  const statOptions = filterOptions?.stats ?? [{ code: scenarioAFilters.stat, label: statLabel }]
  const homeAwayOptions = filterOptions?.home_away_options.length ? filterOptions.home_away_options : ['Home', 'Away']
  const homeAwaySelected = scenarioAFilters.homeAway.length === 1 && scenarioAFilters.homeAway[0] === 'Home' && scenarioBFilters.homeAway.length === 1 && scenarioBFilters.homeAway[0] === 'Away'
  const winLossSelected = scenarioAFilters.marginMin === '0' && scenarioAFilters.marginMax === '200' && scenarioBFilters.marginMin === '-200' && scenarioBFilters.marginMax === '-1'

  const applyShared = (patch: Partial<PlayerStatsFilters>) => {
    onScenarioAChange({ ...scenarioAFilters, ...patch })
    onScenarioBChange({ ...scenarioBFilters, ...patch })
  }

  const updateStat = (stat: string) => {
    applyShared({ stat, ...defaultsForStat(stat) })
  }

  return (
    <div className="comparison-setup">
      <div className="comparison-controls-grid">
        <Field label="Shared stat">
          <Select value={scenarioAFilters.stat} onChange={(event) => updateStat(event.currentTarget.value)}>
            {statOptions.map((stat) => (
              <option key={stat.code} value={stat.code}>{stat.label}</option>
            ))}
          </Select>
        </Field>
        <Field label="Line mode">
          <Segmented
            value={scenarioAFilters.lineMode}
            ariaLabel="Comparison line mode"
            options={[
              { value: 'single', label: 'Single' },
              { value: 'interval', label: 'Interval' },
            ]}
            onChange={(lineMode) => applyShared({ lineMode })}
          />
        </Field>
        {scenarioAFilters.lineMode === 'interval' ? (
          <>
            <Field label="Lower">
              <TextInput inputMode="decimal" value={scenarioAFilters.lowerBound} onChange={(event) => applyShared({ lowerBound: event.currentTarget.value })} />
            </Field>
            <Field label="Upper">
              <TextInput inputMode="decimal" value={scenarioAFilters.upperBound} onChange={(event) => applyShared({ upperBound: event.currentTarget.value })} />
            </Field>
          </>
        ) : (
          <Field label="Reference line">
            <TextInput inputMode="decimal" value={scenarioAFilters.referenceLine} onChange={(event) => applyShared({ referenceLine: event.currentTarget.value })} />
          </Field>
        )}
      </div>
      <div className="quick-filter-grid player-quick-actions" aria-label="Comparison quick filters">
        {homeAwayOptions.includes('Home') && homeAwayOptions.includes('Away') ? (
          <Chip
            active={homeAwaySelected}
            onClick={() => {
              if (homeAwaySelected) {
                onScenarioAChange({ ...scenarioAFilters, homeAway: homeAwayOptions })
                onScenarioBChange({ ...scenarioBFilters, homeAway: homeAwayOptions })
              } else {
                onScenarioAChange({ ...scenarioAFilters, homeAway: ['Home'] })
                onScenarioBChange({ ...scenarioBFilters, homeAway: ['Away'] })
              }
            }}
          >
            Home vs Away
          </Chip>
        ) : null}
        <Chip
          active={winLossSelected}
          onClick={() => {
            if (winLossSelected) {
              onScenarioAChange({ ...scenarioAFilters, marginMin: '-200', marginMax: '200' })
              onScenarioBChange({ ...scenarioBFilters, marginMin: '-200', marginMax: '200' })
            } else {
              onScenarioAChange({ ...scenarioAFilters, marginMin: '0', marginMax: '200' })
              onScenarioBChange({ ...scenarioBFilters, marginMin: '-200', marginMax: '-1' })
            }
          }}
        >
          Win vs Loss
        </Chip>
      </div>
    </div>
  )
}

function PlayerComparisonMode({
  statLabel,
  scenarioAFilters,
  scenarioBFilters,
  scenarioAData,
  scenarioBData,
  comparisonFocus,
  isLoading,
  historySort,
  onSort,
  onFocusChange,
  onEditScenarioA,
  onEditScenarioB,
}: {
  statLabel: string
  scenarioAFilters: PlayerStatsFilters
  scenarioBFilters: PlayerStatsFilters
  scenarioAData: { history: PlayerGameLogEntry[]; summary: PlayerStatSummary | null } | null
  scenarioBData: { history: PlayerGameLogEntry[]; summary: PlayerStatSummary | null } | null
  comparisonFocus: ScenarioId
  isLoading: boolean
  historySort: { key: HistorySortKey; direction: 'asc' | 'desc' }
  onSort: (key: HistorySortKey) => void
  onFocusChange: (scenario: ScenarioId) => void
  onEditScenarioA: () => void
  onEditScenarioB: () => void
}) {
  const historyA = scenarioAData?.history ?? []
  const historyB = scenarioBData?.history ?? []
  const selectedStatKey = normalizeStatKey(scenarioAFilters.stat)
  const focusedHistory = comparisonFocus === 'a' ? historyA : historyB
  const sortedFocusedHistory = useMemo(() => sortHistory(focusedHistory, historySort.key, historySort.direction), [focusedHistory, historySort])

  return (
    <>
      <div className="scenario-grid">
        <ScenarioCard label="Scenario A" filters={scenarioAFilters} history={historyA} summary={scenarioAData?.summary ?? null} onEdit={onEditScenarioA} />
        <ScenarioCard label="Scenario B" filters={scenarioBFilters} history={historyB} summary={scenarioBData?.summary ?? null} onEdit={onEditScenarioB} />
      </div>
      <ComparisonSummaryPanel
        statLabel={statLabel}
        scenarioAFilters={scenarioAFilters}
        scenarioBFilters={scenarioBFilters}
        scenarioAData={scenarioAData}
        scenarioBData={scenarioBData}
        isLoading={isLoading}
      />
      <ComparisonGraphPanel
        statLabel={statLabel}
        scenarioAFilters={scenarioAFilters}
        scenarioBFilters={scenarioBFilters}
        historyA={historyA}
        historyB={historyB}
        isLoading={isLoading}
      />
      <Panel className="table-panel comparison-log-panel">
        <div className="section-heading">
          <h2>{comparisonFocus === 'a' ? 'Scenario A' : 'Scenario B'} game log</h2>
          <Segmented<ScenarioId>
            value={comparisonFocus}
            ariaLabel="Game log scenario"
            options={[
              { value: 'a', label: 'Scenario A' },
              { value: 'b', label: 'Scenario B' },
            ]}
            onChange={onFocusChange}
          />
        </div>
        {sortedFocusedHistory.length === 0 && !isLoading ? (
          <EmptyState title="No game log" body="Adjust the scenario filters to load matching games." />
        ) : (
          <PlayerHistoryTable history={sortedFocusedHistory} selectedStatKey={selectedStatKey} sort={historySort} onSort={onSort} />
        )}
      </Panel>
    </>
  )
}

function ScenarioCard({
  label,
  filters,
  history,
  summary,
  onEdit,
}: {
  label: string
  filters: PlayerStatsFilters
  history: PlayerGameLogEntry[]
  summary: PlayerStatSummary | null
  onEdit: () => void
}) {
  return (
    <Panel className="scenario-card">
      <div className="scenario-card-head">
        <div>
          <h2>{label}</h2>
          <span>{history.length} games · {playerLineLabel(filters)}</span>
        </div>
        <Button variant="ghost" onClick={onEdit}><SlidersHorizontal size={14} /> Edit</Button>
      </div>
      <div className="scenario-metrics">
        <StatPill label="Average" value={comparisonAverage(history)} />
        <StatPill label={filters.lineMode === 'interval' ? 'Within' : 'Over'} value={comparisonOutcomeValue(summary, true)} tone="good" />
        <StatPill label={filters.lineMode === 'interval' ? 'Outside' : 'Under'} value={comparisonOutcomeValue(summary, false)} tone="warn" />
      </div>
      <ScenarioFilterChips filters={filters} />
    </Panel>
  )
}

function ScenarioFilterChips({ filters }: { filters: PlayerStatsFilters }) {
  const parts: string[] = []
  if (filters.seasons.length > 0) parts.push(`Seasons: ${summarize(filters.seasons)}`)
  if (!(filters.homeAway.includes('Home') && filters.homeAway.includes('Away'))) parts.push(filters.homeAway.join(' / '))
  if (filters.oppositions.length > 0) parts.push(`Opp: ${summarize(filters.oppositions)}`)
  if (filters.venues.length > 0) parts.push(`Venue: ${summarize(filters.venues)}`)
  if (filters.marginMin !== '-200' || filters.marginMax !== '200') parts.push(`Margin ${filters.marginMin} to ${filters.marginMax}`)
  if (filters.lastGames.trim()) parts.push(`Last ${filters.lastGames}`)
  return (
    <div className="tag-row scenario-tags">
      {(parts.length ? parts : ['All games']).map((part) => <span className="tag" key={part}>{part}</span>)}
    </div>
  )
}

function ComparisonSummaryPanel({
  statLabel,
  scenarioAFilters,
  scenarioBFilters,
  scenarioAData,
  scenarioBData,
  isLoading,
}: {
  statLabel: string
  scenarioAFilters: PlayerStatsFilters
  scenarioBFilters: PlayerStatsFilters
  scenarioAData: { history: PlayerGameLogEntry[]; summary: PlayerStatSummary | null } | null
  scenarioBData: { history: PlayerGameLogEntry[]; summary: PlayerStatSummary | null } | null
  isLoading: boolean
}) {
  const historyA = scenarioAData?.history ?? []
  const historyB = scenarioBData?.history ?? []
  const summaryA = scenarioAData?.summary ?? null
  const summaryB = scenarioBData?.summary ?? null
  const labels = comparisonOutcomeLabels(scenarioAFilters, scenarioBFilters)
  const rows = [
    ['Games', String(summaryA?.sample_size ?? historyA.length), String(summaryB?.sample_size ?? historyB.length)],
    ['Average', comparisonAverage(historyA), comparisonAverage(historyB)],
    [labels[0], comparisonOutcomeValue(summaryA, true), comparisonOutcomeValue(summaryB, true)],
    [labels[1], comparisonOutcomeValue(summaryA, false), comparisonOutcomeValue(summaryB, false)],
  ]
  return (
    <Panel className="comparison-summary-panel">
      <div className="section-heading">
        <h2>{statLabel} scenario comparison</h2>
        <span>{isLoading ? 'Refreshing' : 'Updated'}</span>
      </div>
      <div className="comparison-table">
        <div className="comparison-table-row comparison-table-head">
          <span>Metric</span>
          <span>A · {playerLineLabel(scenarioAFilters)}</span>
          <span>B · {playerLineLabel(scenarioBFilters)}</span>
        </div>
        {rows.map(([label, a, b]) => (
          <div className="comparison-table-row" key={label}>
            <b>{label}</b>
            <span>{a}</span>
            <span>{b}</span>
          </div>
        ))}
      </div>
    </Panel>
  )
}

function ComparisonGraphPanel({
  statLabel,
  scenarioAFilters,
  scenarioBFilters,
  historyA,
  historyB,
  isLoading,
}: {
  statLabel: string
  scenarioAFilters: PlayerStatsFilters
  scenarioBFilters: PlayerStatsFilters
  historyA: PlayerGameLogEntry[]
  historyB: PlayerGameLogEntry[]
  isLoading: boolean
}) {
  const data = useMemo(() => buildComparisonChartData(historyA, historyB), [historyA, historyB])
  const guidesA = useMemo(() => referenceLinesFromFilters(scenarioAFilters.lineMode, scenarioAFilters.referenceLine, scenarioAFilters.lowerBound, scenarioAFilters.upperBound), [scenarioAFilters])
  const guidesB = useMemo(() => referenceLinesFromFilters(scenarioBFilters.lineMode, scenarioBFilters.referenceLine, scenarioBFilters.lowerBound, scenarioBFilters.upperBound), [scenarioBFilters])
  const domain = useMemo(() => computeChartDomain(data.flatMap((entry) => [entry.scenarioA, entry.scenarioB]), [...guidesA, ...guidesB].map((line) => line.value)), [data, guidesA, guidesB])
  if (data.length === 0 && !isLoading) {
    return (
      <Panel className="chart-panel">
        <EmptyState title="No comparison graph" body="Adjust the scenario filters to load game history." />
      </Panel>
    )
  }
  return (
    <Panel className="chart-panel comparison-chart-panel">
      <div className="section-heading">
        <h2>{statLabel} comparison graph</h2>
        <span>{isLoading ? 'Refreshing' : `${data.length} recency slots`}</span>
      </div>
      <div className="chart-frame">
        <ResponsiveContainer width="100%" height={300}>
          <LineChart data={data}>
            <CartesianGrid vertical={false} stroke="#dce7ec" />
            <XAxis dataKey="gameNumber" tickFormatter={(value) => `#${value}`} tickLine={false} axisLine={false} minTickGap={18} />
            <YAxis tickLine={false} axisLine={false} width={42} domain={domain} />
            <Tooltip content={<ComparisonChartTooltip />} />
            {guidesA.map((line) => <ReferenceLine key={`a-${line.key}`} y={line.value} stroke="#7c3aed" strokeDasharray="7 5" strokeWidth={1.4} />)}
            {guidesB.map((line) => <ReferenceLine key={`b-${line.key}`} y={line.value} stroke="#0f766e" strokeDasharray="7 5" strokeWidth={1.4} />)}
            <Line
              type="linear"
              dataKey="scenarioA"
              name="Scenario A"
              stroke="#7c3aed"
              strokeWidth={2.2}
              dot={(props) => <ComparisonResultDot {...props} scenario="a" />}
              activeDot={(props) => <ComparisonResultDot {...props} scenario="a" isActive />}
              connectNulls
              isAnimationActive={false}
            />
            <Line
              type="linear"
              dataKey="scenarioB"
              name="Scenario B"
              stroke="#0f766e"
              strokeWidth={2.2}
              dot={(props) => <ComparisonResultDot {...props} scenario="b" />}
              activeDot={(props) => <ComparisonResultDot {...props} scenario="b" isActive />}
              connectNulls
              isAnimationActive={false}
            />
          </LineChart>
        </ResponsiveContainer>
      </div>
      <div className="comparison-legend">
        <span><i className="legend-dot legend-dot--a" /> Scenario A</span>
        <span><i className="legend-dot legend-dot--b" /> Scenario B</span>
        <span><i className="legend-symbol legend-symbol--hit" /> Hit</span>
        <span><i className="legend-symbol legend-symbol--miss" /> Miss</span>
        <span className="muted">Dashed lines show each scenario reference.</span>
      </div>
    </Panel>
  )
}

function PlayerHistoryTable({
  history,
  selectedStatKey,
  sort,
  onSort,
}: {
  history: PlayerGameLogEntry[]
  selectedStatKey: PlayerStatKey | null
  sort: { key: HistorySortKey; direction: 'asc' | 'desc' }
  onSort: (key: HistorySortKey) => void
}) {
  return (
    <div className="data-table-wrap">
      <table className="data-table">
        <thead>
          <tr>
            <SortableHistoryHeader label="Date" columnKey="date" sort={sort} onSort={onSort} />
            <SortableHistoryHeader label="Round" columnKey="round" sort={sort} onSort={onSort} />
            <SortableHistoryHeader label="Opposition" columnKey="opposition" sort={sort} onSort={onSort} />
            <SortableHistoryHeader label="Venue" columnKey="venue" sort={sort} onSort={onSort} />
            {PLAYER_STAT_COLUMNS.map((column) => (
              <SortableHistoryHeader key={column.key} label={column.label} columnKey={column.key} sort={sort} onSort={onSort} isHighlighted={column.key === selectedStatKey} />
            ))}
            <SortableHistoryHeader label="Result" columnKey="hit" sort={sort} onSort={onSort} />
          </tr>
        </thead>
        <tbody>
          {history.map((entry) => (
            <tr key={`${entry.date}-${entry.game_number}`}>
              <td>{formatMatchDateTime(entry.date)}</td>
              <td>{entry.round_label ?? '-'}</td>
              <td>{entry.opposition ?? '-'}</td>
              <td>{entry.venue ?? '-'}</td>
              {PLAYER_STAT_COLUMNS.map((column) => {
                const highlighted = column.key === selectedStatKey
                return (
                  <td key={column.key} className={highlighted ? `stat-column-highlight${entry.hit === false ? ' stat-column-highlight--miss' : ''}` : undefined}>
                    <b className="tabular">{formatStatCell(statValue(entry, column.key), column.suffix)}</b>
                  </td>
                )
              })}
              <td><span className={entry.hit ? 'tag tag--good' : 'tag'}>{entry.hit == null ? 'n/a' : entry.hit ? 'Hit' : 'Miss'}</span></td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  )
}

function summarize(values: string[]) {
  if (values.length <= 2) return values.join(', ')
  return `${values.length} selected`
}

function PlayerFiltersDrawer({
  title,
  filterOptions,
  draft,
  onChange,
  onApply,
  onReset,
  onClose,
  showStatControls = true,
}: {
  title: string
  filterOptions: PlayerStatFilterOptions | null
  draft: PlayerStatsFilters
  onChange: (filters: PlayerStatsFilters) => void
  onApply: () => void
  onReset: () => void
  onClose: () => void
  showStatControls?: boolean
}) {
  const statOptions = filterOptions?.stats ?? [{ code: 'disposals', label: 'Disposals' }]
  const updateStat = (stat: string) => {
    const defaults = defaultsForStat(stat)
    onChange({ ...draft, stat, referenceLine: defaults.referenceLine, lowerBound: defaults.lowerBound, upperBound: defaults.upperBound })
  }
  return (
    <div className="drawer-overlay" onClick={onClose}>
      <aside className="drawer" role="dialog" aria-modal="true" aria-label="Player filters" onClick={(event) => event.stopPropagation()}>
        <div className="drawer-head">
          <h2>{title}</h2>
          <button type="button" className="modal-close" onClick={onClose} aria-label="Close filters">
            <X size={16} />
          </button>
        </div>
        <div className="drawer-body">
          {showStatControls ? (
            <>
              <div className="filter-pair">
                <Field label="Statistic">
                  <Select value={draft.stat} onChange={(event) => updateStat(event.currentTarget.value)}>
                    {statOptions.map((stat) => (
                      <option key={stat.code} value={stat.code}>{stat.label}</option>
                    ))}
                  </Select>
                </Field>
                <Field label="Line mode">
                  <Segmented
                    value={draft.lineMode}
                    ariaLabel="Line mode"
                    options={[
                      { value: 'single', label: 'Single' },
                      { value: 'interval', label: 'Interval' },
                    ]}
                    onChange={(value) => onChange({ ...draft, lineMode: value })}
                  />
                </Field>
              </div>

              {draft.lineMode === 'single' ? (
                <Field label="Reference line">
                  <TextInput inputMode="decimal" value={draft.referenceLine} onChange={(event) => onChange({ ...draft, referenceLine: event.currentTarget.value })} />
                </Field>
              ) : (
                <div className="filter-pair">
                  <Field label="Lower bound">
                    <TextInput inputMode="decimal" value={draft.lowerBound} onChange={(event) => onChange({ ...draft, lowerBound: event.currentTarget.value })} />
                  </Field>
                  <Field label="Upper bound">
                    <TextInput inputMode="decimal" value={draft.upperBound} onChange={(event) => onChange({ ...draft, upperBound: event.currentTarget.value })} />
                  </Field>
                </div>
              )}
            </>
          ) : (
            <p className="drawer-note">Stat and line are controlled from the comparison setup. Edit scenario-specific context here.</p>
          )}

          <div className="filter-pair">
            <Field label="Margin min">
              <TextInput inputMode="numeric" value={draft.marginMin} onChange={(event) => onChange({ ...draft, marginMin: event.currentTarget.value })} />
            </Field>
            <Field label="Margin max">
              <TextInput inputMode="numeric" value={draft.marginMax} onChange={(event) => onChange({ ...draft, marginMax: event.currentTarget.value })} />
            </Field>
          </div>

          <div className="filter-pair">
            <Field label="Last N games">
              <TextInput inputMode="numeric" value={draft.lastGames} placeholder="All" onChange={(event) => onChange({ ...draft, lastGames: event.currentTarget.value })} />
            </Field>
            <Field label="Min TOG %">
              <TextInput inputMode="numeric" value={draft.minutesMinimum} onChange={(event) => onChange({ ...draft, minutesMinimum: event.currentTarget.value })} />
            </Field>
          </div>

          <ToggleChipGroup
            title="Seasons"
            options={filterOptions?.seasons ?? []}
            selected={draft.seasons}
            onToggle={(value) => onChange({ ...draft, seasons: toggleArrayValue(draft.seasons, value) })}
          />
          <ToggleChipGroup
            title="Home / Away"
            options={filterOptions?.home_away_options ?? ['Home', 'Away']}
            selected={draft.homeAway}
            onToggle={(value) => onChange({ ...draft, homeAway: toggleArrayValue(draft.homeAway, value) })}
          />
          <ToggleChipGroup
            title="Opposition"
            options={filterOptions?.oppositions ?? []}
            selected={draft.oppositions}
            onToggle={(value) => onChange({ ...draft, oppositions: toggleArrayValue(draft.oppositions, value) })}
          />
          <ToggleChipGroup
            title="Venue"
            options={filterOptions?.venues ?? []}
            selected={draft.venues}
            onToggle={(value) => onChange({ ...draft, venues: toggleArrayValue(draft.venues, value) })}
          />
          <ToggleChipGroup
            title="Weather"
            options={filterOptions?.weather_categories ?? []}
            selected={draft.weatherCategories}
            onToggle={(value) => onChange({ ...draft, weatherCategories: toggleArrayValue(draft.weatherCategories, value) })}
          />
        </div>
        <div className="drawer-foot">
          <Button variant="ghost" onClick={onReset}>Reset</Button>
          <Button variant="primary" onClick={onApply}>Apply filters</Button>
        </div>
      </aside>
    </div>
  )
}

function ToggleChipGroup({
  title,
  options,
  selected,
  onToggle,
  collapsedCount = 12,
}: {
  title: string
  options: string[]
  selected: string[]
  onToggle: (value: string) => void
  collapsedCount?: number
}) {
  const [showAll, setShowAll] = useState(false)
  if (options.length === 0) return null
  const visibleOptions =
    showAll || options.length <= collapsedCount
      ? options
      : Array.from(new Set([...options.slice(0, collapsedCount), ...selected.filter((value) => options.includes(value))]))
  return (
    <div className="filter-group">
      <span className="filter-group-title">{title}</span>
      <div className="chip-row">
        {visibleOptions.map((option) => (
          <Chip key={option} active={selected.includes(option)} onClick={() => onToggle(option)}>
            {option}
          </Chip>
        ))}
      </div>
      {options.length > collapsedCount ? (
        <button type="button" className="link-button" onClick={() => setShowAll((current) => !current)}>
          {showAll ? 'Show fewer' : `Show all ${options.length}`}
        </button>
      ) : null}
    </div>
  )
}

type PlayerStatKey =
  | 'disposals'
  | 'kicks'
  | 'handballs'
  | 'marks'
  | 'goals'
  | 'behinds'
  | 'tackles'
  | 'hitouts'
  | 'frees_for'
  | 'frees_against'
  | 'fantasy'
  | 'cba'
  | 'tog'

type HistorySortKey = 'date' | 'round' | 'opposition' | 'venue' | 'hit' | PlayerStatKey
type PlayerMode = 'stats' | 'comparison'
type ScenarioId = 'a' | 'b'
type FilterTarget = 'stats' | 'scenarioA' | 'scenarioB' | null
type ReferenceResult = 'above' | 'below' | 'inside' | 'outside' | null
type BaseChartPoint = {
  x: number
  date: string
  value: number | null
  hit: boolean | null
  referenceResult: ReferenceResult
}
type ChartPoint = BaseChartPoint & {
  hitValue?: number | null
  missValue?: number | null
  isTransition?: boolean
}
type ComparisonChartPoint = {
  gameNumber: number
  label: string
  scenarioA: number | null
  scenarioB: number | null
  scenarioAHit: boolean | null
  scenarioBHit: boolean | null
  scenarioADate: string
  scenarioBDate: string
}
type ComparisonDotProps = {
  cx?: number
  cy?: number
  payload?: ComparisonChartPoint
}

const PLAYER_STAT_COLUMNS: { key: PlayerStatKey; label: string; suffix?: string }[] = [
  { key: 'disposals', label: 'Disp' },
  { key: 'kicks', label: 'Kicks' },
  { key: 'handballs', label: 'Handballs' },
  { key: 'marks', label: 'Marks' },
  { key: 'goals', label: 'Goals' },
  { key: 'behinds', label: 'Behinds' },
  { key: 'tackles', label: 'Tackles' },
  { key: 'hitouts', label: 'Hitouts' },
  { key: 'frees_for', label: 'Frees for' },
  { key: 'frees_against', label: 'Frees agst' },
  { key: 'fantasy', label: 'Fantasy' },
  { key: 'cba', label: 'CBA', suffix: '%' },
  { key: 'tog', label: 'TOG', suffix: '%' },
]

function SortableHistoryHeader({
  label,
  columnKey,
  sort,
  onSort,
  isHighlighted = false,
}: {
  label: string
  columnKey: HistorySortKey
  sort: { key: HistorySortKey; direction: 'asc' | 'desc' }
  onSort: (key: HistorySortKey) => void
  isHighlighted?: boolean
}) {
  const active = sort.key === columnKey
  return (
    <th aria-sort={active ? (sort.direction === 'asc' ? 'ascending' : 'descending') : 'none'} className={isHighlighted ? 'stat-column-highlight' : undefined}>
      <button type="button" className="sort-header" onClick={() => onSort(columnKey)}>
        <span>{label}</span>
        {active ? sort.direction === 'asc' ? <ArrowUp size={12} /> : <ArrowDown size={12} /> : <ArrowDownUp size={12} />}
      </button>
    </th>
  )
}

function sortHistory(history: PlayerGameLogEntry[], key: HistorySortKey, direction: 'asc' | 'desc') {
  const multiplier = direction === 'asc' ? 1 : -1
  return history.toSorted((left, right) => compareHistoryValue(historyValue(left, key), historyValue(right, key)) * multiplier)
}

function historyValue(entry: PlayerGameLogEntry, key: HistorySortKey) {
  switch (key) {
    case 'date':
      return new Date(entry.date).getTime()
    case 'round':
      return entry.round_label ?? ''
    case 'opposition':
      return entry.opposition ?? ''
    case 'venue':
      return entry.venue ?? ''
    case 'hit':
      return entry.hit == null ? -1 : entry.hit ? 1 : 0
    default:
      return statValue(entry, key) ?? -1
  }
}

function compareHistoryValue(left: string | number, right: string | number) {
  if (typeof left === 'string' || typeof right === 'string') return `${left}`.localeCompare(`${right}`)
  return left - right
}

function statValue(entry: PlayerGameLogEntry, key: PlayerStatKey) {
  return entry[key]
}

function formatStatCell(value: number | null, suffix?: string) {
  if (value == null) return '--'
  const formatted = Number.isInteger(value) ? value.toFixed(0) : value.toFixed(1)
  return suffix ? `${formatted}${suffix}` : formatted
}

function normalizeStatKey(stat: string): PlayerStatKey | null {
  const normalized = stat.replace(/^player_/, '')
  if (normalized === 'fantasy_points') return 'fantasy'
  if (isPlayerStatKey(normalized)) return normalized
  return null
}

function isPlayerStatKey(value: string): value is PlayerStatKey {
  return PLAYER_STAT_COLUMNS.some((column) => column.key === value)
}

function defaultsForStat(stat: string) {
  const key = normalizeStatKey(stat)
  const line = key ? STAT_REFERENCE_DEFAULTS[key] : '19.5'
  const parsed = Number(line)
  const lower = Number.isFinite(parsed) ? formatDefaultLine(Math.max(0.5, parsed - 3)) : line
  const upper = Number.isFinite(parsed) ? formatDefaultLine(parsed + 3) : line
  return { referenceLine: line, lowerBound: lower, upperBound: upper }
}

function formatDefaultLine(value: number) {
  return Number.isInteger(value) ? value.toFixed(0) : value.toFixed(1)
}

const STAT_REFERENCE_DEFAULTS: Record<PlayerStatKey, string> = {
  disposals: '19.5',
  kicks: '12.5',
  handballs: '8.5',
  marks: '4.5',
  goals: '0.5',
  behinds: '0.5',
  tackles: '3.5',
  hitouts: '15.5',
  frees_for: '0.5',
  frees_against: '0.5',
  fantasy: '80.5',
  cba: '50.0',
  tog: '75.0',
}

function referenceLinesFromFilters(lineMode: 'single' | 'interval', referenceLine: string, lowerBound: string, upperBound: string) {
  if (lineMode === 'interval') {
    return [
      { key: 'lower', label: `Lower ${lowerBound}`, value: Number(lowerBound) },
      { key: 'upper', label: `Upper ${upperBound}`, value: Number(upperBound) },
    ].filter((line) => Number.isFinite(line.value))
  }
  const value = Number(referenceLine)
  return Number.isFinite(value) ? [{ key: 'reference', label: `Line ${referenceLine}`, value }] : []
}

function referenceResultForValue(value: number | null, lineMode: 'single' | 'interval', referenceLine: string, lowerBound: string, upperBound: string): ReferenceResult {
  if (value == null) return null
  if (lineMode === 'interval') {
    const lower = Number(lowerBound)
    const upper = Number(upperBound)
    if (!Number.isFinite(lower) || !Number.isFinite(upper)) return null
    return value >= lower && value <= upper ? 'inside' : 'outside'
  }
  const reference = Number(referenceLine)
  if (!Number.isFinite(reference)) return null
  return value > reference ? 'above' : 'below'
}

function playerLineLabel(filters: PlayerStatsFilters) {
  return filters.lineMode === 'interval' ? `${filters.lowerBound} - ${filters.upperBound}` : filters.referenceLine
}

function comparisonAverage(history: PlayerGameLogEntry[]) {
  const values = history.map((entry) => entry.selected_value).filter((value): value is number => value != null && Number.isFinite(value))
  if (values.length === 0) return '--'
  return (values.reduce((total, value) => total + value, 0) / values.length).toFixed(1)
}

function comparisonOutcomeLabels(filtersA: PlayerStatsFilters, filtersB: PlayerStatsFilters): [string, string] {
  if (filtersA.lineMode !== filtersB.lineMode) return ['Outcome 1', 'Outcome 2']
  return filtersA.lineMode === 'interval' ? ['Within', 'Outside'] : ['Over', 'Under']
}

function comparisonOutcomeValue(summary: PlayerStatSummary | null, primary: boolean) {
  if (!summary) return '--'
  const probability = summary.line_mode === 'interval'
    ? primary
      ? summary.proportion_within_interval
      : summary.proportion_outside_interval
    : primary
      ? summary.proportion_over
      : summary.proportion_under
  const price = summary.line_mode === 'interval'
    ? primary
      ? summary.implied_odds_within_interval
      : summary.implied_odds_outside_interval
    : primary
      ? summary.implied_odds_over
      : summary.implied_odds_under
  return `${formatPercent(probability)} / ${formatPrice(price)}`
}

function buildComparisonChartData(historyA: PlayerGameLogEntry[], historyB: PlayerGameLogEntry[]): ComparisonChartPoint[] {
  const count = Math.max(historyA.length, historyB.length)
  return Array.from({ length: count }, (_, index) => {
    const a = historyA[index]
    const b = historyB[index]
    const gameNumber = index + 1
    return {
      gameNumber,
      label: `Game #${gameNumber}`,
      scenarioA: a?.selected_value ?? null,
      scenarioB: b?.selected_value ?? null,
      scenarioAHit: a?.hit ?? null,
      scenarioBHit: b?.hit ?? null,
      scenarioADate: a ? formatMatchDateTime(a.date) : '',
      scenarioBDate: b ? formatMatchDateTime(b.date) : '',
    }
  })
}

function buildSegmentedChartData(points: BaseChartPoint[]): ChartPoint[] {
  return points.flatMap((point, index) => {
    const current = chartPointWithSeries(point)
    const next = points[index + 1]
    if (!next || point.value == null || next.value == null || sameResultSide(point.referenceResult, next.referenceResult)) {
      return [current]
    }

    const midpointValue = (point.value + next.value) / 2
    return [
      current,
      {
        x: (point.x + next.x) / 2,
        date: '',
        value: midpointValue,
        hit: null,
        referenceResult: null,
        hitValue: midpointValue,
        missValue: midpointValue,
        isTransition: true,
      },
    ]
  })
}

function chartPointWithSeries(point: BaseChartPoint): ChartPoint {
  const hitValue = point.value != null && isHitResult(point.referenceResult) ? point.value : null
  const missValue = point.value != null && isMissResult(point.referenceResult) ? point.value : null
  return { ...point, hitValue, missValue }
}

function sameResultSide(left: ReferenceResult, right: ReferenceResult) {
  return isMissResult(left) === isMissResult(right)
}

function isHitResult(result: ReferenceResult) {
  return result == null || result === 'above' || result === 'inside'
}

function isMissResult(result: ReferenceResult) {
  return result === 'below' || result === 'outside'
}

function computeChartDomain(values: (number | null)[], guides: number[]): [number, number] | ['auto', 'auto'] {
  const numericValues = [...values, ...guides].filter((value): value is number => value != null && Number.isFinite(value))
  if (numericValues.length === 0) return ['auto', 'auto']
  const min = Math.min(...numericValues)
  const max = Math.max(...numericValues)
  const spread = Math.max(4, max - min)
  const padding = spread * 0.12
  return [Math.max(0, Math.floor(min - padding)), Math.ceil(max + padding)]
}

function PlayerChartTooltip({
  active,
  payload,
  label,
}: {
  active?: boolean
  payload?: { payload?: ChartPoint }[]
  label?: string
}) {
  if (!active) return null
  const point = payload?.find((item) => item.payload?.value != null)?.payload
  if (!point || point.value == null || point.isTransition) return null
  const result = isMissResult(point.referenceResult) ? 'Miss' : point.referenceResult == null ? 'Value' : 'Hit'
  return (
    <div className="chart-tooltip">
      <b>{point.date || label}</b>
      <span>{point.value}</span>
      <small>{result}</small>
    </div>
  )
}

function ComparisonChartTooltip({
  active,
  payload,
}: {
  active?: boolean
  payload?: { dataKey?: string; value?: number; payload?: ComparisonChartPoint; color?: string }[]
}) {
  if (!active) return null
  const point = payload?.find((item) => item.payload)?.payload
  if (!point) return null
  const rows = [
    { label: 'Scenario A', value: point.scenarioA, hit: point.scenarioAHit, date: point.scenarioADate, color: '#7c3aed' },
    { label: 'Scenario B', value: point.scenarioB, hit: point.scenarioBHit, date: point.scenarioBDate, color: '#0f766e' },
  ].filter((row) => row.value != null)
  if (rows.length === 0) return null
  return (
    <div className="chart-tooltip comparison-tooltip">
      <b>{point.label}</b>
      {rows.map((row) => (
        <div key={row.label}>
          <b style={{ color: row.color }}>{row.label}</b>
          <span>{row.value}</span>
          <small>{row.hit == null ? 'No result' : row.hit ? 'Hit' : 'Miss'} · {row.date}</small>
        </div>
      ))}
    </div>
  )
}

function ComparisonResultDot({
  cx,
  cy,
  payload,
  scenario,
  isActive = false,
}: ComparisonDotProps & {
  scenario: ScenarioId
  isActive?: boolean
}) {
  const hit = scenario === 'a' ? payload?.scenarioAHit : payload?.scenarioBHit
  if (cx == null || cy == null || hit == null) return null
  const radius = isActive ? 8 : 6
  const stroke = hit ? '#047857' : '#b42318'
  const fill = hit ? '#ecfdf3' : '#fff1f0'
  const strokeWidth = isActive ? 2.4 : 2

  return (
    <g>
      <circle cx={cx} cy={cy} r={radius} fill={fill} stroke={stroke} strokeWidth={1.5} />
      {hit ? (
        <path
          d={`M ${cx - 3.2} ${cy - 0.2} L ${cx - 0.8} ${cy + 2.6} L ${cx + 3.8} ${cy - 3.2}`}
          fill="none"
          stroke={stroke}
          strokeLinecap="round"
          strokeLinejoin="round"
          strokeWidth={strokeWidth}
        />
      ) : (
        <>
          <path d={`M ${cx - 3} ${cy - 3} L ${cx + 3} ${cy + 3}`} fill="none" stroke={stroke} strokeLinecap="round" strokeWidth={strokeWidth} />
          <path d={`M ${cx + 3} ${cy - 3} L ${cx - 3} ${cy + 3}`} fill="none" stroke={stroke} strokeLinecap="round" strokeWidth={strokeWidth} />
        </>
      )}
    </g>
  )
}

function ReferenceResultDot({
  cx,
  cy,
  payload,
  isActive = false,
}: {
  cx?: number
  cy?: number
  payload?: ChartPoint
  isActive?: boolean
}) {
  if (cx == null || cy == null || payload?.value == null || payload.referenceResult == null || payload.isTransition) return null
  const success = payload.referenceResult === 'above' || payload.referenceResult === 'inside'
  const radius = isActive ? 8 : 6
  const stroke = success ? '#047857' : '#b42318'
  const fill = success ? '#ecfdf3' : '#fff1f0'
  const strokeWidth = isActive ? 2.4 : 2

  return (
    <g>
      <circle cx={cx} cy={cy} r={radius} fill={fill} stroke={stroke} strokeWidth={1.5} />
      {success ? (
        <path
          d={`M ${cx - 3.2} ${cy - 0.2} L ${cx - 0.8} ${cy + 2.6} L ${cx + 3.8} ${cy - 3.2}`}
          fill="none"
          stroke={stroke}
          strokeLinecap="round"
          strokeLinejoin="round"
          strokeWidth={strokeWidth}
        />
      ) : (
        <>
          <path d={`M ${cx - 3} ${cy - 3} L ${cx + 3} ${cy + 3}`} fill="none" stroke={stroke} strokeLinecap="round" strokeWidth={strokeWidth} />
          <path d={`M ${cx + 3} ${cy - 3} L ${cx - 3} ${cy + 3}`} fill="none" stroke={stroke} strokeLinecap="round" strokeWidth={strokeWidth} />
        </>
      )}
    </g>
  )
}
