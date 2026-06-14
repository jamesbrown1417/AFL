import { useEffect, useMemo, useState } from 'react'
import { Area, AreaChart, CartesianGrid, ReferenceLine, ResponsiveContainer, Tooltip, XAxis, YAxis } from 'recharts'
import { ArrowDown, ArrowDownUp, ArrowUp, Search, SlidersHorizontal, X } from 'lucide-react'
import type { PlayerStatFilterOptions, PlayerGameLogEntry, PlayerStatsFilters } from '../api/types'
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
  const [filtersOpen, setFiltersOpen] = useState(false)
  const [draftFilters, setDraftFilters] = useState<PlayerStatsFilters>(filters)
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

  useEffect(() => {
    if (selectedPlayerId == null && firstPlayer) {
      setSelectedPlayer(firstPlayer.id, firstPlayer.full_name)
    }
  }, [firstPlayer, selectedPlayerId, setSelectedPlayer])

  const filterOptions = usePlayerFilters(settings, selectedPlayerId)
  const historyQuery = usePlayerHistory(settings, selectedPlayerId, filters)
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

  const selectPlayer = (playerId: number, playerName: string) => {
    setSelectedPlayer(playerId, playerName)
    setQuery(playerName)
    setSearchOpen(false)
  }

  const toggleHistorySort = (key: HistorySortKey) => {
    setHistorySort((current) => ({
      key,
      direction: current.key === key && current.direction === 'asc' ? 'desc' : 'asc',
    }))
  }

  const openFilters = () => {
    setDraftFilters(filters)
    setFiltersOpen(true)
  }

  const applyFilters = () => {
    setFilters(draftFilters)
    setFiltersOpen(false)
  }

  const resetFilters = () => {
    setDraftFilters({ ...defaultPlayerFilters, stat: filters.stat })
  }

  const activeFilterCount = useMemo(() => countActiveFilters(filters), [filters])

  return (
    <main className="workspace player-workspace">
      <section className="workspace-main">
        <div className="page-title-row">
          <div>
            <h1>Player</h1>
            <p>{selectedPlayerName || 'Search player history and implied prices'}</p>
          </div>
          <Button variant="secondary" onClick={openFilters}>
            <SlidersHorizontal size={15} /> Filters
            {activeFilterCount > 0 ? <span className="filter-count-badge">{activeFilterCount}</span> : null}
          </Button>
        </div>

        <Panel className="filters-panel">
          <div className="player-controls-bar">
            <div className="autocomplete">
              <div className="input-with-icon">
                <Search size={16} />
                <TextInput
                  value={query}
                  onChange={(event) => {
                    setQuery(event.currentTarget.value)
                    setSearchOpen(true)
                  }}
                  onFocus={() => setSearchOpen(true)}
                  onBlur={() => window.setTimeout(() => setSearchOpen(false), 120)}
                  placeholder="Search players"
                  aria-autocomplete="list"
                  aria-expanded={searchOpen && !playersQuery.isFetching && playerOptions.length > 0}
                />
              </div>
              {searchOpen && playersQuery.isFetching && (
                <div className="autocomplete-menu autocomplete-menu--status" role="status">
                  Searching...
                </div>
              )}
              {searchOpen && !playersQuery.isFetching && playerOptions.length > 0 && (
                <div className="autocomplete-menu" role="listbox" aria-label="Player suggestions">
                  {playerOptions.map((player) => (
                    <button
                      type="button"
                      role="option"
                      key={player.id}
                      onMouseDown={(event) => event.preventDefault()}
                      onClick={() => selectPlayer(player.id, player.full_name)}
                    >
                      {player.full_name}
                    </button>
                  ))}
                </div>
              )}
            </div>
            <FilterSummary filters={filters} />
          </div>
        </Panel>

        {historyQuery.error ? <ErrorBanner message={historyQuery.error instanceof Error ? historyQuery.error.message : 'Failed to load player history.'} /> : null}

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
                  <Area
                    dataKey="hitValue"
                    stroke="#0f766e"
                    strokeWidth={2}
                    fill="url(#historyFill)"
                    dot={false}
                    activeDot={false}
                    isAnimationActive={false}
                  />
                  <Area
                    dataKey="missValue"
                    stroke="#b42318"
                    strokeWidth={2}
                    fill="url(#historyMissFill)"
                    dot={false}
                    activeDot={false}
                    isAnimationActive={false}
                  />
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
                  <Area
                    dataKey="value"
                    stroke="transparent"
                    strokeWidth={0}
                    fill="transparent"
                    dot={<ReferenceResultDot />}
                    activeDot={<ReferenceResultDot isActive />}
                    isAnimationActive={false}
                  />
                </AreaChart>
              </ResponsiveContainer>
            </div>
          )}
        </Panel>

        <Panel className="table-panel">
          <div className="data-table-wrap">
            <table className="data-table">
              <thead>
                <tr>
                  <SortableHistoryHeader label="Date" columnKey="date" sort={historySort} onSort={toggleHistorySort} />
                  <SortableHistoryHeader label="Round" columnKey="round" sort={historySort} onSort={toggleHistorySort} />
                  <SortableHistoryHeader label="Opposition" columnKey="opposition" sort={historySort} onSort={toggleHistorySort} />
                  <SortableHistoryHeader label="Venue" columnKey="venue" sort={historySort} onSort={toggleHistorySort} />
                  {PLAYER_STAT_COLUMNS.map((column) => (
                    <SortableHistoryHeader
                      key={column.key}
                      label={column.label}
                      columnKey={column.key}
                      sort={historySort}
                      onSort={toggleHistorySort}
                      isHighlighted={column.key === selectedStatKey}
                    />
                  ))}
                  <SortableHistoryHeader label="Result" columnKey="hit" sort={historySort} onSort={toggleHistorySort} />
                </tr>
              </thead>
              <tbody>
                {sortedHistory.map((entry) => (
                  <tr key={`${entry.date}-${entry.game_number}`}>
                    <td>{formatMatchDateTime(entry.date)}</td>
                    <td>{entry.round_label ?? '-'}</td>
                    <td>{entry.opposition ?? '-'}</td>
                    <td>{entry.venue ?? '-'}</td>
                    {PLAYER_STAT_COLUMNS.map((column) => {
                      const highlighted = column.key === selectedStatKey
                      return (
                        <td
                          key={column.key}
                          className={highlighted ? `stat-column-highlight${entry.hit === false ? ' stat-column-highlight--miss' : ''}` : undefined}
                        >
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
        </Panel>
      </section>

      {filtersOpen ? (
        <PlayerFiltersDrawer
          filterOptions={filterOptions.data ?? null}
          draft={draftFilters}
          onChange={setDraftFilters}
          onApply={applyFilters}
          onReset={resetFilters}
          onClose={() => setFiltersOpen(false)}
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

function summarize(values: string[]) {
  if (values.length <= 2) return values.join(', ')
  return `${values.length} selected`
}

function PlayerFiltersDrawer({
  filterOptions,
  draft,
  onChange,
  onApply,
  onReset,
  onClose,
}: {
  filterOptions: PlayerStatFilterOptions | null
  draft: PlayerStatsFilters
  onChange: (filters: PlayerStatsFilters) => void
  onApply: () => void
  onReset: () => void
  onClose: () => void
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
          <h2>Filters</h2>
          <button type="button" className="modal-close" onClick={onClose} aria-label="Close filters">
            <X size={16} />
          </button>
        </div>
        <div className="drawer-body">
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
