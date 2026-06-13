import { useEffect, useMemo, useState } from 'react'
import { Area, AreaChart, CartesianGrid, ResponsiveContainer, Tooltip, XAxis, YAxis } from 'recharts'
import { ArrowDown, ArrowDownUp, ArrowUp, Search } from 'lucide-react'
import type { PlayerGameLogEntry } from '../api/types'
import { usePlayerFilters, usePlayerHistory, useStatPlayers } from '../api/queries'
import { useAppStore, useClientSettings } from '../store/useAppStore'
import { formatDateTime, formatPercent, formatPrice, formatShortDate } from '../lib/formatters'
import { EmptyState, ErrorBanner, Field, Panel, Select, StatPill, TextInput } from '../components/ui'

export function PlayerLab() {
  const settings = useClientSettings()
  const selectedPlayerId = useAppStore((state) => state.selectedPlayerId)
  const selectedPlayerName = useAppStore((state) => state.selectedPlayerName)
  const setSelectedPlayer = useAppStore((state) => state.setSelectedPlayer)
  const filters = useAppStore((state) => state.playerFilters)
  const setFilters = useAppStore((state) => state.setPlayerFilters)
  const [query, setQuery] = useState(selectedPlayerName)
  const [searchOpen, setSearchOpen] = useState(false)
  const [historySort, setHistorySort] = useState<{ key: HistorySortKey; direction: 'asc' | 'desc' }>({
    key: 'date',
    direction: 'desc',
  })
  const playersQuery = useStatPlayers(settings, query)
  const firstPlayer = playersQuery.data?.[0]
  const playerOptions = playersQuery.data?.slice(0, 10) ?? []

  useEffect(() => {
    if (selectedPlayerId == null && firstPlayer) {
      setSelectedPlayer(firstPlayer.id, firstPlayer.full_name)
    }
  }, [firstPlayer, selectedPlayerId, setSelectedPlayer])

  const filterOptions = usePlayerFilters(settings, selectedPlayerId)
  const historyQuery = usePlayerHistory(settings, selectedPlayerId, filters)
  const history = useMemo(() => historyQuery.data?.history ?? [], [historyQuery.data?.history])
  const summary = historyQuery.data?.summary ?? null
  const chartData = useMemo(
    () =>
      history.toReversed().map((entry) => ({
        date: formatShortDate(entry.date),
        value: entry.selected_value,
        hit: entry.hit,
      })),
    [history],
  )
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

  return (
    <main className="workspace player-workspace">
      <section className="workspace-main">
        <div className="page-title-row">
          <div>
            <h1>Player</h1>
            <p>{selectedPlayerName || 'Search player history and implied prices'}</p>
          </div>
        </div>

        <Panel className="filters-panel">
          <div className="filter-grid">
            <Field label="Player search">
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
                    aria-expanded={searchOpen && playerOptions.length > 0}
                  />
                </div>
                {searchOpen && playerOptions.length > 0 && (
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
            </Field>
            <Field label="Stat">
              <Select value={filters.stat} onChange={(event) => setFilters({ ...filters, stat: event.currentTarget.value })}>
                {(filterOptions.data?.stats ?? [{ code: 'disposals', label: 'Disposals' }]).map((stat) => (
                  <option key={stat.code} value={stat.code}>{stat.label}</option>
                ))}
              </Select>
            </Field>
            <Field label="Line mode">
              <Select value={filters.lineMode} onChange={(event) => setFilters({ ...filters, lineMode: event.currentTarget.value as 'single' | 'interval' })}>
                <option value="single">Single line</option>
                <option value="interval">Interval</option>
              </Select>
            </Field>
            {filters.lineMode === 'single' ? (
              <Field label="Reference line">
                <TextInput value={filters.referenceLine} onChange={(event) => setFilters({ ...filters, referenceLine: event.currentTarget.value })} />
              </Field>
            ) : (
              <>
                <Field label="Lower bound">
                  <TextInput value={filters.lowerBound} onChange={(event) => setFilters({ ...filters, lowerBound: event.currentTarget.value })} />
                </Field>
                <Field label="Upper bound">
                  <TextInput value={filters.upperBound} onChange={(event) => setFilters({ ...filters, upperBound: event.currentTarget.value })} />
                </Field>
              </>
            )}
            <Field label="Last games">
              <TextInput value={filters.lastGames} onChange={(event) => setFilters({ ...filters, lastGames: event.currentTarget.value })} placeholder="All" />
            </Field>
            <Field label="Minutes min">
              <TextInput value={filters.minutesMinimum} onChange={(event) => setFilters({ ...filters, minutesMinimum: event.currentTarget.value })} />
            </Field>
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
                  </defs>
                  <CartesianGrid vertical={false} stroke="#dce7ec" />
                  <XAxis dataKey="date" tickLine={false} axisLine={false} minTickGap={24} />
                  <YAxis tickLine={false} axisLine={false} width={42} />
                  <Tooltip contentStyle={{ borderRadius: 8, border: '1px solid #dce7ec' }} />
                  <Area dataKey="value" stroke="#0f766e" strokeWidth={2} fill="url(#historyFill)" />
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
                  <SortableHistoryHeader label="TOG" columnKey="tog" sort={historySort} onSort={toggleHistorySort} />
                  <SortableHistoryHeader label="Value" columnKey="value" sort={historySort} onSort={toggleHistorySort} />
                  <SortableHistoryHeader label="Result" columnKey="hit" sort={historySort} onSort={toggleHistorySort} />
                </tr>
              </thead>
              <tbody>
                {sortedHistory.map((entry) => (
                  <tr key={`${entry.date}-${entry.game_number}`}>
                    <td>{formatDateTime(entry.date)}</td>
                    <td>{entry.round_label ?? '-'}</td>
                    <td>{entry.opposition ?? '-'}</td>
                    <td>{entry.venue ?? '-'}</td>
                    <td>{entry.tog == null ? '--' : `${entry.tog.toFixed(0)}%`}</td>
                    <td><b className="tabular">{entry.selected_value ?? '--'}</b></td>
                    <td><span className={entry.hit ? 'tag tag--good' : 'tag'}>{entry.hit == null ? 'n/a' : entry.hit ? 'Hit' : 'Miss'}</span></td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </Panel>
      </section>
    </main>
  )
}

type HistorySortKey = 'date' | 'round' | 'opposition' | 'venue' | 'tog' | 'value' | 'hit'

function SortableHistoryHeader({
  label,
  columnKey,
  sort,
  onSort,
}: {
  label: string
  columnKey: HistorySortKey
  sort: { key: HistorySortKey; direction: 'asc' | 'desc' }
  onSort: (key: HistorySortKey) => void
}) {
  const active = sort.key === columnKey
  return (
    <th aria-sort={active ? (sort.direction === 'asc' ? 'ascending' : 'descending') : 'none'}>
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
    case 'tog':
      return entry.tog ?? -1
    case 'value':
      return entry.selected_value ?? -1
    case 'hit':
      return entry.hit == null ? -1 : entry.hit ? 1 : 0
  }
}

function compareHistoryValue(left: string | number, right: string | number) {
  if (typeof left === 'string' || typeof right === 'string') return `${left}`.localeCompare(`${right}`)
  return left - right
}
