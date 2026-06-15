import { useMemo, useState } from 'react'
import { flexRender, getCoreRowModel, getSortedRowModel, useReactTable, type ColumnDef, type SortingState } from '@tanstack/react-table'
import { Plus, Search, X } from 'lucide-react'
import type { BookmakerSummary, EventSummary, OddsScope, OddsSearchResult } from '../api/types'
import { useOdds } from '../api/queries'
import { useClientSettings, useAppStore, defaultOddsFilters } from '../store/useAppStore'
import { formatDateTime, formatLine, formatPrice, formatSigned, marketLabel, playerPositionTag, selectionTypeLabel, shortMatchLabel } from '../lib/formatters'
import { combinedBasePrice, toDraftLeg } from '../lib/builder'
import { Button, Chip, EmptyState, ErrorBanner, Field, Panel, Segmented, Select, SortIcon, StatPill, TextInput, Toggle } from '../components/ui'

const playerMarkets = [
  [null, 'All props'],
  ['player_disposals', 'Disposals'],
  ['player_fantasy_points', 'Fantasy'],
  ['player_goals', 'Goals'],
  ['player_marks', 'Marks'],
  ['player_tackles', 'Tackles'],
  ['player_kicks', 'Kicks'],
  ['player_handballs', 'Handballs'],
  ['player_hitouts', 'Hitouts'],
  ['player_clearances', 'Clearances'],
] as const

const matchMarkets = [
  [null, 'All match markets'],
  ['h2h', 'H2H'],
  ['line', 'Line'],
  ['total_points', 'Totals'],
] as const

export function OddsWorkspace({
  bookmakers,
  events,
}: {
  bookmakers: BookmakerSummary[]
  events: EventSummary[]
}) {
  const settings = useClientSettings()
  const filters = useAppStore((state) => state.oddsFilters)
  const patchFilters = useAppStore((state) => state.patchOddsFilters)
  const setFilters = useAppStore((state) => state.setOddsFilters)
  const addSgmLeg = useAppStore((state) => state.addSgmLeg)
  const addCgmLeg = useAppStore((state) => state.addCgmLeg)
  const sgmLegs = useAppStore((state) => state.sgmLegs)
  const cgmLegs = useAppStore((state) => state.cgmLegs)
  const builderMode = useAppStore((state) => state.builderMode)
  const setBuilderMode = useAppStore((state) => state.setBuilderMode)
  const removeSgmLeg = useAppStore((state) => state.removeSgmLeg)
  const removeCgmLeg = useAppStore((state) => state.removeCgmLeg)
  const clearSgm = useAppStore((state) => state.clearSgm)
  const clearCgm = useAppStore((state) => state.clearCgm)
  const setActiveView = useAppStore((state) => state.setActiveView)
  const setSelectedPlayer = useAppStore((state) => state.setSelectedPlayer)
  const { data = [], isFetching, error } = useOdds(settings, filters, 150)
  const [query, setQuery] = useState('')
  const [sorting, setSorting] = useState<SortingState>([])

  const enabledBookmakers = useMemo(() => bookmakers.filter((bookmaker) => bookmaker.enabled), [bookmakers])
  const visibleRows = useMemo(() => {
    const normalized = query.trim().toLowerCase()
    if (!normalized) return data
    return data.filter((row) =>
      [row.player?.full_name, row.match_name, row.market_display_name, row.label, row.bookmaker]
        .filter(Boolean)
        .some((value) => value!.toLowerCase().includes(normalized)),
    )
  }, [data, query])

  const columns = useMemo<ColumnDef<OddsSearchResult>[]>(
    () => [
      {
        header: 'Player / match',
        accessorFn: (row) => row.player?.full_name ?? row.match_name,
        cell: ({ row }) => (
          <button
            type="button"
            className="link-cell"
            disabled={!row.original.player}
            onClick={() => row.original.player && setSelectedPlayer(row.original.player.id, row.original.player.full_name)}
          >
            <strong>{row.original.player?.full_name ?? shortMatchLabel(row.original.match_name)}</strong>
            <span>{row.original.player ? shortMatchLabel(row.original.match_name) : formatDateTime(row.original.start_time)}</span>
          </button>
        ),
      },
      {
        header: 'Market',
        accessorFn: (row) => `${marketLabel(row.market_type_code)} ${row.selection_type} ${row.line_value ?? ''}`,
        cell: ({ row }) => (
          <div className="table-stack">
            <b>{marketLabel(row.original.market_type_code)}</b>
            <span>{selectionTypeLabel(row.original.selection_type)} {formatLine(row.original.line_value)}</span>
          </div>
        ),
      },
      {
        header: 'Agency',
        accessorKey: 'bookmaker',
        cell: ({ row }) => <span className="agency">{row.original.bookmaker}</span>,
      },
      {
        header: 'Price',
        accessorKey: 'decimal_price',
        cell: ({ row }) => <b className="tabular">{formatPrice(row.original.decimal_price)}</b>,
      },
      {
        header: 'L10',
        accessorKey: 'diff_last_10',
        cell: ({ row }) => <Delta value={row.original.diff_last_10} />,
      },
      {
        header: 'Season',
        accessorKey: 'diff_2025',
        cell: ({ row }) => <Delta value={row.original.diff_2025} />,
      },
      {
        header: 'Next best',
        accessorKey: 'next_best_prob_diff',
        cell: ({ row }) => <Delta value={row.original.next_best_prob_diff} />,
      },
      {
        header: 'Context',
        accessorFn: (row) => `${row.matchup_difficulty ?? ''} ${row.player_position ?? ''} ${row.weather?.temperature_c ?? ''} ${row.is_best_price ? 'Best' : ''}`,
        cell: ({ row }) => (
          <div className="tag-row">
            {playerPositionTag(row.original.player_position) && <span className="tag">{playerPositionTag(row.original.player_position)}</span>}
            {row.original.matchup_difficulty && <span className="tag tag--amber">{row.original.matchup_difficulty}</span>}
            {row.original.weather?.temperature_c != null && <span className="tag">{Math.round(row.original.weather.temperature_c)} deg</span>}
            {row.original.is_best_price && <span className="tag tag--good">Best</span>}
          </div>
        ),
      },
      {
        header: '',
        id: 'actions',
        enableSorting: false,
        cell: ({ row }) => {
          const draftLeg = toDraftLeg(row.original)
          return (
            <div className="row-actions">
              <Button
                variant="secondary"
                disabled={!draftLeg || !row.original.sgm_eligible}
                onClick={() => draftLeg && addSgmLeg(draftLeg)}
              >
                <Plus size={14} /> SGM
              </Button>
              <Button variant="ghost" disabled={!draftLeg} onClick={() => draftLeg && addCgmLeg(draftLeg)}>
                CGM
              </Button>
            </div>
          )
        },
      },
    ],
    [addCgmLeg, addSgmLeg, setSelectedPlayer],
  )

  // TanStack Table intentionally returns function-bearing instances; this component does not pass it across memoized boundaries.
  // eslint-disable-next-line react-hooks/incompatible-library
  const table = useReactTable({
    data: visibleRows,
    columns,
    state: { sorting },
    onSortingChange: setSorting,
    getCoreRowModel: getCoreRowModel(),
    getSortedRowModel: getSortedRowModel(),
  })
  const positiveL10 = data.filter((row) => (row.diff_last_10 ?? -1) >= 0).length
  const bestPrices = data.filter((row) => row.is_best_price).length
  const sgmReady = data.filter((row) => row.sgm_eligible).length

  return (
    <main className="workspace odds-workspace" aria-label="Odds workspace">
      <section className="workspace-main">
        <div className="page-title-row">
          <div>
            <h1>Odds</h1>
            <p>{isFetching ? 'Refreshing market board' : `${visibleRows.length} rows in view`}</p>
          </div>
          <Segmented<OddsScope>
            value={filters.scope}
            ariaLabel="Odds scope"
            options={[
              { value: 'player', label: 'Player' },
              { value: 'match', label: 'Match' },
            ]}
            onChange={(scope) =>
              patchFilters({
                scope,
                marketTypeCode: null,
                selectionType: scope === 'player' ? filters.selectionType : null,
                sortBy: scope === 'player' ? 'diff_last_10' : 'start_time',
                sortDirection: scope === 'player' ? 'desc' : 'asc',
              })
            }
          />
        </div>

        <div className="summary-strip">
          <StatPill label="Positive L10" value={String(positiveL10)} tone="good" />
          <StatPill label="Best prices" value={String(bestPrices)} tone="warn" />
          <StatPill label="SGM ready" value={String(sgmReady)} tone="neutral" />
          <StatPill label="Loaded" value={String(data.length)} />
        </div>

        <Panel className="filters-panel">
          <div className="filter-grid">
            <Field label="Search board">
              <div className="input-with-icon">
                <Search size={16} />
                <TextInput value={query} onChange={(event) => setQuery(event.currentTarget.value)} placeholder="Player, market, match" />
              </div>
            </Field>
            <Field label="Market">
              <Select value={filters.marketTypeCode ?? ''} onChange={(event) => patchFilters({ marketTypeCode: event.currentTarget.value || null })}>
                {(filters.scope === 'player' ? playerMarkets : matchMarkets).map(([value, label]) => (
                  <option key={value ?? 'all'} value={value ?? ''}>{label}</option>
                ))}
              </Select>
            </Field>
            <Field label="Match">
              <Select value={filters.eventId ?? ''} onChange={(event) => patchFilters({ eventId: event.currentTarget.value ? Number(event.currentTarget.value) : null })}>
                <option value="">All matches</option>
                {events.map((event) => (
                  <option key={event.id} value={event.id}>{shortMatchLabel(event.match_name)}</option>
                ))}
              </Select>
            </Field>
            <Field label="Sort">
              <Select value={`${filters.sortBy}:${filters.sortDirection}`} onChange={(event) => {
                const [sortBy, sortDirection] = event.currentTarget.value.split(':') as [string, 'asc' | 'desc']
                patchFilters({ sortBy, sortDirection })
              }}>
                <option value="diff_last_10:desc">Last-10 diff</option>
                <option value="next_best_prob_diff:desc">Next best diff</option>
                <option value="diff_2025:desc">Season diff</option>
                <option value="price:desc">Price high-low</option>
                <option value="price:asc">Price low-high</option>
                <option value="player:asc">Player A-Z</option>
                <option value="start_time:asc">Start time</option>
              </Select>
            </Field>
          </div>
          <div className="chip-row">
            {enabledBookmakers.map((bookmaker) => (
              <Chip
                key={bookmaker.code}
                active={filters.bookmakerCodes.length === 0 || filters.bookmakerCodes.includes(bookmaker.code)}
                onClick={() => {
                  const selected = new Set(filters.bookmakerCodes.length ? filters.bookmakerCodes : enabledBookmakers.map((item) => item.code))
                  if (selected.has(bookmaker.code)) selected.delete(bookmaker.code)
                  else selected.add(bookmaker.code)
                  patchFilters({ bookmakerCodes: Array.from(selected) })
                }}
              >
                {bookmaker.display_name}
              </Chip>
            ))}
          </div>
          <div className="quick-filter-grid">
            <Chip active={filters.minDiffLast10 >= 0} onClick={() => patchFilters({ minDiffLast10: filters.minDiffLast10 >= 0 ? -1 : 0 })}>
              Positive L10
            </Chip>
            <Chip active={filters.minNextBestProbDiff >= 0} onClick={() => patchFilters({ minNextBestProbDiff: filters.minNextBestProbDiff >= 0 ? -1 : 0 })}>
              Positive next best
            </Chip>
            <Chip
              active={filters.matchupDifficulties.length > 0}
              onClick={() =>
                patchFilters({
                  matchupDifficulties: filters.matchupDifficulties.length > 0 ? [] : ['Neutral', 'Good', 'Excellent'],
                })
              }
            >
              Favorable matchup
            </Chip>
            <Toggle checked={filters.bestOnly} onChange={(bestOnly) => patchFilters({ bestOnly })} label="Best only" />
            <Toggle checked={filters.sgmOnly} onChange={(sgmOnly) => patchFilters({ sgmOnly })} label="SGM only" />
            <Button variant="ghost" onClick={() => setFilters({ ...defaultOddsFilters, bookmakerCodes: enabledBookmakers.map((item) => item.code) })}>Reset</Button>
          </div>
        </Panel>

        {error ? <ErrorBanner message={error instanceof Error ? error.message : 'Failed to load odds.'} /> : null}

        <Panel className="table-panel">
          {visibleRows.length === 0 && !isFetching ? (
            <EmptyState title="No odds" body="Adjust market, agency, match, or quick filters." />
          ) : (
            <div className="data-table-wrap">
              <table className="data-table">
                <caption className="visually-hidden">Odds market board</caption>
                <thead>
                  {table.getHeaderGroups().map((headerGroup) => (
                    <tr key={headerGroup.id}>
                      {headerGroup.headers.map((header) => (
                        <th key={header.id} aria-sort={header.column.getIsSorted() === 'asc' ? 'ascending' : header.column.getIsSorted() === 'desc' ? 'descending' : 'none'}>
                          {header.column.getCanSort() ? (
                            <button type="button" className="sort-header" onClick={header.column.getToggleSortingHandler()}>
                              <span>{flexRender(header.column.columnDef.header, header.getContext())}</span>
                              <SortIcon state={header.column.getIsSorted()} />
                            </button>
                          ) : (
                            <span>{flexRender(header.column.columnDef.header, header.getContext())}</span>
                          )}
                        </th>
                      ))}
                    </tr>
                  ))}
                </thead>
                <tbody>
                  {table.getRowModel().rows.map((row) => (
                    <tr key={`${row.original.selection_id}-${row.original.bookmaker}`}>
                      {row.getVisibleCells().map((cell) => (
                        <td key={cell.id}>{flexRender(cell.column.columnDef.cell, cell.getContext())}</td>
                      ))}
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}
        </Panel>
      </section>
      <aside className="odds-builder-rail">
        <div className="builder-panel-head">
          <div>
            <h2>{builderMode.toUpperCase()} draft</h2>
            <span>{(builderMode === 'sgm' ? sgmLegs : cgmLegs).length} legs selected</span>
          </div>
          <Segmented
            value={builderMode}
            ariaLabel="Builder mode"
            options={[
              { value: 'sgm', label: 'SGM' },
              { value: 'cgm', label: 'CGM' },
            ]}
            onChange={setBuilderMode}
          />
        </div>
        <DraftRail
          legs={builderMode === 'sgm' ? sgmLegs : cgmLegs}
          onRemove={builderMode === 'sgm' ? removeSgmLeg : removeCgmLeg}
          onClear={builderMode === 'sgm' ? clearSgm : clearCgm}
        />
        <Button
          className="compare-button"
          disabled={(builderMode === 'sgm' ? sgmLegs : cgmLegs).length < 2}
          onClick={() => setActiveView(builderMode)}
        >
          Open {builderMode.toUpperCase()} builder
        </Button>
      </aside>
    </main>
  )
}

function Delta({ value }: { value: number | null }) {
  return <span className={value == null ? 'delta' : value >= 0 ? 'delta delta--good' : 'delta delta--bad'}>{formatSigned(value)}</span>
}

function DraftRail({
  legs,
  onRemove,
  onClear,
}: {
  legs: ReturnType<typeof useAppStore.getState>['sgmLegs']
  onRemove: (selectionId: number) => void
  onClear: () => void
}) {
  return (
    <>
      <div className="builder-stats">
        <StatPill label="Base price" value={formatPrice(combinedBasePrice(legs))} tone="warn" />
        <StatPill label="Status" value={legs.length >= 2 ? 'Ready' : '2+ legs'} tone={legs.length >= 2 ? 'good' : 'neutral'} />
      </div>
      <div className="draft-list">
        {legs.length === 0 ? (
          <EmptyState title="No draft legs" body="Use SGM or CGM actions in the odds table." />
        ) : (
          legs.map((leg) => (
            <div className="draft-leg" key={leg.selection_id}>
              <div>
                <strong>{leg.label}</strong>
                <span>{marketLabel(leg.market_type_code)} | {shortMatchLabel(leg.event_label)}</span>
              </div>
              <b>{formatPrice(leg.base_price)}</b>
              <button type="button" onClick={() => onRemove(leg.selection_id)} aria-label="Remove leg"><X size={15} /></button>
            </div>
          ))
        )}
      </div>
      <Button variant="ghost" onClick={onClear} disabled={legs.length === 0}>Clear draft</Button>
    </>
  )
}
