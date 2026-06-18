import { useEffect, useMemo, useState } from 'react'
import { flexRender, getCoreRowModel, getSortedRowModel, useReactTable, type ColumnDef, type SortingState } from '@tanstack/react-table'
import { Plus, ReceiptText, Search, X } from 'lucide-react'
import type { BookmakerSummary, EventSummary, OddsScope, OddsSearchResult } from '../api/types'
import { useOdds } from '../api/queries'
import { useClientSettings, useAppStore, defaultOddsFilters, defaultPlayerFilters } from '../store/useAppStore'
import { formatDateTime, formatLine, formatPrice, formatSigned, marketLabel, playerPositionTag, selectionTypeLabel, shortMatchLabel } from '../lib/formatters'
import { combinedBasePrice, toDraftLeg, marketTypeToStatCode } from '../lib/builder'
import { Button, Chip, EmptyState, ErrorBanner, Field, Panel, Segmented, Select, SortIcon, TextInput, Toggle } from '../components/ui'
import { CandidateContextMenu, AgencyPriceDialog } from './BuilderWorkspace'

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
  const setPlayerFilters = useAppStore((state) => state.setPlayerFilters)
  const [contextMenu, setContextMenu] = useState<{ x: number; y: number; selection: OddsSearchResult } | null>(null)
  const [priceDialogSelection, setPriceDialogSelection] = useState<OddsSearchResult | null>(null)

  const openContextMenu = (event: React.MouseEvent, selection: OddsSearchResult) => {
    event.preventDefault()
    setContextMenu({ x: event.clientX, y: event.clientY, selection })
  }

  const viewPlayerStat = (selection: OddsSearchResult) => {
    if (!selection.player) return
    const stat = marketTypeToStatCode(selection.market_type_code)
    setPlayerFilters({
      ...defaultPlayerFilters,
      stat: stat ?? defaultPlayerFilters.stat,
      lineMode: 'single',
      referenceLine: selection.line_value != null ? formatLine(selection.line_value) : defaultPlayerFilters.referenceLine,
    })
    setSelectedPlayer(selection.player.id, selection.player.full_name)
  }

  useEffect(() => {
    if (!contextMenu) return
    const close = () => setContextMenu(null)
    window.addEventListener('click', close)
    return () => window.removeEventListener('click', close)
  }, [contextMenu])

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
        id: 'name',
        header: 'Player / match',
        accessorFn: (row) => row.player?.full_name ?? row.match_name,
        cell: ({ row }) => (
          <div className="table-stack">
            <strong>{row.original.player?.full_name ?? shortMatchLabel(row.original.match_name)}</strong>
            <span>{row.original.player ? shortMatchLabel(row.original.match_name) : formatDateTime(row.original.start_time)}</span>
          </div>
        ),
      },
      {
        id: 'market',
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
        id: 'agency',
        header: 'Agency',
        accessorKey: 'bookmaker',
        cell: ({ row }) => <span className="agency">{row.original.bookmaker}</span>,
      },
      {
        id: 'price',
        header: 'Price',
        accessorKey: 'decimal_price',
        cell: ({ row }) => (
          <span className="price-cell">
            <b className="tabular">{formatPrice(row.original.decimal_price)}</b>
            {row.original.is_best_price ? <span className="best-price-badge">Best</span> : null}
          </span>
        ),
      },
      {
        id: 'next-best',
        header: 'Next best',
        accessorKey: 'next_best_prob_diff',
        cell: ({ row }) => <Delta value={row.original.next_best_prob_diff} />,
      },
      {
        id: 'l10',
        header: 'L10',
        accessorKey: 'diff_last_10',
        cell: ({ row }) => <Delta value={row.original.diff_last_10} />,
      },
      {
        id: 'season',
        header: 'Season',
        accessorKey: 'diff_2025',
        cell: ({ row }) => <Delta value={row.original.diff_2025} />,
      },
      {
        id: 'home-away',
        header: 'H/A',
        accessorKey: 'home_away_diff',
        cell: ({ row }) => <Delta value={row.original.home_away_diff} />,
      },
      {
        id: 'win-loss',
        header: 'W/L',
        accessorKey: 'win_loss_diff',
        cell: ({ row }) => <Delta value={row.original.win_loss_diff} />,
      },
      {
        id: 'context',
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
                className="odds-action-button odds-action-button--sgm"
                disabled={!draftLeg || !row.original.sgm_eligible}
                onClick={() => draftLeg && addSgmLeg(draftLeg)}
              >
                <Plus size={14} /> SGM
              </Button>
              <Button className="odds-action-button" variant="ghost" disabled={!draftLeg} onClick={() => draftLeg && addCgmLeg(draftLeg)}>
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
  const activeFilterCount =
    (query.trim() ? 1 : 0) +
    (filters.marketTypeCode ? 1 : 0) +
    (filters.eventId ? 1 : 0) +
    (filters.bookmakerCodes.length > 0 ? 1 : 0) +
    (filters.minDiffLast10 >= 0 ? 1 : 0) +
    (filters.minNextBestProbDiff >= 0 ? 1 : 0) +
    (filters.matchupDifficulties.length > 0 ? 1 : 0) +
    (filters.bestOnly ? 1 : 0) +
    (filters.sgmOnly ? 1 : 0)
  const subtitle = `${visibleRows.length} of ${data.length} ${filters.scope === 'player' ? 'player prop' : 'match'} selections`
  const summaryStats = [
    { label: 'Positive L10 edge', value: String(positiveL10), tone: 'good' },
    { label: 'Best prices', value: String(bestPrices), tone: 'primary' },
    { label: filters.scope === 'player' ? 'SGM eligible' : 'Match selections', value: String(filters.scope === 'player' ? sgmReady : data.length), tone: 'indigo' },
    { label: 'In view', value: String(visibleRows.length), tone: 'neutral' },
  ]

  return (
    <main className="workspace odds-workspace" aria-label="Odds workspace">
      <section className="workspace-main">
        <div className="page-title-row">
          <div>
            <h1>Odds</h1>
            <p>{isFetching ? 'Refreshing market board' : subtitle}</p>
          </div>
          <Segmented<OddsScope>
            className="odds-scope-tabs"
            value={filters.scope}
            ariaLabel="Odds scope"
            options={[
              { value: 'player', label: 'Player props' },
              { value: 'match', label: 'Match markets' },
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

        <div className="odds-summary-grid">
          {summaryStats.map((stat) => (
            <div className={`odds-summary-card odds-summary-card--${stat.tone}`} key={stat.label}>
              <span aria-hidden="true" />
              <div>
                <strong>{stat.value}</strong>
                <small>{stat.label}</small>
              </div>
            </div>
          ))}
        </div>

        <Panel className="filters-panel">
          <div className="filter-grid">
            <Field label="Search board">
              <div className="input-with-icon">
                <Search size={16} />
                <TextInput value={query} onChange={(event) => setQuery(event.currentTarget.value)} placeholder="Player, market, agency, match" />
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
            <span className="odds-chip-label">Agency</span>
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
            <span className="odds-chip-label">Signal</span>
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
            {activeFilterCount > 0 ? (
              <Button variant="ghost" onClick={() => {
                setQuery('')
                setFilters({ ...defaultOddsFilters, bookmakerCodes: enabledBookmakers.map((item) => item.code) })
              }}>Reset</Button>
            ) : null}
          </div>
        </Panel>

        {error ? <ErrorBanner message={error instanceof Error ? error.message : 'Failed to load odds.'} /> : null}

        <Panel className="table-panel">
          {visibleRows.length === 0 && !isFetching ? (
            <EmptyState title="No odds" body="Adjust market, agency, match, or quick filters." />
          ) : (
            <div className="data-table-wrap odds-table-wrap">
              <table className="data-table odds-board-table">
                <caption className="visually-hidden">Odds market board</caption>
                <thead>
                  {table.getHeaderGroups().map((headerGroup) => (
                    <tr key={headerGroup.id}>
                      {headerGroup.headers.map((header) => (
                        <th
                          key={header.id}
                          className={columnClassName(header.column.id)}
                          aria-sort={header.column.getIsSorted() === 'asc' ? 'ascending' : header.column.getIsSorted() === 'desc' ? 'descending' : 'none'}
                        >
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
                    <tr
                      key={`${row.original.selection_id}-${row.original.bookmaker}`}
                      onContextMenu={(event) => openContextMenu(event, row.original)}
                    >
                      {row.getVisibleCells().map((cell) => (
                        <td
                          key={cell.id}
                          className={[
                            columnClassName(cell.column.id),
                            cell.column.id === 'price' && row.original.is_best_price ? 'is-best-price-cell' : '',
                          ].filter(Boolean).join(' ') || undefined}
                        >
                          {flexRender(cell.column.columnDef.cell, cell.getContext())}
                        </td>
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

      {contextMenu ? (
        <CandidateContextMenu
          x={contextMenu.x}
          y={contextMenu.y}
          selection={contextMenu.selection}
          onViewPlayer={() => {
            viewPlayerStat(contextMenu.selection)
            setContextMenu(null)
          }}
          onCompareAgencies={() => {
            setPriceDialogSelection(contextMenu.selection)
            setContextMenu(null)
          }}
        />
      ) : null}

      {priceDialogSelection ? (
        <AgencyPriceDialog
          settings={settings}
          selection={priceDialogSelection}
          onClose={() => setPriceDialogSelection(null)}
        />
      ) : null}
    </main>
  )
}

function Delta({ value }: { value: number | null }) {
  return <span className={value == null ? 'delta' : value >= 0 ? 'delta delta--good' : 'delta delta--bad'}>{formatSigned(value)}</span>
}

function columnClassName(columnId: string) {
  if (columnId === 'name') return 'is-sticky-column'
  if (columnId === 'context') return 'is-context-column'
  if (['price', 'next-best', 'l10', 'season', 'home-away', 'win-loss', 'actions'].includes(columnId)) return 'is-numeric-column'
  return undefined
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
  const combinedPrice = combinedBasePrice(legs)
  const ready = legs.length >= 2

  return (
    <>
      <div className="odds-builder-stats">
        <div>
          <span>Combined</span>
          <strong>{formatPrice(combinedPrice)}</strong>
        </div>
        <div>
          <span>Status</span>
          <strong className={ready ? 'is-ready' : undefined}>{ready ? 'Ready to price' : legs.length === 0 ? 'Empty' : `Add ${2 - legs.length} more`}</strong>
        </div>
      </div>
      <div className="draft-list">
        {legs.length === 0 ? (
          <div className="odds-empty-draft">
            <div aria-hidden="true"><ReceiptText size={20} /></div>
            <strong>No legs yet</strong>
            <span>Use the <b>SGM</b> or <b>CGM</b> action on any market to start a draft.</span>
          </div>
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
