import { useMemo, useRef, useState } from 'react'
import { flexRender, getCoreRowModel, getSortedRowModel, useReactTable, type ColumnDef, type SortingState } from '@tanstack/react-table'
import { Calculator, Search } from 'lucide-react'
import clsx from 'clsx'
import type { ArbFilters, ArbSearchResult, BookmakerSummary } from '../api/types'
import { useArbs } from '../api/queries'
import { AdaptiveRail, Button, Chip, EmptyState, ErrorBanner, Field, Panel, Segmented, Select, SortIcon, TextInput } from '../components/ui'
import { bookmakerLabel, formatLine, formatPrice, marketLabel, shortMatchLabel } from '../lib/formatters'
import { defaultArbFilters, useAppStore, useClientSettings } from '../store/useAppStore'

type ArbScope = 'all' | 'player' | 'match'
type ArbSortValue = 'margin:desc' | 'margin:asc' | 'implied_probability_sum:asc' | 'over_price:desc' | 'selection:asc'

const scopeOptions: { value: ArbScope; label: string }[] = [
  { value: 'all', label: 'All opps' },
  { value: 'player', label: 'Player props' },
  { value: 'match', label: 'Match markets' },
]

const sortOptions: { value: ArbSortValue; label: string }[] = [
  { value: 'margin:desc', label: 'Margin high to low' },
  { value: 'margin:asc', label: 'Margin low to high' },
  { value: 'implied_probability_sum:asc', label: 'Implied sum low to high' },
  { value: 'over_price:desc', label: 'Over price high to low' },
  { value: 'selection:asc', label: 'Selection A to Z' },
]

export function ArbWorkspace({ bookmakers }: { bookmakers: BookmakerSummary[] }) {
  const settings = useClientSettings()
  const filters = useAppStore((state) => state.arbFilters)
  const patchFilters = useAppStore((state) => state.patchArbFilters)
  const setFilters = useAppStore((state) => state.setArbFilters)
  const { data = [], isFetching, error } = useArbs(settings, filters, 500)
  const [scope, setScope] = useState<ArbScope>('all')
  const [selectedArbId, setSelectedArbId] = useState<string | null>(null)
  const [stake, setStake] = useState('100')
  const [sorting, setSorting] = useState<SortingState>([{ id: 'margin', desc: true }])
  const [railOpen, setRailOpen] = useState(false)
  const [mobileFiltersOpen, setMobileFiltersOpen] = useState(false)
  const railTriggerRef = useRef<HTMLButtonElement>(null)

  const scopedRows = useMemo(() => {
    if (scope === 'all') return data
    return data.filter((row) => (scope === 'player' ? isPlayerArb(row) : !isPlayerArb(row)))
  }, [data, scope])

  const agencies = useMemo(() => {
    const names = new Map<string, string>()
    bookmakers.filter((bookmaker) => bookmaker.enabled).forEach((bookmaker) => {
      names.set(bookmaker.display_name.toLowerCase(), bookmaker.display_name)
    })
    data.forEach((row) => {
      names.set(row.over_agency.toLowerCase(), bookmakerLabel(row.over_agency))
      names.set(row.under_agency.toLowerCase(), bookmakerLabel(row.under_agency))
    })
    return Array.from(names.values()).toSorted((left, right) => left.localeCompare(right))
  }, [bookmakers, data])

  const marketOptions = useMemo(() => {
    const names = new Set<string>(filters.markets)
    data.forEach((row) => names.add(row.market_name))
    return Array.from(names).filter(Boolean).toSorted((left, right) => left.localeCompare(right))
  }, [data, filters.markets])

  const trueArbs = scopedRows.filter((row) => row.margin >= 0).length
  const nearArbs = scopedRows.length - trueArbs
  const bestMargin = scopedRows.length > 0 ? Math.max(...scopedRows.map((row) => row.margin)) : null
  const avgArbMargin = trueArbs > 0 ? scopedRows.filter((row) => row.margin >= 0).reduce((total, row) => total + row.margin, 0) / trueArbs : null
  const trueArbsOnly = Number.parseFloat(filters.minMargin) >= 0
  const selectedArb = selectedArbId ? data.find((row) => row.id === selectedArbId) ?? null : null

  const columns = useMemo<ColumnDef<ArbSearchResult>[]>(
    () => [
      {
        id: 'selection',
        header: 'Selection',
        accessorFn: (row) => `${selectionTitle(row)} ${row.match_name}`,
        cell: ({ row }) => {
          const original = row.original
          return (
            <div className="arb-selection-cell">
              <span className={clsx('arb-status-badge', original.margin >= 0 ? 'arb-status-badge--arb' : 'arb-status-badge--near')}>
                {original.margin >= 0 ? 'Arb' : 'Near'}
              </span>
              <div className="table-stack">
                <b>{selectionTitle(original)}</b>
                <span>{selectionSubtitle(original)}</span>
              </div>
            </div>
          )
        },
      },
      {
        header: 'Market',
        accessorFn: (row) => `${row.market_name} ${row.over_line ?? ''} ${row.under_line ?? ''}`,
        cell: ({ row }) => (
          <div className="table-stack">
            <b>{row.original.market_name}</b>
            <span>{lineLabel(row.original)}</span>
          </div>
        ),
      },
      {
        header: 'Over',
        accessorKey: 'over_price',
        cell: ({ row }) => <PriceAgency price={row.original.over_price} agency={row.original.over_agency} tone="over" />,
      },
      {
        header: 'Under',
        accessorKey: 'under_price',
        cell: ({ row }) => <PriceAgency price={row.original.under_price} agency={row.original.under_agency} tone="under" />,
      },
      {
        header: 'Imp sum',
        accessorKey: 'implied_probability_sum',
        cell: ({ row }) => <span className="tabular">{(row.original.implied_probability_sum * 100).toFixed(2)}%</span>,
      },
      {
        header: 'Margin',
        accessorKey: 'margin',
        cell: ({ row }) => (
          <b className={clsx('arb-margin', row.original.margin >= 0 ? 'arb-margin--positive' : 'arb-margin--near')}>
            {formatMargin(row.original.margin)}
          </b>
        ),
      },
      {
        header: 'Teams',
        accessorFn: (row) => `${row.player_team ?? ''} ${row.opposition_team ?? ''}`,
        cell: ({ row }) => (
          <div className="tag-row arb-team-tags">
            {row.original.player_team ? <span className="tag">{teamCode(row.original.player_team)}</span> : null}
            {row.original.opposition_team ? <span className="tag tag--amber">v {teamCode(row.original.opposition_team)}</span> : null}
          </div>
        ),
      },
      {
        id: 'actions',
        header: '',
        enableSorting: false,
        cell: ({ row }) => (
          <Button
            variant="secondary"
            className="arb-stake-button"
            onClick={(event) => {
              event.stopPropagation()
              setSelectedArbId(row.original.id)
              setRailOpen(true)
            }}
          >
            <Calculator size={13} /> Stake
          </Button>
        ),
      },
    ],
    [],
  )

  // TanStack Table intentionally returns function-bearing instances; this component does not pass it across memoized boundaries.
  // eslint-disable-next-line react-hooks/incompatible-library
  const table = useReactTable({
    data: scopedRows,
    columns,
    state: { sorting },
    onSortingChange: setSorting,
    getCoreRowModel: getCoreRowModel(),
    getSortedRowModel: getSortedRowModel(),
  })

  const sortValue = sortingToSelectValue(sorting)
  const activeFilters = countActiveFilters(filters, scope)

  return (
    <main className="workspace arb-workspace" aria-label="Arb finder">
      <section className="workspace-main">
        <div className="page-title-row">
          <div>
            <h1>Arbs</h1>
            <p>{isFetching ? 'Refreshing arb board' : `${table.getRowModel().rows.length} of ${scopedRows.length} opportunities in view`}</p>
          </div>
          <div className="page-actions arb-page-actions">
            <Segmented
              value={scope}
              options={scopeOptions}
              onChange={(nextScope) => {
                setScope(nextScope)
                setSelectedArbId(null)
                setRailOpen(false)
              }}
              ariaLabel="Arb scope"
              className="arb-scope-tabs"
            />
            <button ref={railTriggerRef} type="button" className="button button--secondary adaptive-rail-trigger" onClick={() => setRailOpen(true)}>
              <Calculator size={15} /> Calculator
            </button>
          </div>
        </div>

        <div className="arb-summary-grid">
          <SummaryCard label="True arbs" value={String(trueArbs)} tone="good" />
          <SummaryCard label="Near arbs" value={String(nearArbs)} tone="warn" />
          <SummaryCard label="Best margin" value={bestMargin == null ? '--' : formatMargin(bestMargin)} tone="primary" />
          <SummaryCard label="Avg arb margin" value={avgArbMargin == null ? '--' : formatMargin(avgArbMargin)} tone="indigo" />
        </div>

        <Panel className={clsx('filters-panel responsive-filters', mobileFiltersOpen && 'mobile-filters-open')}>
          <button type="button" className="button button--secondary mobile-filter-toggle" aria-expanded={mobileFiltersOpen} onClick={() => setMobileFiltersOpen((open) => !open)}>
            Filters ({activeFilters})
          </button>
          <div className="filter-grid arb-filter-grid">
            <Field label="Search" className="mobile-filter-primary">
              <div className="input-with-icon">
                <Search size={16} />
                <TextInput value={filters.query} onChange={(event) => patchFilters({ query: event.currentTarget.value })} placeholder="Player, match, market, agency" />
              </div>
            </Field>
            <Field label="Market" className="mobile-filter-secondary">
              <Select value={filters.markets[0] ?? ''} onChange={(event) => patchFilters({ markets: event.currentTarget.value ? [event.currentTarget.value] : [] })}>
                <option value="">All markets</option>
                {marketOptions.map((market) => (
                  <option key={market} value={market}>{marketLabel(marketCode(market))}</option>
                ))}
              </Select>
            </Field>
            <Field label="Min margin" className="mobile-filter-secondary">
              <div className="arb-percent-input">
                <TextInput inputMode="decimal" value={filters.minMargin} onChange={(event) => patchFilters({ minMargin: event.currentTarget.value })} />
                <span>%</span>
              </div>
            </Field>
            <Field label="Max margin" className="mobile-filter-secondary">
              <div className="arb-percent-input">
                <TextInput inputMode="decimal" value={filters.maxMargin} onChange={(event) => patchFilters({ maxMargin: event.currentTarget.value })} placeholder="No cap" />
                <span>%</span>
              </div>
            </Field>
            <Field label="Sort" className="mobile-filter-primary">
              <Select value={sortValue} onChange={(event) => setSorting(selectValueToSorting(event.currentTarget.value as ArbSortValue))}>
                {sortOptions.map((option) => (
                  <option key={option.value} value={option.value}>{option.label}</option>
                ))}
              </Select>
            </Field>
          </div>
          <div className="chip-row mobile-filter-secondary">
            <span className="odds-chip-label">Agency</span>
            {agencies.map((agency) => (
              <Chip
                key={agency}
                active={filters.agencies.length === 0 || filters.agencies.some((item) => item.toLowerCase() === agency.toLowerCase())}
                onClick={() => patchFilters({ agencies: toggleFilter(filters.agencies, agency) })}
              >
                {agency}
              </Chip>
            ))}
          </div>
          <div className="quick-filter-grid arb-signal-row mobile-filter-secondary">
            <span className="odds-chip-label">Signal</span>
            <Chip
              active={trueArbsOnly}
              onClick={() => patchFilters({ minMargin: trueArbsOnly ? defaultArbFilters.minMargin : '0' })}
            >
              <span className="arb-chip-dot" /> True arbs only
            </Chip>
            {activeFilters > 0 ? (
              <Button variant="ghost" className="arb-reset-button" onClick={() => {
                setScope('all')
                setSelectedArbId(null)
                setFilters(defaultArbFilters)
              }}>
                Reset filters
              </Button>
            ) : null}
          </div>
        </Panel>

        {error ? <ErrorBanner message={error instanceof Error ? error.message : 'Failed to load arbs.'} /> : null}

        <Panel className="table-panel">
          {scopedRows.length === 0 && !isFetching ? (
            <EmptyState title="No arbs in range" body="Lower the minimum margin, clear agency filters, or broaden the market." />
          ) : (
            <div className="data-table-wrap arb-table-wrap">
              <table className="data-table arb-board-table">
                <caption className="visually-hidden">Arbitrage opportunities</caption>
                <thead>
                  {table.getHeaderGroups().map((headerGroup) => (
                    <tr key={headerGroup.id}>
                      {headerGroup.headers.map((header) => (
                        <th
                          key={header.id}
                          className={arbColumnClassName(header.column.id)}
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
                      key={row.original.id}
                      className={clsx(row.original.margin >= 0 && 'arb-row--positive', row.original.id === selectedArbId && 'is-selected')}
                      onClick={() => {
                        setSelectedArbId(row.original.id)
                        setRailOpen(true)
                      }}
                    >
                      {row.getVisibleCells().map((cell) => (
                        <td key={cell.id} className={arbColumnClassName(cell.column.id)}>
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

      <StakeCalculatorRail
        selected={selectedArb}
        stake={stake}
        setStake={setStake}
        clearSelection={() => setSelectedArbId(null)}
        open={railOpen}
        onClose={() => setRailOpen(false)}
        triggerRef={railTriggerRef}
      />
    </main>
  )
}

function SummaryCard({ label, value, tone }: { label: string; value: string; tone: 'good' | 'warn' | 'primary' | 'indigo' }) {
  return (
    <div className={clsx('arb-summary-card', `arb-summary-card--${tone}`)}>
      <span />
      <div>
        <strong>{value}</strong>
        <small>{label}</small>
      </div>
    </div>
  )
}

function StakeCalculatorRail({
  selected,
  stake,
  setStake,
  clearSelection,
  open,
  onClose,
  triggerRef,
}: {
  selected: ArbSearchResult | null
  stake: string
  setStake: (value: string) => void
  clearSelection: () => void
  open: boolean
  onClose: () => void
  triggerRef: React.RefObject<HTMLButtonElement | null>
}) {
  const stakeValue = Number.parseFloat(stake)
  const stakeNumber = Number.isFinite(stakeValue) ? stakeValue : 0
  const calc = selected ? calculateStakeSplit(selected, stakeNumber) : null

  return (
    <AdaptiveRail open={open} onClose={onClose} label="Stake calculator" className="arb-calculator-rail" triggerRef={triggerRef}>
      <div className="builder-panel-head">
        <div>
          <h2>Stake calculator</h2>
          <span>{selected ? selectionTitle(selected) : 'No arb selected'}</span>
        </div>
        {selected ? (
          <span className={clsx('arb-status-badge', selected.margin >= 0 ? 'arb-status-badge--arb' : 'arb-status-badge--near')}>
            {selected.margin >= 0 ? 'Arb' : 'Near'}
          </span>
        ) : null}
      </div>

      {!selected || !calc ? (
        <div className="arb-empty-calc">
          <div><Calculator size={20} /></div>
          <strong>Pick an arb to size it</strong>
          <span>Select any row, or hit Stake, and the split will appear here.</span>
        </div>
      ) : (
        <div className="arb-calculator-content">
          <div className="arb-selected-card">
            <b>{selectionTitle(selected)}</b>
            <span>{selected.market_name} · {lineLabel(selected)} · {shortMatchLabel(selected.match_name)}</span>
          </div>

          <Field label="Total stake">
            <TextInput className="arb-stake-input" inputMode="decimal" value={stake} onChange={(event) => setStake(event.currentTarget.value)} />
          </Field>
          <div className="arb-stake-presets">
            {['50', '100', '250', '500'].map((value) => (
              <button key={value} type="button" className={clsx(stake === value && 'is-active')} onClick={() => setStake(value)}>
                ${value}
              </button>
            ))}
          </div>

          <div className="arb-leg-card arb-leg-card--over">
            <span>Over leg</span>
            <b>{formatCurrency(calc.overStake)}</b>
            <small>{bookmakerLabel(selected.over_agency)} @ {formatPrice(selected.over_price)}</small>
          </div>
          <div className="arb-leg-card arb-leg-card--under">
            <span>Under leg</span>
            <b>{formatCurrency(calc.underStake)}</b>
            <small>{bookmakerLabel(selected.under_agency)} @ {formatPrice(selected.under_price)}</small>
          </div>

          <div className={clsx('arb-profit-panel', calc.profit >= 0 ? 'arb-profit-panel--positive' : 'arb-profit-panel--near')}>
            <span>{calc.profit >= 0 ? 'Guaranteed profit' : 'Worst-case loss'}</span>
            <strong>{calc.profit >= 0 ? '+' : '-'}{formatCurrency(Math.abs(calc.profit))}</strong>
            <small>Return {formatCurrency(calc.returnValue)} · ROI {calc.roi >= 0 ? '+' : ''}{calc.roi.toFixed(2)}%</small>
          </div>

          <div className="arb-calculator-actions">
            <Button variant="ghost" onClick={clearSelection}>Clear</Button>
          </div>
        </div>
      )}
    </AdaptiveRail>
  )
}

function PriceAgency({ price, agency, tone }: { price: number; agency: string; tone: 'over' | 'under' }) {
  return (
    <div className="table-stack arb-price-agency">
      <b className="tabular">{formatPrice(price)}</b>
      <span className={clsx('agency', tone === 'over' ? 'agency--over' : 'agency--under')}>{bookmakerLabel(agency)}</span>
    </div>
  )
}

function calculateStakeSplit(row: ArbSearchResult, stake: number) {
  const impliedSum = row.implied_probability_sum || 1 / row.over_price + 1 / row.under_price
  const overStake = stake * (1 / row.over_price) / impliedSum
  const underStake = stake * (1 / row.under_price) / impliedSum
  const returnValue = impliedSum > 0 ? stake / impliedSum : 0
  const profit = returnValue - stake
  const roi = stake > 0 ? (profit / stake) * 100 : 0
  return { overStake, underStake, returnValue, profit, roi }
}

function selectionTitle(row: ArbSearchResult) {
  return isPlayerArb(row) ? row.player_name : shortMatchLabel(row.match_name)
}

function selectionSubtitle(row: ArbSearchResult) {
  if (!isPlayerArb(row)) return row.market_name
  const team = row.player_team ? teamCode(row.player_team) : ''
  const opponent = row.opposition_team ? `v ${teamCode(row.opposition_team)}` : ''
  return [team, opponent, shortMatchLabel(row.match_name)].filter(Boolean).join(' · ')
}

function lineLabel(row: ArbSearchResult) {
  return row.over_line === row.under_line
    ? `O/U ${formatLine(row.over_line)}`
    : `O ${formatLine(row.over_line)} / U ${formatLine(row.under_line)}`
}

function isPlayerArb(row: ArbSearchResult) {
  return (row.player_name ?? '').trim().length > 0
}

function formatMargin(value: number) {
  return `${value >= 0 ? '+' : ''}${value.toFixed(2)}%`
}

function formatCurrency(value: number) {
  if (!Number.isFinite(value)) return '$0.00'
  return `$${value.toFixed(2)}`
}

function toggleFilter(values: string[], value: string) {
  const exists = values.some((item) => item.toLowerCase() === value.toLowerCase())
  if (exists) return values.filter((item) => item.toLowerCase() !== value.toLowerCase())
  return [...values, value]
}

function marketCode(market: string) {
  return market.toLowerCase().replace('player ', 'player_').replaceAll(' ', '_')
}

function teamCode(teamName: string) {
  const label = shortMatchLabel(`${teamName} v ${teamName}`)
  return label.includes(' v ') ? label.split(' v ')[0] : teamName
}

function sortingToSelectValue(sorting: SortingState): ArbSortValue {
  const current = sorting[0]
  if (!current) return 'margin:desc'
  if (current.id === 'implied_probability_sum') return 'implied_probability_sum:asc'
  if (current.id === 'over_price') return 'over_price:desc'
  if (current.id === 'selection') return 'selection:asc'
  return current.desc ? 'margin:desc' : 'margin:asc'
}

function selectValueToSorting(value: ArbSortValue): SortingState {
  const [id, direction] = value.split(':')
  return [{ id, desc: direction === 'desc' }]
}

function countActiveFilters(filters: ArbFilters, scope: ArbScope) {
  let count = scope === 'all' ? 0 : 1
  if (filters.query.trim()) count += 1
  if (filters.markets.length > 0) count += 1
  if (filters.agencies.length > 0) count += 1
  if (filters.minMargin !== defaultArbFilters.minMargin) count += 1
  if (filters.maxMargin !== defaultArbFilters.maxMargin) count += 1
  return count
}

function arbColumnClassName(columnId: string) {
  return clsx(
    columnId === 'selection' && 'is-sticky-column',
    ['over_price', 'under_price', 'implied_probability_sum', 'margin', 'actions'].includes(columnId) && 'is-numeric-column',
    columnId === 'player_team_opposition_team' && 'is-context-column',
  )
}
