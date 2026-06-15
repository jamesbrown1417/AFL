import { useMemo, useState } from 'react'
import { flexRender, getCoreRowModel, getSortedRowModel, useReactTable, type ColumnDef, type SortingState } from '@tanstack/react-table'
import { RefreshCcw, Search } from 'lucide-react'
import clsx from 'clsx'
import type { ArbSearchResult, BookmakerSummary } from '../api/types'
import { useArbs } from '../api/queries'
import { Button, Chip, EmptyState, ErrorBanner, Field, Panel, Select, SortIcon, StatPill, TextInput } from '../components/ui'
import { bookmakerLabel, formatDateTime, formatLine, formatPrice, marketLabel, shortMatchLabel } from '../lib/formatters'
import { defaultArbFilters, useAppStore, useClientSettings } from '../store/useAppStore'

export function ArbWorkspace({ bookmakers }: { bookmakers: BookmakerSummary[] }) {
  const settings = useClientSettings()
  const filters = useAppStore((state) => state.arbFilters)
  const patchFilters = useAppStore((state) => state.patchArbFilters)
  const setFilters = useAppStore((state) => state.setArbFilters)
  const { data = [], isFetching, error } = useArbs(settings, filters, 500)
  const [sorting, setSorting] = useState<SortingState>([{ id: 'margin', desc: true }])

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

  const trueArbs = data.filter((row) => row.margin > 0).length
  const nearArbs = data.length - trueArbs
  const bestMargin = data.length > 0 ? Math.max(...data.map((row) => row.margin)) : null
  const sourceModifiedAt = data.find((row) => row.source_modified_at)?.source_modified_at ?? null

  const columns = useMemo<ColumnDef<ArbSearchResult>[]>(
    () => [
      {
        header: 'Status',
        accessorKey: 'status',
        cell: ({ row }) => (
          <span className={clsx('tag', row.original.margin > 0 ? 'tag--good' : 'tag--amber')}>
            {row.original.margin > 0 ? 'Arb' : 'Near'}
          </span>
        ),
      },
      {
        header: 'Margin',
        accessorKey: 'margin',
        cell: ({ row }) => (
          <b className={clsx('arb-margin', row.original.margin > 0 ? 'arb-margin--positive' : 'arb-margin--near')}>
            {formatMargin(row.original.margin)}
          </b>
        ),
      },
      {
        header: 'Player / match',
        accessorFn: (row) => `${row.player_name} ${row.match_name}`,
        cell: ({ row }) => (
          <div className="table-stack">
            <b>{row.original.player_name}</b>
            <span>{shortMatchLabel(row.original.match_name)}</span>
          </div>
        ),
      },
      {
        header: 'Market',
        accessorFn: (row) => `${row.market_name} ${row.over_line ?? ''} ${row.under_line ?? ''}`,
        cell: ({ row }) => (
          <div className="table-stack">
            <b>{row.original.market_name}</b>
            <span>
              Over {formatLine(row.original.over_line)} / Under {formatLine(row.original.under_line)}
            </span>
          </div>
        ),
      },
      {
        header: 'Over',
        accessorFn: (row) => row.over_price,
        cell: ({ row }) => <PriceAgency price={row.original.over_price} agency={row.original.over_agency} />,
      },
      {
        header: 'Under',
        accessorFn: (row) => row.under_price,
        cell: ({ row }) => <PriceAgency price={row.original.under_price} agency={row.original.under_agency} />,
      },
      {
        header: 'Imp sum',
        accessorKey: 'implied_probability_sum',
        cell: ({ row }) => <span className="tabular">{(row.original.implied_probability_sum * 100).toFixed(2)}%</span>,
      },
      {
        header: 'Teams',
        accessorFn: (row) => `${row.player_team ?? ''} ${row.opposition_team ?? ''}`,
        cell: ({ row }) => (
          <div className="tag-row">
            {row.original.player_team ? <span className="tag">{teamCode(row.original.player_team)}</span> : null}
            {row.original.opposition_team ? <span className="tag tag--amber">v {teamCode(row.original.opposition_team)}</span> : null}
          </div>
        ),
      },
    ],
    [],
  )

  // TanStack Table intentionally returns function-bearing instances; this component does not pass it across memoized boundaries.
  // eslint-disable-next-line react-hooks/incompatible-library
  const table = useReactTable({
    data,
    columns,
    state: { sorting },
    onSortingChange: setSorting,
    getCoreRowModel: getCoreRowModel(),
    getSortedRowModel: getSortedRowModel(),
  })

  return (
    <main className="workspace arb-workspace" aria-label="Arb finder">
      <section className="workspace-main">
        <div className="page-title-row">
          <div>
            <h1>Arbs</h1>
            <p>{isFetching ? 'Refreshing arb board' : `${data.length} opportunities in view`}</p>
          </div>
          <Button variant="secondary" onClick={() => setFilters(defaultArbFilters)}>
            <RefreshCcw size={15} /> Reset
          </Button>
        </div>

        <div className="summary-strip">
          <StatPill label="True arbs" value={String(trueArbs)} tone={trueArbs > 0 ? 'good' : 'neutral'} />
          <StatPill label="Near arbs" value={String(nearArbs)} tone="warn" />
          <StatPill label="Best margin" value={bestMargin == null ? '--' : formatMargin(bestMargin)} tone={bestMargin != null && bestMargin > 0 ? 'good' : 'warn'} />
          <StatPill label="Source" value={formatDateTime(sourceModifiedAt)} />
        </div>

        <Panel className="filters-panel">
          <div className="filter-grid arb-filter-grid">
            <Field label="Search arbs">
              <div className="input-with-icon">
                <Search size={16} />
                <TextInput value={filters.query} onChange={(event) => patchFilters({ query: event.currentTarget.value })} placeholder="Player, match, agency" />
              </div>
            </Field>
            <Field label="Market">
              <Select value={filters.markets[0] ?? ''} onChange={(event) => patchFilters({ markets: event.currentTarget.value ? [event.currentTarget.value] : [] })}>
                <option value="">All player markets</option>
                {marketOptions.map((market) => (
                  <option key={market} value={market}>{marketLabel(marketCode(market))}</option>
                ))}
              </Select>
            </Field>
            <Field label="Min margin">
              <TextInput inputMode="decimal" value={filters.minMargin} onChange={(event) => patchFilters({ minMargin: event.currentTarget.value })} />
            </Field>
            <Field label="Max margin">
              <TextInput inputMode="decimal" value={filters.maxMargin} onChange={(event) => patchFilters({ maxMargin: event.currentTarget.value })} placeholder="No cap" />
            </Field>
          </div>
          <div className="chip-row">
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
        </Panel>

        {error ? <ErrorBanner message={error instanceof Error ? error.message : 'Failed to load arbs.'} /> : null}

        <Panel className="table-panel">
          {data.length === 0 && !isFetching ? (
            <EmptyState title="No arbs in range" body="Lower the minimum margin, clear agency filters, or broaden the market." />
          ) : (
            <div className="data-table-wrap">
              <table className="data-table">
                <caption className="visually-hidden">Arbitrage opportunities</caption>
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
                    <tr key={row.original.id} className={clsx(row.original.margin > 0 && 'arb-row--positive')}>
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
    </main>
  )
}

function PriceAgency({ price, agency }: { price: number; agency: string }) {
  return (
    <div className="table-stack">
      <b className="tabular">{formatPrice(price)}</b>
      <span className="agency">{agency}</span>
    </div>
  )
}

function formatMargin(value: number) {
  return `${value >= 0 ? '+' : ''}${value.toFixed(2)}%`
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
