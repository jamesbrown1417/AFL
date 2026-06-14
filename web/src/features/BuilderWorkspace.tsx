import { useDeferredValue, useEffect, useMemo, useState } from 'react'
import clsx from 'clsx'
import { ArrowDown, ArrowUp, BarChart3, Check, Filter, LineChart, Search, Trash2, X } from 'lucide-react'
import type { ClientSettings } from '../api/client'
import type { BookmakerSummary, BuilderMode, CgmAgencyComparison, DraftLeg, EventSummary, OddsSearchResult, SgmAgencyComparison, SortField } from '../api/types'
import { useBuilderOdds, useCompareCgm, useCompareSgm, useSelectionAgencyPrices } from '../api/queries'
import { defaultMetricFilters, defaultPlayerFilters, useAppStore, useClientSettings } from '../store/useAppStore'
import { allMarketCode, buildCandidateGroups, combinedBasePrice, defaultDescending, lineWithSideLabel, marketTypeToStatCode, orderedMarketCodes, sortCandidateRows, toDraftLeg } from '../lib/builder'
import { bookmakerLabel, formatDateTime, formatLine, formatPrice, formatSigned, marketLabel, playerPositionTag, selectionTypeLabel, shortMatchLabel } from '../lib/formatters'
import { Button, Chip, EmptyState, ErrorBanner, Field, Panel, Segmented, Select, StatPill, TextInput, Toggle } from '../components/ui'

export function BuilderWorkspace({
  mode,
  bookmakers,
  events,
}: {
  mode: BuilderMode
  bookmakers: BookmakerSummary[]
  events: EventSummary[]
}) {
  const settings = useClientSettings()
  const selectedDefault = useAppStore((state) => state.defaultBookmaker)
  const metricFilters = useAppStore((state) => state.metricFilters)
  const setMetricFilters = useAppStore((state) => state.setMetricFilters)
  const displayMode = useAppStore((state) => state.displayMode)
  const setDisplayMode = useAppStore((state) => state.setDisplayMode)
  const sgmLegs = useAppStore((state) => state.sgmLegs)
  const cgmLegs = useAppStore((state) => state.cgmLegs)
  const addSgmLeg = useAppStore((state) => state.addSgmLeg)
  const addCgmLeg = useAppStore((state) => state.addCgmLeg)
  const removeSgmLeg = useAppStore((state) => state.removeSgmLeg)
  const removeCgmLeg = useAppStore((state) => state.removeCgmLeg)
  const clearSgm = useAppStore((state) => state.clearSgm)
  const clearCgm = useAppStore((state) => state.clearCgm)
  const forceRefresh = useAppStore((state) => state.sgmForceRefresh)
  const setForceRefresh = useAppStore((state) => state.setSgmForceRefresh)
  const setSelectedPlayer = useAppStore((state) => state.setSelectedPlayer)
  const setPlayerFilters = useAppStore((state) => state.setPlayerFilters)

  const enabledBookmakers = useMemo(() => bookmakers.filter((bookmaker) => bookmaker.enabled), [bookmakers])
  const firstBookmaker = enabledBookmakers.find((bookmaker) => bookmaker.code === selectedDefault)?.code ?? enabledBookmakers[0]?.code ?? ''
  const [bookmaker, setBookmaker] = useState('')
  // Fall back to the preferred/first agency until the user explicitly picks one (agencies load async).
  const effectiveBookmaker = bookmaker || firstBookmaker
  const [sgmEventId, setSgmEventId] = useState<number | null>(null)
  const [selectedEventIds, setSelectedEventIds] = useState<Set<number>>(new Set())
  const [bestOnly, setBestOnly] = useState(false)
  const [selectedMarket, setSelectedMarket] = useState(allMarketCode)
  const [playerQuery, setPlayerQuery] = useState('')
  const [sortField, setSortField] = useState<SortField>('next_best')
  const [descending, setDescending] = useState(true)
  const [notice, setNotice] = useState<string | null>(null)
  const [contextMenu, setContextMenu] = useState<{ x: number; y: number; selection: OddsSearchResult } | null>(null)
  const [priceDialogSelection, setPriceDialogSelection] = useState<OddsSearchResult | null>(null)

  const legs = mode === 'sgm' ? sgmLegs : cgmLegs
  const selectedSelectionIds = useMemo(() => new Set(legs.map((leg) => leg.selection_id)), [legs])
  // SGM always has one match in focus: the user's explicit pick, otherwise the first fixture.
  const effectiveSgmEventId = sgmEventId ?? events[0]?.id ?? null
  const eventIds = mode === 'sgm' ? (effectiveSgmEventId == null ? [] : [effectiveSgmEventId]) : Array.from(selectedEventIds)
  const effectiveEventIds = mode === 'cgm' && eventIds.length === 0 ? [] : eventIds
  const candidateQuery = useBuilderOdds(settings, effectiveBookmaker, effectiveEventIds, metricFilters, bestOnly, Boolean(effectiveBookmaker) && (mode === 'cgm' || effectiveSgmEventId != null))
  const candidates = useMemo(() => {
    const rows = candidateQuery.data ?? []
    if (mode === 'cgm') {
      const draftedEventIds = new Set(cgmLegs.map((leg) => leg.event_id))
      return rows.filter((row) => !draftedEventIds.has(row.event_id))
    }
    return rows
  }, [candidateQuery.data, cgmLegs, mode])
  const deferredPlayerQuery = useDeferredValue(playerQuery)
  const marketCodes = useMemo(() => [allMarketCode, ...orderedMarketCodes(candidates)], [candidates])
  const visibleCandidates = useMemo(() => {
    const normalizedQuery = deferredPlayerQuery.trim().toLowerCase()
    return candidates.filter((row) => {
      const marketMatches = selectedMarket === allMarketCode || row.market_type_code === selectedMarket
      if (!marketMatches) return false
      if (!normalizedQuery) return true
      return [row.player?.full_name, row.label]
        .filter(Boolean)
        .some((value) => value!.toLowerCase().includes(normalizedQuery))
    })
  }, [candidates, deferredPlayerQuery, selectedMarket])
  const rowCandidates = useMemo(() => sortCandidateRows(visibleCandidates, sortField, descending), [visibleCandidates, sortField, descending])
  const groups = useMemo(() => buildCandidateGroups(visibleCandidates), [visibleCandidates])
  const compareSgm = useCompareSgm(settings)
  const compareCgm = useCompareCgm(settings)

  const toggleLeg = (selection: OddsSearchResult) => {
    setNotice(null)
    const draftLeg = toDraftLeg(selection)
    if (!draftLeg) {
      setNotice('That leg does not have a current price.')
      return
    }
    if (mode === 'sgm') {
      if (!selection.sgm_eligible) {
        setNotice('That leg is not ready for SGM pricing.')
        return
      }
      addSgmLeg(draftLeg)
      return
    }
    const alreadySelected = cgmLegs.some((leg) => leg.selection_id === draftLeg.selection_id)
    if (!alreadySelected && cgmLegs.some((leg) => leg.event_id === draftLeg.event_id)) {
      setNotice('Cross-game multis allow one leg per match. Pick a different game.')
      return
    }
    addCgmLeg(draftLeg)
  }

  const confirmDraftSwitch = () => {
    if (legs.length === 0) return true
    return window.confirm(`Switching clears your current draft. You currently have ${legs.length} leg${legs.length === 1 ? '' : 's'} selected.`)
  }

  const handleSelectBookmaker = (code: string) => {
    if (code === effectiveBookmaker) return
    if (!confirmDraftSwitch()) return
    if (mode === 'sgm') clearSgm()
    else {
      clearCgm()
      setSelectedEventIds(new Set())
    }
    setNotice(null)
    setBookmaker(code)
  }

  const handleSelectSgmEvent = (eventId: number | null) => {
    if (eventId === effectiveSgmEventId) return
    if (sgmLegs.length > 0 && !confirmDraftSwitch()) return
    if (sgmLegs.length > 0) clearSgm()
    setNotice(null)
    setSgmEventId(eventId)
  }

  const selectedSgmEvent = mode === 'sgm' ? events.find((event) => event.id === effectiveSgmEventId) ?? null : null
  const sgmWeather = mode === 'sgm' ? candidates.find((row) => row.weather)?.weather ?? null : null

  const handleSort = (field: SortField) => {
    if (field === sortField) {
      setDescending((current) => !current)
    } else {
      setSortField(field)
      setDescending(defaultDescending(field))
    }
  }

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
    window.addEventListener('scroll', close, true)
    window.addEventListener('resize', close)
    return () => {
      window.removeEventListener('click', close)
      window.removeEventListener('scroll', close, true)
      window.removeEventListener('resize', close)
    }
  }, [contextMenu])

  const compare = () => {
    if (mode === 'sgm') {
      const eventId = legs[0]?.event_id ?? effectiveSgmEventId
      if (eventId != null && legs.length >= 2) {
        compareSgm.mutate({ eventId, selectionIds: legs.map((leg) => leg.selection_id), forceRefresh })
      }
      return
    }
    if (legs.length >= 2) compareCgm.mutate({ selectionIds: legs.map((leg) => leg.selection_id) })
  }

  return (
    <main className="workspace builder-workspace">
      <section className="workspace-main">
        <div className="page-title-row">
          <div>
            <h1>{mode === 'sgm' ? 'SGM builder' : 'CGM builder'}</h1>
            <p>{mode === 'sgm' ? 'One match, multiple legs' : 'One leg per match across games'}</p>
          </div>
          <Segmented
            value={displayMode}
            ariaLabel="Candidate display"
            options={[
              { value: 'row', label: 'Rows' },
              { value: 'tile', label: 'Tiles' },
            ]}
            onChange={setDisplayMode}
          />
        </div>

        <Panel className="builder-controls">
          <div className="filter-grid">
            <Field label="Source agency">
              <Select value={effectiveBookmaker} onChange={(event) => handleSelectBookmaker(event.currentTarget.value)}>
                {enabledBookmakers.map((item) => (
                  <option key={item.code} value={item.code}>{item.live_pricing_enabled ? `${item.display_name} • live` : item.display_name}</option>
                ))}
              </Select>
            </Field>
            {mode === 'sgm' ? (
              <Field label="Match">
                <Select value={effectiveSgmEventId ?? ''} onChange={(event) => handleSelectSgmEvent(event.currentTarget.value ? Number(event.currentTarget.value) : null)}>
                  {events.map((event) => (
                    <option key={event.id} value={event.id}>{shortMatchLabel(event.match_name)} | {formatDateTime(event.start_time)}</option>
                  ))}
                </Select>
              </Field>
            ) : (
              <Field label="Match filter">
                <Select
                  value=""
                  onChange={(event) => {
                    const value = Number(event.currentTarget.value)
                    if (!value) return
                    setSelectedEventIds((current) => new Set(current).add(value))
                  }}
                >
                  <option value="">Add match filter</option>
                  {events
                    .filter((event) => !selectedEventIds.has(event.id) && !cgmLegs.some((leg) => leg.event_id === event.id))
                    .map((event) => (
                      <option key={event.id} value={event.id}>{shortMatchLabel(event.match_name)}</option>
                    ))}
                </Select>
              </Field>
            )}
            <Field label="Market">
              <Select value={selectedMarket} onChange={(event) => setSelectedMarket(event.currentTarget.value)}>
                {marketCodes.map((code) => (
                  <option key={code} value={code}>{marketLabel(code)}</option>
                ))}
              </Select>
            </Field>
            <Field label="Player search">
              <div className="input-with-icon">
                <Search size={16} />
                <TextInput value={playerQuery} onChange={(event) => setPlayerQuery(event.currentTarget.value)} placeholder="Player name" />
              </div>
            </Field>
          </div>
          <div className="quick-filter-grid">
            <Chip
              active={metricFilters.minDiffLast10 >= 0}
              onClick={() => setMetricFilters({ ...metricFilters, minDiffLast10: metricFilters.minDiffLast10 >= 0 ? -1 : 0 })}
            >
              Positive L10
            </Chip>
            <Chip
              active={metricFilters.minNextBestProbDiff >= 0}
              onClick={() => setMetricFilters({ ...metricFilters, minNextBestProbDiff: metricFilters.minNextBestProbDiff >= 0 ? -1 : 0 })}
            >
              Positive next best
            </Chip>
            <Chip
              active={metricFilters.matchupDifficulties.length > 0}
              onClick={() =>
                setMetricFilters({
                  ...metricFilters,
                  matchupDifficulties: metricFilters.matchupDifficulties.length > 0 ? [] : ['Neutral', 'Good', 'Excellent'],
                })
              }
            >
              Favorable matchup
            </Chip>
            <Toggle checked={bestOnly} onChange={setBestOnly} label="Best only" />
            {mode === 'sgm' && <Toggle checked={forceRefresh} onChange={setForceRefresh} label="Force quote refresh" />}
            <Button variant="ghost" onClick={() => setMetricFilters(defaultMetricFilters)}><Filter size={15} /> Reset filters</Button>
          </div>
          {mode === 'sgm' && selectedSgmEvent && (
            <p className="match-context">
              {[
                selectedSgmEvent.venue,
                selectedSgmEvent.round_label,
                sgmWeather?.label,
                sgmWeather?.temperature_c != null ? `${Math.round(sgmWeather.temperature_c)}°C` : null,
                sgmWeather?.wind_kph != null ? `${Math.round(sgmWeather.wind_kph)} km/h wind` : null,
              ]
                .filter(Boolean)
                .join(' · ') || 'Match details unavailable'}
            </p>
          )}
          {mode === 'cgm' && selectedEventIds.size > 0 && (
            <div className="chip-row">
              {Array.from(selectedEventIds).map((eventId) => {
                const event = events.find((item) => item.id === eventId)
                return (
                  <Chip key={eventId} active onClick={() => setSelectedEventIds((current) => new Set(Array.from(current).filter((id) => id !== eventId)))}>
                    {event ? shortMatchLabel(event.match_name) : eventId} <X size={12} />
                  </Chip>
                )
              })}
            </div>
          )}
        </Panel>

        {candidateQuery.error ? <ErrorBanner message={candidateQuery.error instanceof Error ? candidateQuery.error.message : 'Failed to load builder legs.'} /> : null}

        <Panel className="candidate-panel">
          <div className="section-heading">
            <h2>{marketLabel(selectedMarket)} options</h2>
            <span>{candidateQuery.isFetching ? 'Loading' : `${visibleCandidates.length} selections`}</span>
          </div>
          {notice ? <p className="builder-notice" role="status">{notice}</p> : null}
          {visibleCandidates.length === 0 && !candidateQuery.isFetching ? (
            <EmptyState title="No eligible legs" body="Change agency, match, market, player search, or metric filters." />
          ) : displayMode === 'row' ? (
            <div className="candidate-table">
              <CandidateHeader sortField={sortField} descending={descending} onSort={handleSort} />
              <div className="candidate-list">
                {rowCandidates.map((selection) => (
                  <CandidateRow
                    key={`${selection.selection_id}-${selection.bookmaker}`}
                    selection={selection}
                    selected={selectedSelectionIds.has(selection.selection_id)}
                    onToggle={() => toggleLeg(selection)}
                    onContextMenu={(event) => openContextMenu(event, selection)}
                    disabled={selection.decimal_price == null || (mode === 'sgm' && !selection.sgm_eligible)}
                  />
                ))}
              </div>
            </div>
          ) : (
            <div className="candidate-grid">
              {groups.map((group) => (
                <div className="candidate-tile" key={group.key}>
                  <div className="tile-head">
                    <strong>{group.title}</strong>
                    <span>{group.subtitle}</span>
                    <div className="tag-row">
                      {playerPositionTag(group.playerPosition) ? <span className="tag">{playerPositionTag(group.playerPosition)}</span> : null}
                      <MatchupBadge value={group.matchupDifficulty} />
                    </div>
                  </div>
                  <div className="tile-lines">
                    {group.selections.map((selection) => (
                      <button
                        type="button"
                        key={selection.selection_id}
                        className={selectedSelectionIds.has(selection.selection_id) ? 'is-selected' : ''}
                        disabled={selection.decimal_price == null || (mode === 'sgm' && !selection.sgm_eligible)}
                        onClick={() => toggleLeg(selection)}
                        onContextMenu={(event) => openContextMenu(event, selection)}
                      >
                        <span>{selectionTypeLabel(selection.selection_type)} {formatPrice(selection.decimal_price)}</span>
                        <b>{selection.line_value ?? '-'}</b>
                      </button>
                    ))}
                  </div>
                </div>
              ))}
            </div>
          )}
        </Panel>
      </section>

      <BuilderPanel
        mode={mode}
        legs={legs}
        isComparing={compareSgm.isPending || compareCgm.isPending}
        sgmResults={compareSgm.data?.results ?? []}
        cgmResults={compareCgm.data?.results ?? []}
        error={compareSgm.error instanceof Error ? compareSgm.error.message : compareCgm.error instanceof Error ? compareCgm.error.message : null}
        onCompare={compare}
        onClear={mode === 'sgm' ? clearSgm : clearCgm}
        onRemove={mode === 'sgm' ? removeSgmLeg : removeCgmLeg}
      />

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

function CandidateHeader({
  sortField,
  descending,
  onSort,
}: {
  sortField: SortField
  descending: boolean
  onSort: (field: SortField) => void
}) {
  const columns: { label: string; field: SortField; numeric: boolean }[] = [
    { label: 'Line', field: 'line', numeric: true },
    { label: 'Price', field: 'price', numeric: true },
    { label: 'L10', field: 'diff_last_10', numeric: true },
    { label: 'Szn', field: 'diff_2025', numeric: true },
    { label: 'NB', field: 'next_best', numeric: true },
  ]
  const playerActive = sortField === 'player'
  return (
    <div className="candidate-head-row">
      <button
        type="button"
        className={clsx('candidate-head-cell', playerActive && 'is-active')}
        onClick={() => onSort('player')}
        aria-sort={playerActive ? (descending ? 'descending' : 'ascending') : 'none'}
      >
        Player
        {playerActive ? (descending ? <ArrowDown size={12} /> : <ArrowUp size={12} />) : null}
      </button>
      <div className="candidate-head-cell">Matchup</div>
      {columns.map((column) => {
        const active = sortField === column.field
        return (
          <button
            key={column.field}
            type="button"
            className={clsx('candidate-head-cell', column.numeric && 'col-num', active && 'is-active')}
            onClick={() => onSort(column.field)}
            aria-sort={active ? (descending ? 'descending' : 'ascending') : 'none'}
          >
            {column.label}
            {active ? (descending ? <ArrowDown size={12} /> : <ArrowUp size={12} />) : null}
          </button>
        )
      })}
    </div>
  )
}

function CandidateContextMenu({
  x,
  y,
  selection,
  onViewPlayer,
  onCompareAgencies,
}: {
  x: number
  y: number
  selection: OddsSearchResult
  onViewPlayer: () => void
  onCompareAgencies: () => void
}) {
  const left = Math.min(x, window.innerWidth - 240)
  const top = Math.min(y, window.innerHeight - 120)
  return (
    <div className="context-menu" style={{ left, top }} role="menu" onClick={(event) => event.stopPropagation()}>
      <button type="button" role="menuitem" onClick={onViewPlayer} disabled={selection.player == null}>
        <LineChart size={15} /> View in Player tab
      </button>
      <button type="button" role="menuitem" onClick={onCompareAgencies}>
        <BarChart3 size={15} /> Compare agency prices
      </button>
    </div>
  )
}

function MatchupBadge({ value }: { value?: string | null }) {
  if (!value) return null
  const normalized = value.trim().toLowerCase()
  return <span className={clsx('tag', 'matchup-tag', `matchup-tag--${normalized.replaceAll(/\s+/g, '-')}`)}>{value}</span>
}

function AgencyPriceDialog({
  settings,
  selection,
  onClose,
}: {
  settings: ClientSettings
  selection: OddsSearchResult
  onClose: () => void
}) {
  const pricesQuery = useSelectionAgencyPrices(settings, selection)
  const rows = pricesQuery.data ?? []
  const bestPrice = rows[0]?.decimal_price ?? null
  return (
    <div className="modal-overlay" onClick={onClose}>
      <div className="modal" role="dialog" aria-modal="true" onClick={(event) => event.stopPropagation()}>
        <div className="modal-head">
          <div>
            <strong>{selection.player?.full_name ?? selection.label}</strong>
            <span>{marketLabel(selection.market_type_code)} · {lineWithSideLabel(selection)} · {shortMatchLabel(selection.match_name)}</span>
          </div>
          <button type="button" className="modal-close" onClick={onClose} aria-label="Close">
            <X size={16} />
          </button>
        </div>
        {pricesQuery.isLoading ? (
          <p className="modal-status">Loading agency prices…</p>
        ) : pricesQuery.error ? (
          <ErrorBanner message={pricesQuery.error instanceof Error ? pricesQuery.error.message : 'Failed to load agency prices.'} />
        ) : rows.length === 0 ? (
          <EmptyState title="No prices" body="No agency currently lists this exact selection." />
        ) : (
          <div className="agency-price-list">
            {rows.map((row) => {
              const isBest = row.decimal_price != null && row.decimal_price === bestPrice
              return (
                <div className={clsx('agency-price-row', isBest && 'is-best')} key={`${row.bookmaker}-${row.selection_id}`}>
                  <span>{bookmakerLabel(row.bookmaker)}</span>
                  <b className="tabular">{formatPrice(row.decimal_price)}</b>
                  {isBest ? <small className="agency-best">Best</small> : <small>{formatSigned(row.next_best_prob_diff)}</small>}
                </div>
              )
            })}
          </div>
        )}
      </div>
    </div>
  )
}

function CandidateRow({
  selection,
  selected,
  disabled,
  onToggle,
  onContextMenu,
}: {
  selection: OddsSearchResult
  selected: boolean
  disabled: boolean
  onToggle: () => void
  onContextMenu: (event: React.MouseEvent) => void
}) {
  return (
    <div
      className={clsx('candidate-row', selected && 'is-selected', disabled && 'is-disabled')}
      role="button"
      tabIndex={disabled ? -1 : 0}
      aria-pressed={selected}
      aria-disabled={disabled}
      title={disabled ? 'Not available for this builder' : selected ? 'Click to remove from draft' : 'Click to add to draft'}
      onClick={() => {
        if (!disabled) onToggle()
      }}
      onContextMenu={onContextMenu}
      onKeyDown={(event) => {
        if (!disabled && (event.key === 'Enter' || event.key === ' ')) {
          event.preventDefault()
          onToggle()
        }
      }}
    >
      <div className="candidate-primary">
        <strong>
          {selected ? <Check size={13} className="candidate-check" /> : null}
          {selection.player?.full_name ?? selection.label}
        </strong>
        <span>{marketLabel(selection.market_type_code)} | {shortMatchLabel(selection.match_name)}</span>
      </div>
      <div className="candidate-context">
        {playerPositionTag(selection.player_position) ? <span className="tag">{playerPositionTag(selection.player_position)}</span> : null}
        <MatchupBadge value={selection.matchup_difficulty} />
      </div>
      <span className="col-num">{lineWithSideLabel(selection)}</span>
      <b className="col-num tabular">{formatPrice(selection.decimal_price)}</b>
      <span className={clsx('col-num delta', (selection.diff_last_10 ?? -1) >= 0 ? 'delta--good' : 'delta--bad')}>{formatSigned(selection.diff_last_10)}</span>
      <span className={clsx('col-num delta', (selection.diff_2025 ?? -1) >= 0 ? 'delta--good' : 'delta--bad')}>{formatSigned(selection.diff_2025)}</span>
      <span className={clsx('col-num delta', (selection.next_best_prob_diff ?? -1) >= 0 ? 'delta--good' : 'delta--bad')}>{formatSigned(selection.next_best_prob_diff)}</span>
    </div>
  )
}

function BuilderPanel({
  mode,
  legs,
  isComparing,
  sgmResults,
  cgmResults,
  error,
  onCompare,
  onClear,
  onRemove,
}: {
  mode: BuilderMode
  legs: DraftLeg[]
  isComparing: boolean
  sgmResults: SgmAgencyComparison[]
  cgmResults: CgmAgencyComparison[]
  error: string | null
  onCompare: () => void
  onClear: () => void
  onRemove: (selectionId: number) => void
}) {
  const legBySelectionId = useMemo(() => new Map(legs.map((leg) => [leg.selection_id, leg])), [legs])
  return (
    <aside className="builder-panel">
      <div className="builder-panel-head">
        <div>
          <h2>{mode.toUpperCase()} draft</h2>
          <span>{legs.length} legs</span>
        </div>
        <Button variant="ghost" onClick={onClear} disabled={legs.length === 0}><Trash2 size={15} /> Clear</Button>
      </div>
      <div className="builder-stats">
        <StatPill label="Base price" value={formatPrice(combinedBasePrice(legs))} tone="warn" />
        <StatPill label="Required" value={legs.length >= 2 ? 'Ready' : '2+ legs'} tone={legs.length >= 2 ? 'good' : 'neutral'} />
      </div>
      <div className="draft-list">
        {legs.length === 0 ? (
          <EmptyState title="No legs selected" body="Add priceable selections from the board." />
        ) : (
          legs.map((leg) => (
            <div className="draft-leg" key={leg.selection_id}>
              <div>
                <strong>{leg.label}</strong>
                <span>{marketLabel(leg.market_type_code)} | {shortMatchLabel(leg.event_label)} | {bookmakerLabel(leg.bookmaker)}</span>
                <div className="tag-row">
                  {playerPositionTag(leg.player_position) ? <span className="tag">{playerPositionTag(leg.player_position)}</span> : null}
                  <MatchupBadge value={leg.matchup_difficulty} />
                </div>
              </div>
              <b>{formatPrice(leg.base_price)}</b>
              <button type="button" onClick={() => onRemove(leg.selection_id)} aria-label="Remove leg">
                <X size={15} />
              </button>
            </div>
          ))
        )}
      </div>
      {error ? <ErrorBanner message={error} /> : null}
      <Button className="compare-button" disabled={legs.length < 2 || isComparing} onClick={onCompare}>
        {isComparing ? 'Comparing' : 'Compare agencies'}
      </Button>
      <div className="quote-results">
        {mode === 'sgm'
          ? sgmResults.map((result, index) => (
              <SgmComparisonCard key={result.quote_id} result={result} rank={index + 1} legBySelectionId={legBySelectionId} />
            ))
          : cgmResults.map((result, index) => (
              <CgmComparisonCard key={result.bookmaker} result={result} rank={index + 1} legBySelectionId={legBySelectionId} />
            ))}
      </div>
    </aside>
  )
}

function SgmComparisonCard({ result, rank, legBySelectionId }: { result: SgmAgencyComparison; rank: number; legBySelectionId: Map<number, DraftLeg> }) {
  return (
    <div className={clsx('comparison-card', rank === 1 && 'comparison-card--best')}>
      <div className="comparison-head">
        <div>
          <strong>#{rank} {bookmakerLabel(result.bookmaker)}</strong>
          <span>{result.legs.length} legs priced</span>
        </div>
        <b className="comparison-price tabular">{formatPrice(result.quoted_price)}</b>
      </div>
      <div className="comparison-metrics">
        <StatPill label="Local" value={formatPrice(result.unadjusted_price)} />
        <StatPill label="Factor" value={result.adjustment_factor.toFixed(3)} />
        <StatPill label="Cache" value={result.from_cache ? 'Yes' : 'No'} />
      </div>
      <div className="comparison-legs">
        {result.legs.map((leg) => (
          <div className="comparison-leg" key={leg.selection_id}>
            <span>{leg.label}</span>
            <MatchupBadge value={legBySelectionId.get(leg.selection_id)?.matchup_difficulty} />
            <b className="tabular">{formatPrice(leg.base_price)}</b>
          </div>
        ))}
      </div>
      <small className="comparison-foot">Quoted {formatDateTime(result.quoted_at)}</small>
    </div>
  )
}

function CgmComparisonCard({ result, rank, legBySelectionId }: { result: CgmAgencyComparison; rank: number; legBySelectionId: Map<number, DraftLeg> }) {
  return (
    <div className={clsx('comparison-card', rank === 1 && 'comparison-card--best')}>
      <div className="comparison-head">
        <div>
          <strong>#{rank} {bookmakerLabel(result.bookmaker)}</strong>
          <span>{result.selection_count} legs priced</span>
        </div>
        <b className="comparison-price tabular">{formatPrice(result.quoted_price)}</b>
      </div>
      <div className="comparison-legs">
        {result.legs.map((leg) => (
          <div className="comparison-leg comparison-leg--stacked" key={leg.selection_id}>
            <div>
              <span>{leg.label}</span>
              <small>{shortMatchLabel(leg.match_name)}</small>
              <MatchupBadge value={legBySelectionId.get(leg.selection_id)?.matchup_difficulty} />
            </div>
            <b className="tabular">{formatPrice(leg.base_price)}</b>
          </div>
        ))}
      </div>
    </div>
  )
}
