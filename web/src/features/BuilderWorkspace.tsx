import { useDeferredValue, useEffect, useMemo, useRef, useState } from 'react'
import clsx from 'clsx'
import { useVirtualizer } from '@tanstack/react-virtual'
import { ArrowDown, ArrowUp, BarChart3, Check, Filter, LineChart, Search, Trash2, X } from 'lucide-react'
import type { ClientSettings } from '../api/client'
import type { BookmakerSummary, BuilderMode, CgmAgencyComparison, DraftLeg, EventSummary, MetricFilters, OddsSearchResult, SgmAgencyComparison, SortField } from '../api/types'
import { useBuilderOdds, useCompareCgm, useCompareSgm, useSelectionAgencyPrices } from '../api/queries'
import { defaultMetricFilters, defaultPlayerFilters, useAppStore, useClientSettings } from '../store/useAppStore'
import { allMarketCode, buildCandidateGroups, combinedBasePrice, defaultDescending, favorableMatchupDifficulties, isFavorableMatchupSet, lineWithSideLabel, marketTypeToStatCode, orderedMarketCodes, rawMatchupDifficulty, sortCandidateRows, toDraftLeg } from '../lib/builder'
import { aflTeamCode, bookmakerLabel, formatDateTime, formatLine, formatPrice, formatShortDate, formatSigned, marketLabel, playerPositionTag, selectionTypeLabel, shortMatchLabel } from '../lib/formatters'
import { AdaptiveRail, Button, Chip, ConfirmDialog, EmptyState, ErrorBanner, Field, Panel, Segmented, Select, StatPill, TextInput, Toggle } from '../components/ui'

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
  const sgmContext = useAppStore((state) => state.sgmContext)
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
  const effectiveBookmaker = mode === 'sgm' && sgmContext ? sgmContext.bookmaker : bookmaker || firstBookmaker
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
  const [pendingSwitch, setPendingSwitch] = useState<(() => void) | null>(null)
  const [showMetricFilters, setShowMetricFilters] = useState(false)
  const [builderRailOpen, setBuilderRailOpen] = useState(false)
  const builderRailTriggerRef = useRef<HTMLButtonElement>(null)
  const [draftMetricFilters, setDraftMetricFilters] = useState<MetricFilters>(defaultMetricFilters)
  const normalizedMetricFilters = useMemo(() => normalizeMetricFilters(metricFilters), [metricFilters])
  const activeMetricFilterCount = useMemo(() => countActiveMetricFilters(normalizedMetricFilters), [normalizedMetricFilters])

  const legs = mode === 'sgm' ? sgmLegs : cgmLegs
  const selectedSelectionIds = useMemo(() => new Set(legs.map((leg) => leg.selection_id)), [legs])
  // SGM always has one match in focus: the user's explicit pick, otherwise the first fixture.
  const effectiveSgmEventId = sgmContext?.eventId ?? sgmEventId ?? events[0]?.id ?? null
  const eventIds = useMemo(
    () => mode === 'sgm' ? (effectiveSgmEventId == null ? [] : [effectiveSgmEventId]) : Array.from(selectedEventIds),
    [effectiveSgmEventId, mode, selectedEventIds],
  )
  const effectiveEventIds = useMemo(() => mode === 'cgm' && eventIds.length === 0 ? [] : eventIds, [eventIds, mode])
  const candidateQuery = useBuilderOdds(settings, effectiveBookmaker, effectiveEventIds, normalizedMetricFilters, bestOnly, Boolean(effectiveBookmaker) && (mode === 'cgm' || effectiveSgmEventId != null))
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
  const candidateScrollRef = useRef<HTMLDivElement>(null)
  // TanStack Virtual intentionally returns function-bearing instances local to this component.
  // eslint-disable-next-line react-hooks/incompatible-library
  const rowVirtualizer = useVirtualizer({
    count: displayMode === 'row' ? rowCandidates.length : 0,
    getScrollElement: () => candidateScrollRef.current,
    estimateSize: () => 61,
    overscan: 10,
  })
  const virtualRows = rowVirtualizer.getVirtualItems()
  const visibleRange = virtualRows.length > 0
    ? `${virtualRows[0].index + 1}–${virtualRows[virtualRows.length - 1].index + 1}`
    : rowCandidates.length > 0 ? '1–1' : '0–0'
  const compareSgm = useCompareSgm(settings)
  const compareCgm = useCompareCgm(settings)
  const favorableMatchups = favorableMatchupDifficulties(normalizedMetricFilters.selectionType)

  const updateSelectionType = (selectionType: string | null) => {
    setMetricFilters({
      ...normalizedMetricFilters,
      selectionType,
      matchupDifficulties: isFavorableMatchupSet(normalizedMetricFilters.matchupDifficulties)
        ? [...favorableMatchupDifficulties(selectionType)]
        : normalizedMetricFilters.matchupDifficulties,
    })
  }

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

  const requestDraftSwitch = (action: () => void) => {
    if (legs.length === 0) { action(); return }
    setPendingSwitch(() => action)
  }

  const handleSelectBookmaker = (code: string) => {
    if (code === effectiveBookmaker) return
    requestDraftSwitch(() => {
      if (mode === 'sgm') clearSgm()
      else {
        clearCgm()
        setSelectedEventIds(new Set())
      }
      setNotice(null)
      setBookmaker(code)
    })
  }

  const handleSelectSgmEvent = (eventId: number | null) => {
    if (eventId === effectiveSgmEventId) return
    if (sgmLegs.length === 0) { setSgmEventId(eventId); return }
    requestDraftSwitch(() => {
      clearSgm()
      setNotice(null)
      setSgmEventId(eventId)
    })
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

  useEffect(() => {
    if (showMetricFilters) setDraftMetricFilters(normalizedMetricFilters)
  }, [normalizedMetricFilters, showMetricFilters])

  useEffect(() => {
    rowVirtualizer.scrollToOffset(0)
  }, [deferredPlayerQuery, descending, effectiveBookmaker, effectiveEventIds, normalizedMetricFilters, rowVirtualizer, selectedMarket, sortField])

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
    <main className="workspace builder-workspace" aria-label={mode === 'sgm' ? 'SGM builder' : 'CGM builder'}>
      <section className="workspace-main">
        <div className="page-title-row">
          <div>
            <h1>{mode === 'sgm' ? 'SGM builder' : 'CGM builder'}</h1>
            <p>{mode === 'sgm' ? 'One match, multiple legs' : 'One leg per match across games'}</p>
            {mode === 'sgm' && sgmContext ? (
              <span className="builder-context-pill">{bookmakerLabel(sgmContext.bookmaker)} · {shortMatchLabel(sgmContext.eventLabel)}</span>
            ) : null}
          </div>
          <div className="page-actions">
            <Segmented
              value={displayMode}
              ariaLabel="Candidate display"
              options={[
                { value: 'row', label: 'Rows' },
                { value: 'tile', label: 'Tiles' },
              ]}
              onChange={setDisplayMode}
            />
            <button ref={builderRailTriggerRef} type="button" className="button button--secondary adaptive-rail-trigger" onClick={() => setBuilderRailOpen(true)}>
              Draft ({legs.length})
            </button>
          </div>
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
                  {sgmContext && !events.some((event) => event.id === sgmContext.eventId) ? (
                    <option value={sgmContext.eventId}>{shortMatchLabel(sgmContext.eventLabel)} | saved draft</option>
                  ) : null}
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
            <Field label="Side">
              <Select value={normalizedMetricFilters.selectionType ?? ''} onChange={(event) => updateSelectionType(event.currentTarget.value || null)}>
                <option value="">Overs & unders</option>
                <option value="over">Overs only</option>
                <option value="under">Unders only</option>
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
            <Chip active={normalizedMetricFilters.selectionType === 'under'} onClick={() => updateSelectionType(normalizedMetricFilters.selectionType === 'under' ? null : 'under')}>
              Unders only
            </Chip>
            <Chip
              active={normalizedMetricFilters.minDiffLast10 >= 0}
              onClick={() => setMetricFilters({ ...normalizedMetricFilters, minDiffLast10: normalizedMetricFilters.minDiffLast10 >= 0 ? -1 : 0 })}
            >
              Positive L10
            </Chip>
            <Chip
              active={normalizedMetricFilters.minNextBestProbDiff >= 0}
              onClick={() => setMetricFilters({ ...normalizedMetricFilters, minNextBestProbDiff: normalizedMetricFilters.minNextBestProbDiff >= 0 ? -1 : 0 })}
            >
              Positive next best
            </Chip>
            <Chip
              active={normalizedMetricFilters.matchupDifficulties.length > 0}
              onClick={() =>
                setMetricFilters({
                  ...normalizedMetricFilters,
                  matchupDifficulties: normalizedMetricFilters.matchupDifficulties.length > 0 ? [] : [...favorableMatchups],
                })
              }
            >
              Favorable matchup
            </Chip>
            <Chip
              active={normalizedMetricFilters.favorableHomeAway}
              onClick={() => setMetricFilters({ ...normalizedMetricFilters, favorableHomeAway: !normalizedMetricFilters.favorableHomeAway })}
            >
              H/A edge
            </Chip>
            <Chip
              active={normalizedMetricFilters.favorableWinLoss}
              onClick={() => setMetricFilters({ ...normalizedMetricFilters, favorableWinLoss: !normalizedMetricFilters.favorableWinLoss })}
            >
              W/L edge
            </Chip>
            <Button variant="secondary" onClick={() => setShowMetricFilters(true)}>
              <Filter size={15} /> Filters
              {activeMetricFilterCount > 0 ? <span className="filter-count-badge">{activeMetricFilterCount}</span> : null}
            </Button>
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
            <span>{candidateQuery.isFetching ? 'Loading complete board' : displayMode === 'row' ? `Showing ${visibleRange} of ${visibleCandidates.length}` : `${visibleCandidates.length} selections`}</span>
          </div>
          {notice ? <p className="builder-notice" role="status">{notice}</p> : null}
          {visibleCandidates.length === 0 && !candidateQuery.isFetching ? (
            <EmptyState title="No eligible legs" body="Change agency, match, market, player search, or metric filters." />
          ) : displayMode === 'row' ? (
            <div className="candidate-table">
              <CandidateHeader sortField={sortField} descending={descending} onSort={handleSort} />
              <div ref={candidateScrollRef} className={clsx('candidate-virtual-scroll', candidateQuery.isFetching && 'is-refreshing')} aria-busy={candidateQuery.isFetching}>
                <div className="candidate-virtual-spacer" style={{ height: rowVirtualizer.getTotalSize() }}>
                  {virtualRows.map((virtualRow) => {
                    const selection = rowCandidates[virtualRow.index]
                    return (
                      <div
                        key={`${selection.selection_id}-${selection.bookmaker}`}
                        ref={rowVirtualizer.measureElement}
                        data-index={virtualRow.index}
                        className="candidate-virtual-item"
                        style={{ transform: `translateY(${virtualRow.start}px)` }}
                      >
                        <CandidateRow
                          mode={mode}
                          selection={selection}
                          selected={selectedSelectionIds.has(selection.selection_id)}
                          onToggle={() => toggleLeg(selection)}
                          onContextMenu={(event) => openContextMenu(event, selection)}
                          disabled={candidateQuery.isFetching || selection.decimal_price == null || (mode === 'sgm' && !selection.sgm_eligible)}
                        />
                      </div>
                    )
                  })}
                </div>
              </div>
            </div>
          ) : (
            <VirtualCandidateGrid
              groups={groups}
              mode={mode}
              selectedSelectionIds={selectedSelectionIds}
              isRefreshing={candidateQuery.isFetching}
              onToggle={toggleLeg}
              onContextMenu={openContextMenu}
            />
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
        context={mode === 'sgm' ? sgmContext : null}
        open={builderRailOpen}
        onClose={() => setBuilderRailOpen(false)}
        triggerRef={builderRailTriggerRef}
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

      {pendingSwitch ? (
        <ConfirmDialog
          message={`Switching clears your current draft. You currently have ${legs.length} leg${legs.length === 1 ? '' : 's'} selected.`}
          onConfirm={() => { pendingSwitch(); setPendingSwitch(null) }}
          onCancel={() => setPendingSwitch(null)}
        />
      ) : null}

      {showMetricFilters ? (
        <MetricFiltersDrawer
          filters={draftMetricFilters}
          onChange={setDraftMetricFilters}
          onApply={() => {
            setMetricFilters(normalizeMetricFilters(draftMetricFilters))
            setShowMetricFilters(false)
          }}
          onReset={() => setDraftMetricFilters(defaultMetricFilters)}
          onClose={() => setShowMetricFilters(false)}
        />
      ) : null}
    </main>
  )
}

const matchupOptions = ['Terrible', 'Bad', 'Neutral', 'Good', 'Excellent']

function MetricFiltersDrawer({
  filters,
  onChange,
  onApply,
  onReset,
  onClose,
}: {
  filters: MetricFilters
  onChange: (filters: MetricFilters) => void
  onApply: () => void
  onReset: () => void
  onClose: () => void
}) {
  const toggleMatchup = (difficulty: string) => {
    onChange({
      ...filters,
      matchupDifficulties: filters.matchupDifficulties.includes(difficulty)
        ? filters.matchupDifficulties.filter((item) => item !== difficulty)
        : [...filters.matchupDifficulties, difficulty],
    })
  }
  const updateSelectionType = (selectionType: string | null) => {
    onChange({
      ...filters,
      selectionType,
      matchupDifficulties: isFavorableMatchupSet(filters.matchupDifficulties)
        ? [...favorableMatchupDifficulties(selectionType)]
        : filters.matchupDifficulties,
    })
  }

  return (
    <div className="drawer-overlay" onClick={onClose}>
      <aside className="drawer" role="dialog" aria-modal="true" aria-label="Selection filters" onClick={(event) => event.stopPropagation()}>
        <div className="drawer-head">
          <div>
            <h2>Selection filters</h2>
            <p className="muted">Set exact thresholds for SGM and CGM candidate legs.</p>
          </div>
          <button type="button" className="modal-close" onClick={onClose} aria-label="Close filters">
            <X size={18} />
          </button>
        </div>
        <div className="drawer-foot">
          <Button variant="ghost" onClick={onReset}>Clear</Button>
          <Button variant="accent" onClick={onApply}>Apply filters</Button>
        </div>
        <div className="drawer-body">
          <section className="metric-filter-section">
            <h3>Side</h3>
            <div className="filter-chip-row">
              <Chip active={filters.selectionType == null} onClick={() => updateSelectionType(null)}>All</Chip>
              <Chip active={filters.selectionType === 'over'} onClick={() => updateSelectionType('over')}>Overs</Chip>
              <Chip active={filters.selectionType === 'under'} onClick={() => updateSelectionType('under')}>Unders</Chip>
            </div>
          </section>
          <section className="metric-filter-section">
            <h3>Matchup</h3>
            <div className="filter-chip-row">
              {matchupOptions.map((difficulty) => (
                <Chip key={difficulty} active={filters.matchupDifficulties.includes(difficulty)} onClick={() => toggleMatchup(difficulty)}>
                  {difficulty}
                </Chip>
              ))}
            </div>
          </section>

          <section className="metric-filter-section">
            <h3>Price</h3>
            <div className="filter-pair">
              <Field label="Min price">
                <TextInput
                  inputMode="decimal"
                  value={filters.minPrice}
                  placeholder="Any"
                  onChange={(event) => onChange({ ...filters, minPrice: event.currentTarget.value })}
                />
              </Field>
              <Field label="Max price">
                <TextInput
                  inputMode="decimal"
                  value={filters.maxPrice}
                  placeholder="Any"
                  onChange={(event) => onChange({ ...filters, maxPrice: event.currentTarget.value })}
                />
              </Field>
            </div>
          </section>

          <section className="metric-filter-section">
            <h3>Model thresholds</h3>
            <MetricNumberPair
              title="Last 10 diff"
              min={filters.minDiffLast10}
              max={filters.maxDiffLast10}
              onMinChange={(value) => onChange({ ...filters, minDiffLast10: value ?? defaultMetricFilters.minDiffLast10 })}
              onMaxChange={(value) => onChange({ ...filters, maxDiffLast10: value ?? defaultMetricFilters.maxDiffLast10 })}
            />
            <MetricNumberPair
              title="Season diff"
              min={filters.minDiff2025}
              max={filters.maxDiff2025}
              onMinChange={(value) => onChange({ ...filters, minDiff2025: value ?? defaultMetricFilters.minDiff2025 })}
              onMaxChange={(value) => onChange({ ...filters, maxDiff2025: value ?? defaultMetricFilters.maxDiff2025 })}
            />
            <MetricNumberPair
              title="Next best diff"
              min={filters.minNextBestProbDiff}
              max={filters.maxNextBestProbDiff}
              onMinChange={(value) => onChange({ ...filters, minNextBestProbDiff: value ?? defaultMetricFilters.minNextBestProbDiff })}
              onMaxChange={(value) => onChange({ ...filters, maxNextBestProbDiff: value ?? defaultMetricFilters.maxNextBestProbDiff })}
            />
            <MetricNumberPair
              title="Home / away diff"
              min={filters.minHomeAwayDiff}
              max={filters.maxHomeAwayDiff}
              onMinChange={(value) => onChange({ ...filters, minHomeAwayDiff: value })}
              onMaxChange={(value) => onChange({ ...filters, maxHomeAwayDiff: value })}
              optional
            />
            <MetricNumberPair
              title="Win / loss diff"
              min={filters.minWinLossDiff}
              max={filters.maxWinLossDiff}
              onMinChange={(value) => onChange({ ...filters, minWinLossDiff: value })}
              onMaxChange={(value) => onChange({ ...filters, maxWinLossDiff: value })}
              optional
            />
          </section>
        </div>
      </aside>
    </div>
  )
}

function MetricNumberPair({
  title,
  min,
  max,
  onMinChange,
  onMaxChange,
  optional = false,
}: {
  title: string
  min: number | null
  max: number | null
  onMinChange: (value: number | null) => void
  onMaxChange: (value: number | null) => void
  optional?: boolean
}) {
  return (
    <div className="metric-filter-range">
      <span>{title}</span>
      <div className="filter-pair">
        <Field label="Min">
          <TextInput
            type="number"
            step="0.05"
            value={formatMetricInputValue(min)}
            placeholder={optional ? 'Any' : undefined}
            onChange={(event) => onMinChange(parseMetricInputValue(event.currentTarget.value))}
          />
        </Field>
        <Field label="Max">
          <TextInput
            type="number"
            step="0.05"
            value={formatMetricInputValue(max)}
            placeholder={optional ? 'Any' : undefined}
            onChange={(event) => onMaxChange(parseMetricInputValue(event.currentTarget.value))}
          />
        </Field>
      </div>
    </div>
  )
}

function normalizeMetricFilters(filters: Partial<MetricFilters> | null | undefined): MetricFilters {
  return {
    ...defaultMetricFilters,
    ...filters,
    matchupDifficulties: filters?.matchupDifficulties ?? defaultMetricFilters.matchupDifficulties,
    minPrice: filters?.minPrice ?? defaultMetricFilters.minPrice,
    maxPrice: filters?.maxPrice ?? defaultMetricFilters.maxPrice,
    minDiff2025: filters?.minDiff2025 ?? defaultMetricFilters.minDiff2025,
    maxDiff2025: filters?.maxDiff2025 ?? defaultMetricFilters.maxDiff2025,
    minDiffLast10: filters?.minDiffLast10 ?? defaultMetricFilters.minDiffLast10,
    maxDiffLast10: filters?.maxDiffLast10 ?? defaultMetricFilters.maxDiffLast10,
    minHomeAwayDiff: filters?.minHomeAwayDiff ?? defaultMetricFilters.minHomeAwayDiff,
    maxHomeAwayDiff: filters?.maxHomeAwayDiff ?? defaultMetricFilters.maxHomeAwayDiff,
    minWinLossDiff: filters?.minWinLossDiff ?? defaultMetricFilters.minWinLossDiff,
    maxWinLossDiff: filters?.maxWinLossDiff ?? defaultMetricFilters.maxWinLossDiff,
    minNextBestProbDiff: filters?.minNextBestProbDiff ?? defaultMetricFilters.minNextBestProbDiff,
    maxNextBestProbDiff: filters?.maxNextBestProbDiff ?? defaultMetricFilters.maxNextBestProbDiff,
  }
}

function countActiveMetricFilters(filters: MetricFilters) {
  let count = 0
  if (filters.selectionType != null) count += 1
  if (filters.matchupDifficulties.length > 0) count += 1
  if (filters.minPrice.trim() !== '' || filters.maxPrice.trim() !== '') count += 1
  if (filters.minDiffLast10 !== defaultMetricFilters.minDiffLast10 || filters.maxDiffLast10 !== defaultMetricFilters.maxDiffLast10) count += 1
  if (filters.minDiff2025 !== defaultMetricFilters.minDiff2025 || filters.maxDiff2025 !== defaultMetricFilters.maxDiff2025) count += 1
  if (filters.minNextBestProbDiff !== defaultMetricFilters.minNextBestProbDiff || filters.maxNextBestProbDiff !== defaultMetricFilters.maxNextBestProbDiff) count += 1
  if (filters.minHomeAwayDiff != null || filters.maxHomeAwayDiff != null) count += 1
  if (filters.minWinLossDiff != null || filters.maxWinLossDiff != null) count += 1
  if (filters.favorableHomeAway) count += 1
  if (filters.favorableWinLoss) count += 1
  return count
}

function formatMetricInputValue(value: number | null) {
  return value == null ? '' : String(value)
}

function parseMetricInputValue(value: string) {
  const trimmed = value.trim()
  if (!trimmed) return null
  const parsed = Number(trimmed)
  return Number.isFinite(parsed) ? parsed : null
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
  const columns: { label: string; field?: SortField; numeric: boolean }[] = [
    { label: 'Loc', numeric: false },
    { label: 'Team line', field: 'player_team_line', numeric: true },
    { label: 'Line', field: 'line', numeric: true },
    { label: 'Price', field: 'price', numeric: true },
    { label: 'L10', field: 'diff_last_10', numeric: true },
    { label: 'Szn', field: 'diff_2025', numeric: true },
    { label: 'H/A', field: 'home_away_diff', numeric: true },
    { label: 'W/L', field: 'win_loss_diff', numeric: true },
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
        const field = column.field
        const active = field != null && sortField === field
        if (field == null) {
          return <div key={column.label} className={clsx('candidate-head-cell', column.numeric && 'col-num')}>{column.label}</div>
        }
        return (
          <button
            key={column.field}
            type="button"
            className={clsx('candidate-head-cell', column.numeric && 'col-num', active && 'is-active')}
            onClick={() => onSort(field)}
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

export function CandidateContextMenu({
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

function TeamContextTags({ selection }: { selection: Pick<OddsSearchResult, 'player_team' | 'player_home_away' | 'player_team_line'> }) {
  const teamCode = selection.player_team ? aflTeamCode(selection.player_team) : null
  return (
    <>
      {selection.player_home_away ? <span className="tag">{selection.player_home_away}</span> : null}
      {teamCode ? <span className="tag">{teamCode}</span> : null}
      <TeamLineBadge value={selection.player_team_line} />
    </>
  )
}

function TeamLineBadge({ value }: { value?: number | null }) {
  if (value == null) return null
  return <span className={clsx('team-line-badge', value >= 0 ? 'team-line-badge--win' : 'team-line-badge--loss')}>{formatSigned(value)}</span>
}

function TeamLineCell({ value }: { value?: number | null }) {
  return (
    <span className={clsx('col-num', 'team-line-cell', value == null ? 'team-line-cell--empty' : value >= 0 ? 'team-line-cell--win' : 'team-line-cell--loss')}>
      {formatSigned(value)}
    </span>
  )
}

function matchDetailLine(startTime?: string | null, venue?: string | null) {
  const parts = [formatShortDate(startTime), venue].filter((part) => part && part !== 'TBA')
  return parts.length ? parts.join(' | ') : 'TBA'
}

export function AgencyPriceDialog({
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
  mode,
  selection,
  selected,
  disabled,
  onToggle,
  onContextMenu,
}: {
  mode: BuilderMode
  selection: OddsSearchResult
  selected: boolean
  disabled: boolean
  onToggle: () => void
  onContextMenu: (event: React.MouseEvent) => void
}) {
  return (
    <button
      type="button"
      className={clsx('candidate-row', selected && 'is-selected', disabled && 'is-disabled')}
      tabIndex={disabled ? -1 : undefined}
      aria-pressed={selected}
      aria-disabled={disabled}
      title={disabled ? 'Not available for this builder' : selected ? 'Click to remove from draft' : 'Click to add to draft'}
      onClick={() => { if (!disabled) onToggle() }}
      onContextMenu={onContextMenu}
    >
      <div className="candidate-primary">
        <strong>
          {selected ? <Check size={13} className="candidate-check" /> : null}
          {selection.player?.full_name ?? selection.label}
        </strong>
        <span>
          {marketLabel(selection.market_type_code)} | {shortMatchLabel(selection.match_name)}
          {mode === 'cgm' ? ` | ${matchDetailLine(selection.start_time, selection.venue)}` : ''}
        </span>
      </div>
      <div className="candidate-context">
        {playerPositionTag(selection.player_position) ? <span className="tag">{playerPositionTag(selection.player_position)}</span> : null}
        <MatchupBadge value={rawMatchupDifficulty(selection)} />
      </div>
      <span className="candidate-home-away">{selection.player_home_away ?? '-'}</span>
      <TeamLineCell value={selection.player_team_line} />
      <span className="col-num">{lineWithSideLabel(selection)}</span>
      <b className="col-num tabular">{formatPrice(selection.decimal_price)}</b>
      <span className={clsx('col-num delta', (selection.diff_last_10 ?? -1) >= 0 ? 'delta--good' : 'delta--bad')}>{formatSigned(selection.diff_last_10)}</span>
      <span className={clsx('col-num delta', (selection.diff_2025 ?? -1) >= 0 ? 'delta--good' : 'delta--bad')}>{formatSigned(selection.diff_2025)}</span>
      <span className={clsx('col-num delta', (selection.home_away_diff ?? 0) >= 0 ? 'delta--good' : 'delta--bad')}>{formatSigned(selection.home_away_diff)}</span>
      <span className={clsx('col-num delta', (selection.win_loss_diff ?? 0) >= 0 ? 'delta--good' : 'delta--bad')}>{formatSigned(selection.win_loss_diff)}</span>
      <span className={clsx('col-num delta', (selection.next_best_prob_diff ?? -1) >= 0 ? 'delta--good' : 'delta--bad')}>{formatSigned(selection.next_best_prob_diff)}</span>
    </button>
  )
}

type CandidateGroup = ReturnType<typeof buildCandidateGroups>[number]

function VirtualCandidateGrid({
  groups,
  mode,
  selectedSelectionIds,
  isRefreshing,
  onToggle,
  onContextMenu,
}: {
  groups: CandidateGroup[]
  mode: BuilderMode
  selectedSelectionIds: Set<number>
  isRefreshing: boolean
  onToggle: (selection: OddsSearchResult) => void
  onContextMenu: (event: React.MouseEvent, selection: OddsSearchResult) => void
}) {
  const scrollRef = useRef<HTMLDivElement>(null)
  const [containerWidth, setContainerWidth] = useState(0)

  useEffect(() => {
    const element = scrollRef.current
    if (!element) return
    const updateWidth = () => setContainerWidth(element.clientWidth)
    updateWidth()
    const observer = new ResizeObserver(updateWidth)
    observer.observe(element)
    return () => observer.disconnect()
  }, [])

  const columnCount = Math.max(1, Math.floor((containerWidth + 10) / 260))
  const groupRows = useMemo(() => {
    const rows: CandidateGroup[][] = []
    for (let index = 0; index < groups.length; index += columnCount) rows.push(groups.slice(index, index + columnCount))
    return rows
  }, [columnCount, groups])
  // TanStack Virtual intentionally returns function-bearing instances local to this component.
  // eslint-disable-next-line react-hooks/incompatible-library
  const virtualizer = useVirtualizer({
    count: groupRows.length,
    getScrollElement: () => scrollRef.current,
    estimateSize: () => 250,
    overscan: 4,
  })

  useEffect(() => {
    virtualizer.scrollToOffset(0)
  }, [columnCount, groups, virtualizer])

  return (
    <div ref={scrollRef} className={clsx('candidate-virtual-scroll candidate-grid-scroll', isRefreshing && 'is-refreshing')} aria-busy={isRefreshing}>
      <div className="candidate-virtual-spacer" style={{ height: virtualizer.getTotalSize() }}>
        {virtualizer.getVirtualItems().map((virtualRow) => (
          <div
            key={virtualRow.key}
            ref={virtualizer.measureElement}
            data-index={virtualRow.index}
            className="candidate-virtual-item candidate-grid-row"
            style={{
              gridTemplateColumns: `repeat(${columnCount}, minmax(250px, 1fr))`,
              transform: `translateY(${virtualRow.start}px)`,
            }}
          >
            {groupRows[virtualRow.index].map((group) => (
              <CandidateTile
                key={group.key}
                group={group}
                mode={mode}
                selectedSelectionIds={selectedSelectionIds}
                disabled={isRefreshing}
                onToggle={onToggle}
                onContextMenu={onContextMenu}
              />
            ))}
          </div>
        ))}
      </div>
    </div>
  )
}

function CandidateTile({
  group,
  mode,
  selectedSelectionIds,
  disabled,
  onToggle,
  onContextMenu,
}: {
  group: CandidateGroup
  mode: BuilderMode
  selectedSelectionIds: Set<number>
  disabled: boolean
  onToggle: (selection: OddsSearchResult) => void
  onContextMenu: (event: React.MouseEvent, selection: OddsSearchResult) => void
}) {
  return (
    <div className="candidate-tile">
      <div className="tile-head">
        <strong>{group.title}</strong>
        <span>{group.subtitle}</span>
        <div className="tag-row">
          {playerPositionTag(group.playerPosition) ? <span className="tag">{playerPositionTag(group.playerPosition)}</span> : null}
          <MatchupBadge value={group.matchupDifficulty} />
          <TeamContextTags selection={group.selections[0]} />
        </div>
      </div>
      <div className="tile-lines">
        {group.selections.map((selection) => (
          <button
            type="button"
            key={selection.selection_id}
            className={selectedSelectionIds.has(selection.selection_id) ? 'is-selected' : ''}
            disabled={disabled || selection.decimal_price == null || (mode === 'sgm' && !selection.sgm_eligible)}
            onClick={() => onToggle(selection)}
            onContextMenu={(event) => onContextMenu(event, selection)}
          >
            <span>{selectionTypeLabel(selection.selection_type)} {formatPrice(selection.decimal_price)}</span>
            <b>{selection.line_value ?? '-'}</b>
          </button>
        ))}
      </div>
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
  context,
  open,
  onClose,
  triggerRef,
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
  context: ReturnType<typeof useAppStore.getState>['sgmContext']
  open: boolean
  onClose: () => void
  triggerRef: React.RefObject<HTMLButtonElement | null>
  onCompare: () => void
  onClear: () => void
  onRemove: (selectionId: number) => void
}) {
  const legBySelectionId = useMemo(() => new Map(legs.map((leg) => [leg.selection_id, leg])), [legs])
  return (
    <AdaptiveRail open={open} onClose={onClose} label={`${mode.toUpperCase()} draft`} className="builder-panel" triggerRef={triggerRef}>
      <div className="builder-panel-head">
        <div>
          <h2>{mode.toUpperCase()} draft</h2>
          <span>{legs.length} legs</span>
          {mode === 'sgm' && context ? <small className="draft-context">{bookmakerLabel(context.bookmaker)} · {shortMatchLabel(context.eventLabel)}</small> : null}
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
                  <TeamContextTags selection={leg} />
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
    </AdaptiveRail>
  )
}

function SgmComparisonCard({ result, rank }: { result: SgmAgencyComparison; rank: number; legBySelectionId: Map<number, DraftLeg> }) {
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
            <b className="tabular">{formatPrice(leg.base_price)}</b>
          </div>
        ))}
      </div>
      <small className="comparison-foot">Quoted {formatDateTime(result.quoted_at)}</small>
    </div>
  )
}

function CgmComparisonCard({ result, rank }: { result: CgmAgencyComparison; rank: number; legBySelectionId: Map<number, DraftLeg> }) {
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
          <div className="comparison-leg" key={leg.selection_id}>
            <span>{leg.label}</span>
            <b className="tabular">{formatPrice(leg.base_price)}</b>
          </div>
        ))}
      </div>
    </div>
  )
}
