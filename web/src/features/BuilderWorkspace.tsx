import { useMemo, useState } from 'react'
import { Check, Filter, Trash2, X } from 'lucide-react'
import type { BookmakerSummary, BuilderMode, CgmAgencyComparison, DraftLeg, EventSummary, OddsSearchResult, SgmAgencyComparison, SortField } from '../api/types'
import { useBuilderOdds, useCompareCgm, useCompareSgm } from '../api/queries'
import { defaultMetricFilters, useAppStore, useClientSettings } from '../store/useAppStore'
import { allMarketCode, buildCandidateGroups, combinedBasePrice, defaultDescending, orderedMarketCodes, sortCandidateRows, toDraftLeg } from '../lib/builder'
import { bookmakerLabel, formatDateTime, formatPrice, formatSigned, marketLabel, selectionTypeLabel, shortMatchLabel } from '../lib/formatters'
import { Button, Chip, EmptyState, ErrorBanner, Field, Panel, Segmented, Select, StatPill, Toggle } from '../components/ui'

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

  const enabledBookmakers = useMemo(() => bookmakers.filter((bookmaker) => bookmaker.enabled), [bookmakers])
  const firstBookmaker = enabledBookmakers.find((bookmaker) => bookmaker.code === selectedDefault)?.code ?? enabledBookmakers[0]?.code ?? ''
  const [bookmaker, setBookmaker] = useState(firstBookmaker)
  const [sgmEventId, setSgmEventId] = useState<number | null>(events[0]?.id ?? null)
  const [selectedEventIds, setSelectedEventIds] = useState<Set<number>>(new Set())
  const [bestOnly, setBestOnly] = useState(false)
  const [selectedMarket, setSelectedMarket] = useState(allMarketCode)
  const [sortField, setSortField] = useState<SortField>('next_best')
  const [descending, setDescending] = useState(true)

  const legs = mode === 'sgm' ? sgmLegs : cgmLegs
  const selectedSelectionIds = useMemo(() => new Set(legs.map((leg) => leg.selection_id)), [legs])
  const eventIds = mode === 'sgm' ? (sgmEventId == null ? [] : [sgmEventId]) : Array.from(selectedEventIds)
  const effectiveEventIds = mode === 'cgm' && eventIds.length === 0 ? [] : eventIds
  const candidateQuery = useBuilderOdds(settings, bookmaker, effectiveEventIds, metricFilters, bestOnly, Boolean(bookmaker) && (mode === 'cgm' || sgmEventId != null))
  const candidates = useMemo(() => {
    const rows = candidateQuery.data ?? []
    if (mode === 'cgm') {
      const draftedEventIds = new Set(cgmLegs.map((leg) => leg.event_id))
      return rows.filter((row) => !draftedEventIds.has(row.event_id))
    }
    return rows
  }, [candidateQuery.data, cgmLegs, mode])
  const marketCodes = useMemo(() => [allMarketCode, ...orderedMarketCodes(candidates)], [candidates])
  const visibleCandidates = useMemo(
    () => candidates.filter((row) => selectedMarket === allMarketCode || row.market_type_code === selectedMarket),
    [candidates, selectedMarket],
  )
  const rowCandidates = useMemo(() => sortCandidateRows(visibleCandidates, sortField, descending), [visibleCandidates, sortField, descending])
  const groups = useMemo(() => buildCandidateGroups(visibleCandidates), [visibleCandidates])
  const compareSgm = useCompareSgm(settings)
  const compareCgm = useCompareCgm(settings)

  const toggleLeg = (selection: OddsSearchResult) => {
    const draftLeg = toDraftLeg(selection)
    if (!draftLeg) return
    if (mode === 'sgm') addSgmLeg(draftLeg)
    else addCgmLeg(draftLeg)
  }

  const compare = () => {
    if (mode === 'sgm') {
      const eventId = legs[0]?.event_id ?? sgmEventId
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
              <Select value={bookmaker} onChange={(event) => setBookmaker(event.currentTarget.value)}>
                {enabledBookmakers.map((item) => (
                  <option key={item.code} value={item.code}>{item.display_name}</option>
                ))}
              </Select>
            </Field>
            {mode === 'sgm' ? (
              <Field label="Match">
                <Select value={sgmEventId ?? ''} onChange={(event) => setSgmEventId(event.currentTarget.value ? Number(event.currentTarget.value) : null)}>
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
            <Field label="Sort">
              <Select
                value={sortField}
                onChange={(event) => {
                  const field = event.currentTarget.value as SortField
                  setSortField(field)
                  setDescending(defaultDescending(field))
                }}
              >
                <option value="next_best">Next best diff</option>
                <option value="diff_last_10">Last-10 diff</option>
                <option value="diff_2025">Season diff</option>
                <option value="price">Price</option>
                <option value="player">Player</option>
                <option value="line">Line</option>
              </Select>
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
          {visibleCandidates.length === 0 && !candidateQuery.isFetching ? (
            <EmptyState title="No eligible legs" body="Change agency, match, market, or metric filters." />
          ) : displayMode === 'row' ? (
            <div className="candidate-list">
              {rowCandidates.map((selection) => (
                <CandidateRow
                  key={`${selection.selection_id}-${selection.bookmaker}`}
                  selection={selection}
                  selected={selectedSelectionIds.has(selection.selection_id)}
                  onToggle={() => toggleLeg(selection)}
                  disabled={selection.decimal_price == null || (mode === 'sgm' && !selection.sgm_eligible)}
                />
              ))}
            </div>
          ) : (
            <div className="candidate-grid">
              {groups.map((group) => (
                <div className="candidate-tile" key={group.key}>
                  <div className="tile-head">
                    <strong>{group.title}</strong>
                    <span>{group.subtitle}</span>
                  </div>
                  <div className="tile-lines">
                    {group.selections.map((selection) => (
                      <button
                        type="button"
                        key={selection.selection_id}
                        className={selectedSelectionIds.has(selection.selection_id) ? 'is-selected' : ''}
                        disabled={selection.decimal_price == null || (mode === 'sgm' && !selection.sgm_eligible)}
                        onClick={() => toggleLeg(selection)}
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
    </main>
  )
}

function CandidateRow({
  selection,
  selected,
  disabled,
  onToggle,
}: {
  selection: OddsSearchResult
  selected: boolean
  disabled: boolean
  onToggle: () => void
}) {
  return (
    <div className="candidate-row">
      <div className="candidate-primary">
        <strong>{selection.player?.full_name ?? selection.label}</strong>
        <span>{marketLabel(selection.market_type_code)} | {shortMatchLabel(selection.match_name)}</span>
      </div>
      <span>{selectionTypeLabel(selection.selection_type)} {selection.line_value ?? ''}</span>
      <b className="tabular">{formatPrice(selection.decimal_price)}</b>
      <span className={(selection.diff_last_10 ?? -1) >= 0 ? 'delta delta--good' : 'delta delta--bad'}>{formatSigned(selection.diff_last_10)}</span>
      <span className={(selection.next_best_prob_diff ?? -1) >= 0 ? 'delta delta--good' : 'delta delta--bad'}>{formatSigned(selection.next_best_prob_diff)}</span>
      <Button variant={selected ? 'primary' : 'secondary'} disabled={disabled} onClick={onToggle}>
        {selected ? <Check size={15} /> : null}
        {selected ? 'Selected' : 'Add'}
      </Button>
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
        {(mode === 'sgm' ? sgmResults : cgmResults).map((result) => (
          <div className="quote-row" key={'quote_id' in result ? result.quote_id : result.bookmaker}>
            <span>{bookmakerLabel(result.bookmaker)}</span>
            <b>{formatPrice(result.quoted_price)}</b>
            {'adjustment_factor' in result ? <small>{result.from_cache ? 'cache' : 'live'} | x{result.adjustment_factor.toFixed(3)}</small> : null}
          </div>
        ))}
      </div>
    </aside>
  )
}
