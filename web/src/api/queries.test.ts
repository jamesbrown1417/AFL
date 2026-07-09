import { describe, expect, it, vi } from 'vitest'
import type { OddsSearchResult } from './types'
import { BUILDER_ODDS_PAGE_SIZE, fetchAllBuilderOdds, oddsFiltersToQuery } from './queries'
import { defaultOddsFilters } from '../store/useAppStore'

function row(selectionId: number, bookmaker = 'sportsbet') {
  return {
    selection_id: selectionId,
    bookmaker,
    market_type_code: 'player_disposals',
  } as OddsSearchResult
}

describe('fetchAllBuilderOdds', () => {
  it('returns a complete single page', async () => {
    const fetchPage = vi.fn().mockResolvedValue([row(1), row(2)])
    const result = await fetchAllBuilderOdds(fetchPage, { scope: 'player' })

    expect(result.map((item) => item.selection_id)).toEqual([1, 2])
    expect(fetchPage).toHaveBeenCalledWith(expect.objectContaining({ limit: BUILDER_ODDS_PAGE_SIZE, offset: 0 }))
  })

  it('loads additional pages and deduplicates agency selections', async () => {
    const firstPage = Array.from({ length: BUILDER_ODDS_PAGE_SIZE }, (_, index) => row(index))
    const fetchPage = vi.fn()
      .mockResolvedValueOnce(firstPage)
      .mockResolvedValueOnce([row(BUILDER_ODDS_PAGE_SIZE - 1), row(BUILDER_ODDS_PAGE_SIZE)])

    const result = await fetchAllBuilderOdds(fetchPage, { scope: 'player' })

    expect(result).toHaveLength(BUILDER_ODDS_PAGE_SIZE + 1)
    expect(fetchPage.mock.calls.map(([query]) => query.offset)).toEqual([0, BUILDER_ODDS_PAGE_SIZE])
  })

  it('rejects the complete load when a later page fails', async () => {
    const firstPage = Array.from({ length: BUILDER_ODDS_PAGE_SIZE }, (_, index) => row(index))
    const fetchPage = vi.fn()
      .mockResolvedValueOnce(firstPage)
      .mockRejectedValueOnce(new Error('second page failed'))

    await expect(fetchAllBuilderOdds(fetchPage, { scope: 'player' })).rejects.toThrow('second page failed')
  })
})

describe('oddsFiltersToQuery', () => {
  it('maps under and contextual edge filters to the backend query', () => {
    const query = oddsFiltersToQuery({
      ...defaultOddsFilters,
      selectionType: 'under',
      favorableHomeAway: true,
      favorableWinLoss: true,
      minHomeAwayDiff: -4,
      maxWinLossDiff: 6,
    })

    expect(query).toEqual(expect.objectContaining({
      selection_type: 'under',
      favorable_home_away: true,
      favorable_win_loss: true,
      min_home_away_diff: -4,
      max_win_loss_diff: 6,
    }))
  })
})
