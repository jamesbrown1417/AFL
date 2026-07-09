import { beforeEach, describe, expect, it, vi } from 'vitest'
import type { DraftLeg } from '../api/types'
import { migratePersistedAppState, sgmContextFromLeg, useAppStore } from './useAppStore'

function leg(selectionId: number, eventId = 10, bookmaker = 'sportsbet'): DraftLeg {
  return {
    selection_id: selectionId,
    event_id: eventId,
    event_label: eventId === 10 ? 'AAA v BBB' : 'CCC v DDD',
    bookmaker,
    label: `Player ${selectionId} Over 19.5`,
    market_type_code: 'player_disposals',
    selection_type: 'over',
    base_price: 1.9,
    start_time: null,
    venue: null,
    player_team: null,
    player_home_away: null,
    player_team_line: null,
    diff_2025: null,
    diff_last_10: null,
    home_away_diff: null,
    win_loss_diff: null,
    next_best_prob_diff: null,
    is_best_price: false,
  }
}

describe('SGM draft context', () => {
  beforeEach(() => {
    vi.useRealTimers()
    useAppStore.setState({ sgmLegs: [], sgmContext: null, sgmUndo: null })
  })

  it('appends compatible legs and retains their context', () => {
    useAppStore.getState().addSgmLeg(leg(1))
    useAppStore.getState().addSgmLeg(leg(2))

    expect(useAppStore.getState().sgmLegs.map((item) => item.selection_id)).toEqual([1, 2])
    expect(useAppStore.getState().sgmContext).toEqual(sgmContextFromLeg(leg(1)))
    expect(useAppStore.getState().sgmUndo).toBeNull()
  })

  it('replaces an incompatible draft and restores it with undo', () => {
    vi.useFakeTimers()
    vi.setSystemTime(new Date('2026-07-10T00:00:00Z'))
    useAppStore.getState().addSgmLeg(leg(1))
    useAppStore.getState().addSgmLeg(leg(2))
    useAppStore.getState().addSgmLeg(leg(3, 20, 'neds'))

    expect(useAppStore.getState().sgmLegs.map((item) => item.selection_id)).toEqual([3])
    expect(useAppStore.getState().sgmUndo?.legs.map((item) => item.selection_id)).toEqual([1, 2])

    useAppStore.getState().undoSgmReplacement()
    expect(useAppStore.getState().sgmLegs.map((item) => item.selection_id)).toEqual([1, 2])
    expect(useAppStore.getState().sgmContext).toEqual(sgmContextFromLeg(leg(1)))
    expect(useAppStore.getState().sgmUndo).toBeNull()
  })

  it('invalidates undo after the replacement draft is edited', () => {
    useAppStore.getState().addSgmLeg(leg(1))
    useAppStore.getState().addSgmLeg(leg(3, 20, 'neds'))
    useAppStore.getState().addSgmLeg(leg(4, 20, 'neds'))

    expect(useAppStore.getState().sgmUndo).toBeNull()
    useAppStore.getState().undoSgmReplacement()
    expect(useAppStore.getState().sgmLegs.map((item) => item.selection_id)).toEqual([3, 4])
  })

  it('does not restore an expired undo snapshot', () => {
    vi.useFakeTimers()
    vi.setSystemTime(new Date('2026-07-10T00:00:00Z'))
    useAppStore.getState().addSgmLeg(leg(1))
    useAppStore.getState().addSgmLeg(leg(3, 20, 'neds'))
    vi.advanceTimersByTime(8_001)

    useAppStore.getState().undoSgmReplacement()
    expect(useAppStore.getState().sgmLegs.map((item) => item.selection_id)).toEqual([3])
    expect(useAppStore.getState().sgmUndo).toBeNull()
  })

  it('clears context when the final leg is removed', () => {
    useAppStore.getState().addSgmLeg(leg(1))
    useAppStore.getState().removeSgmLeg(1)

    expect(useAppStore.getState().sgmLegs).toEqual([])
    expect(useAppStore.getState().sgmContext).toBeNull()
  })

  it('derives context when migrating a version-one draft', () => {
    const migrated = migratePersistedAppState({ sgmLegs: [leg(1)] }) as ReturnType<typeof useAppStore.getState>
    expect(migrated.sgmContext).toEqual(sgmContextFromLeg(leg(1)))
  })
})
