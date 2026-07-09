import { describe, expect, it } from 'vitest'
import type { OddsSearchResult } from '../api/types'
import { favorableMatchupDifficulties, rawMatchupDifficulty, sortCandidateRows } from './builder'

function candidate(selectionId: number, nextBest: number, name: string) {
  return {
    selection_id: selectionId,
    next_best_prob_diff: nextBest,
    label: name,
    line_value: 19.5,
    selection_type: 'over',
  } as OddsSearchResult
}

describe('sortCandidateRows', () => {
  it('sorts the complete candidate set before rendering', () => {
    const rows = [
      candidate(1, 0.08, 'Middle'),
      candidate(2, 0.31, 'Best'),
      candidate(3, -0.12, 'Worst'),
    ]

    expect(sortCandidateRows(rows, 'next_best', true).map((row) => row.selection_id)).toEqual([2, 1, 3])
  })

  it('switches favorable matchup labels for unders', () => {
    expect(favorableMatchupDifficulties('over')).toEqual(['Neutral', 'Good', 'Excellent'])
    expect(favorableMatchupDifficulties('under')).toEqual(['Neutral', 'Bad', 'Terrible'])
  })

  it('uses the raw over-side matchup label for display', () => {
    expect(rawMatchupDifficulty({ over_matchup_difficulty: 'Terrible', matchup_difficulty: 'Excellent' })).toBe('Terrible')
  })
})
