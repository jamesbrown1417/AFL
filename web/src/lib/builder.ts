import type { DraftLeg, OddsSearchResult, SortField } from '../api/types'
import { formatLine, marketLabel, selectionTypeLabel, shortMatchLabel } from './formatters'

export const allMarketCode = '__all__'
export const OVER_FAVORABLE_MATCHUPS = ['Neutral', 'Good', 'Excellent']
export const UNDER_FAVORABLE_MATCHUPS = ['Neutral', 'Bad', 'Terrible']

export function favorableMatchupDifficulties(selectionType: string | null) {
  return selectionType === 'under' ? UNDER_FAVORABLE_MATCHUPS : OVER_FAVORABLE_MATCHUPS
}

export function isFavorableMatchupSet(values: string[]) {
  const normalized = values.toSorted().join('|')
  return normalized === OVER_FAVORABLE_MATCHUPS.toSorted().join('|') || normalized === UNDER_FAVORABLE_MATCHUPS.toSorted().join('|')
}

export function rawMatchupDifficulty(selection: Pick<OddsSearchResult, 'over_matchup_difficulty' | 'matchup_difficulty'>) {
  return selection.over_matchup_difficulty ?? selection.matchup_difficulty
}

const preferredMarketOrder = [
  'player_disposals',
  'player_fantasy_points',
  'player_goals',
  'player_marks',
  'player_tackles',
  'player_kicks',
  'player_handballs',
  'player_hitouts',
  'player_clearances',
  'total_points',
  'line',
  'h2h',
]

export interface CandidateGroup {
  key: string
  title: string
  subtitle: string
  playerPosition: string | null
  matchupDifficulty: string | null
  selections: OddsSearchResult[]
}

export function orderedMarketCodes(legs: OddsSearchResult[]) {
  const order = new Map(preferredMarketOrder.map((code, index) => [code, index]))
  return Array.from(new Set(legs.map((leg) => leg.market_type_code))).sort((a, b) => {
    const byOrder = (order.get(a) ?? 999) - (order.get(b) ?? 999)
    return byOrder || marketLabel(a).localeCompare(marketLabel(b))
  })
}

export function buildCandidateGroups(legs: OddsSearchResult[]) {
  const groups = new Map<string, OddsSearchResult[]>()
  for (const leg of legs) {
    const key = leg.player ? `${leg.market_type_code}|player|${leg.player.id}` : `${leg.market_type_code}|match`
    groups.set(key, [...(groups.get(key) ?? []), leg])
  }
  return Array.from(groups.entries())
    .map(([key, selections]) => {
      const first = selections[0]
      return {
        key,
        title: first.player?.full_name ?? marketLabel(first.market_type_code),
        subtitle: first.player
          ? `${marketLabel(first.market_type_code)} | ${shortMatchLabel(first.match_name)}`
          : shortMatchLabel(first.match_name),
        playerPosition: first.player_position,
        matchupDifficulty: rawMatchupDifficulty(first),
        selections: selections.toSorted((a, b) => (a.line_value ?? 999) - (b.line_value ?? 999)),
      } satisfies CandidateGroup
    })
    .toSorted((a, b) => {
      const aBest = Math.max(...a.selections.map((selection) => selection.next_best_prob_diff ?? -999))
      const bBest = Math.max(...b.selections.map((selection) => selection.next_best_prob_diff ?? -999))
      return bBest - aBest || a.title.localeCompare(b.title)
    })
}

export function sortCandidateRows(legs: OddsSearchResult[], field: SortField, descending: boolean) {
  const direction = descending ? -1 : 1
  return legs.toSorted((a, b) => {
    const primary = compareSortValue(sortValue(a, field), sortValue(b, field)) * direction
    if (primary !== 0) return primary
    return (
      (a.player?.full_name ?? a.label).localeCompare(b.player?.full_name ?? b.label) ||
      (a.line_value ?? 999) - (b.line_value ?? 999) ||
      selectionTypeLabel(a.selection_type).localeCompare(selectionTypeLabel(b.selection_type))
    )
  })
}

export function defaultDescending(field: SortField) {
  return !['player', 'line'].includes(field)
}

export function lineWithSideLabel(selection: OddsSearchResult) {
  const line = formatLine(selection.line_value)
  if (selection.selection_type === 'over') return line === '-' ? 'Over' : `O ${line}`
  if (selection.selection_type === 'under') return line === '-' ? 'Under' : `U ${line}`
  return line === '-' ? selectionTypeLabel(selection.selection_type) : line
}

export function toDraftLeg(selection: OddsSearchResult): DraftLeg | null {
  if (selection.decimal_price == null) return null
  return {
    selection_id: selection.selection_id,
    event_id: selection.event_id,
    event_label: selection.match_name,
    bookmaker: selection.bookmaker,
    label: selection.label,
    market_type_code: selection.market_type_code,
    selection_type: selection.selection_type,
    base_price: selection.decimal_price,
    start_time: selection.start_time,
    venue: selection.venue,
    player_team: selection.player_team,
    player_home_away: selection.player_home_away,
    player_team_line: selection.player_team_line,
    diff_2025: selection.diff_2025,
    diff_last_10: selection.diff_last_10,
    home_away_diff: selection.home_away_diff,
    win_loss_diff: selection.win_loss_diff,
    next_best_prob_diff: selection.next_best_prob_diff,
    player_position: selection.player_position,
    matchup_difficulty: rawMatchupDifficulty(selection),
    is_best_price: selection.is_best_price,
  }
}

export function combinedBasePrice(legs: DraftLeg[]) {
  return legs.reduce((price, leg) => price * leg.base_price, legs.length ? 1 : 0)
}

const marketStatCodes: Record<string, string> = {
  player_disposals: 'disposals',
  player_fantasy_points: 'fantasy_points',
  player_tackles: 'tackles',
  player_marks: 'marks',
  player_goals: 'goals',
  player_kicks: 'kicks',
  player_handballs: 'handballs',
  player_hitouts: 'hitouts',
  player_clearances: 'clearances',
}

export function marketTypeToStatCode(marketTypeCode: string): string | null {
  return marketStatCodes[marketTypeCode] ?? null
}

function sortValue(selection: OddsSearchResult, field: SortField) {
  switch (field) {
    case 'player':
      return selection.player?.full_name ?? selection.label
    case 'line':
      return selection.line_value ?? 999
    case 'player_team_line':
      return selection.player_team_line ?? -999
    case 'price':
      return selection.decimal_price ?? -999
    case 'diff_2025':
      return selection.diff_2025 ?? -999
    case 'diff_last_10':
      return selection.diff_last_10 ?? -999
    case 'home_away_diff':
      return selection.home_away_diff ?? -999
    case 'win_loss_diff':
      return selection.win_loss_diff ?? -999
    case 'next_best':
      return selection.next_best_prob_diff ?? -999
  }
}

function compareSortValue(left: string | number, right: string | number) {
  if (typeof left === 'string' || typeof right === 'string') return `${left}`.localeCompare(`${right}`)
  return left - right
}
