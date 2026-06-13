export function formatDateTime(value?: string | null) {
  if (!value) return 'TBA'
  const date = new Date(value)
  if (Number.isNaN(date.getTime())) return value
  return new Intl.DateTimeFormat(undefined, {
    weekday: 'short',
    day: 'numeric',
    month: 'short',
    hour: 'numeric',
    minute: '2-digit',
  }).format(date)
}

export function formatMatchDateTime(value?: string | null) {
  if (!value) return 'TBA'
  const date = new Date(value)
  if (Number.isNaN(date.getTime())) return value
  return new Intl.DateTimeFormat(undefined, {
    weekday: 'short',
    day: 'numeric',
    month: 'short',
    year: 'numeric',
    hour: 'numeric',
    minute: '2-digit',
  }).format(date)
}

export function formatShortDate(value?: string | null) {
  if (!value) return 'TBA'
  const date = new Date(value)
  if (Number.isNaN(date.getTime())) return value
  return new Intl.DateTimeFormat(undefined, { day: 'numeric', month: 'short' }).format(date)
}

export function formatPrice(value?: number | null) {
  return value == null ? '--' : value.toFixed(2)
}

export function formatPercent(value?: number | null) {
  return value == null ? '--' : `${(value * 100).toFixed(1)}%`
}

export function formatSigned(value?: number | null) {
  return value == null ? '--' : `${value >= 0 ? '+' : ''}${value.toFixed(2)}`
}

export function formatLine(value?: number | null) {
  if (value == null) return '-'
  return Number.isInteger(value) ? value.toFixed(0) : value.toFixed(1)
}

export function selectionTypeLabel(value: string) {
  const labels: Record<string, string> = {
    over: 'Over',
    under: 'Under',
    home: 'Home',
    away: 'Away',
  }
  return labels[value] ?? titleize(value)
}

export function bookmakerLabel(code: string) {
  const known: Record<string, string> = {
    sportsbet: 'Sportsbet',
    tab: 'TAB',
    pointsbet: 'PointsBet',
    bet365: 'bet365',
    neds: 'Neds',
    dabble: 'Dabble',
    betr: 'Betr',
    betright: 'BetRight',
    betfair: 'Betfair',
  }
  return known[code] ?? titleize(code)
}

export function marketLabel(code: string | null | undefined) {
  const labels: Record<string, string> = {
    __all__: 'All',
    player_disposals: 'Disposals',
    player_fantasy_points: 'Fantasy',
    player_goals: 'Goals',
    player_marks: 'Marks',
    player_tackles: 'Tackles',
    player_kicks: 'Kicks',
    player_handballs: 'Handballs',
    player_hitouts: 'Hitouts',
    player_clearances: 'Clearances',
    total_points: 'Totals',
    line: 'Line',
    h2h: 'H2H',
  }
  return code ? labels[code] ?? titleize(code.replaceAll('_', ' ')) : 'All'
}

export function shortMatchLabel(matchName: string) {
  const normalized = matchName.replace(/\s+vs\s+/i, ' v ')
  const parts = normalized.split(' v ')
  if (parts.length !== 2) return matchName
  const home = aflTeamCode(parts[0])
  const away = aflTeamCode(parts[1])
  return home && away ? `${home} v ${away}` : matchName
}

export function playerPositionTag(value?: string | null) {
  if (!value) return null
  const normalized = value.trim().toUpperCase().replaceAll('-', '_')
  const tags: Record<string, string> = {
    KEY_DEFENDER: 'KDEF',
    MEDIUM_DEFENDER: 'MDEF',
    KEY_FORWARD: 'KFWD',
    MEDIUM_FORWARD: 'MFWD',
    MIDFIELDER: 'MID',
    MIDFIELDER_FORWARD: 'MID/F',
    RUCK: 'RUC',
  }
  return tags[normalized] ?? normalized.replaceAll('_', ' ')
}

export function titleize(value: string) {
  return value.replace(/\w\S*/g, (word) => word.charAt(0).toUpperCase() + word.slice(1).toLowerCase())
}

function aflTeamCode(teamName: string) {
  const normalized = teamName.trim().toLowerCase().replaceAll('.', '')
  if (normalized.includes('port adelaide') || normalized.startsWith('port ') || normalized.includes(' power')) return 'PTA'
  if (normalized.includes('north melbourne') || normalized.includes('kangaroos')) return 'NTH'
  if (normalized === 'adelaide' || normalized.includes('adelaide crows') || normalized.endsWith(' crows')) return 'ADE'
  if (normalized.includes('brisbane')) return 'BRL'
  if (normalized.includes('carlton')) return 'CAR'
  if (normalized.includes('collingwood')) return 'COL'
  if (normalized.includes('essendon')) return 'ESS'
  if (normalized.includes('fremantle')) return 'FRE'
  if (normalized.includes('geelong')) return 'GEE'
  if (normalized.includes('gold coast')) return 'GCS'
  if (normalized.includes('greater western sydney') || normalized.includes('gws')) return 'GWS'
  if (normalized.includes('hawthorn')) return 'HAW'
  if (normalized.includes('melbourne')) return 'MEL'
  if (normalized.includes('richmond')) return 'RIC'
  if (normalized.includes('st kilda')) return 'STK'
  if (normalized.includes('sydney')) return 'SYD'
  if (normalized.includes('west coast')) return 'WCE'
  if (normalized.includes('western bulldogs') || normalized.includes('bulldogs') || normalized.includes('footscray')) return 'WBD'
  return null
}
