import { Activity, BarChart3, Database, GitCompareArrows, Percent, RefreshCw, Settings, SlidersHorizontal, Trophy, UserRound } from 'lucide-react'
import clsx from 'clsx'
import { useQueryClient } from '@tanstack/react-query'
import type { ReactNode } from 'react'
import type { DataStatusResponse, HealthResponse } from '../api/types'
import { formatDateTime } from '../lib/formatters'
import { useAppStore } from '../store/useAppStore'
import { IconButton } from './ui'

const nav = [
  { key: 'player', label: 'Player', icon: UserRound },
  { key: 'odds', label: 'Odds', icon: BarChart3 },
  { key: 'arbs', label: 'Arbs', icon: Percent },
  { key: 'sgm', label: 'SGM', icon: Trophy },
  { key: 'cgm', label: 'CGM', icon: GitCompareArrows },
  { key: 'settings', label: 'Settings', icon: Settings },
] as const

export function AppShell({
  health,
  dataStatus,
  children,
}: {
  health?: HealthResponse
  dataStatus?: DataStatusResponse
  children: ReactNode
}) {
  const activeView = useAppStore((state) => state.activeView)
  const setActiveView = useAppStore((state) => state.setActiveView)
  const themeMode = useAppStore((state) => state.themeMode)
  const queryClient = useQueryClient()

  return (
    <div className={clsx('app-shell', `theme-${themeMode}`)}>
      <a href="#main-content" className="skip-link">Skip to main content</a>
      <aside className="sidebar">
        <div className="brand">
          <div className="brand-mark">AE</div>
          <div>
            <strong>AFL Edge</strong>
            <span>Odds workstation</span>
          </div>
        </div>
        <nav className="side-nav" aria-label="Main navigation">
          {nav.map((item) => {
            const Icon = item.icon
            return (
              <button
                key={item.key}
                type="button"
                aria-label={item.label}
                aria-current={activeView === item.key ? 'page' : undefined}
                className={clsx(activeView === item.key && 'is-active')}
                onClick={() => setActiveView(item.key)}
              >
                <Icon size={18} />
                <span>{item.label}</span>
              </button>
            )
          })}
        </nav>
      </aside>

      <div className="app-frame" id="main-content">
        <header className="topbar">
          <div className="status-cluster">
            <div className="status-item">
              <Activity size={16} />
              <span>Backend</span>
              <b className={health?.database_ok ? 'good' : 'bad'}>{health?.database_ok ? 'Online' : 'Check'}</b>
            </div>
            <div className="status-item">
              <Database size={16} />
              <span>Import</span>
              <b>{formatDateTime(health?.last_successful_import_at)}</b>
            </div>
            <div className="status-item hide-small">
              <SlidersHorizontal size={16} />
              <span>Data</span>
              <b>{formatDateTime(dataStatus?.generated_at)}</b>
            </div>
          </div>
          <IconButton label="Refresh data" onClick={() => void queryClient.invalidateQueries()}>
            <RefreshCw size={18} />
          </IconButton>
        </header>
        {children}
      </div>
    </div>
  )
}
