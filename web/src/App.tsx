import { lazy, Suspense, useEffect, useMemo, useState } from 'react'
import { useBookmakers, useDataStatus, useEvents, useHealth } from './api/queries'
import { AppShell } from './components/AppShell'
import { ErrorBoundary } from './components/ErrorBoundary'
import { useAppStore, useClientSettings } from './store/useAppStore'
import './App.css'

const OddsWorkspace = lazy(() => import('./features/OddsWorkspace').then((module) => ({ default: module.OddsWorkspace })))
const ArbWorkspace = lazy(() => import('./features/ArbWorkspace').then((module) => ({ default: module.ArbWorkspace })))
const PlayerLab = lazy(() => import('./features/PlayerLab').then((module) => ({ default: module.PlayerLab })))
const BuilderWorkspace = lazy(() => import('./features/BuilderWorkspace').then((module) => ({ default: module.BuilderWorkspace })))
const SettingsView = lazy(() => import('./features/SettingsView').then((module) => ({ default: module.SettingsView })))

function App() {
  const settings = useClientSettings()
  const activeView = useAppStore((state) => state.activeView)
  const themeMode = useAppStore((state) => state.themeMode)
  const setActiveView = useAppStore((state) => state.setActiveView)
  const health = useHealth(settings)
  const dataStatus = useDataStatus(settings)
  const bookmakers = useBookmakers(settings)
  const defaultBookmaker = useAppStore((state) => state.defaultBookmaker)
  const events = useEvents(settings, defaultBookmaker)
  const [mountedBuilderViews, setMountedBuilderViews] = useState({
    sgm: activeView === 'sgm',
    cgm: activeView === 'cgm',
  })

  useEffect(() => {
    document.documentElement.dataset.theme = themeMode
  }, [themeMode])

  useEffect(() => {
    if (activeView === 'sgm' || activeView === 'cgm') return
    if (!['player', 'odds', 'arbs', 'settings'].includes(activeView)) setActiveView('odds')
  }, [activeView, setActiveView])

  useEffect(() => {
    if (activeView !== 'sgm' && activeView !== 'cgm') return
    setMountedBuilderViews((current) => (
      current[activeView] ? current : { ...current, [activeView]: true }
    ))
  }, [activeView])

  const bookmakerRows = useMemo(() => bookmakers.data ?? [], [bookmakers.data])
  const eventRows = useMemo(() => events.data ?? [], [events.data])

  return (
    <ErrorBoundary>
    <AppShell health={health.data} dataStatus={dataStatus.data}>
      <Suspense fallback={<div className="screen-loading">Loading workspace</div>}>
        {activeView === 'odds' ? <OddsWorkspace bookmakers={bookmakerRows} events={eventRows} /> : null}
        {activeView === 'arbs' ? <ArbWorkspace bookmakers={bookmakerRows} /> : null}
        {activeView === 'player' ? <PlayerLab /> : null}
        {mountedBuilderViews.sgm ? (
          <div hidden={activeView !== 'sgm'}>
            <BuilderWorkspace mode="sgm" bookmakers={bookmakerRows} events={eventRows} />
          </div>
        ) : null}
        {mountedBuilderViews.cgm ? (
          <div hidden={activeView !== 'cgm'}>
            <BuilderWorkspace mode="cgm" bookmakers={bookmakerRows} events={eventRows} />
          </div>
        ) : null}
        {activeView === 'settings' ? <SettingsView bookmakers={bookmakerRows} health={health.data} dataStatus={dataStatus.data} /> : null}
      </Suspense>
    </AppShell>
    </ErrorBoundary>
  )
}

export default App
