import { useState } from 'react'
import type { BookmakerSummary, DataStatusResponse, HealthResponse } from '../api/types'
import { useAppStore } from '../store/useAppStore'
import { formatDateTime } from '../lib/formatters'
import { Button, Field, Panel, Select, TextInput, Toggle } from '../components/ui'

export function SettingsView({
  bookmakers,
  health,
  dataStatus,
}: {
  bookmakers: BookmakerSummary[]
  health?: HealthResponse
  dataStatus?: DataStatusResponse
}) {
  const apiBaseUrl = useAppStore((state) => state.apiBaseUrl)
  const authToken = useAppStore((state) => state.authToken)
  const defaultBookmaker = useAppStore((state) => state.defaultBookmaker)
  const themeMode = useAppStore((state) => state.themeMode)
  const current = { apiBaseUrl, authToken, defaultBookmaker, themeMode }
  const setSettings = useAppStore((state) => state.setSettings)
  const [draft, setDraft] = useState(current)
  const [saved, setSaved] = useState(false)

  return (
    <main className="workspace settings-workspace" aria-label="Settings">
      <section className="workspace-main">
        <div className="page-title-row">
          <div>
            <h1>Settings</h1>
            <p>Backend connection and workspace defaults</p>
          </div>
        </div>

        <Panel className="settings-panel">
          <div className="filter-grid">
            <Field label="API base URL">
              <TextInput value={draft.apiBaseUrl} onChange={(event) => setDraft({ ...draft, apiBaseUrl: event.currentTarget.value })} />
            </Field>
            <Field label="Bearer token">
              <TextInput
                value={draft.authToken}
                type="password"
                onChange={(event) => setDraft({ ...draft, authToken: event.currentTarget.value })}
                placeholder="Required when backend auth is enabled"
              />
            </Field>
            <Field label="Default agency">
              <Select value={draft.defaultBookmaker} onChange={(event) => setDraft({ ...draft, defaultBookmaker: event.currentTarget.value })}>
                {bookmakers.map((bookmaker) => (
                  <option key={bookmaker.code} value={bookmaker.code}>{bookmaker.display_name}</option>
                ))}
              </Select>
            </Field>
            <Toggle checked={draft.themeMode === 'dark'} onChange={(checked) => setDraft({ ...draft, themeMode: checked ? 'dark' : 'light' })} label="Dark mode" />
          </div>
          <div className="settings-actions">
            <Button onClick={() => { setSettings(draft); setSaved(true); setTimeout(() => setSaved(false), 2500) }}>
              {saved ? 'Saved!' : 'Save settings'}
            </Button>
            <Button variant="ghost" onClick={() => setDraft({ ...draft, apiBaseUrl: '/api/v1/' })}>Use Vite proxy</Button>
          </div>
        </Panel>

        <Panel className="status-panel">
          <div className="section-heading">
            <h2>Backend status</h2>
            <span>{health?.database_ok ? 'Online' : 'Unavailable'}</span>
          </div>
          <div className="status-list">
            <div><span>Status</span><b>{health?.status ?? '--'}</b></div>
            <div><span>Database</span><b>{health?.database_ok ? 'OK' : 'Check'}</b></div>
            <div><span>Last import</span><b>{formatDateTime(health?.last_successful_import_at)}</b></div>
            <div><span>Data generated</span><b>{formatDateTime(dataStatus?.generated_at)}</b></div>
          </div>
        </Panel>

        <Panel className="table-panel">
          <div className="section-heading">
            <h2>Data files</h2>
            <span>{dataStatus?.sections.length ?? 0} sections</span>
          </div>
          <div className="data-table-wrap">
            <table className="data-table">
              <caption className="visually-hidden">Data file sections</caption>
              <thead>
                <tr>
                  <th>Section</th>
                  <th>File</th>
                  <th>Updated</th>
                </tr>
              </thead>
              <tbody>
                {dataStatus?.sections.flatMap((section) =>
                  section.files.map((file) => (
                    <tr key={`${section.code}-${file.relative_path}`}>
                      <td>{section.title}</td>
                      <td>{file.relative_path}</td>
                      <td>{formatDateTime(file.modified_at)}</td>
                    </tr>
                  )),
                )}
              </tbody>
            </table>
          </div>
        </Panel>
      </section>
    </main>
  )
}
