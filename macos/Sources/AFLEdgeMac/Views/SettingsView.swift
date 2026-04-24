import SwiftUI

struct SettingsView: View {
    let settingsStore: AppSettingsStore
    let api: AFLAPIClient

    @State private var apiBaseURL = ""
    @State private var authToken = ""
    @State private var defaultBookmaker = "sportsbet"
    @State private var tailscaleUserLogin = ""
    @State private var tailscaleUserHeaderName = "Tailscale-User-Login"
    @State private var bookmakers: [BookmakerSummary] = []
    @State private var health: HealthResponse?
    @State private var dataStatus: DataStatusResponse?
    @State private var isWorking = false
    @State private var message: String?
    @State private var errorMessage: String?

    var body: some View {
        TabView {
            Form {
                Section("Backend") {
                    TextField("API base URL", text: $apiBaseURL)
                        .textContentType(.URL)
                    SecureField("Bearer token", text: $authToken)
                    TextField("Tailscale user login", text: $tailscaleUserLogin)
                    TextField("Tailscale identity header", text: $tailscaleUserHeaderName)
                    if !AppSettingsStore.includesAPIPrefix(apiBaseURL) {
                        Text("The URL must include /api/v1/.")
                            .font(.caption)
                            .foregroundStyle(.red)
                    }
                }

                Section("Defaults") {
                    Picker("Default bookmaker", selection: $defaultBookmaker) {
                        if bookmakers.isEmpty {
                            Text(defaultBookmaker).tag(defaultBookmaker)
                        }
                        ForEach(bookmakers) { bookmaker in
                            Text(bookmaker.displayName).tag(bookmaker.code)
                        }
                    }
                    Toggle("Use dense tables", isOn: Binding(
                        get: { settingsStore.denseTables },
                        set: { settingsStore.setDenseTables($0) }
                    ))
                    Picker("Builder display", selection: Binding(
                        get: { settingsStore.preferredDisplayMode },
                        set: { settingsStore.setPreferredDisplayMode($0) }
                    )) {
                        ForEach(BuilderDisplayMode.allCases) { mode in
                            Text(mode.label).tag(mode)
                        }
                    }
                }

                Section {
                    HStack {
                        Button {
                            save()
                        } label: {
                            Label("Save Settings", systemImage: "checkmark.circle.fill")
                        }
                        .buttonStyle(AFLPrimaryButtonStyle())
                        .keyboardShortcut(.defaultAction)
                        .disabled(isWorking || !AppSettingsStore.includesAPIPrefix(apiBaseURL))

                        Button {
                            Task { await testConnection() }
                        } label: {
                            Label("Test Connection", systemImage: "network")
                        }
                        .buttonStyle(AFLSecondaryButtonStyle())
                        .disabled(isWorking)
                    }
                }

                if let message {
                    Section {
                        Label(message, systemImage: "checkmark.circle.fill")
                            .foregroundStyle(AFLTheme.success)
                    }
                }
                if let errorMessage {
                    Section {
                        Label(errorMessage, systemImage: "exclamationmark.triangle.fill")
                            .foregroundStyle(AFLTheme.danger)
                    }
                }
            }
            .formStyle(.grouped)
            .tabItem {
                Label("Connection", systemImage: "network")
            }

            Form {
                Section("Health") {
                    if let health {
                        LabeledContent("Status", value: health.status)
                        LabeledContent("Database", value: health.databaseOk ? "Reachable" : "Unavailable")
                        LabeledContent("Last import", value: AFLFormatters.dateTimeInAdelaide(health.lastSuccessfulImportAt))
                    } else {
                        Text("Run a connection test to load backend health.")
                            .foregroundStyle(.secondary)
                    }
                }

                Section("Data") {
                    if let dataStatus {
                        LabeledContent("Generated", value: AFLFormatters.dateTimeInAdelaide(dataStatus.generatedAt))
                        LabeledContent("Sections", value: "\(dataStatus.sections.count)")
                    } else {
                        Text("Data status will appear after a successful connection test.")
                            .foregroundStyle(.secondary)
                    }
                }
            }
            .formStyle(.grouped)
            .tabItem {
                Label("Status", systemImage: "externaldrive.badge.checkmark")
            }
        }
        .frame(width: 560, height: 430)
        .scenePadding()
        .tint(AFLTheme.accent)
        .background(AFLTheme.detailBackground)
        .preferredColorScheme(.light)
        .task {
            apiBaseURL = settingsStore.settings.apiBaseURL
            authToken = settingsStore.settings.authToken
            defaultBookmaker = settingsStore.settings.defaultBookmaker
            tailscaleUserLogin = settingsStore.settings.tailscaleUserLogin
            tailscaleUserHeaderName = settingsStore.settings.tailscaleUserHeaderName
            await loadBookmakers()
        }
    }

    private func save() {
        settingsStore.save(
            AppSettings(
                apiBaseURL: apiBaseURL,
                authToken: authToken,
                defaultBookmaker: defaultBookmaker,
                tailscaleUserLogin: tailscaleUserLogin,
                tailscaleUserHeaderName: tailscaleUserHeaderName
            )
        )
        apiBaseURL = settingsStore.settings.apiBaseURL
        authToken = settingsStore.settings.authToken
        defaultBookmaker = settingsStore.settings.defaultBookmaker
        tailscaleUserLogin = settingsStore.settings.tailscaleUserLogin
        tailscaleUserHeaderName = settingsStore.settings.tailscaleUserHeaderName
        message = "Settings saved."
        errorMessage = nil
    }

    private func loadBookmakers() async {
        do {
            bookmakers = try await api.bookmakers()
        } catch {
            // Settings must remain usable before the backend is configured.
        }
    }

    private func testConnection() async {
        isWorking = true
        message = nil
        errorMessage = nil
        save()
        do {
            async let healthResult = api.health()
            async let statusResult = api.dataStatus()
            async let bookmakerResult = api.bookmakers()
            health = try await healthResult
            dataStatus = try await statusResult
            bookmakers = try await bookmakerResult
            message = "Backend check succeeded."
            AppLog.settings.info("Backend connection test succeeded")
        } catch {
            errorMessage = error.localizedDescription
        }
        isWorking = false
    }
}
