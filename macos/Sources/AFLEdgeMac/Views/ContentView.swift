import SwiftUI

struct ContentView: View {
    let container: AppContainer

    @SceneStorage("selectedDestination") private var selectedDestinationRaw = TopLevelDestination.playerLab.rawValue
    @SceneStorage("showInspector") private var showInspector = true

    @State private var playerStore: PlayerLabStore
    @State private var oddsStore: OddsStore
    @State private var sgmStore: SgmBuilderStore
    @State private var cgmStore: CgmBuilderStore
    @State private var fixturesStore: FixturesStore
    @State private var dataStatusStore: DataStatusStore

    init(container: AppContainer) {
        self.container = container
        _playerStore = State(initialValue: PlayerLabStore(api: container.apiClient))
        _oddsStore = State(initialValue: OddsStore(api: container.apiClient))
        _sgmStore = State(initialValue: SgmBuilderStore(api: container.apiClient, draftStore: container.sgmDraftStore))
        _cgmStore = State(initialValue: CgmBuilderStore(api: container.apiClient, draftStore: container.cgmDraftStore))
        _fixturesStore = State(initialValue: FixturesStore(api: container.apiClient, settings: container.settingsStore.settings))
        _dataStatusStore = State(initialValue: DataStatusStore(api: container.apiClient))
    }

    private var selectedDestination: TopLevelDestination {
        get { TopLevelDestination(rawValue: selectedDestinationRaw) ?? .playerLab }
        nonmutating set { selectedDestinationRaw = newValue.rawValue }
    }

    var body: some View {
        NavigationSplitView {
            List(selection: Binding(
                get: { selectedDestination },
                set: { selectedDestination = $0 ?? .playerLab }
            )) {
                Section {
                    BrandHeader()
                }
                .listRowSeparator(.hidden)

                Section("Workspace") {
                    ForEach(TopLevelDestination.allCases) { destination in
                        Label(destination.title, systemImage: destination.systemImage)
                            .tag(destination)
                    }
                }
            }
            .navigationSplitViewColumnWidth(min: 220, ideal: 250, max: 290)
        } detail: {
            detailView
        }
        .focusedSceneValue(\.toggleInspectorAction, { showInspector.toggle() })
    }

    @ViewBuilder
    private var detailView: some View {
        switch selectedDestination {
        case .playerLab:
            PlayerLabView(store: playerStore, showInspector: $showInspector)
                .focusedSceneValue(\.refreshAction, { Task { await playerStore.refresh() } })
        case .odds:
            OddsView(
                store: oddsStore,
                showInspector: $showInspector,
                sgmDraftStore: container.sgmDraftStore,
                onOpenPlayer: { row in
                    selectedDestination = .playerLab
                    Task { await playerStore.openFromOdds(row) }
                }
            )
                .focusedSceneValue(\.refreshAction, { Task { await oddsStore.refresh(resetVisibleCount: true) } })
        case .sgmBuilder:
            SgmBuilderView(store: sgmStore, showInspector: $showInspector)
                .focusedSceneValue(\.refreshAction, { Task { await sgmStore.refresh() } })
                .focusedSceneValue(\.clearDraftAction, { container.sgmDraftStore.clear() })
        case .cgmBuilder:
            CgmBuilderView(store: cgmStore, showInspector: $showInspector)
                .focusedSceneValue(\.refreshAction, { Task { await cgmStore.refresh() } })
                .focusedSceneValue(\.clearDraftAction, { container.cgmDraftStore.clearDraft() })
        case .fixtures:
            FixturesView(store: fixturesStore, sgmDraftStore: container.sgmDraftStore, showInspector: $showInspector)
                .focusedSceneValue(\.refreshAction, { Task { await fixturesStore.refreshEvents() } })
        case .dataStatus:
            DataStatusView(store: dataStatusStore)
                .focusedSceneValue(\.refreshAction, { Task { await dataStatusStore.refresh() } })
        }
    }
}
