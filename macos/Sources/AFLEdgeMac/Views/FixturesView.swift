import SwiftUI

struct FixturesView: View {
    @Bindable var store: FixturesStore
    let sgmDraftStore: SgmDraftStore
    @Binding var showInspector: Bool
    @FocusState private var searchFocused: Bool

    var body: some View {
        HSplitView {
            VStack(alignment: .leading, spacing: 12) {
                SectionHeader(title: "Fixtures", subtitle: "Browse matches, markets, and selections.")
                TextField("Search event", text: $store.searchQuery)
                    .textFieldStyle(.roundedBorder)
                    .focused($searchFocused)
                    .onSubmit { Task { await store.refreshEvents() } }
                Picker("Bookmaker", selection: Binding(
                    get: { store.selectedBookmaker },
                    set: { store.selectBookmaker($0) }
                )) {
                    ForEach(store.bookmakers) { bookmaker in
                        Text(bookmaker.displayName).tag(bookmaker.code)
                    }
                }
                List(store.events, selection: Binding(
                    get: { store.selectedEventId },
                    set: { id in
                        guard let id, let event = store.events.first(where: { $0.id == id }) else { return }
                        store.selectEvent(event)
                    }
                )) { event in
                    VStack(alignment: .leading, spacing: 2) {
                        Text(AFLFormatters.shortAFLMatchLabel(event.matchName))
                        Text(AFLFormatters.dateTime(event.startTime))
                            .font(.caption)
                            .foregroundStyle(.secondary)
                    }
                    .tag(event.id)
                }
                .listStyle(.sidebar)
            }
            .frame(minWidth: 280, idealWidth: 330, maxWidth: 390)
            .padding()
            .aflPaneBackground()

            VStack(alignment: .leading, spacing: 14) {
                if let event = store.selectedEvent {
                    EventSummaryPanel(event: event)
                }
                if store.isLoadingEvents || store.isLoadingMarkets {
                    LoadingStateView(message: "Loading fixture data")
                }
                if let error = store.errorMessage {
                    ErrorStateView(message: error)
                }
                HSplitView {
                    VStack(alignment: .leading) {
                        Text("Markets")
                            .font(.headline)
                        Table(store.markets, selection: $store.selectedMarketId) {
                            TableColumn("Market") { market in
                                VStack(alignment: .leading) {
                                    Text(market.player?.fullName ?? market.displayName)
                                    Text(market.displayName)
                                        .font(.caption)
                                        .foregroundStyle(.secondary)
                                }
                            }
                            TableColumn("Line") { market in
                                Text(market.lineValue.map { String(format: "%.1f", $0) } ?? "--")
                            }
                            TableColumn("Types") { market in
                                Text(market.availableSelectionTypes.joined(separator: " / "))
                            }
                        }
                        .aflTableSurface()
                        .onChange(of: store.selectedMarketId) { _, id in
                            if let id, let market = store.markets.first(where: { $0.id == id }) {
                                store.selectMarket(market)
                            }
                        }
                    }
                    VStack(alignment: .leading) {
                        Text("Selections")
                            .font(.headline)
                        if store.isLoadingSelections {
                            LoadingStateView(message: "Loading selections")
                        }
                        Table(store.selections) {
                            TableColumn("Label") { selection in
                                Text(selection.label)
                            }
                            TableColumn("Side") { selection in
                                Text(selection.selectionType)
                            }
                            TableColumn("Price") { selection in
                                Text(AFLFormatters.decimalPrice(selection.decimalPrice))
                            }
                            TableColumn("Edge") { selection in
                                Text(AFLFormatters.edgePercent(selection.edgePct))
                            }
                            TableColumn("Action") { selection in
                                Button("Add SGM") {
                                    addSelectionToSgm(selection)
                                }
                                .buttonStyle(.borderless)
                                .disabled(!selection.sgmEligible || selection.decimalPrice == nil)
                            }
                        }
                        .aflTableSurface()
                    }
                }
            }
            .padding()
        }
        .aflDetailBackground()
        .navigationTitle("Fixtures")
        .toolbar {
            ToolbarItemGroup {
                Button {
                    Task { await store.refreshEvents() }
                } label: {
                    Label("Refresh", systemImage: "arrow.clockwise")
                }
                Button {
                    showInspector.toggle()
                } label: {
                    Label("Inspector", systemImage: "sidebar.right")
                }
            }
        }
        .inspector(isPresented: $showInspector) {
            FixturesInspector(store: store)
                .inspectorColumnWidth(min: 300, ideal: 340, max: 420)
        }
        .focusedSceneValue(\.focusSearchAction, { searchFocused = true })
        .task {
            await store.bootstrap()
        }
    }

    private func addSelectionToSgm(_ selection: SelectionSummary) {
        guard let event = store.selectedEvent, let price = selection.decimalPrice else { return }
        let leg = DraftLeg(
            selectionId: selection.id,
            eventId: event.id,
            eventLabel: event.matchName,
            bookmaker: store.selectedBookmaker,
            label: selection.label,
            marketTypeCode: selection.selectionType,
            selectionType: selection.selectionType,
            basePrice: price,
            diff2025: nil,
            diffLast10: nil
        )
        let result = sgmDraftStore.addLeg(leg)
        store.infoMessage = result.message
    }
}

private struct EventSummaryPanel: View {
    var event: EventSummary

    var body: some View {
        HStack(alignment: .top) {
            VStack(alignment: .leading, spacing: 5) {
                Text(event.matchName)
                    .font(.title2.weight(.semibold))
                Text(AFLFormatters.dateTime(event.startTime))
                    .foregroundStyle(.secondary)
                HStack {
                    if let round = event.roundLabel {
                        Pill(round)
                    }
                    if let venue = event.venue {
                        Pill(venue, systemImage: "mappin.and.ellipse")
                    }
                }
            }
            Spacer()
            HStack {
                ForEach(event.availableBookmakers.prefix(5), id: \.self) { code in
                    Pill(code)
                }
            }
        }
        .aflCard()
    }
}

private struct FixturesInspector: View {
    @Bindable var store: FixturesStore

    var body: some View {
        Form {
            Section("Filters") {
                Picker("Bookmaker", selection: Binding(
                    get: { store.selectedBookmaker },
                    set: { store.selectBookmaker($0) }
                )) {
                    ForEach(store.bookmakers) { bookmaker in
                        Text(bookmaker.displayName).tag(bookmaker.code)
                    }
                }
                TextField("Player market filter", text: $store.playerQuery)
                Button("Reload Markets") {
                    if let event = store.selectedEvent {
                        Task { await store.loadMarkets(event: event) }
                    }
                }
            }
            if let message = store.infoMessage {
                Section("Status") {
                    Text(message)
                }
            }
            Section("Selected") {
                if let event = store.selectedEvent {
                    LabeledContent("Match", value: event.matchName)
                    LabeledContent("Start", value: AFLFormatters.dateTime(event.startTime))
                    LabeledContent("Venue", value: event.venue ?? "--")
                } else {
                    Text("No event selected.")
                        .foregroundStyle(.secondary)
                }
            }
        }
        .formStyle(.grouped)
    }
}
