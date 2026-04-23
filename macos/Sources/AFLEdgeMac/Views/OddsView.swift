import SwiftUI

struct OddsView: View {
    @Bindable var store: OddsStore
    @Binding var showInspector: Bool
    let sgmDraftStore: SgmDraftStore
    var onOpenPlayer: (OddsSearchResult) -> Void
    @FocusState private var searchFocused: Bool

    var body: some View {
        VStack(alignment: .leading, spacing: 14) {
            HStack(alignment: .center) {
                SectionHeader(
                    title: "Odds",
                    subtitle: store.filters.scope == .match
                        ? "Processed match markets across H2H, line, and totals."
                        : "Processed player props with model and market context."
                )
                Spacer()
                Picker("Scope", selection: Binding(
                    get: { store.filters.scope },
                    set: { store.setScope($0) }
                )) {
                    ForEach(OddsScope.allCases) { scope in
                        Text(scope.label).tag(scope)
                    }
                }
                .pickerStyle(.segmented)
                .frame(width: 220)
            }

            HStack {
                TextField("Search label or player", text: $store.filters.query)
                    .textFieldStyle(.roundedBorder)
                    .focused($searchFocused)
                    .onSubmit {
                        Task { await store.refresh(resetVisibleCount: true) }
                    }
                Button("Apply") {
                    Task { await store.refresh(resetVisibleCount: true) }
                }
                Button("Load More") {
                    Task { await store.loadMore() }
                }
                .disabled(!store.hasMore || store.isLoading)
            }

            activeFilterRow

            if store.isLoading {
                LoadingStateView(message: "Loading odds")
            }
            if let error = store.errorMessage {
                ErrorStateView(message: error)
            }
            if !store.isLoading && store.rows.isEmpty {
                EmptyStateView(title: "No odds", message: "Adjust the current filters or refresh processed data.")
            } else {
                OddsTable(store: store, sgmDraftStore: sgmDraftStore, onOpenPlayer: onOpenPlayer)
            }
        }
        .padding()
        .aflDetailBackground()
        .navigationTitle("Odds")
        .toolbar {
            ToolbarItemGroup {
                Button {
                    Task { await store.refresh(resetVisibleCount: true) }
                } label: {
                    Label("Refresh", systemImage: "arrow.clockwise")
                }
                Button {
                    showInspector.toggle()
                } label: {
                    Label("Filters", systemImage: "slider.horizontal.3")
                }
            }
        }
        .inspector(isPresented: $showInspector) {
            OddsInspector(store: store)
                .inspectorColumnWidth(min: 340, ideal: 390, max: 470)
        }
        .sheet(item: $store.alternateUndersTarget) { target in
            AlternateUndersSheet(store: store, target: target)
        }
        .focusedSceneValue(\.focusSearchAction, { searchFocused = true })
        .task {
            await store.bootstrap()
        }
    }

    private var activeFilterRow: some View {
        ScrollView(.horizontal, showsIndicators: false) {
            HStack(spacing: 6) {
                Pill("\(store.rows.count) rows")
                Pill(store.filters.scope.label)
                Pill("Sort: \(sortLabel(store.filters))")
                if store.filters.bestOnly {
                    Pill("Best only", systemImage: "star.fill")
                }
                if store.filters.sgmOnly {
                    Pill("SGM-ready")
                }
                ForEach(store.filters.bookmakerCodes, id: \.self) { code in
                    Pill(code)
                }
                if let event = store.events.first(where: { $0.id == store.filters.eventId }) {
                    Pill(AFLFormatters.shortAFLMatchLabel(event.matchName))
                }
                if let market = store.filters.marketTypeCode {
                    Pill(market.replacingOccurrences(of: "_", with: " "))
                }
                if let side = store.filters.selectionType {
                    Pill(side.capitalized)
                }
                if !store.filters.includePlayerIds.isEmpty {
                    Pill("Include \(store.filters.includePlayerIds.count)")
                }
                if !store.filters.excludePlayerIds.isEmpty {
                    Pill("Exclude \(store.filters.excludePlayerIds.count)")
                }
                if !store.filters.minPriceText.isEmpty || !store.filters.maxPriceText.isEmpty {
                    Pill("Odds \(store.filters.minPriceText.ifBlank("-"))-\(store.filters.maxPriceText.ifBlank("-"))")
                }
                if !store.filters.minEdgeText.isEmpty {
                    Pill("Edge \(store.filters.minEdgeText)+")
                }
                if !isDefaultDiffRange(store.filters.minDiffLast10, store.filters.maxDiffLast10) {
                    Pill("L10 \(AFLFormatters.signedMetric(store.filters.minDiffLast10)) to \(AFLFormatters.signedMetric(store.filters.maxDiffLast10))")
                }
                if !isDefaultDiffRange(store.filters.minDiff2025, store.filters.maxDiff2025) {
                    Pill("2025 \(AFLFormatters.signedMetric(store.filters.minDiff2025)) to \(AFLFormatters.signedMetric(store.filters.maxDiff2025))")
                }
                if !isDefaultDiffRange(store.filters.minNextBestProbDiff, store.filters.maxNextBestProbDiff) {
                    Pill("NB \(AFLFormatters.signedMetric(store.filters.minNextBestProbDiff)) to \(AFLFormatters.signedMetric(store.filters.maxNextBestProbDiff))")
                }
            }
        }
    }
}

private struct OddsTable: View {
    @Bindable var store: OddsStore
    let sgmDraftStore: SgmDraftStore
    var onOpenPlayer: (OddsSearchResult) -> Void

    @State private var sortOrder: [KeyPathComparator<OddsSearchResult>] = [
        KeyPathComparator(\OddsSearchResult.nextBestSort, order: .reverse),
    ]

    private var sortedRows: [OddsSearchResult] {
        store.rows.sorted(using: sortOrder)
    }

    private var selectedSelectionIds: Set<Int> {
        Set(sgmDraftStore.state.legs.map(\.selectionId))
    }

    var body: some View {
        Table(sortedRows, selection: $store.selectedRowId, sortOrder: $sortOrder) {
            TableColumn("Match", value: \.matchName) { row in
                VStack(alignment: .leading, spacing: 2) {
                    Text(AFLFormatters.shortAFLMatchLabel(row.matchName))
                    Text(AFLFormatters.dateTime(row.startTime))
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }
            }
            TableColumn("Selection", value: \.label) { row in
                VStack(alignment: .leading, spacing: 2) {
                    Text(row.label)
                        .lineLimit(1)
                    Text(row.player?.fullName ?? row.marketDisplayName)
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }
            }
            TableColumn("Book", value: \.bookmaker) { row in
                Text(row.bookmaker)
            }
            TableColumn("Line", value: \.lineSort) { row in
                Text(row.lineValue.map { String(format: "%.1f", $0) } ?? "--")
                    .monospacedDigit()
            }
            TableColumn("Price", value: \.priceSort) { row in
                Text(AFLFormatters.decimalPrice(row.decimalPrice))
                    .monospacedDigit()
            }
            TableColumn("NB", value: \.nextBestSort) { row in
                Text(AFLFormatters.signedMetric(row.nextBestProbDiff))
                    .monospacedDigit()
            }
            TableColumn("L10", value: \.diffLast10Sort) { row in
                Text(AFLFormatters.signedMetric(row.diffLast10))
                    .monospacedDigit()
            }
            TableColumn("2025", value: \.diff2025Sort) { row in
                Text(AFLFormatters.signedMetric(row.diff2025))
                    .monospacedDigit()
            }
            TableColumn("Context") { row in
                HStack {
                    if let position = AFLFormatters.playerPositionTag(row.playerPosition) {
                        Pill(position)
                    }
                    if let matchup = AFLFormatters.matchupDifficultyTag(row.matchupDifficulty) {
                        Pill(matchup)
                    }
                    if let temp = AFLFormatters.weatherTemperatureTag(row.weather?.temperatureC) {
                        Pill(temp)
                    }
                    if let rain = AFLFormatters.weatherRainTag(row.weather?.precipMm) {
                        Pill(rain)
                    }
                }
            }
        }
        .contextMenu(forSelectionType: OddsSearchResult.ID.self) { selection in
            if let row = contextMenuRow(for: selection) {
                oddsContextMenu(for: row)
            }
        }
        .aflTableSurface()
    }

    private func contextMenuRow(for selection: Set<OddsSearchResult.ID>) -> OddsSearchResult? {
        if let selectionId = selection.first {
            return sortedRows.first { $0.id == selectionId }
        }
        if let selectedRowId = store.selectedRowId {
            return sortedRows.first { $0.id == selectedRowId }
        }
        return nil
    }

    @ViewBuilder
    private func oddsContextMenu(for row: OddsSearchResult) -> some View {
        if row.player != nil {
            Button("Open In Player Lab") {
                onOpenPlayer(row)
            }

            Button("Show Alternate Unders") {
                Task { await store.openAlternateUnders(for: row) }
            }
            Divider()
        }

        if selectedSelectionIds.contains(row.selectionId) {
            Button("Remove From SGM") {
                sgmDraftStore.removeLeg(selectionId: row.selectionId)
            }
        } else {
            Button("Add To SGM") {
                addRowToSgm(row)
            }
            .disabled(!row.sgmEligible || row.decimalPrice == nil)
        }
    }

    private func addRowToSgm(_ row: OddsSearchResult) {
        guard let price = row.decimalPrice else { return }
        _ = sgmDraftStore.addLeg(
            DraftLeg(
                selectionId: row.selectionId,
                eventId: row.eventId,
                eventLabel: row.matchName,
                bookmaker: row.bookmaker,
                label: row.label,
                marketTypeCode: row.marketTypeCode,
                selectionType: row.selectionType,
                basePrice: price,
                diff2025: row.diff2025,
                diffLast10: row.diffLast10,
                nextBestProbDiff: row.nextBestProbDiff,
                isBestPrice: row.isBestPrice
            )
        )
    }
}

private struct OddsInspector: View {
    @Bindable var store: OddsStore

    var body: some View {
        Form {
            Section {
                InspectorPrimaryActionBlock(
                    title: "Apply Filters",
                    subtitle: "Run the current bookmaker, market, and player filters.",
                    secondaryTitle: "Reset Filters",
                    secondarySystemImage: "arrow.uturn.backward",
                    primaryAction: { store.applyFilters(store.filters) },
                    secondaryAction: {
                        var reset = OddsFilters(scope: store.filters.scope)
                        reset.bookmakerCodes = store.defaultBookmakerCodes
                        store.applyFilters(reset)
                    }
                )
            }

            if let selected = store.rows.first(where: { $0.id == store.selectedRowId }) {
                SelectedOddsDetail(row: selected)
            }

            Section("Bookmakers") {
                ForEach(store.bookmakers) { bookmaker in
                    Toggle(bookmaker.displayName, isOn: Binding(
                        get: { store.filters.bookmakerCodes.contains(bookmaker.code) },
                        set: { selected in
                            if selected, !store.filters.bookmakerCodes.contains(bookmaker.code) {
                                store.filters.bookmakerCodes.append(bookmaker.code)
                            } else if !selected {
                                store.filters.bookmakerCodes.removeAll { $0 == bookmaker.code }
                            }
                        }
                    ))
                }
            }

            Section("Market") {
                Toggle("SGM-ready only", isOn: $store.filters.sgmOnly)
                Picker("Market type", selection: Binding(
                    get: { store.filters.marketTypeCode ?? "" },
                    set: { store.filters.marketTypeCode = $0.isEmpty ? nil : $0 }
                )) {
                    Text("All").tag("")
                    ForEach(marketOptions(scope: store.filters.scope), id: \.code) { option in
                        Text(option.label).tag(option.code)
                    }
                }
                Picker("Event", selection: Binding(
                    get: { store.filters.eventId ?? -1 },
                    set: { store.filters.eventId = $0 == -1 ? nil : $0 }
                )) {
                    Text("All events").tag(-1)
                    ForEach(store.events) { event in
                        Text(AFLFormatters.shortAFLMatchLabel(event.matchName)).tag(event.id)
                    }
                }
                Picker("Sort", selection: Binding(
                    get: { "\(store.filters.sortBy)|\(store.filters.sortDirection)" },
                    set: {
                        let parts = $0.split(separator: "|")
                        store.filters.sortBy = String(parts.first ?? "diff_last_10")
                        store.filters.sortDirection = String(parts.dropFirst().first ?? "desc")
                    }
                )) {
                    ForEach(sortOptions(scope: store.filters.scope), id: \.label) { option in
                        Text(option.label).tag("\(option.sortBy)|\(option.sortDirection)")
                    }
                }
            }

            if store.filters.scope == .player {
                Section("Player Props") {
                    Picker("Side", selection: Binding(
                        get: { store.filters.selectionType ?? "" },
                        set: { store.filters.selectionType = $0.isEmpty ? nil : $0 }
                    )) {
                        Text("All").tag("")
                        Text("Overs").tag("over")
                        Text("Unders").tag("under")
                    }
                    Toggle("Best price only", isOn: $store.filters.bestOnly)
                    TextField("Min price", text: $store.filters.minPriceText)
                    TextField("Max price", text: $store.filters.maxPriceText)
                    TextField("Minimum edge %", text: $store.filters.minEdgeText)
                }
                PlayerFilterSection(
                    title: "Include Players",
                    players: store.allPlayers,
                    selectedIds: $store.filters.includePlayerIds
                )
                PlayerFilterSection(
                    title: "Exclude Players",
                    players: store.allPlayers,
                    selectedIds: $store.filters.excludePlayerIds
                )
                MultiSelectSection(title: "Matchup", options: matchupDifficultyOptions, selection: $store.filters.matchupDifficulties)
                MetricRangeSection(title: "Diff L10", min: $store.filters.minDiffLast10, max: $store.filters.maxDiffLast10)
                MetricRangeSection(title: "Diff 2025", min: $store.filters.minDiff2025, max: $store.filters.maxDiff2025)
                MetricRangeSection(title: "Next best diff", min: $store.filters.minNextBestProbDiff, max: $store.filters.maxNextBestProbDiff)
            }

            Section("Quick Filters") {
                ForEach(QuickFilterPreset.allCases) { preset in
                    Button(preset.label) {
                        store.applyFilters(store.filters.applying(preset))
                    }
                }
            }
        }
        .formStyle(.grouped)
    }
}

private struct SelectedOddsDetail: View {
    var row: OddsSearchResult

    var body: some View {
        Section("Selected Row") {
            Text(row.label)
                .font(.headline)
            LabeledContent("Player", value: row.player?.fullName ?? "--")
            LabeledContent("Match", value: row.matchName)
            LabeledContent("Start", value: AFLFormatters.dateTime(row.startTime))
            LabeledContent("Bookmaker", value: row.bookmaker)
            LabeledContent("Market", value: row.marketDisplayName)
            LabeledContent("Price", value: AFLFormatters.decimalPrice(row.decimalPrice))
            LabeledContent("Next best diff", value: AFLFormatters.signedMetric(row.nextBestProbDiff))
            LabeledContent("Diff L10", value: AFLFormatters.signedMetric(row.diffLast10))
            LabeledContent("Diff 2025", value: AFLFormatters.signedMetric(row.diff2025))
            if let weather = row.weather {
                LabeledContent("Weather", value: weather.label ?? "--")
            }
        }
    }
}

private struct PlayerFilterSection: View {
    var title: String
    var players: [PlayerSummary]
    @Binding var selectedIds: [Int]
    @State private var search = ""

    private var visiblePlayers: [PlayerSummary] {
        let trimmed = search.trimmingCharacters(in: .whitespacesAndNewlines)
        let source = trimmed.isEmpty
            ? players.filter { selectedIds.contains($0.id) }
            : players.filter { $0.fullName.localizedCaseInsensitiveContains(trimmed) }
        return Array(source.prefix(20))
    }

    var body: some View {
        Section(title) {
            TextField("Search player", text: $search)
            if selectedIds.isEmpty && search.isEmpty {
                Text("Search to add players.")
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
            ForEach(visiblePlayers) { player in
                Toggle(player.fullName, isOn: Binding(
                    get: { selectedIds.contains(player.id) },
                    set: { selected in
                        if selected, !selectedIds.contains(player.id) {
                            selectedIds.append(player.id)
                        } else if !selected {
                            selectedIds.removeAll { $0 == player.id }
                        }
                    }
                ))
            }
            if !selectedIds.isEmpty {
                Button("Clear \(title)") {
                    selectedIds = []
                }
            }
        }
    }
}

struct MetricRangeSection: View {
    var title: String
    @Binding var min: Double
    @Binding var max: Double

    var body: some View {
        Section(title) {
            LabeledContent("Min", value: AFLFormatters.signedMetric(min))
            Slider(value: $min, in: oddsDiffSliderMin...max, step: 0.05)
            LabeledContent("Max", value: AFLFormatters.signedMetric(max))
            Slider(value: $max, in: min...oddsDiffSliderMax, step: 0.05)
        }
    }
}

private struct AlternateUndersSheet: View {
    @Bindable var store: OddsStore
    var target: OddsSearchResult

    var body: some View {
        VStack(alignment: .leading, spacing: 12) {
            SectionHeader(title: "Alternate Unders", subtitle: target.label)
            if store.isLoadingAlternateUnders {
                LoadingStateView(message: "Loading alternate under lines")
            }
            if let error = store.alternateUndersError {
                ErrorStateView(message: error)
            }
            Table(store.alternateUnders) {
                TableColumn("Line") { row in Text(row.lineValue.map { String(format: "%.1f", $0) } ?? "--") }
                TableColumn("Book") { row in Text(row.bookmaker) }
                TableColumn("Price") { row in Text(AFLFormatters.decimalPrice(row.decimalPrice)) }
                TableColumn("Match") { row in Text(AFLFormatters.shortAFLMatchLabel(row.matchName)) }
            }
            HStack {
                Spacer()
                Button("Done") {
                    store.closeAlternateUnders()
                }
                .keyboardShortcut(.cancelAction)
            }
        }
        .padding()
        .frame(width: 760, height: 480)
    }
}

private struct MarketOption {
    var code: String
    var label: String
}

private struct SortOption {
    var sortBy: String
    var sortDirection: String
    var label: String
}

private func marketOptions(scope: OddsScope) -> [MarketOption] {
    switch scope {
    case .player:
        [
            MarketOption(code: "player_disposals", label: "Disposals"),
            MarketOption(code: "player_fantasy_points", label: "Fantasy"),
            MarketOption(code: "player_goals", label: "Goals"),
            MarketOption(code: "player_marks", label: "Marks"),
            MarketOption(code: "player_tackles", label: "Tackles"),
            MarketOption(code: "player_kicks", label: "Kicks"),
            MarketOption(code: "player_handballs", label: "Handballs"),
            MarketOption(code: "player_hitouts", label: "Hitouts"),
            MarketOption(code: "player_clearances", label: "Clearances"),
        ]
    case .match:
        [
            MarketOption(code: "h2h", label: "H2H"),
            MarketOption(code: "line", label: "Line"),
            MarketOption(code: "total_points", label: "Totals"),
        ]
    }
}

private func sortOptions(scope: OddsScope) -> [SortOption] {
    switch scope {
    case .player:
        [
            SortOption(sortBy: "diff_last_10", sortDirection: "desc", label: "Diff L10"),
            SortOption(sortBy: "next_best_prob_diff", sortDirection: "desc", label: "Next best diff"),
            SortOption(sortBy: "diff_2025", sortDirection: "desc", label: "Diff 2025"),
            SortOption(sortBy: "price", sortDirection: "desc", label: "Price high-low"),
            SortOption(sortBy: "price", sortDirection: "asc", label: "Price low-high"),
            SortOption(sortBy: "player", sortDirection: "asc", label: "Player A-Z"),
            SortOption(sortBy: "match", sortDirection: "asc", label: "Match"),
        ]
    case .match:
        [
            SortOption(sortBy: "start_time", sortDirection: "asc", label: "Start time"),
            SortOption(sortBy: "next_best_prob_diff", sortDirection: "desc", label: "Next best diff"),
            SortOption(sortBy: "price", sortDirection: "desc", label: "Price high-low"),
            SortOption(sortBy: "price", sortDirection: "asc", label: "Price low-high"),
            SortOption(sortBy: "market", sortDirection: "asc", label: "Market"),
            SortOption(sortBy: "match", sortDirection: "asc", label: "Match"),
        ]
    }
}

private func sortLabel(_ filters: OddsFilters) -> String {
    sortOptions(scope: filters.scope)
        .first { $0.sortBy == filters.sortBy && $0.sortDirection == filters.sortDirection }?
        .label
        ?? "\(filters.sortBy) \(filters.sortDirection)"
}

private func isDefaultDiffRange(_ min: Double, _ max: Double) -> Bool {
    min == oddsDiffSliderMin && max == oddsDiffSliderMax
}

private extension String {
    func ifBlank(_ fallback: String) -> String {
        trimmingCharacters(in: .whitespacesAndNewlines).isEmpty ? fallback : self
    }
}
