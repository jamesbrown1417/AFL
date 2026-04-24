import SwiftUI

struct PlayerLabView: View {
    @Bindable var store: PlayerLabStore
    @Binding var showInspector: Bool
    @FocusState private var searchFocused: Bool

    private var searchListHeight: CGFloat {
        CGFloat(min(max(store.searchResults.count * 34 + 14, 112), 360))
    }

    var body: some View {
        HSplitView {
            VStack(alignment: .leading, spacing: 12) {
                SectionHeader(
                    title: "Player Lab",
                    subtitle: "Search players, tune the sample, and inspect hit rates."
                )
                TextField("Search player", text: $store.searchQuery)
                    .textFieldStyle(.roundedBorder)
                    .focused($searchFocused)
                    .onChange(of: store.searchQuery) { _, newValue in
                        store.updateSearchQuery(newValue)
                    }

                VStack(alignment: .leading, spacing: 8) {
                    HStack {
                        Text("Results")
                            .font(.caption.weight(.semibold))
                            .foregroundStyle(AFLTheme.primaryStrong)
                        Spacer()
                        Text("\(store.searchResults.count)")
                            .font(.caption)
                            .foregroundStyle(.secondary)
                            .monospacedDigit()
                    }

                    List(store.searchResults, selection: Binding(
                        get: { store.selectedPlayer?.id },
                        set: { id in
                            if let id, let player = store.allPlayers.first(where: { $0.id == id }) {
                                store.selectPlayer(player)
                            }
                        }
                    )) { player in
                        Text(player.fullName)
                            .lineLimit(1)
                            .tag(player.id)
                    }
                    .listStyle(.inset)
                    .scrollContentBackground(.hidden)
                    .frame(height: searchListHeight)
                }
                .aflPanelSurface()

                Spacer(minLength: 0)
            }
            .frame(minWidth: 230, idealWidth: 280, maxWidth: 340)
            .padding()
            .aflPaneBackground()

            VStack(alignment: .leading, spacing: 14) {
                HStack {
                    Picker("Mode", selection: Binding(
                        get: { store.mode },
                        set: { store.setMode($0) }
                    )) {
                        ForEach(PlayerLabMode.allCases) { mode in
                            Text(mode.label).tag(mode)
                        }
                    }
                    .pickerStyle(.segmented)
                    .frame(width: 220)

                    Spacer()
                }
                .aflControlSurface()

                if store.mode == .stats {
                    PlayerStatsWorkspace(store: store)
                } else {
                    PlayerComparisonWorkspace(store: store)
                }
            }
            .padding()
            .frame(minWidth: 720)
        }
        .aflDetailBackground()
        .navigationTitle(store.selectedPlayer?.fullName ?? "Player Lab")
        .toolbar {
            ToolbarItemGroup {
                Button {
                    Task { await store.refresh() }
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
            PlayerFilterInspector(store: store)
                .inspectorColumnWidth(min: 320, ideal: 360, max: 440)
        }
        .focusedSceneValue(\.focusSearchAction, { searchFocused = true })
        .task {
            await store.bootstrap()
        }
    }
}

private struct PlayerStatsWorkspace: View {
    @Bindable var store: PlayerLabStore

    var body: some View {
        VStack(alignment: .leading, spacing: 14) {
            PlayerSummaryStrip(summary: store.summary, history: store.history)
            PlayerFilterSummaryCard(filters: store.filters, options: store.filterOptions)

            HStack {
                Picker("View", selection: $store.historyViewMode) {
                    ForEach(PlayerHistoryViewMode.allCases) { mode in
                        Text(mode.label).tag(mode)
                    }
                }
                .pickerStyle(.segmented)
                .frame(width: 180)

                Spacer()
                Text("\(store.history.count) games")
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
            .aflControlSurface()

            if store.isLoading {
                LoadingStateView(message: "Loading player history")
            }
            if let error = store.errorMessage {
                ErrorStateView(message: error)
            }
            if let info = store.infoMessage {
                InfoStateView(message: info)
            }
            if !store.isLoading && store.history.isEmpty {
                EmptyStateView(title: "No game logs", message: "Adjust filters or choose another player.")
            } else if store.historyViewMode == .graph {
                PlayerHistoryGraph(history: store.history, filters: store.filters)
            } else {
                PlayerGameLogTable(history: store.history, selectedGameId: $store.selectedGameId)
            }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topLeading)
    }
}

private struct PlayerComparisonWorkspace: View {
    @Bindable var store: PlayerLabStore

    var focusedScenario: PlayerComparisonScenarioState {
        store.comparisonFocus == .scenarioA ? store.scenarioA : store.scenarioB
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 14) {
            HStack {
                Picker("View", selection: $store.comparisonViewMode) {
                    ForEach(PlayerComparisonViewMode.allCases) { mode in
                        Text(mode.label).tag(mode)
                    }
                }
                .pickerStyle(.segmented)
                .frame(width: 260)

                if store.comparisonViewMode == .gameLog {
                    Picker("Focus", selection: $store.comparisonFocus) {
                        ForEach(PlayerComparisonFocus.allCases) { focus in
                            Text(focus.label).tag(focus)
                        }
                    }
                    .pickerStyle(.segmented)
                    .frame(width: 220)
                }

                Spacer()
                Button {
                    Task { await store.refreshComparison() }
                } label: {
                    Label("Refresh Scenarios", systemImage: "arrow.clockwise")
                }
                .buttonStyle(AFLSecondaryButtonStyle())
                .disabled(store.scenarioA.isLoading || store.scenarioB.isLoading)
            }
            .aflControlSurface()

            HStack(alignment: .top, spacing: 12) {
                ComparisonScenarioCard(
                    title: "Scenario A",
                    state: store.scenarioA,
                    playerName: store.selectedPlayer?.fullName,
                    onUseCurrent: { store.copyCurrentFilters(to: .scenarioA) },
                    onRefresh: { Task { await store.refreshComparison(focus: .scenarioA) } }
                )
                ComparisonScenarioCard(
                    title: "Scenario B",
                    state: store.scenarioB,
                    playerName: store.selectedPlayer?.fullName,
                    onUseCurrent: { store.copyCurrentFilters(to: .scenarioB) },
                    onRefresh: { Task { await store.refreshComparison(focus: .scenarioB) } }
                )
            }

            if store.comparisonViewMode == .summary {
                PlayerComparisonSummary(scenarioA: store.scenarioA, scenarioB: store.scenarioB)
            } else if store.comparisonViewMode == .graph {
                PlayerComparisonGraph(scenarioA: store.scenarioA, scenarioB: store.scenarioB)
            } else if focusedScenario.isLoading {
                LoadingStateView(message: "Loading \(store.comparisonFocus.label)")
            } else if let error = focusedScenario.errorMessage {
                ErrorStateView(message: error)
            } else if focusedScenario.history.isEmpty {
                EmptyStateView(title: "No game log", message: "Adjust the \(store.comparisonFocus.label) filters in the inspector.")
            } else {
                PlayerGameLogTable(history: focusedScenario.history, selectedGameId: .constant(nil))
            }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .topLeading)
    }
}

private struct PlayerSummaryStrip: View {
    var summary: PlayerStatSummary?
    var history: [PlayerGameLogEntry]

    var body: some View {
        LazyVGrid(
            columns: [GridItem(.adaptive(minimum: 160), spacing: 12, alignment: .top)],
            alignment: .leading,
            spacing: 12
        ) {
            MetricTile(title: "Sample", value: "\(summary?.sampleSize ?? history.count)", detail: summary?.statLabel)
            MetricTile(title: "Over", value: AFLFormatters.percent(summary?.proportionOver), detail: AFLFormatters.decimalPrice(summary?.impliedOddsOver))
            MetricTile(title: "Under", value: AFLFormatters.percent(summary?.proportionUnder), detail: AFLFormatters.decimalPrice(summary?.impliedOddsUnder))
            MetricTile(title: "Interval", value: AFLFormatters.percent(summary?.proportionWithinInterval), detail: "Within range")
        }
    }
}

private struct PlayerGameLogTable: View {
    var history: [PlayerGameLogEntry]
    @Binding var selectedGameId: PlayerGameLogEntry.ID?

    @State private var sortOrder: [KeyPathComparator<PlayerGameLogEntry>] = [
        KeyPathComparator(\PlayerGameLogEntry.date, order: .reverse),
    ]

    private var sortedHistory: [PlayerGameLogEntry] {
        history.sorted(using: sortOrder)
    }

    var body: some View {
        Table(sortedHistory, selection: $selectedGameId, sortOrder: $sortOrder) {
            TableColumn("Date", value: \.date) { row in
                PlayerHistoryTextCell(text: AFLFormatters.dateTime(row.date), rowHit: row.hit)
            }
            TableColumn("Round", value: \.roundSort) { row in
                PlayerHistoryTextCell(text: row.roundLabel ?? "--", rowHit: row.hit)
            }
            TableColumn("Team", value: \.teamSort) { row in
                PlayerHistoryTextCell(text: row.team ?? "--", rowHit: row.hit)
            }
            TableColumn("Opponent", value: \.oppositionSort) { row in
                PlayerHistoryTextCell(text: row.opposition ?? "--", rowHit: row.hit)
            }
            TableColumn("Venue", value: \.venueSort) { row in
                PlayerHistoryTextCell(text: row.venue ?? "--", rowHit: row.hit)
            }
            TableColumn("Value", value: \.selectedValueSort) { row in
                PlayerHistoryTextCell(
                    text: row.selectedValue.map { String(format: "%.1f", $0) } ?? "--",
                    rowHit: row.hit,
                    alignment: .trailing,
                    emphasized: true
                )
            }
            TableColumn("Hit", value: \.hitSort) { row in
                PlayerHistoryHitBadge(hit: row.hit)
            }
            TableColumn("TOG", value: \.togSort) { row in
                PlayerHistoryTextCell(
                    text: row.tog.map { String(format: "%.0f%%", $0) } ?? "--",
                    rowHit: row.hit,
                    alignment: .trailing
                )
            }
            TableColumn("Weather", value: \.weatherSort) { row in
                PlayerHistoryTextCell(text: row.weather ?? "--", rowHit: row.hit)
            }
        }
        .aflTableSurface()
    }
}

private struct PlayerFilterSummaryCard: View {
    var filters: PlayerStatsFilters
    var options: PlayerStatFilterOptions?

    private var rows: [(String, String, Color, Color)] {
        [
            ("Metric", resolvedStatLabel(filters: filters, options: options), AFLColor.orange50, AFLColor.orange300),
            ("Line", lineLabel(filters), AFLColor.orange50, AFLColor.orange300),
            ("Seasons", summarizeFilterValues(filters.seasons, allValues: options?.seasons) ?? "Any", AFLColor.orange50, AFLColor.orange300),
            ("Last Games", filters.lastGamesText.trimmedNonEmpty.map { "\($0) games" } ?? "All", AFLColor.blue50, AFLColor.blue200),
            ("TOG", filters.minutesMinimumText == "0" ? "Any" : "\(filters.minutesMinimumText)%+", AFLColor.blue50, AFLColor.blue200),
            ("Margin", filters.marginMinText == "-200" && filters.marginMaxText == "200" ? "Full range" : "\(filters.marginMinText) to \(filters.marginMaxText)", AFLColor.blue50, AFLColor.blue200),
            ("Home/Away", summarizeFilterValues(filters.homeAway, allValues: options?.homeAwayOptions) ?? "All", AFLColor.blue25, AFLColor.blue300),
            ("Opposition", summarizeFilterValues(filters.oppositions, allValues: options?.oppositions) ?? "All", AFLColor.blue25, AFLColor.blue300),
            ("Venue", summarizeFilterValues(filters.venues, allValues: options?.venues) ?? "All", AFLColor.blue25, AFLColor.blue300),
            ("Weather", summarizeFilterValues(filters.weatherCategories, allValues: options?.weatherCategories) ?? "All", AFLColor.blue25, AFLColor.blue300),
        ]
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 10) {
            HStack {
                VStack(alignment: .leading, spacing: 3) {
                    Text("Current Filters")
                        .font(.headline)
                    Text("Stat, sample, and context applied to the current player view.")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }
                Spacer()
                Pill(lineLabel(filters), systemImage: "line.3.horizontal.decrease.circle")
            }

            LazyVGrid(
                columns: [GridItem(.adaptive(minimum: 132), spacing: 8, alignment: .top)],
                alignment: .leading,
                spacing: 8
            ) {
                ForEach(Array(rows.enumerated()), id: \.offset) { _, row in
                    PlayerFilterSummaryTile(label: row.0, value: row.1, tint: row.2, border: row.3)
                }
            }
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .aflCard()
    }
}

private struct PlayerFilterSummaryTile: View {
    var label: String
    var value: String
    var tint: Color
    var border: Color

    var body: some View {
        VStack(alignment: .leading, spacing: 4) {
            Text(label.uppercased())
                .font(.caption2.weight(.semibold))
                .foregroundStyle(AFLColor.navy700)
                .lineLimit(1)
            Text(value)
                .font(.callout.weight(.medium))
                .foregroundStyle(AFLColor.navy950)
                .lineLimit(2)
                .fixedSize(horizontal: false, vertical: true)
        }
        .frame(maxWidth: .infinity, minHeight: 54, alignment: .topLeading)
        .padding(9)
        .background(tint.opacity(0.82), in: .rect(cornerRadius: 8))
        .overlay(
            RoundedRectangle(cornerRadius: 8)
                .stroke(border.opacity(0.62), lineWidth: 1)
        )
    }
}

private struct PlayerHistoryTextCell: View {
    var text: String
    var rowHit: Bool?
    var alignment: Alignment = .leading
    var emphasized = false

    var body: some View {
        Text(text)
            .font(emphasized ? .callout.weight(.semibold) : .callout)
            .monospacedDigit()
            .foregroundStyle(historyCellForeground(hit: rowHit, emphasized: emphasized))
            .frame(maxWidth: .infinity, alignment: alignment)
            .padding(.horizontal, 6)
            .padding(.vertical, 3)
            .background(historyRowTint(hit: rowHit), in: .rect(cornerRadius: 4))
    }
}

private struct PlayerHistoryHitBadge: View {
    var hit: Bool?

    var body: some View {
        let label: String
        let icon: String
        let foreground: Color
        let background: Color

        switch hit {
        case true:
            label = "Hit"
            icon = "checkmark.circle.fill"
            foreground = AFLTheme.success
            background = AFLColor.positiveSurface
        case false:
            label = "Miss"
            icon = "xmark.circle.fill"
            foreground = AFLTheme.danger
            background = AFLColor.negativeSurface
        case nil:
            label = "--"
            icon = "minus.circle"
            foreground = AFLColor.navy700
            background = AFLColor.blue50
        }

        return Label(label, systemImage: icon)
            .font(.caption.weight(.semibold))
            .labelStyle(.titleAndIcon)
            .foregroundStyle(foreground)
            .frame(maxWidth: .infinity, alignment: .center)
            .padding(.horizontal, 7)
            .padding(.vertical, 4)
            .background(background.opacity(0.64), in: .capsule)
    }
}

private func resolvedStatLabel(filters: PlayerStatsFilters, options: PlayerStatFilterOptions?) -> String {
    options?.stats.first(where: { $0.code == filters.statCode })?.label ?? statLabel(filters.statCode)
}

private func summarizeFilterValues(
    _ selected: [String],
    allValues: [String]? = nil,
    maxVisible: Int = 3
) -> String? {
    guard !selected.isEmpty else { return nil }
    if let allValues, !allValues.isEmpty, Set(selected) == Set(allValues) {
        return "All"
    }
    let visible = selected.prefix(maxVisible)
    let suffix = selected.count > maxVisible ? " +\(selected.count - maxVisible)" : ""
    return visible.joined(separator: ", ") + suffix
}

private func historyRowTint(hit: Bool?) -> Color {
    switch hit {
    case true:
        AFLColor.positiveSurface.opacity(0.32)
    case false:
        AFLColor.negativeSurface.opacity(0.28)
    case nil:
        AFLColor.blue50.opacity(0.18)
    }
}

private func historyCellForeground(hit: Bool?, emphasized: Bool) -> Color {
    guard emphasized else { return AFLColor.navy950 }
    return switch hit {
    case true:
        AFLTheme.success
    case false:
        AFLTheme.danger
    case nil:
        AFLColor.navy950
    }
}

private struct ComparisonScenarioCard: View {
    var title: String
    var state: PlayerComparisonScenarioState
    var playerName: String?
    var onUseCurrent: () -> Void
    var onRefresh: () -> Void

    var body: some View {
        VStack(alignment: .leading, spacing: 10) {
            HStack(alignment: .top) {
                VStack(alignment: .leading, spacing: 3) {
                    Text(title)
                        .font(.headline)
                    Text(playerName ?? "No player selected")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }
                Spacer()
                Button("Use Current", action: onUseCurrent)
                    .controlSize(.small)
                Button {
                    onRefresh()
                } label: {
                    Image(systemName: "arrow.clockwise")
                }
                .buttonStyle(.borderless)
                .disabled(state.isLoading)
            }

            HStack(spacing: 8) {
                DenseScenarioMetric(title: "Games", value: "\(state.history.count)")
                DenseScenarioMetric(title: "Average", value: averageValue(state.history))
                DenseScenarioMetric(title: primaryOutcomeLabel(state.filters), value: primaryOutcomeValue(state.summary, filters: state.filters))
            }

            PlayerFilterChipRow(filters: state.filters)

            if state.isLoading {
                Label("Refreshing scenario", systemImage: "clock.arrow.circlepath")
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
            if let error = state.errorMessage {
                Text(error)
                    .font(.caption)
                    .foregroundStyle(AFLTheme.danger)
            }
            if let info = state.infoMessage {
                Text(info)
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .aflCard()
    }
}

private struct DenseScenarioMetric: View {
    var title: String
    var value: String

    var body: some View {
        VStack(alignment: .leading, spacing: 3) {
            Text(title)
                .font(.caption2)
                .foregroundStyle(.secondary)
            Text(value)
                .font(.callout.weight(.semibold))
                .monospacedDigit()
                .lineLimit(1)
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .padding(8)
        .background(AFLColor.blue50.opacity(0.82), in: .rect(cornerRadius: 8))
        .overlay(RoundedRectangle(cornerRadius: 8).stroke(AFLColor.blue200.opacity(0.72)))
    }
}

private struct PlayerFilterChipRow: View {
    var filters: PlayerStatsFilters

    var body: some View {
        ScrollView(.horizontal, showsIndicators: false) {
            HStack(spacing: 6) {
                Pill(statLabel(filters.statCode))
                Pill(lineLabel(filters))
                if !filters.lastGamesText.isEmpty {
                    Pill("Last \(filters.lastGamesText)")
                }
                if !filters.seasons.isEmpty {
                    Pill(filters.seasons.joined(separator: "/"))
                }
                if !filters.homeAway.isEmpty && Set(filters.homeAway) != Set(["Home", "Away"]) {
                    Pill(filters.homeAway.joined(separator: "/"))
                }
            }
        }
    }
}

private struct PlayerComparisonSummary: View {
    var scenarioA: PlayerComparisonScenarioState
    var scenarioB: PlayerComparisonScenarioState

    var body: some View {
        VStack(alignment: .leading, spacing: 12) {
            Text("Scenario Comparison")
                .font(.headline)
            ComparisonSummaryRow(label: "Games", scenarioA: "\(scenarioA.history.count)", scenarioB: "\(scenarioB.history.count)")
            ComparisonSummaryRow(label: "Average", scenarioA: averageValue(scenarioA.history), scenarioB: averageValue(scenarioB.history))
            ComparisonSummaryRow(
                label: primaryOutcomeLabel(scenarioA.filters),
                scenarioA: primaryOutcomeValue(scenarioA.summary, filters: scenarioA.filters),
                scenarioB: primaryOutcomeValue(scenarioB.summary, filters: scenarioB.filters)
            )
            ComparisonSummaryRow(
                label: secondaryOutcomeLabel(scenarioA.filters),
                scenarioA: secondaryOutcomeValue(scenarioA.summary, filters: scenarioA.filters),
                scenarioB: secondaryOutcomeValue(scenarioB.summary, filters: scenarioB.filters)
            )
            if scenarioA.isLoading || scenarioB.isLoading {
                ProgressView()
            }
            if let error = scenarioA.errorMessage ?? scenarioB.errorMessage {
                Text(error)
                    .foregroundStyle(AFLTheme.danger)
            }
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .aflCard()
    }
}

private struct ComparisonSummaryRow: View {
    var label: String
    var scenarioA: String
    var scenarioB: String

    var body: some View {
        Grid(alignment: .leading, horizontalSpacing: 18, verticalSpacing: 6) {
            GridRow {
                Text(label)
                    .foregroundStyle(.secondary)
                Text(scenarioA)
                    .monospacedDigit()
                Text(scenarioB)
                    .monospacedDigit()
            }
        }
        .font(.callout)
    }
}

private struct PlayerHistoryGraph: View {
    var history: [PlayerGameLogEntry]
    var filters: PlayerStatsFilters

    var body: some View {
        let ordered = history.sorted { $0.gameNumber < $1.gameNumber }
        let values = ordered.compactMap(\.selectedValue)
        if ordered.isEmpty || values.isEmpty {
            EmptyStateView(title: "No graph", message: "No selected stat values are available for the current filter set.")
        } else {
            VStack(alignment: .leading, spacing: 12) {
                Text("\(statLabel(filters.statCode)) Trend")
                    .font(.headline)
                PlayerHistoryCanvas(history: ordered, filters: filters)
                    .frame(maxWidth: .infinity, minHeight: 260, idealHeight: 280, maxHeight: 320)
                HStack(spacing: 12) {
                    GraphLegendDot(color: AFLTheme.success, label: "Hit")
                    GraphLegendDot(color: AFLTheme.danger, label: "Miss")
                    GraphLegendDot(color: AFLTheme.primary, label: "Reference")
                    Spacer()
                    Text("Oldest to latest")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }
            }
            .frame(maxWidth: .infinity, alignment: .leading)
            .aflCard()
        }
    }
}

private struct PlayerComparisonGraph: View {
    var scenarioA: PlayerComparisonScenarioState
    var scenarioB: PlayerComparisonScenarioState

    var body: some View {
        let values = scenarioA.history.compactMap(\.selectedValue) + scenarioB.history.compactMap(\.selectedValue)
        if scenarioA.isLoading || scenarioB.isLoading {
            LoadingStateView(message: "Loading comparison graph")
        } else if values.isEmpty {
            EmptyStateView(title: "No graph", message: "Adjust the scenario filters to load game history.")
        } else {
            VStack(alignment: .leading, spacing: 12) {
                Text("Scenario Comparison Graph")
                    .font(.headline)
                PlayerComparisonCanvas(scenarioA: scenarioA, scenarioB: scenarioB)
                    .frame(maxWidth: .infinity, minHeight: 260, idealHeight: 280, maxHeight: 320)
                HStack(spacing: 12) {
                    GraphLegendDot(color: AFLTheme.accent, label: "Scenario A")
                    GraphLegendDot(color: AFLTheme.primary, label: "Scenario B")
                    Spacer()
                    Text("Oldest to latest")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }
            }
            .frame(maxWidth: .infinity, alignment: .leading)
            .aflCard()
        }
    }
}

private struct PlayerHistoryCanvas: View {
    var history: [PlayerGameLogEntry]
    var filters: PlayerStatsFilters

    var body: some View {
        let values = history.compactMap(\.selectedValue)
        let guides = guideValues(filters)
        let range = chartRange(values: values, guides: guides)
        Canvas { context, size in
            let plot = chartPlotRect(size)
            drawGrid(context: context, plot: plot, range: range)
            drawGuides(context: context, plot: plot, range: range, filters: filters, color: AFLTheme.primary)
            drawHistoryPath(context: context, plot: plot, range: range, history: history, color: AFLColor.navy700)
            drawHistoryPoints(context: context, plot: plot, range: range, history: history)
            drawAxis(context: context, plot: plot)
        }
        .background(AFLColor.blue50.opacity(0.72), in: .rect(cornerRadius: 8))
        .overlay(RoundedRectangle(cornerRadius: 8).stroke(AFLColor.blue200.opacity(0.72)))
    }
}

private struct PlayerComparisonCanvas: View {
    var scenarioA: PlayerComparisonScenarioState
    var scenarioB: PlayerComparisonScenarioState

    var body: some View {
        let historyA = scenarioA.history.sorted { $0.gameNumber < $1.gameNumber }
        let historyB = scenarioB.history.sorted { $0.gameNumber < $1.gameNumber }
        let values = historyA.compactMap(\.selectedValue) + historyB.compactMap(\.selectedValue)
        let guides = guideValues(scenarioA.filters) + guideValues(scenarioB.filters)
        let range = chartRange(values: values, guides: guides)
        Canvas { context, size in
            let plot = chartPlotRect(size)
            drawGrid(context: context, plot: plot, range: range)
            drawGuides(context: context, plot: plot, range: range, filters: scenarioA.filters, color: AFLTheme.accent)
            drawGuides(context: context, plot: plot, range: range, filters: scenarioB.filters, color: AFLTheme.primary)
            drawHistoryPath(context: context, plot: plot, range: range, history: historyA, color: AFLTheme.accent)
            drawHistoryPath(context: context, plot: plot, range: range, history: historyB, color: AFLTheme.primary)
            drawComparisonPoints(context: context, plot: plot, range: range, history: historyA, color: AFLTheme.accent)
            drawComparisonPoints(context: context, plot: plot, range: range, history: historyB, color: AFLTheme.primary)
            drawAxis(context: context, plot: plot)
        }
        .background(AFLColor.blue50.opacity(0.72), in: .rect(cornerRadius: 8))
        .overlay(RoundedRectangle(cornerRadius: 8).stroke(AFLColor.blue200.opacity(0.72)))
    }
}

private struct GraphLegendDot: View {
    var color: Color
    var label: String

    var body: some View {
        HStack(spacing: 5) {
            Circle()
                .fill(color)
                .frame(width: 9, height: 9)
            Text(label)
                .font(.caption)
                .foregroundStyle(.secondary)
        }
    }
}

private struct PlayerFilterInspector: View {
    @Bindable var store: PlayerLabStore

    var body: some View {
        Form {
            Section {
                InspectorPrimaryActionBlock(
                    title: "Apply Filters",
                    subtitle: "Update the summary, graph, and game log after editing filters.",
                    secondaryTitle: "Refresh Player Data",
                    secondarySystemImage: "arrow.clockwise",
                    primaryAction: { store.applyFilters(store.filters) },
                    secondaryAction: { Task { await store.refresh() } }
                )
            }

            if let player = store.selectedPlayer {
                Section("Player") {
                    LabeledContent("Name", value: player.fullName)
                }
            }

            Section("Stat") {
                Picker("Metric", selection: $store.filters.statCode) {
                    ForEach(store.filterOptions?.stats ?? []) { option in
                        Text(option.label).tag(option.code)
                    }
                }
                Picker("Line mode", selection: $store.filters.lineMode) {
                    Text("Single").tag("single")
                    Text("Interval").tag("interval")
                }
                if store.filters.lineMode == "interval" {
                    TextField("Lower bound", text: $store.filters.lowerBoundText)
                    TextField("Upper bound", text: $store.filters.upperBoundText)
                } else {
                    TextField("Reference line", text: $store.filters.referenceLineText)
                }
            }

            Section("Sample") {
                TextField("Last games", text: $store.filters.lastGamesText)
                TextField("Minimum minutes", text: $store.filters.minutesMinimumText)
                HStack {
                    TextField("Margin min", text: $store.filters.marginMinText)
                    TextField("Margin max", text: $store.filters.marginMaxText)
                }
            }

            if let options = store.filterOptions {
                MultiSelectSection(title: "Seasons", options: options.seasons, selection: $store.filters.seasons)
                MultiSelectSection(title: "Oppositions", options: options.oppositions, selection: $store.filters.oppositions)
                MultiSelectSection(title: "Venues", options: options.venues, selection: $store.filters.venues)
                MultiSelectSection(title: "Weather", options: options.weatherCategories, selection: $store.filters.weatherCategories)
                MultiSelectSection(title: "Home/Away", options: options.homeAwayOptions, selection: $store.filters.homeAway)
            }

            Section("Comparison") {
                Picker("Mode", selection: Binding(
                    get: { store.comparisonViewMode },
                    set: { store.comparisonViewMode = $0 }
                )) {
                    ForEach(PlayerComparisonViewMode.allCases) { mode in
                        Text(mode.label).tag(mode)
                    }
                }
                Picker("Game log focus", selection: Binding(
                    get: { store.comparisonFocus },
                    set: { store.comparisonFocus = $0 }
                )) {
                    ForEach(PlayerComparisonFocus.allCases) { focus in
                        Text(focus.label).tag(focus)
                    }
                }
                Button("Refresh Scenarios") {
                    Task { await store.refreshComparison() }
                }
                Button("Use Current as Scenario A") {
                    store.copyCurrentFilters(to: .scenarioA)
                }
                Button("Use Current as Scenario B") {
                    store.copyCurrentFilters(to: .scenarioB)
                }
            }

            if store.mode == .comparison || store.comparisonViewMode != .summary {
                PlayerScenarioFilterEditor(
                    title: "Scenario A Filters",
                    filters: Binding(
                        get: { store.scenarioA.filters },
                        set: { store.setScenarioFilters($0, for: .scenarioA) }
                    ),
                    options: store.filterOptions,
                    onRefresh: { Task { await store.refreshComparison(focus: .scenarioA) } }
                )
                PlayerScenarioFilterEditor(
                    title: "Scenario B Filters",
                    filters: Binding(
                        get: { store.scenarioB.filters },
                        set: { store.setScenarioFilters($0, for: .scenarioB) }
                    ),
                    options: store.filterOptions,
                    onRefresh: { Task { await store.refreshComparison(focus: .scenarioB) } }
                )
            }
        }
        .formStyle(.grouped)
    }
}

private struct PlayerScenarioFilterEditor: View {
    var title: String
    @Binding var filters: PlayerStatsFilters
    var options: PlayerStatFilterOptions?
    var onRefresh: () -> Void

    var body: some View {
        Section(title) {
            TextField("Last games", text: $filters.lastGamesText)
            TextField("Minimum minutes", text: $filters.minutesMinimumText)
            HStack {
                TextField("Margin min", text: $filters.marginMinText)
                TextField("Margin max", text: $filters.marginMaxText)
            }
            Button("Refresh \(title.replacingOccurrences(of: " Filters", with: ""))") {
                onRefresh()
            }
        }
        if let options {
            MultiSelectSection(title: "\(title) Seasons", options: options.seasons, selection: $filters.seasons)
            MultiSelectSection(title: "\(title) Oppositions", options: options.oppositions, selection: $filters.oppositions)
            MultiSelectSection(title: "\(title) Venues", options: options.venues, selection: $filters.venues)
            MultiSelectSection(title: "\(title) Weather", options: options.weatherCategories, selection: $filters.weatherCategories)
            MultiSelectSection(title: "\(title) Home/Away", options: options.homeAwayOptions, selection: $filters.homeAway)
        }
    }
}

struct MultiSelectSection: View {
    var title: String
    var options: [String]
    @Binding var selection: [String]

    var body: some View {
        if !options.isEmpty {
            Section(title) {
                ForEach(options, id: \.self) { option in
                    Toggle(option, isOn: Binding(
                        get: { selection.contains(option) },
                        set: { selected in
                            if selected, !selection.contains(option) {
                                selection.append(option)
                            } else if !selected {
                                selection.removeAll { $0 == option }
                            }
                        }
                    ))
                }
            }
        }
    }
}

private func chartPlotRect(_ size: CGSize) -> CGRect {
    CGRect(
        x: 34,
        y: 16,
        width: max(size.width - 50, 1),
        height: max(size.height - 42, 1)
    )
}

private func chartRange(values: [Double], guides: [Double]) -> ClosedRange<Double> {
    let allValues = values + guides
    guard let low = allValues.min(), let high = allValues.max() else {
        return 0...1
    }
    let span = max(high - low, 1)
    let padding: Double
    if span <= 2 {
        padding = 0.5
    } else if span <= 8 {
        padding = 1
    } else {
        padding = span * 0.06
    }
    let lower = floor((low - padding) * 2) / 2
    let upper = ceil((high + padding) * 2) / 2
    guard lower < upper else { return (lower - 1)...(upper + 1) }
    return lower...upper
}

private func guideValues(_ filters: PlayerStatsFilters) -> [Double] {
    if filters.lineMode == "interval" {
        return [filters.lowerBoundText.doubleValue, filters.upperBoundText.doubleValue].compactMap(\.self)
    }
    return [filters.referenceLineText.doubleValue].compactMap(\.self)
}

private func xPosition(index: Int, count: Int, plot: CGRect) -> CGFloat {
    guard count > 1 else { return plot.midX }
    return plot.minX + (CGFloat(index) / CGFloat(count - 1)) * plot.width
}

private func yPosition(value: Double, range: ClosedRange<Double>, plot: CGRect) -> CGFloat {
    let span = max(range.upperBound - range.lowerBound, 1)
    let normalized = (value - range.lowerBound) / span
    return plot.maxY - CGFloat(normalized) * plot.height
}

private func drawGrid(context: GraphicsContext, plot: CGRect, range: ClosedRange<Double>) {
    for step in 0...4 {
        let fraction = CGFloat(step) / 4
        let y = plot.minY + fraction * plot.height
        var path = Path()
        path.move(to: CGPoint(x: plot.minX, y: y))
        path.addLine(to: CGPoint(x: plot.maxX, y: y))
        context.stroke(path, with: .color(AFLColor.blue300.opacity(0.35)), lineWidth: 1)
    }
}

private func drawAxis(context: GraphicsContext, plot: CGRect) {
    var path = Path()
    path.move(to: CGPoint(x: plot.minX, y: plot.maxY))
    path.addLine(to: CGPoint(x: plot.maxX, y: plot.maxY))
    context.stroke(path, with: .color(AFLColor.navy700.opacity(0.32)), lineWidth: 1.2)
}

private func drawGuides(
    context: GraphicsContext,
    plot: CGRect,
    range: ClosedRange<Double>,
    filters: PlayerStatsFilters,
    color: Color
) {
    if filters.lineMode == "interval",
       let lower = filters.lowerBoundText.doubleValue,
       let upper = filters.upperBoundText.doubleValue {
        let lowerY = yPosition(value: lower, range: range, plot: plot)
        let upperY = yPosition(value: upper, range: range, plot: plot)
        let band = CGRect(
            x: plot.minX,
            y: min(lowerY, upperY),
            width: plot.width,
            height: max(abs(lowerY - upperY), 1)
        )
        context.fill(Path(band), with: .color(color.opacity(0.12)))
        drawGuideLine(context: context, plot: plot, y: lowerY, color: color)
        drawGuideLine(context: context, plot: plot, y: upperY, color: color)
    } else if let line = filters.referenceLineText.doubleValue {
        drawGuideLine(context: context, plot: plot, y: yPosition(value: line, range: range, plot: plot), color: color)
    }
}

private func drawGuideLine(context: GraphicsContext, plot: CGRect, y: CGFloat, color: Color) {
    var path = Path()
    path.move(to: CGPoint(x: plot.minX, y: y))
    path.addLine(to: CGPoint(x: plot.maxX, y: y))
    context.stroke(
        path,
        with: .color(color.opacity(0.86)),
        style: StrokeStyle(lineWidth: 2, dash: [8, 6])
    )
}

private func drawHistoryPath(
    context: GraphicsContext,
    plot: CGRect,
    range: ClosedRange<Double>,
    history: [PlayerGameLogEntry],
    color: Color
) {
    var path = Path()
    var started = false
    for (index, entry) in history.enumerated() {
        guard let value = entry.selectedValue else { continue }
        let point = CGPoint(
            x: xPosition(index: index, count: history.count, plot: plot),
            y: yPosition(value: value, range: range, plot: plot)
        )
        if started {
            path.addLine(to: point)
        } else {
            path.move(to: point)
            started = true
        }
    }
    context.stroke(path, with: .color(color.opacity(0.7)), lineWidth: 2)
}

private func drawHistoryPoints(
    context: GraphicsContext,
    plot: CGRect,
    range: ClosedRange<Double>,
    history: [PlayerGameLogEntry]
) {
    for (index, entry) in history.enumerated() {
        guard let value = entry.selectedValue else { continue }
        let point = CGPoint(
            x: xPosition(index: index, count: history.count, plot: plot),
            y: yPosition(value: value, range: range, plot: plot)
        )
        let color = switch entry.hit {
        case true: AFLTheme.success
        case false: AFLTheme.danger
        case nil: AFLTheme.primary
        }
        context.fill(
            Path(ellipseIn: CGRect(x: point.x - 4.5, y: point.y - 4.5, width: 9, height: 9)),
            with: .color(color)
        )
    }
}

private func drawComparisonPoints(
    context: GraphicsContext,
    plot: CGRect,
    range: ClosedRange<Double>,
    history: [PlayerGameLogEntry],
    color: Color
) {
    for (index, entry) in history.enumerated() {
        guard let value = entry.selectedValue else { continue }
        let point = CGPoint(
            x: xPosition(index: index, count: history.count, plot: plot),
            y: yPosition(value: value, range: range, plot: plot)
        )
        context.fill(
            Path(ellipseIn: CGRect(x: point.x - 4, y: point.y - 4, width: 8, height: 8)),
            with: .color(color)
        )
    }
}

private func statLabel(_ code: String) -> String {
    code
        .replacingOccurrences(of: "_", with: " ")
        .split(separator: " ")
        .map { $0.capitalized }
        .joined(separator: " ")
}

private func lineLabel(_ filters: PlayerStatsFilters) -> String {
    if filters.lineMode == "interval" {
        return "\(filters.lowerBoundText)-\(filters.upperBoundText)"
    }
    return "Line \(filters.referenceLineText)"
}

@MainActor
private func averageValue(_ history: [PlayerGameLogEntry]) -> String {
    let values = history.compactMap(\.selectedValue)
    guard !values.isEmpty else { return "--" }
    let average = values.reduce(0, +) / Double(values.count)
    return String(format: "%.1f", average)
}

private func primaryOutcomeLabel(_ filters: PlayerStatsFilters) -> String {
    filters.lineMode == "interval" ? "In" : "Over"
}

private func secondaryOutcomeLabel(_ filters: PlayerStatsFilters) -> String {
    filters.lineMode == "interval" ? "Out" : "Under"
}

@MainActor
private func primaryOutcomeValue(_ summary: PlayerStatSummary?, filters: PlayerStatsFilters) -> String {
    guard let summary else { return "--" }
    let probability = filters.lineMode == "interval" ? summary.proportionWithinInterval : summary.proportionOver
    let price = filters.lineMode == "interval" ? summary.impliedOddsWithinInterval : summary.impliedOddsOver
    return "\(AFLFormatters.percent(probability)) @ \(AFLFormatters.decimalPrice(price))"
}

@MainActor
private func secondaryOutcomeValue(_ summary: PlayerStatSummary?, filters: PlayerStatsFilters) -> String {
    guard let summary else { return "--" }
    let probability = filters.lineMode == "interval" ? summary.proportionOutsideInterval : summary.proportionUnder
    let price = filters.lineMode == "interval" ? summary.impliedOddsOutsideInterval : summary.impliedOddsUnder
    return "\(AFLFormatters.percent(probability)) @ \(AFLFormatters.decimalPrice(price))"
}
