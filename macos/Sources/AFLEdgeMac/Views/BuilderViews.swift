import SwiftUI

// MARK: - SGM Builder

struct SgmBuilderView: View {
    @Bindable var store: SgmBuilderStore
    @Binding var showInspector: Bool

    var body: some View {
        VStack(alignment: .leading, spacing: 12) {
            BuilderTopBar(
                title: "SGM Builder",
                subtitle: "Build one-match multis and compare agency pricing.",
                displayMode: $store.displayMode
            )

            SgmControlBar(store: store)

            if !store.marketCodes.isEmpty {
                MarketFilterBar(marketCodes: store.marketCodes, selected: $store.marketFilter)
            }

            if let info = store.infoMessage {
                InfoStateView(message: info)
            }

            VSplitView {
                CandidatePane(
                    isLoading: store.isLoadingOptions,
                    errorMessage: store.errorMessage,
                    rows: store.visibleLegs,
                    selectedId: $store.selectedCandidateId,
                    selectedSelectionIds: Set(store.draft.legs.map(\.selectionId)),
                    displayMode: store.displayMode,
                    sortField: $store.sortField,
                    sortDescending: $store.sortDescending,
                    emptyTitle: "No eligible legs",
                    emptyMessage: "No SGM-ready selections matched the current controls."
                ) { row in
                    store.toggleLeg(row)
                }
                .frame(minHeight: 260)

                SgmBottomPanel(store: store)
                    .frame(minHeight: 200, idealHeight: 260)
            }
        }
        .padding()
        .aflDetailBackground()
        .navigationTitle("SGM Builder")
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
                    Label("Filters", systemImage: "line.3.horizontal.decrease.circle")
                }
            }
        }
        .inspector(isPresented: $showInspector) {
            SgmFiltersInspector(store: store)
                .inspectorColumnWidth(min: 320, ideal: 360, max: 420)
        }
        .task {
            await store.bootstrap()
        }
    }
}

// MARK: - CGM Builder

struct CgmBuilderView: View {
    @Bindable var store: CgmBuilderStore
    @Binding var showInspector: Bool

    var body: some View {
        VStack(alignment: .leading, spacing: 12) {
            BuilderTopBar(
                title: "CGM Builder",
                subtitle: "Build cross-game multis with one leg per match.",
                displayMode: $store.displayMode
            )

            CgmControlBar(store: store)

            if !store.marketCodes.isEmpty {
                MarketFilterBar(marketCodes: store.marketCodes, selected: $store.marketFilter)
            }

            if let info = store.infoMessage ?? store.state.infoMessage {
                InfoStateView(message: info)
            }

            VSplitView {
                CandidatePane(
                    isLoading: store.isLoadingOptions,
                    errorMessage: store.errorMessage ?? store.state.latestError,
                    rows: store.visibleLegs,
                    selectedId: $store.selectedCandidateId,
                    selectedSelectionIds: Set(store.state.selectedLegs.map(\.selectionId)),
                    displayMode: store.displayMode,
                    sortField: $store.sortField,
                    sortDescending: $store.sortDescending,
                    emptyTitle: "No player props",
                    emptyMessage: "No player props match the current filters and one-leg-per-game rule."
                ) { row in
                    store.toggleLeg(row)
                }
                .frame(minHeight: 260)

                CgmBottomPanel(store: store)
                    .frame(minHeight: 200, idealHeight: 260)
            }
        }
        .padding()
        .aflDetailBackground()
        .navigationTitle("CGM Builder")
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
                    Label("Filters", systemImage: "line.3.horizontal.decrease.circle")
                }
            }
        }
        .inspector(isPresented: $showInspector) {
            CgmFiltersInspector(store: store)
                .inspectorColumnWidth(min: 320, ideal: 360, max: 420)
        }
        .task {
            await store.bootstrap()
        }
    }
}

// MARK: - Top Bar

private struct BuilderTopBar: View {
    var title: String
    var subtitle: String
    @Binding var displayMode: BuilderDisplayMode

    var body: some View {
        HStack(alignment: .firstTextBaseline) {
            SectionHeader(title: title, subtitle: subtitle)
            Spacer()
            Picker("Display", selection: $displayMode) {
                ForEach(BuilderDisplayMode.allCases) { mode in
                    Text(mode.label).tag(mode)
                }
            }
            .pickerStyle(.segmented)
            .labelsHidden()
            .frame(width: 140)
        }
    }
}

// MARK: - Control Bars

private struct SgmControlBar: View {
    @Bindable var store: SgmBuilderStore

    var body: some View {
        HStack(spacing: 12) {
            Picker("Bookmaker", selection: Binding(
                get: { store.selectedBookmaker ?? "" },
                set: { if !$0.isEmpty { store.selectBookmaker($0) } }
            )) {
                ForEach(store.bookmakers.filter(\.enabled)) { bookmaker in
                    Text(bookmaker.displayName).tag(bookmaker.code)
                }
            }
            .frame(width: 220)

            Picker("Match", selection: Binding(
                get: { store.selectedEventId ?? -1 },
                set: { if $0 != -1 { store.selectEvent($0) } }
            )) {
                Text("Select a match").tag(-1)
                ForEach(store.events) { event in
                    Text(AFLFormatters.shortAFLMatchLabel(event.matchName)).tag(event.id)
                }
            }
            .frame(width: 260)

            Toggle("Best only", isOn: Binding(
                get: { store.bestOnly },
                set: { store.setBestOnly($0) }
            ))
            .toggleStyle(.switch)

            Spacer()
        }
    }
}

private struct CgmControlBar: View {
    @Bindable var store: CgmBuilderStore

    var body: some View {
        HStack(spacing: 12) {
            Picker("Bookmaker", selection: Binding(
                get: { store.state.selectedBookmaker ?? "" },
                set: { if !$0.isEmpty { store.selectBookmaker($0) } }
            )) {
                ForEach(store.bookmakers.filter(\.enabled)) { bookmaker in
                    Text(bookmaker.displayName).tag(bookmaker.code)
                }
            }
            .frame(width: 220)

            Toggle("Best only", isOn: Binding(
                get: { store.bestOnly },
                set: { store.setBestOnly($0) }
            ))
            .toggleStyle(.switch)

            if !store.state.selectedEventIds.isEmpty {
                Pill("\(store.state.selectedEventIds.count) match filter")
                Button("Clear match filter") {
                    store.draftStore.clearEventSelection()
                }
                .buttonStyle(.link)
            }

            Spacer()
        }
    }
}

// MARK: - Market Filter Bar

private struct MarketFilterBar: View {
    var marketCodes: [String]
    @Binding var selected: String

    var body: some View {
        ScrollView(.horizontal, showsIndicators: false) {
            HStack(spacing: 8) {
                TogglePillButton(title: "All", isSelected: selected == "__all__") {
                    selected = "__all__"
                }
                ForEach(marketCodes, id: \.self) { code in
                    TogglePillButton(
                        title: prettyMarketName(code),
                        isSelected: selected == code
                    ) {
                        selected = code
                    }
                }
            }
        }
    }

    private func prettyMarketName(_ code: String) -> String {
        code.replacingOccurrences(of: "player_", with: "")
            .replacingOccurrences(of: "_", with: " ")
            .capitalized
    }
}

// MARK: - Candidate Pane

private struct CandidatePane: View {
    var isLoading: Bool
    var errorMessage: String?
    var rows: [OddsSearchResult]
    @Binding var selectedId: OddsSearchResult.ID?
    var selectedSelectionIds: Set<Int>
    var displayMode: BuilderDisplayMode
    @Binding var sortField: BuilderSortField
    @Binding var sortDescending: Bool
    var emptyTitle: String
    var emptyMessage: String
    var onToggle: (OddsSearchResult) -> Void

    var body: some View {
        VStack(alignment: .leading, spacing: 10) {
            CandidatePaneHeader(
                rowCount: rows.count,
                displayMode: displayMode,
                sortField: $sortField,
                sortDescending: $sortDescending
            )

            if let errorMessage {
                ErrorStateView(message: errorMessage)
            }

            Group {
                if isLoading {
                    LoadingStateView(message: "Loading candidate legs…")
                } else if rows.isEmpty {
                    EmptyStateView(title: emptyTitle, message: emptyMessage)
                } else if displayMode == .tile {
                    CandidateLegGrid(
                        rows: rows,
                        selectedSelectionIds: selectedSelectionIds,
                        onToggle: onToggle
                    )
                } else {
                    CandidateLegTable(
                        rows: rows,
                        selectedId: $selectedId,
                        selectedSelectionIds: selectedSelectionIds,
                        onToggle: onToggle
                    )
                }
            }
            .frame(maxWidth: .infinity, maxHeight: .infinity)
        }
    }
}

private struct CandidatePaneHeader: View {
    var rowCount: Int
    var displayMode: BuilderDisplayMode
    @Binding var sortField: BuilderSortField
    @Binding var sortDescending: Bool

    var body: some View {
        HStack {
            Text("Candidate Legs")
                .font(.headline)
            Text("(\(rowCount))")
                .foregroundStyle(.secondary)
                .monospacedDigit()
            Spacer()
            if displayMode == .tile {
                Picker("Sort", selection: $sortField) {
                    ForEach(BuilderSortField.allCases) { field in
                        Text(field.label).tag(field)
                    }
                }
                .frame(width: 180)
                Button {
                    sortDescending.toggle()
                } label: {
                    Image(systemName: sortDescending ? "arrow.down" : "arrow.up")
                }
                .help(sortDescending ? "Sort descending" : "Sort ascending")
                .buttonStyle(.bordered)
            } else {
                Text("Click a column header to sort · Double-click a row to add")
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
        }
    }
}

// MARK: - Candidate Table (sortable)

private struct CandidateLegTable: View {
    var rows: [OddsSearchResult]
    @Binding var selectedId: OddsSearchResult.ID?
    var selectedSelectionIds: Set<Int>
    var onToggle: (OddsSearchResult) -> Void

    @State private var sortOrder: [KeyPathComparator<OddsSearchResult>] = [
        KeyPathComparator(\OddsSearchResult.nextBestSort, order: .reverse),
    ]

    private var sortedRows: [OddsSearchResult] {
        rows.sorted(using: sortOrder)
    }

    var body: some View {
        Table(sortedRows, selection: $selectedId, sortOrder: $sortOrder) {
            TableColumn("") { row in
                Image(systemName: selectedSelectionIds.contains(row.selectionId) ? "checkmark.circle.fill" : "circle")
                    .foregroundStyle(selectedSelectionIds.contains(row.selectionId) ? AFLTheme.accent : .secondary)
            }
            .width(22)

            TableColumn("Player", value: \.playerSortKey) { row in
                VStack(alignment: .leading, spacing: 2) {
                    Text(row.player?.fullName ?? row.label)
                        .fontWeight(.medium)
                    Text(AFLFormatters.shortAFLMatchLabel(row.matchName))
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }
            }

            TableColumn("Market", value: \.marketDisplayName) { row in
                Text(row.marketDisplayName)
            }

            TableColumn("Line", value: \.lineSort) { row in
                Text(row.lineValue.map { String(format: "%.1f", $0) } ?? "--")
                    .monospacedDigit()
            }
            .width(56)

            TableColumn("Side", value: \.selectionType) { row in
                Text(row.selectionType.capitalized)
            }
            .width(64)

            TableColumn("Price", value: \.priceSort) { row in
                Text(AFLFormatters.decimalPrice(row.decimalPrice))
                    .monospacedDigit()
                    .fontWeight(.semibold)
            }
            .width(64)

            TableColumn("NB", value: \.nextBestSort) { row in
                Text(AFLFormatters.signedMetric(row.nextBestProbDiff))
                    .monospacedDigit()
            }
            .width(64)

            TableColumn("L10", value: \.diffLast10Sort) { row in
                Text(AFLFormatters.signedMetric(row.diffLast10))
                    .monospacedDigit()
            }
            .width(64)

            TableColumn("2025", value: \.diff2025Sort) { row in
                Text(AFLFormatters.signedMetric(row.diff2025))
                    .monospacedDigit()
            }
            .width(64)

            TableColumn("Tags") { row in
                HStack(spacing: 4) {
                    if row.isBestPrice {
                        Pill("Best", systemImage: "star.fill")
                    }
                    if let matchup = AFLFormatters.matchupDifficultyTag(row.matchupDifficulty) {
                        Pill(matchup)
                    }
                }
            }
        }
        .contextMenu(forSelectionType: OddsSearchResult.ID.self) { ids in
            if let id = ids.first, let row = sortedRows.first(where: { $0.id == id }) {
                Button(selectedSelectionIds.contains(row.selectionId) ? "Remove leg" : "Add leg") {
                    onToggle(row)
                }
            }
        } primaryAction: { ids in
            if let id = ids.first, let row = sortedRows.first(where: { $0.id == id }) {
                onToggle(row)
            }
        }
        .aflTableSurface()
    }
}

// MARK: - Candidate Grid

private struct CandidateLegGrid: View {
    var rows: [OddsSearchResult]
    var selectedSelectionIds: Set<Int>
    var onToggle: (OddsSearchResult) -> Void

    private let columns = [
        GridItem(.adaptive(minimum: 240, maximum: 320), spacing: 10, alignment: .top),
    ]

    var body: some View {
        ScrollView {
            LazyVGrid(columns: columns, alignment: .leading, spacing: 10) {
                ForEach(rows) { row in
                    CandidateLegCard(
                        row: row,
                        isSelected: selectedSelectionIds.contains(row.selectionId),
                        onToggle: { onToggle(row) }
                    )
                }
            }
            .padding(.vertical, 2)
        }
    }
}

private struct CandidateLegCard: View {
    var row: OddsSearchResult
    var isSelected: Bool
    var onToggle: () -> Void

    var body: some View {
        Button(action: onToggle) {
            VStack(alignment: .leading, spacing: 8) {
                HStack(alignment: .top) {
                    VStack(alignment: .leading, spacing: 2) {
                        Text(row.player?.fullName ?? row.label)
                            .font(.headline)
                            .lineLimit(1)
                        Text(AFLFormatters.shortAFLMatchLabel(row.matchName))
                            .font(.caption)
                            .foregroundStyle(.secondary)
                    }
                    Spacer()
                    Text(AFLFormatters.decimalPrice(row.decimalPrice))
                        .font(.title3.weight(.semibold))
                        .monospacedDigit()
                        .foregroundStyle(AFLTheme.accent)
                }

                Text(row.label)
                    .font(.subheadline)
                    .lineLimit(2)
                    .multilineTextAlignment(.leading)

                HStack(spacing: 4) {
                    Pill(row.marketDisplayName)
                    if let line = row.lineValue {
                        Pill(String(format: "%.1f", line))
                    }
                    if row.isBestPrice {
                        Pill("Best", systemImage: "star.fill")
                    }
                }

                HStack(spacing: 10) {
                    Text("NB \(AFLFormatters.signedMetric(row.nextBestProbDiff))")
                        .monospacedDigit()
                    Text("L10 \(AFLFormatters.signedMetric(row.diffLast10))")
                        .monospacedDigit()
                }
                .font(.caption)
                .foregroundStyle(.secondary)
            }
            .padding()
            .frame(maxWidth: .infinity, alignment: .leading)
            .background(
                isSelected
                    ? AnyShapeStyle(AFLColor.orange100.opacity(0.92))
                    : AnyShapeStyle(AFLTheme.cardBackground),
                in: .rect(cornerRadius: 10)
            )
            .overlay(
                RoundedRectangle(cornerRadius: 10)
                    .stroke(isSelected ? AFLTheme.accent.opacity(0.55) : AFLColor.blue200.opacity(0.72), lineWidth: isSelected ? 1.5 : 1)
            )
        }
        .buttonStyle(.plain)
        .contextMenu {
            Button(isSelected ? "Remove leg" : "Add leg", action: onToggle)
        }
        .disabled(row.decimalPrice == nil)
    }
}

// MARK: - SGM Bottom Panel

private struct SgmBottomPanel: View {
    @Bindable var store: SgmBuilderStore

    private var canCompare: Bool {
        store.draft.legs.count >= 2 && !store.isLoadingQuote
    }

    private var combinedProduct: Double? {
        let prices = store.draft.legs.map(\.basePrice)
        guard !prices.isEmpty else { return nil }
        return prices.reduce(1.0, *)
    }

    var body: some View {
        HStack(spacing: 12) {
            DraftSection(
                title: "Draft",
                legCount: store.draft.legs.count,
                subtitle: store.draft.eventLabel,
                emptyMessage: "Double-click a candidate leg to add it to your multi.",
                legs: store.draft.legs,
                onRemove: { store.draftStore.removeLeg(selectionId: $0) },
                onClear: store.draft.legs.isEmpty ? nil : { store.draftStore.clear() }
            )
            .frame(minWidth: 280, idealWidth: 340, maxWidth: 400)

            CompareSection(
                combinedProduct: combinedProduct,
                buttonTitle: store.isLoadingQuote ? "Comparing…" : "Compare Agencies",
                isEnabled: canCompare,
                isBusy: store.isLoadingQuote,
                hint: store.draft.legs.count < 2 ? "Add at least 2 legs to compare." : nil,
                error: store.draft.latestError
            ) {
                Task { await store.quote() }
            }
            .frame(width: 230)

            ComparisonResultsSection(
                results: store.draft.latestComparisons.map { .init(
                    id: $0.id,
                    bookmaker: $0.bookmaker,
                    quotedPrice: $0.quotedPrice,
                    detail: "Unadjusted \(AFLFormatters.decimalPrice($0.unadjustedPrice))"
                ) }
            )
            .frame(maxWidth: .infinity)
        }
        .padding(.top, 6)
    }
}

// MARK: - CGM Bottom Panel

private struct CgmBottomPanel: View {
    @Bindable var store: CgmBuilderStore

    private var canCompare: Bool {
        store.state.selectedLegs.count >= 2 && !store.isComparing
    }

    private var combinedProduct: Double? {
        let prices = store.state.selectedLegs.map(\.basePrice)
        guard !prices.isEmpty else { return nil }
        return prices.reduce(1.0, *)
    }

    var body: some View {
        HStack(spacing: 12) {
            DraftSection(
                title: "Draft",
                legCount: store.state.selectedLegs.count,
                subtitle: store.state.selectedBookmaker,
                emptyMessage: "Double-click a candidate leg to add it. One leg per match.",
                legs: store.state.selectedLegs,
                onRemove: { store.draftStore.removeLeg(selectionId: $0) },
                onClear: store.state.selectedLegs.isEmpty ? nil : { store.draftStore.clearDraft() }
            )
            .frame(minWidth: 280, idealWidth: 340, maxWidth: 400)

            CompareSection(
                combinedProduct: combinedProduct,
                buttonTitle: store.isComparing ? "Comparing…" : "Compare Agencies",
                isEnabled: canCompare,
                isBusy: store.isComparing,
                hint: store.state.selectedLegs.count < 2 ? "Add legs from at least 2 different matches." : nil,
                error: store.state.latestError
            ) {
                Task { await store.compare() }
            }
            .frame(width: 230)

            ComparisonResultsSection(
                results: store.state.comparisonResults.map { .init(
                    id: $0.id,
                    bookmaker: $0.bookmaker,
                    quotedPrice: $0.quotedPrice,
                    detail: "\($0.selectionCount) selections"
                ) }
            )
            .frame(maxWidth: .infinity)
        }
        .padding(.top, 6)
    }
}

// MARK: - Shared Bottom Panel Sections

private struct DraftSection: View {
    var title: String
    var legCount: Int
    var subtitle: String?
    var emptyMessage: String
    var legs: [DraftLeg]
    var onRemove: (Int) -> Void
    var onClear: (() -> Void)?

    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            HStack(alignment: .firstTextBaseline) {
                HStack(spacing: 6) {
                    Text(title).font(.headline)
                    Text("· \(legCount) \(legCount == 1 ? "leg" : "legs")")
                        .foregroundStyle(.secondary)
                        .monospacedDigit()
                }
                Spacer()
                if let onClear {
                    Button("Clear", action: onClear)
                        .buttonStyle(.link)
                }
            }
            if let subtitle, !subtitle.isEmpty {
                Text(subtitle)
                    .font(.caption)
                    .foregroundStyle(.secondary)
                    .padding(.top, 2)
            }

            Divider().padding(.top, 8)

            if legs.isEmpty {
                VStack(spacing: 8) {
                    Image(systemName: "square.stack.3d.up")
                        .font(.title2)
                        .foregroundStyle(.secondary)
                    Text(emptyMessage)
                        .font(.caption)
                        .foregroundStyle(.secondary)
                        .multilineTextAlignment(.center)
                }
                .frame(maxWidth: .infinity, maxHeight: .infinity)
                .padding(16)
            } else {
                ScrollView {
                    VStack(spacing: 6) {
                        ForEach(legs) { leg in
                            DraftLegCard(leg: leg) {
                                onRemove(leg.selectionId)
                            }
                        }
                    }
                    .padding(.vertical, 8)
                }
            }
        }
        .padding(12)
        .background(AFLTheme.cardBackground, in: .rect(cornerRadius: 10))
        .overlay(
            RoundedRectangle(cornerRadius: 10)
                .stroke(AFLColor.blue200.opacity(0.72))
        )
    }
}

private struct CompareSection: View {
    var combinedProduct: Double?
    var buttonTitle: String
    var isEnabled: Bool
    var isBusy: Bool
    var hint: String?
    var error: String?
    var action: () -> Void

    var body: some View {
        VStack(alignment: .leading, spacing: 10) {
            if let combinedProduct {
                VStack(alignment: .leading, spacing: 2) {
                    Text("Unadjusted product")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                    Text(AFLFormatters.decimalPrice(combinedProduct))
                        .font(.title.weight(.bold))
                        .monospacedDigit()
                        .foregroundStyle(AFLTheme.accent)
                }
            } else {
                VStack(alignment: .leading, spacing: 2) {
                    Text("Unadjusted product")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                    Text("--")
                        .font(.title.weight(.bold))
                        .monospacedDigit()
                        .foregroundStyle(.secondary)
                }
            }

            Button(action: action) {
                HStack {
                    if isBusy {
                        ProgressView().controlSize(.small)
                    } else {
                        Image(systemName: "dollarsign.arrow.circlepath")
                    }
                    Text(buttonTitle)
                        .fontWeight(.semibold)
                }
                .frame(maxWidth: .infinity)
                .padding(.vertical, 4)
            }
            .buttonStyle(.borderedProminent)
            .controlSize(.large)
            .disabled(!isEnabled)
            .keyboardShortcut(.return, modifiers: [.command])

            if let hint {
                Text(hint)
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
            if let error {
                Text(error)
                    .font(.caption)
                    .foregroundStyle(.red)
            }

            Spacer(minLength: 0)
        }
        .padding(12)
        .frame(maxHeight: .infinity, alignment: .top)
        .background(AFLTheme.cardBackground, in: .rect(cornerRadius: 10))
        .overlay(
            RoundedRectangle(cornerRadius: 10)
                .stroke(AFLColor.blue200.opacity(0.72))
        )
    }
}

private struct ComparisonResultItem: Identifiable {
    var id: String
    var bookmaker: String
    var quotedPrice: Double
    var detail: String
}

private struct ComparisonResultsSection: View {
    var results: [ComparisonResultItem]

    private var sorted: [ComparisonResultItem] {
        results.sorted { $0.quotedPrice > $1.quotedPrice }
    }

    private var bestPrice: Double? {
        results.map(\.quotedPrice).max()
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            HStack {
                Text("Agency Comparison")
                    .font(.headline)
                Text("(\(results.count))")
                    .foregroundStyle(.secondary)
                    .monospacedDigit()
                Spacer()
            }

            Divider().padding(.top, 8)

            if results.isEmpty {
                VStack(spacing: 8) {
                    Image(systemName: "chart.bar.doc.horizontal")
                        .font(.title2)
                        .foregroundStyle(.secondary)
                    Text("Press Compare Agencies to price your multi across bookmakers.")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                        .multilineTextAlignment(.center)
                }
                .frame(maxWidth: .infinity, maxHeight: .infinity)
                .padding(16)
            } else {
                ScrollView {
                    VStack(spacing: 6) {
                        ForEach(sorted) { result in
                            ComparisonRow(
                                title: result.bookmaker,
                                price: result.quotedPrice,
                                detail: result.detail,
                                isBest: result.quotedPrice == bestPrice
                            )
                        }
                    }
                    .padding(.vertical, 8)
                }
            }
        }
        .padding(12)
        .background(AFLTheme.cardBackground, in: .rect(cornerRadius: 10))
        .overlay(
            RoundedRectangle(cornerRadius: 10)
                .stroke(AFLColor.blue200.opacity(0.72))
        )
    }
}

private struct DraftLegCard: View {
    var leg: DraftLeg
    var onRemove: () -> Void

    var body: some View {
        HStack(alignment: .top, spacing: 10) {
            VStack(alignment: .leading, spacing: 3) {
                Text(leg.label)
                    .font(.subheadline.weight(.medium))
                    .lineLimit(2)
                Text(leg.eventLabel)
                    .font(.caption)
                    .foregroundStyle(.secondary)
                    .lineLimit(1)
            }
            Spacer()
            VStack(alignment: .trailing, spacing: 3) {
                Text(AFLFormatters.decimalPrice(leg.basePrice))
                    .font(.subheadline.weight(.semibold))
                    .monospacedDigit()
                    .foregroundStyle(AFLTheme.accent)
                Button {
                    onRemove()
                } label: {
                    Image(systemName: "xmark.circle.fill")
                        .foregroundStyle(.secondary)
                }
                .buttonStyle(.plain)
                .help("Remove leg")
            }
        }
        .padding(10)
        .background(AFLColor.blue50.opacity(0.55), in: .rect(cornerRadius: 8))
        .overlay(
            RoundedRectangle(cornerRadius: 8)
                .stroke(AFLColor.blue200.opacity(0.72))
        )
    }
}

private struct ComparisonRow: View {
    var title: String
    var price: Double
    var detail: String
    var isBest: Bool

    var body: some View {
        HStack {
            VStack(alignment: .leading, spacing: 2) {
                HStack(spacing: 6) {
                    Text(title)
                        .font(.subheadline.weight(.semibold))
                    if isBest {
                        Pill("Best", systemImage: "star.fill")
                    }
                }
                Text(detail)
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
            Spacer()
            Text(AFLFormatters.decimalPrice(price))
                .font(.title3.weight(.semibold))
                .monospacedDigit()
                .foregroundStyle(isBest ? AFLTheme.accent : .primary)
        }
        .padding(10)
        .background(
            isBest
                ? AnyShapeStyle(AFLColor.orange100.opacity(0.55))
                : AnyShapeStyle(AFLColor.blue50.opacity(0.45)),
            in: .rect(cornerRadius: 8)
        )
        .overlay(
            RoundedRectangle(cornerRadius: 8)
                .stroke(isBest ? AFLTheme.accent.opacity(0.45) : AFLColor.blue200.opacity(0.72))
        )
    }
}

// MARK: - Inspector (filters only)

private struct SgmFiltersInspector: View {
    @Bindable var store: SgmBuilderStore

    var body: some View {
        Form {
            Section {
                InspectorPrimaryActionBlock(
                    title: "Apply Filters",
                    subtitle: "Update candidate legs for the current bookmaker, match, and metric filters.",
                    secondaryTitle: "Refresh Builder Data",
                    secondarySystemImage: "arrow.clockwise",
                    primaryAction: { store.applyMetricFilters(store.metricFilters) },
                    secondaryAction: { Task { await store.refresh() } }
                )
            }

            Section("Quote Options") {
                Toggle("Force refresh on next compare", isOn: Binding(
                    get: { store.draft.forceRefresh },
                    set: { store.draftStore.setForceRefresh($0) }
                ))
            }
            MetricFiltersForm(filters: $store.metricFilters) {
                store.applyMetricFilters(store.metricFilters)
            }
        }
        .formStyle(.grouped)
    }
}

private struct CgmFiltersInspector: View {
    @Bindable var store: CgmBuilderStore

    var body: some View {
        Form {
            Section {
                InspectorPrimaryActionBlock(
                    title: "Apply Filters",
                    subtitle: "Update cross-game candidates for the current bookmaker, match filter, and metrics.",
                    secondaryTitle: "Refresh Builder Data",
                    secondarySystemImage: "arrow.clockwise",
                    primaryAction: { store.applyMetricFilters(store.metricFilters) },
                    secondaryAction: { Task { await store.refresh() } }
                )
            }

            Section("Match Filter") {
                if store.availableEvents.isEmpty {
                    Text("No matches available.")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                } else {
                    Button("Clear Match Filter") {
                        store.draftStore.clearEventSelection()
                    }
                    .disabled(store.state.selectedEventIds.isEmpty)
                    ForEach(store.availableEvents) { event in
                        Toggle(AFLFormatters.shortAFLMatchLabel(event.matchName), isOn: Binding(
                            get: { store.state.selectedEventIds.contains(event.id) },
                            set: { _ in store.draftStore.toggleEventSelection(event.id) }
                        ))
                    }
                }
            }
            MetricFiltersForm(filters: $store.metricFilters) {
                store.applyMetricFilters(store.metricFilters)
            }
        }
        .formStyle(.grouped)
    }
}

private struct MetricFiltersForm: View {
    @Binding var filters: SelectionMetricFilters
    var onApply: () -> Void

    var body: some View {
        MultiSelectSection(title: "Matchup", options: matchupDifficultyOptions, selection: $filters.matchupDifficulties)
        Section("Price") {
            TextField("Min price", text: $filters.minPriceText)
            TextField("Max price", text: $filters.maxPriceText)
        }
        MetricRangeSection(title: "Diff L10", min: $filters.minDiffLast10, max: $filters.maxDiffLast10)
        MetricRangeSection(title: "Diff 2025", min: $filters.minDiff2025, max: $filters.maxDiff2025)
        MetricRangeSection(title: "Next best diff", min: $filters.minNextBestProbDiff, max: $filters.maxNextBestProbDiff)
        Section("Quick Filters") {
            ForEach(QuickFilterPreset.allCases) { preset in
                Button(preset.label) {
                    filters = filters.applying(preset)
                    onApply()
                }
            }
        }
    }
}
