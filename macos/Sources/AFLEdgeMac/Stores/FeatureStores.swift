import Foundation
import Observation

@MainActor
@Observable
final class PlayerLabStore {
    private let api: AFLAPIClient

    var searchQuery = "Tim English"
    var allPlayers: [PlayerSummary] = []
    var searchResults: [PlayerSummary] = []
    var selectedPlayer: PlayerSummary?
    var filterOptions: PlayerStatFilterOptions?
    var filters = PlayerStatsFilters()
    var history: [PlayerGameLogEntry] = []
    var summary: PlayerStatSummary?
    var selectedGameId: PlayerGameLogEntry.ID?
    var mode: PlayerLabMode = .stats
    var historyViewMode: PlayerHistoryViewMode = .table
    var comparisonViewMode: PlayerComparisonViewMode = .summary
    var comparisonFocus: PlayerComparisonFocus = .scenarioA
    var scenarioA = PlayerComparisonScenarioState()
    var scenarioB = PlayerComparisonScenarioState()
    var isLoading = false
    var errorMessage: String?
    var infoMessage: String?

    init(api: AFLAPIClient) {
        self.api = api
    }

    func bootstrap() async {
        guard allPlayers.isEmpty else { return }
        isLoading = true
        errorMessage = nil
        do {
            let players = try await api.searchStatPlayers(query: "", limit: 5000)
            allPlayers = players
            let selected = players.first { $0.fullName.localizedCaseInsensitiveCompare("Tim English") == .orderedSame } ?? players.first
            selectedPlayer = selected
            searchQuery = selected?.fullName ?? "Tim English"
            searchResults = filterPlayers(searchQuery)
            if let selected {
                await loadPlayer(selected)
            } else {
                isLoading = false
                errorMessage = "Could not find the default player."
            }
        } catch {
            isLoading = false
            errorMessage = error.localizedDescription
        }
    }

    func updateSearchQuery(_ query: String) {
        searchQuery = query
        searchResults = filterPlayers(query)
        errorMessage = nil
    }

    func selectPlayer(_ player: PlayerSummary) {
        selectedPlayer = player
        searchQuery = player.fullName
        searchResults = filterPlayers(player.fullName)
        Task { await loadPlayer(player) }
    }

    func openFromOdds(_ row: OddsSearchResult) async {
        guard let rowPlayer = row.player else { return }
        if allPlayers.isEmpty {
            await bootstrap()
        }
        let player = allPlayers.first { $0.id == rowPlayer.id } ?? rowPlayer
        selectedPlayer = player
        searchQuery = player.fullName
        searchResults = filterPlayers(player.fullName)
        await loadPlayer(player, oddsRow: row)
    }

    func applyFilters(_ filters: PlayerStatsFilters) {
        self.filters = filters
        Task { await refresh() }
    }

    func setMode(_ mode: PlayerLabMode) {
        self.mode = mode
        if mode == .comparison {
            Task { await refreshComparisonIfNeeded() }
        }
    }

    func copyCurrentFilters(to focus: PlayerComparisonFocus) {
        switch focus {
        case .scenarioA:
            scenarioA = PlayerComparisonScenarioState(filters: filters)
        case .scenarioB:
            scenarioB = PlayerComparisonScenarioState(filters: filters)
        }
        Task { await refreshComparison(focus: focus) }
    }

    func setScenarioFilters(_ filters: PlayerStatsFilters, for focus: PlayerComparisonFocus) {
        switch focus {
        case .scenarioA:
            scenarioA.filters = filters
            scenarioA.history = []
            scenarioA.summary = nil
            scenarioA.errorMessage = nil
            scenarioA.infoMessage = nil
        case .scenarioB:
            scenarioB.filters = filters
            scenarioB.history = []
            scenarioB.summary = nil
            scenarioB.errorMessage = nil
            scenarioB.infoMessage = nil
        }
    }

    func refresh() async {
        guard let player = selectedPlayer else { return }
        isLoading = true
        errorMessage = nil
        infoMessage = nil
        do {
            async let historyResult = api.playerStatHistory(playerId: player.id, filters: filters)
            if filters.canRequestSummary {
                async let summaryResult = api.playerStatSummary(playerId: player.id, filters: filters)
                history = try await historyResult
                summary = try await summaryResult
            } else {
                history = try await historyResult
                summary = nil
            }
            selectedGameId = history.first?.id
            infoMessage = playerSummaryInfoMessage(filters)
            isLoading = false
            AppLog.app.info("Player Lab refreshed")
        } catch {
            isLoading = false
            errorMessage = error.localizedDescription
        }
    }

    func refreshComparisonIfNeeded() async {
        guard scenarioA.history.isEmpty, scenarioB.history.isEmpty else { return }
        await refreshComparison()
    }

    func refreshComparison() async {
        guard let player = selectedPlayer else { return }
        let filtersA = scenarioA.filters
        let filtersB = scenarioB.filters
        scenarioA.isLoading = true
        scenarioA.errorMessage = nil
        scenarioA.infoMessage = nil
        scenarioB.isLoading = true
        scenarioB.errorMessage = nil
        scenarioB.infoMessage = nil
        async let loadedA = comparisonScenario(playerId: player.id, filters: filtersA)
        async let loadedB = comparisonScenario(playerId: player.id, filters: filtersB)
        scenarioA = await loadedA
        scenarioB = await loadedB
        AppLog.app.info("Player comparison refreshed")
    }

    func refreshComparison(focus: PlayerComparisonFocus) async {
        guard let player = selectedPlayer else { return }
        switch focus {
        case .scenarioA:
            let filters = scenarioA.filters
            scenarioA.isLoading = true
            scenarioA.errorMessage = nil
            scenarioA.infoMessage = nil
            scenarioA = await comparisonScenario(playerId: player.id, filters: filters)
        case .scenarioB:
            let filters = scenarioB.filters
            scenarioB.isLoading = true
            scenarioB.errorMessage = nil
            scenarioB.infoMessage = nil
            scenarioB = await comparisonScenario(playerId: player.id, filters: filters)
        }
    }

    private func loadPlayer(_ player: PlayerSummary, oddsRow: OddsSearchResult? = nil) async {
        isLoading = true
        errorMessage = nil
        summary = nil
        history = []
        do {
            let options = try await api.playerStatFilters(playerId: player.id)
            filterOptions = options
            filters = filtersForOddsRow(options: options, defaults: defaultPlayerStatsFilters(options: options), row: oddsRow)
            scenarioA = PlayerComparisonScenarioState(filters: filters)
            scenarioB = PlayerComparisonScenarioState(filters: filters)
            await refresh()
        } catch {
            isLoading = false
            errorMessage = error.localizedDescription
        }
    }

    private func filterPlayers(_ query: String) -> [PlayerSummary] {
        let trimmed = query.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !trimmed.isEmpty else { return Array(allPlayers.prefix(60)) }
        return allPlayers
            .filter { $0.fullName.localizedCaseInsensitiveContains(trimmed) }
            .prefix(80)
            .map(\.self)
    }

    private func comparisonScenario(playerId: Int, filters: PlayerStatsFilters) async -> PlayerComparisonScenarioState {
        var state = PlayerComparisonScenarioState(filters: filters)
        do {
            async let historyResult = api.playerStatHistory(playerId: playerId, filters: filters)
            if filters.canRequestSummary {
                async let summaryResult = api.playerStatSummary(playerId: playerId, filters: filters)
                state.history = try await historyResult
                state.summary = try await summaryResult
            } else {
                state.history = try await historyResult
                state.summary = nil
            }
            state.infoMessage = playerSummaryInfoMessage(filters)
        } catch {
            state.errorMessage = error.localizedDescription
        }
        return state
    }
}

@MainActor
@Observable
final class OddsStore {
    private let api: AFLAPIClient

    var bookmakers: [BookmakerSummary] = []
    var events: [EventSummary] = []
    var allPlayers: [PlayerSummary] = []
    var filters = OddsFilters()
    var defaultBookmakerCodes: [String] = []
    var rows: [OddsSearchResult] = []
    var selectedRowId: OddsSearchResult.ID?
    var visibleCount = 50
    var hasMore = false
    var alternateUndersTarget: OddsSearchResult?
    var alternateUnders: [OddsSearchResult] = []
    var isLoading = false
    var isLoadingMore = false
    var isLoadingAlternateUnders = false
    var alternateUndersError: String?
    var errorMessage: String?
    var infoMessage: String?

    init(api: AFLAPIClient) {
        self.api = api
    }

    func bootstrap() async {
        guard bookmakers.isEmpty else { return }
        isLoading = true
        do {
            async let bookmakerResult = api.bookmakers()
            async let eventResult = api.events()
            async let playerResult = api.searchPlayers(query: "", limit: 5000)
            bookmakers = try await bookmakerResult
            events = try await eventResult
            allPlayers = try await playerResult
            defaultBookmakerCodes = bookmakers.filter(\.enabled).map(\.code)
            filters.bookmakerCodes = defaultBookmakerCodes
            await refresh(resetVisibleCount: true)
        } catch {
            isLoading = false
            errorMessage = error.localizedDescription
        }
    }

    func setScope(_ scope: OddsScope) {
        filters.scope = scope
        let sort = defaultSort(scope: scope)
        filters.sortBy = sort.sortBy
        filters.sortDirection = sort.sortDirection
        if scope == .match {
            filters.includePlayerIds = []
            filters.excludePlayerIds = []
            filters.selectionType = nil
            filters.bestOnly = false
            filters.matchupDifficulties = []
            filters.minPriceText = ""
            filters.maxPriceText = ""
            filters.minEdgeText = ""
            filters.minDiff2025 = oddsDiffSliderMin
            filters.maxDiff2025 = oddsDiffSliderMax
            filters.minDiffLast10 = oddsDiffSliderMin
            filters.maxDiffLast10 = oddsDiffSliderMax
            filters.minNextBestProbDiff = oddsDiffSliderMin
            filters.maxNextBestProbDiff = oddsDiffSliderMax
        }
        Task { await refresh(resetVisibleCount: true) }
    }

    func applyFilters(_ next: OddsFilters) {
        filters = next
        Task { await refresh(resetVisibleCount: true) }
    }

    func refresh(resetVisibleCount: Bool = false) async {
        if resetVisibleCount {
            visibleCount = 50
        }
        isLoading = true
        isLoadingMore = false
        errorMessage = nil
        do {
            let result = try await api.odds(filters: filters, limit: visibleCount + 1)
            rows = Array(result.prefix(visibleCount))
            selectedRowId = rows.first?.id
            hasMore = result.count > visibleCount
            isLoading = false
            AppLog.app.info("Odds refreshed")
        } catch {
            isLoading = false
            isLoadingMore = false
            errorMessage = error.localizedDescription
        }
    }

    func loadMore() async {
        guard !isLoading, !isLoadingMore, hasMore else { return }
        isLoadingMore = true
        visibleCount += 50
        await refresh()
    }

    func openAlternateUnders(for row: OddsSearchResult) async {
        guard let player = row.player else { return }
        alternateUndersTarget = row
        alternateUnders = []
        isLoadingAlternateUnders = true
        alternateUndersError = nil
        do {
            let rows = try await api.odds(
                bookmakers: filters.bookmakerCodes.isEmpty ? defaultBookmakerCodes : filters.bookmakerCodes,
                scope: .player,
                marketType: row.marketTypeCode,
                eventId: row.eventId,
                includePlayerIds: [player.id],
                sortBy: "price",
                sortDirection: "asc",
                selectionType: "under",
                limit: 200
            )
            alternateUnders = rows.sorted { lhs, rhs in
                let lhsLine = lhs.lineValue ?? -.infinity
                let rhsLine = rhs.lineValue ?? -.infinity
                if lhsLine != rhsLine {
                    return lhsLine > rhsLine
                }
                let lhsPrice = lhs.decimalPrice ?? -.infinity
                let rhsPrice = rhs.decimalPrice ?? -.infinity
                if lhsPrice != rhsPrice {
                    return lhsPrice > rhsPrice
                }
                return lhs.bookmaker < rhs.bookmaker
            }
            isLoadingAlternateUnders = false
        } catch {
            isLoadingAlternateUnders = false
            alternateUndersError = error.localizedDescription
        }
    }

    func closeAlternateUnders() {
        alternateUndersTarget = nil
        alternateUnders = []
        alternateUndersError = nil
        isLoadingAlternateUnders = false
    }
}

@MainActor
@Observable
final class SgmBuilderStore {
    private let api: AFLAPIClient
    let draftStore: SgmDraftStore

    var bookmakers: [BookmakerSummary] = []
    var events: [EventSummary] = []
    var selectedBookmaker: String?
    var selectedEventId: Int?
    var bestOnly = false
    var metricFilters = SelectionMetricFilters()
    var candidateLegs: [OddsSearchResult] = []
    var selectedCandidateId: OddsSearchResult.ID?
    var displayMode: BuilderDisplayMode = .row
    var sortField: BuilderSortField = .nextBest
    var sortDescending = true
    var marketFilter = "__all__"
    var isLoadingOptions = false
    var isLoadingQuote = false
    var errorMessage: String?
    var infoMessage: String?

    init(api: AFLAPIClient, draftStore: SgmDraftStore) {
        self.api = api
        self.draftStore = draftStore
    }

    var draft: SgmDraftState { draftStore.state }

    var marketCodes: [String] {
        Array(Set(candidateLegs.map(\.marketTypeCode))).sorted()
    }

    var visibleLegs: [OddsSearchResult] {
        let filtered = candidateLegs.filter { marketFilter == "__all__" || $0.marketTypeCode == marketFilter }
        return sortedCandidateRows(filtered)
    }

    func bootstrap() async {
        guard bookmakers.isEmpty else { return }
        await refresh()
    }

    func refresh() async {
        isLoadingOptions = true
        errorMessage = nil
        infoMessage = nil
        do {
            bookmakers = try await api.bookmakers()
            selectedBookmaker = selectedBookmaker
                ?? draft.bookmaker
                ?? bookmakers.first(where: { $0.livePricingEnabled && $0.enabled })?.code
                ?? bookmakers.first(where: \.enabled)?.code
            if let selectedBookmaker {
                await loadEvents(bookmaker: selectedBookmaker)
            } else {
                isLoadingOptions = false
            }
        } catch {
            isLoadingOptions = false
            errorMessage = error.localizedDescription
        }
    }

    func selectBookmaker(_ bookmaker: String) {
        if !draft.legs.isEmpty, draft.bookmaker != bookmaker {
            draftStore.clear()
        }
        selectedBookmaker = bookmaker
        selectedEventId = nil
        events = []
        candidateLegs = []
        Task { await loadEvents(bookmaker: bookmaker) }
    }

    func selectEvent(_ eventId: Int) {
        if !draft.legs.isEmpty, draft.eventId != eventId {
            draftStore.clear()
        }
        selectedEventId = eventId
        Task { await loadCandidateLegs() }
    }

    func setBestOnly(_ bestOnly: Bool) {
        self.bestOnly = bestOnly
        Task { await loadCandidateLegs() }
    }

    func applyMetricFilters(_ filters: SelectionMetricFilters) {
        metricFilters = filters
        Task { await loadCandidateLegs() }
    }

    func toggleLeg(_ row: OddsSearchResult) {
        if draft.legs.contains(where: { $0.selectionId == row.selectionId }) {
            draftStore.removeLeg(selectionId: row.selectionId)
            infoMessage = "Leg removed."
            errorMessage = nil
            return
        }
        guard row.sgmEligible, let price = row.decimalPrice else {
            errorMessage = "That leg is not ready for SGM pricing."
            infoMessage = nil
            return
        }
        let result = draftStore.addLeg(row.draftLeg(basePrice: price))
        infoMessage = result.message
        errorMessage = result.applied ? nil : result.message
    }

    func quote() async {
        let draft = draftStore.state
        guard let eventId = selectedEventId ?? draft.eventId, draft.legs.count >= 2 else {
            errorMessage = "Choose one match and at least two legs before comparing."
            return
        }
        isLoadingQuote = true
        errorMessage = nil
        infoMessage = nil
        do {
            let comparison = try await api.compareSgm(
                eventId: eventId,
                selectionIds: draft.legs.map(\.selectionId),
                forceRefresh: draft.forceRefresh
            )
            draftStore.setComparisons(comparison.results)
            infoMessage = comparison.results.isEmpty ? "No agency currently offers the full combination." : "Comparison updated."
            isLoadingQuote = false
        } catch {
            draftStore.setError(error.localizedDescription)
            isLoadingQuote = false
            errorMessage = error.localizedDescription
        }
    }

    private func loadEvents(bookmaker: String) async {
        isLoadingOptions = true
        do {
            events = try await api.events(bookmaker: bookmaker)
            selectedEventId = selectedEventId
                ?? draft.eventId
                ?? events.first?.id
            await loadCandidateLegs()
        } catch {
            isLoadingOptions = false
            errorMessage = error.localizedDescription
        }
    }

    private func loadCandidateLegs() async {
        guard let selectedBookmaker, let selectedEventId else {
            isLoadingOptions = false
            candidateLegs = []
            return
        }
        isLoadingOptions = true
        do {
            let rows = try await api.odds(
                bookmakers: [selectedBookmaker],
                scope: .player,
                eventId: selectedEventId,
                sortBy: "market",
                sortDirection: "asc",
                matchupDifficulties: metricFilters.matchupDifficulties,
                minPrice: metricFilters.minPriceText.doubleValue,
                maxPrice: metricFilters.maxPriceText.doubleValue,
                minDiff2025: metricFilters.minDiff2025,
                maxDiff2025: metricFilters.maxDiff2025,
                minDiffLast10: metricFilters.minDiffLast10,
                maxDiffLast10: metricFilters.maxDiffLast10,
                minNextBestProbDiff: metricFilters.minNextBestProbDiff,
                maxNextBestProbDiff: metricFilters.maxNextBestProbDiff,
                bestOnly: bestOnly,
                limit: 5000
            )
            candidateLegs = rows.filter { $0.marketTypeCode.hasPrefix("player_") }
            selectedCandidateId = visibleLegs.first?.id
            isLoadingOptions = false
        } catch {
            candidateLegs = []
            isLoadingOptions = false
            errorMessage = error.localizedDescription
        }
    }

    private func sortedCandidateRows(_ rows: [OddsSearchResult]) -> [OddsSearchResult] {
        rows.sorted { lhs, rhs in
            let ordered: Bool
            switch sortField {
            case .player:
                ordered = (lhs.player?.fullName ?? lhs.label) < (rhs.player?.fullName ?? rhs.label)
            case .line:
                ordered = (lhs.lineValue ?? -.infinity) < (rhs.lineValue ?? -.infinity)
            case .type:
                ordered = lhs.selectionType < rhs.selectionType
            case .nextBest:
                ordered = (lhs.nextBestProbDiff ?? -.infinity) < (rhs.nextBestProbDiff ?? -.infinity)
            case .price:
                ordered = (lhs.decimalPrice ?? -.infinity) < (rhs.decimalPrice ?? -.infinity)
            case .diffLast10:
                ordered = (lhs.diffLast10 ?? -.infinity) < (rhs.diffLast10 ?? -.infinity)
            case .diff2025:
                ordered = (lhs.diff2025 ?? -.infinity) < (rhs.diff2025 ?? -.infinity)
            }
            return sortDescending ? !ordered : ordered
        }
    }
}

@MainActor
@Observable
final class CgmBuilderStore {
    private let api: AFLAPIClient
    let draftStore: CgmDraftStore

    var bookmakers: [BookmakerSummary] = []
    var events: [EventSummary] = []
    var bestOnly = false
    var metricFilters = SelectionMetricFilters()
    var candidateLegs: [OddsSearchResult] = []
    var selectedCandidateId: OddsSearchResult.ID?
    var displayMode: BuilderDisplayMode = .row
    var sortField: BuilderSortField = .nextBest
    var sortDescending = true
    var marketFilter = "__all__"
    var isLoadingOptions = false
    var isComparing = false
    var errorMessage: String?
    var infoMessage: String?

    init(api: AFLAPIClient, draftStore: CgmDraftStore) {
        self.api = api
        self.draftStore = draftStore
    }

    var state: CgmDraftState { draftStore.state }

    var availableEvents: [EventSummary] {
        let draftedEventIds = Set(state.selectedLegs.map(\.eventId))
        return events.filter { !draftedEventIds.contains($0.id) }
    }

    var marketCodes: [String] {
        Array(Set(filteredByEvent.map(\.marketTypeCode))).sorted()
    }

    var visibleLegs: [OddsSearchResult] {
        let rows = filteredByEvent.filter { marketFilter == "__all__" || $0.marketTypeCode == marketFilter }
        return sortedCandidateRows(rows)
    }

    private var filteredByEvent: [OddsSearchResult] {
        let draftedEventIds = Set(state.selectedLegs.map(\.eventId))
        let eventIds = state.selectedEventIds
        return candidateLegs.filter { row in
            !draftedEventIds.contains(row.eventId)
                && (eventIds.isEmpty || eventIds.contains(row.eventId))
        }
    }

    func bootstrap() async {
        guard bookmakers.isEmpty else { return }
        await refresh()
    }

    func refresh() async {
        isLoadingOptions = true
        errorMessage = nil
        infoMessage = nil
        do {
            bookmakers = try await api.bookmakers()
            let selected = state.selectedBookmaker
                ?? bookmakers.first(where: \.enabled)?.code
            draftStore.selectBookmaker(selected)
            if let selected {
                await loadBookmakerData(bookmaker: selected)
            } else {
                isLoadingOptions = false
            }
        } catch {
            isLoadingOptions = false
            errorMessage = error.localizedDescription
        }
    }

    func selectBookmaker(_ bookmaker: String) {
        draftStore.selectBookmaker(bookmaker)
        Task { await loadBookmakerData(bookmaker: bookmaker) }
    }

    func setBestOnly(_ bestOnly: Bool) {
        self.bestOnly = bestOnly
        Task {
            if let bookmaker = state.selectedBookmaker {
                await loadBookmakerData(bookmaker: bookmaker)
            }
        }
    }

    func applyMetricFilters(_ filters: SelectionMetricFilters) {
        metricFilters = filters
        Task {
            if let bookmaker = state.selectedBookmaker {
                await loadBookmakerData(bookmaker: bookmaker)
            }
        }
    }

    func toggleLeg(_ row: OddsSearchResult) {
        guard let price = row.decimalPrice else {
            errorMessage = "That leg does not have a current price."
            infoMessage = nil
            return
        }
        let result = draftStore.toggleLeg(row.draftLeg(basePrice: price))
        errorMessage = result.applied ? nil : result.message
        infoMessage = result.applied ? result.message : nil
    }

    func compare() async {
        let legs = state.selectedLegs
        guard legs.count >= 2 else {
            errorMessage = "Choose at least two legs before comparing."
            infoMessage = nil
            return
        }
        isComparing = true
        errorMessage = nil
        infoMessage = nil
        do {
            let comparison = try await api.compareCgm(selectionIds: legs.map(\.selectionId))
            draftStore.setComparisons(comparison.results)
            infoMessage = comparison.results.isEmpty ? "No agency currently offers the full combination." : "Comparison updated."
            isComparing = false
        } catch {
            draftStore.setError(error.localizedDescription)
            isComparing = false
            errorMessage = error.localizedDescription
        }
    }

    private func loadBookmakerData(bookmaker: String) async {
        isLoadingOptions = true
        do {
            async let eventResult = api.events(bookmaker: bookmaker)
            async let oddsResult = api.odds(
                bookmakers: [bookmaker],
                scope: .player,
                sortBy: "next_best_prob_diff",
                sortDirection: "desc",
                matchupDifficulties: metricFilters.matchupDifficulties,
                minPrice: metricFilters.minPriceText.doubleValue,
                maxPrice: metricFilters.maxPriceText.doubleValue,
                minDiff2025: metricFilters.minDiff2025,
                maxDiff2025: metricFilters.maxDiff2025,
                minDiffLast10: metricFilters.minDiffLast10,
                maxDiffLast10: metricFilters.maxDiffLast10,
                minNextBestProbDiff: metricFilters.minNextBestProbDiff,
                maxNextBestProbDiff: metricFilters.maxNextBestProbDiff,
                bestOnly: bestOnly,
                limit: 5000
            )
            events = try await eventResult
            let odds = try await oddsResult
            candidateLegs = odds.filter { $0.marketTypeCode.hasPrefix("player_") }
            selectedCandidateId = visibleLegs.first?.id
            isLoadingOptions = false
        } catch {
            candidateLegs = []
            isLoadingOptions = false
            errorMessage = error.localizedDescription
        }
    }

    private func sortedCandidateRows(_ rows: [OddsSearchResult]) -> [OddsSearchResult] {
        rows.sorted { lhs, rhs in
            let ordered: Bool
            switch sortField {
            case .player:
                ordered = (lhs.player?.fullName ?? lhs.label) < (rhs.player?.fullName ?? rhs.label)
            case .line:
                ordered = (lhs.lineValue ?? -.infinity) < (rhs.lineValue ?? -.infinity)
            case .type:
                ordered = lhs.selectionType < rhs.selectionType
            case .nextBest:
                ordered = (lhs.nextBestProbDiff ?? -.infinity) < (rhs.nextBestProbDiff ?? -.infinity)
            case .price:
                ordered = (lhs.decimalPrice ?? -.infinity) < (rhs.decimalPrice ?? -.infinity)
            case .diffLast10:
                ordered = (lhs.diffLast10 ?? -.infinity) < (rhs.diffLast10 ?? -.infinity)
            case .diff2025:
                ordered = (lhs.diff2025 ?? -.infinity) < (rhs.diff2025 ?? -.infinity)
            }
            return sortDescending ? !ordered : ordered
        }
    }
}

@MainActor
@Observable
final class FixturesStore {
    private let api: AFLAPIClient

    var bookmakers: [BookmakerSummary] = []
    var selectedBookmaker = "sportsbet"
    var searchQuery = ""
    var events: [EventSummary] = []
    var selectedEventId: EventSummary.ID?
    var selectedEvent: EventSummary?
    var markets: [MarketSummary] = []
    var selectedMarketId: MarketSummary.ID?
    var selections: [SelectionSummary] = []
    var playerQuery = ""
    var isLoadingEvents = false
    var isLoadingMarkets = false
    var isLoadingSelections = false
    var errorMessage: String?
    var infoMessage: String?

    init(api: AFLAPIClient, settings: AppSettings) {
        self.api = api
        self.selectedBookmaker = settings.defaultBookmaker
    }

    func bootstrap() async {
        guard bookmakers.isEmpty else { return }
        await refreshEvents()
    }

    func refreshEvents() async {
        isLoadingEvents = true
        errorMessage = nil
        do {
            async let bookmakerResult = api.bookmakers()
            async let eventResult = api.events(bookmaker: selectedBookmaker, query: searchQuery)
            bookmakers = try await bookmakerResult
            events = try await eventResult
            selectedEventId = selectedEventId ?? events.first?.id
            selectedEvent = events.first { $0.id == selectedEventId } ?? events.first
            isLoadingEvents = false
            if let selectedEvent {
                await loadMarkets(event: selectedEvent)
            }
        } catch {
            isLoadingEvents = false
            errorMessage = error.localizedDescription
        }
    }

    func selectBookmaker(_ bookmaker: String) {
        selectedBookmaker = bookmaker
        selectedEventId = nil
        markets = []
        selections = []
        Task { await refreshEvents() }
    }

    func selectEvent(_ event: EventSummary) {
        selectedEventId = event.id
        selectedEvent = event
        Task { await loadMarkets(event: event) }
    }

    func selectMarket(_ market: MarketSummary) {
        selectedMarketId = market.id
        Task { await loadSelections(market: market) }
    }

    func loadMarkets(event: EventSummary) async {
        isLoadingMarkets = true
        errorMessage = nil
        do {
            markets = try await api.markets(eventId: event.id, bookmaker: selectedBookmaker, playerQuery: playerQuery)
            selectedMarketId = markets.first?.id
            isLoadingMarkets = false
            if let market = markets.first {
                await loadSelections(market: market)
            } else {
                selections = []
            }
        } catch {
            isLoadingMarkets = false
            errorMessage = error.localizedDescription
        }
    }

    func loadSelections(market: MarketSummary) async {
        isLoadingSelections = true
        errorMessage = nil
        do {
            selections = try await api.selections(marketId: market.id, bookmaker: selectedBookmaker)
            isLoadingSelections = false
        } catch {
            isLoadingSelections = false
            errorMessage = error.localizedDescription
        }
    }
}

@MainActor
@Observable
final class DataStatusStore {
    private let api: AFLAPIClient

    var response: DataStatusResponse?
    var health: HealthResponse?
    var isLoading = false
    var errorMessage: String?

    init(api: AFLAPIClient) {
        self.api = api
    }

    func refresh() async {
        isLoading = true
        errorMessage = nil
        do {
            async let healthResult = api.health()
            async let statusResult = api.dataStatus()
            health = try await healthResult
            response = try await statusResult
            isLoading = false
        } catch {
            isLoading = false
            errorMessage = error.localizedDescription
        }
    }
}

private func defaultPlayerStatsFilters(options: PlayerStatFilterOptions) -> PlayerStatsFilters {
    let defaultStat = options.stats.first { $0.code == "disposals" }?.code
        ?? options.stats.first?.code
        ?? "disposals"
    let defaultSeasons = Array(options.seasons.prefix(2)).isEmpty ? Array(options.seasons.prefix(1)) : Array(options.seasons.prefix(2))
    let defaultHomeAway = options.homeAwayOptions.isEmpty ? ["Home", "Away"] : options.homeAwayOptions
    var filters = PlayerStatsFilters()
    filters.statCode = defaultStat
    filters.seasons = defaultSeasons
    filters.homeAway = defaultHomeAway
    return filters
}

private func filtersForOddsRow(
    options: PlayerStatFilterOptions,
    defaults: PlayerStatsFilters,
    row: OddsSearchResult?
) -> PlayerStatsFilters {
    guard let row else { return defaults }
    let requestedStatCode: String
    switch row.marketTypeCode {
    case "player_disposals": requestedStatCode = "disposals"
    case "player_fantasy_points": requestedStatCode = "fantasy_points"
    case "player_tackles": requestedStatCode = "tackles"
    case "player_marks": requestedStatCode = "marks"
    case "player_goals": requestedStatCode = "goals"
    case "player_kicks": requestedStatCode = "kicks"
    case "player_handballs": requestedStatCode = "handballs"
    case "player_hitouts": requestedStatCode = "hitouts"
    default: requestedStatCode = defaults.statCode
    }
    let statCode = options.stats.first { $0.code == requestedStatCode }?.code ?? defaults.statCode
    var filters = defaults
    filters.statCode = statCode
    filters.lineMode = "single"
    filters.referenceLineText = row.lineValue.map(formatLineForPrefill) ?? defaults.referenceLineText
    filters.lowerBoundText = ""
    filters.upperBoundText = ""
    return filters
}

private func formatLineForPrefill(_ line: Double) -> String {
    if line.truncatingRemainder(dividingBy: 1) == 0 {
        return "\(Int(line))"
    }
    return String(format: "%.1f", line)
}

private func playerSummaryInfoMessage(_ filters: PlayerStatsFilters) -> String? {
    if filters.lineMode == "interval", !filters.canRequestSummary {
        return "Enter a valid lower and upper bound to calculate interval hit rates."
    }
    if filters.lineMode != "interval", !filters.canRequestSummary {
        return "Enter a valid reference line to calculate over/under hit rates."
    }
    return nil
}

private func defaultSort(scope: OddsScope) -> (sortBy: String, sortDirection: String) {
    switch scope {
    case .player: ("diff_last_10", "desc")
    case .match: ("start_time", "asc")
    }
}

private extension OddsSearchResult {
    func draftLeg(basePrice: Double) -> DraftLeg {
        DraftLeg(
            selectionId: selectionId,
            eventId: eventId,
            eventLabel: matchName,
            bookmaker: bookmaker,
            label: label,
            marketTypeCode: marketTypeCode,
            selectionType: selectionType,
            basePrice: basePrice,
            diff2025: diff2025,
            diffLast10: diffLast10,
            nextBestProbDiff: nextBestProbDiff,
            isBestPrice: isBestPrice
        )
    }
}
