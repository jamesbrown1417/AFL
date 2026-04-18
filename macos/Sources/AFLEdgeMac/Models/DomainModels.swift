import Foundation

struct AppSettings: Codable, Equatable {
    var apiBaseURL: String = "http://127.0.0.1:8000/api/v1/"
    var authToken: String = ""
    var defaultBookmaker: String = "sportsbet"
    var tailscaleUserLogin: String = ""
    var tailscaleUserHeaderName: String = "Tailscale-User-Login"
}

let oddsDiffSliderMin = -1.0
let oddsDiffSliderMax = 1.0
let matchupDifficultyOptions = ["Terrible", "Bad", "Neutral", "Good", "Excellent"]

enum TopLevelDestination: String, CaseIterable, Identifiable {
    case playerLab
    case odds
    case sgmBuilder
    case cgmBuilder
    case fixtures
    case dataStatus

    var id: String { rawValue }

    var title: String {
        switch self {
        case .playerLab: "Player Lab"
        case .odds: "Odds"
        case .sgmBuilder: "SGM Builder"
        case .cgmBuilder: "CGM Builder"
        case .fixtures: "Fixtures"
        case .dataStatus: "Data Status"
        }
    }

    var systemImage: String {
        switch self {
        case .playerLab: "person.text.rectangle"
        case .odds: "chart.line.uptrend.xyaxis"
        case .sgmBuilder: "square.grid.3x3"
        case .cgmBuilder: "rectangle.3.group"
        case .fixtures: "calendar"
        case .dataStatus: "externaldrive.badge.checkmark"
        }
    }
}

enum OddsScope: String, CaseIterable, Identifiable, Codable {
    case match
    case player

    var id: String { rawValue }
    var label: String { rawValue.capitalized }
}

enum BuilderDisplayMode: String, CaseIterable, Identifiable, Codable {
    case row
    case tile

    var id: String { rawValue }

    var label: String {
        switch self {
        case .row: "Row"
        case .tile: "Tile"
        }
    }
}

enum BuilderSortField: String, CaseIterable, Identifiable, Codable {
    case player
    case line
    case type
    case nextBest
    case price
    case diffLast10
    case diff2025

    var id: String { rawValue }

    var label: String {
        switch self {
        case .player: "Player"
        case .line: "Line"
        case .type: "Type"
        case .nextBest: "Next best"
        case .price: "Price"
        case .diffLast10: "Diff L10"
        case .diff2025: "Diff 2025"
        }
    }
}

enum PlayerLabMode: String, CaseIterable, Identifiable, Codable {
    case stats
    case comparison

    var id: String { rawValue }

    var label: String {
        switch self {
        case .stats: "Stats"
        case .comparison: "Comparison"
        }
    }
}

enum PlayerHistoryViewMode: String, CaseIterable, Identifiable, Codable {
    case table
    case graph

    var id: String { rawValue }

    var label: String {
        switch self {
        case .table: "Table"
        case .graph: "Graph"
        }
    }
}

enum PlayerComparisonViewMode: String, CaseIterable, Identifiable, Codable {
    case summary
    case graph
    case gameLog

    var id: String { rawValue }

    var label: String {
        switch self {
        case .summary: "Summary"
        case .graph: "Graph"
        case .gameLog: "Game Log"
        }
    }
}

enum PlayerComparisonFocus: String, CaseIterable, Identifiable, Codable {
    case scenarioA
    case scenarioB

    var id: String { rawValue }

    var label: String {
        switch self {
        case .scenarioA: "Scenario A"
        case .scenarioB: "Scenario B"
        }
    }
}

enum QuickFilterPreset: String, CaseIterable, Identifiable {
    case last10Positive
    case last10AndNBPositive
    case last10NBAndFavorableMatchup

    var id: String { rawValue }

    var label: String {
        switch self {
        case .last10Positive: "L10 positive"
        case .last10AndNBPositive: "L10 + NB positive"
        case .last10NBAndFavorableMatchup: "L10 + NB + matchup"
        }
    }
}

struct OddsFilters: Codable, Equatable {
    var scope: OddsScope = .player
    var query: String = ""
    var bookmakerCodes: [String] = []
    var marketTypeCode: String?
    var eventId: Int?
    var includePlayerIds: [Int] = []
    var excludePlayerIds: [Int] = []
    var sortBy: String = "diff_last_10"
    var sortDirection: String = "desc"
    var selectionType: String?
    var matchupDifficulties: [String] = []
    var minPriceText: String = ""
    var maxPriceText: String = ""
    var minDiff2025: Double = oddsDiffSliderMin
    var maxDiff2025: Double = oddsDiffSliderMax
    var minDiffLast10: Double = oddsDiffSliderMin
    var maxDiffLast10: Double = oddsDiffSliderMax
    var minNextBestProbDiff: Double = oddsDiffSliderMin
    var maxNextBestProbDiff: Double = oddsDiffSliderMax
    var minEdgeText: String = ""
    var bestOnly: Bool = false
    var sgmOnly: Bool = false

    func hasActiveFilters(defaultBookmakers: [String]) -> Bool {
        !query.isEmpty
            || Set(bookmakerCodes) != Set(defaultBookmakers)
            || marketTypeCode != nil
            || eventId != nil
            || !includePlayerIds.isEmpty
            || !excludePlayerIds.isEmpty
            || selectionType != nil
            || !matchupDifficulties.isEmpty
            || !minPriceText.isEmpty
            || !maxPriceText.isEmpty
            || minDiff2025 != oddsDiffSliderMin
            || maxDiff2025 != oddsDiffSliderMax
            || minDiffLast10 != oddsDiffSliderMin
            || maxDiffLast10 != oddsDiffSliderMax
            || minNextBestProbDiff != oddsDiffSliderMin
            || maxNextBestProbDiff != oddsDiffSliderMax
            || !minEdgeText.isEmpty
            || bestOnly
            || sgmOnly
    }
}

struct SelectionMetricFilters: Codable, Equatable {
    var matchupDifficulties: [String] = []
    var minPriceText: String = ""
    var maxPriceText: String = ""
    var minDiff2025: Double = oddsDiffSliderMin
    var maxDiff2025: Double = oddsDiffSliderMax
    var minDiffLast10: Double = oddsDiffSliderMin
    var maxDiffLast10: Double = oddsDiffSliderMax
    var minNextBestProbDiff: Double = oddsDiffSliderMin
    var maxNextBestProbDiff: Double = oddsDiffSliderMax
}

extension OddsFilters {
    func applying(_ preset: QuickFilterPreset) -> OddsFilters {
        switch preset {
        case .last10Positive:
            var copy = self
            copy.minDiffLast10 = 0
            copy.maxDiffLast10 = oddsDiffSliderMax
            copy.minNextBestProbDiff = oddsDiffSliderMin
            copy.maxNextBestProbDiff = oddsDiffSliderMax
            copy.matchupDifficulties = []
            return copy
        case .last10AndNBPositive:
            var copy = self
            copy.minDiffLast10 = 0
            copy.maxDiffLast10 = oddsDiffSliderMax
            copy.minNextBestProbDiff = 0
            copy.maxNextBestProbDiff = oddsDiffSliderMax
            copy.matchupDifficulties = []
            return copy
        case .last10NBAndFavorableMatchup:
            var copy = self
            copy.minDiffLast10 = 0
            copy.maxDiffLast10 = oddsDiffSliderMax
            copy.minNextBestProbDiff = 0
            copy.maxNextBestProbDiff = oddsDiffSliderMax
            copy.matchupDifficulties = ["Neutral", "Good", "Excellent"]
            return copy
        }
    }
}

extension SelectionMetricFilters {
    func applying(_ preset: QuickFilterPreset) -> SelectionMetricFilters {
        switch preset {
        case .last10Positive:
            var copy = self
            copy.minDiffLast10 = 0
            copy.maxDiffLast10 = oddsDiffSliderMax
            copy.minNextBestProbDiff = oddsDiffSliderMin
            copy.maxNextBestProbDiff = oddsDiffSliderMax
            copy.matchupDifficulties = []
            return copy
        case .last10AndNBPositive:
            var copy = self
            copy.minDiffLast10 = 0
            copy.maxDiffLast10 = oddsDiffSliderMax
            copy.minNextBestProbDiff = 0
            copy.maxNextBestProbDiff = oddsDiffSliderMax
            copy.matchupDifficulties = []
            return copy
        case .last10NBAndFavorableMatchup:
            var copy = self
            copy.minDiffLast10 = 0
            copy.maxDiffLast10 = oddsDiffSliderMax
            copy.minNextBestProbDiff = 0
            copy.maxNextBestProbDiff = oddsDiffSliderMax
            copy.matchupDifficulties = ["Neutral", "Good", "Excellent"]
            return copy
        }
    }
}

struct PlayerStatsFilters: Codable, Equatable {
    var statCode: String = "disposals"
    var seasons: [String] = []
    var oppositions: [String] = []
    var venues: [String] = []
    var weatherCategories: [String] = []
    var homeAway: [String] = ["Home", "Away"]
    var marginMinText: String = "-200"
    var marginMaxText: String = "200"
    var lastGamesText: String = ""
    var minutesMinimumText: String = "0"
    var lineMode: String = "single"
    var referenceLineText: String = "19.5"
    var lowerBoundText: String = "19.5"
    var upperBoundText: String = "25.5"

    var canRequestSummary: Bool {
        switch lineMode {
        case "interval":
            lowerBoundText.asDouble != nil && upperBoundText.asDouble != nil
        default:
            referenceLineText.asDouble != nil
        }
    }

    var resolvedHistoryLineMode: String? {
        switch lineMode {
        case "interval":
            lowerBoundText.asDouble != nil && upperBoundText.asDouble != nil ? "interval" : nil
        default:
            referenceLineText.asDouble != nil ? "single" : nil
        }
    }
}

struct PlayerComparisonScenarioState: Equatable {
    var filters = PlayerStatsFilters()
    var history: [PlayerGameLogEntry] = []
    var summary: PlayerStatSummary?
    var isLoading = false
    var errorMessage: String?
    var infoMessage: String?
}

struct DraftLeg: Codable, Equatable, Identifiable, Hashable {
    var selectionId: Int
    var eventId: Int
    var eventLabel: String
    var bookmaker: String
    var label: String
    var marketTypeCode: String
    var selectionType: String
    var basePrice: Double
    var diff2025: Double?
    var diffLast10: Double?
    var nextBestProbDiff: Double?
    var isBestPrice: Bool = false

    var id: Int { selectionId }
}

struct SgmDraftState: Codable, Equatable {
    var bookmaker: String?
    var eventId: Int?
    var eventLabel: String?
    var legs: [DraftLeg] = []
    var forceRefresh: Bool = false
    var latestQuote: SgmQuoteResponse?
    var latestComparisons: [SgmAgencyComparison] = []
    var latestError: String?
}

struct DraftMutationResult: Equatable {
    var applied: Bool
    var message: String?
}

private extension String {
    var asDouble: Double? {
        Double(trimmingCharacters(in: .whitespacesAndNewlines))
    }
}
