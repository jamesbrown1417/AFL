import Foundation

struct HealthResponse: Codable, Equatable {
    var status: String
    var databaseOk: Bool
    var lastSuccessfulImportAt: Date?
}

struct DataFileStatus: Codable, Equatable, Identifiable {
    var fileName: String
    var relativePath: String
    var modifiedAt: Date

    var id: String { relativePath }
}

struct DataFileSection: Codable, Equatable, Identifiable {
    var code: String
    var title: String
    var category: String
    var files: [DataFileStatus]

    var id: String { code }
}

struct DataStatusResponse: Codable, Equatable {
    var generatedAt: Date
    var sections: [DataFileSection]
}

struct TeamSummary: Codable, Equatable, Identifiable, Hashable {
    var id: Int
    var name: String
}

struct PlayerSummary: Codable, Equatable, Identifiable, Hashable {
    var id: Int
    var fullName: String
}

struct PlayerStatOption: Codable, Equatable, Identifiable, Hashable {
    var code: String
    var label: String

    var id: String { code }
}

struct PlayerStatFilterOptions: Codable, Equatable, Identifiable {
    var playerId: Int
    var playerName: String
    var stats: [PlayerStatOption]
    var seasons: [String]
    var oppositions: [String]
    var venues: [String]
    var weatherCategories: [String]
    var homeAwayOptions: [String]

    var id: Int { playerId }
}

struct PlayerGameLogEntry: Codable, Equatable, Identifiable {
    var date: Date
    var roundLabel: String?
    var home: String?
    var venue: String?
    var weather: String?
    var away: String?
    var team: String?
    var opposition: String?
    var margin: Int?
    var tog: Double?
    var disposals: Double?
    var kicks: Double?
    var handballs: Double?
    var marks: Double?
    var goals: Double?
    var behinds: Double?
    var tackles: Double?
    var hitouts: Double?
    var freesFor: Double?
    var freesAgainst: Double?
    var fantasy: Double?
    var cba: Double?
    var gameNumber: Int
    var selectedStat: String
    var selectedValue: Double?
    var hit: Bool?

    var id: String { "\(gameNumber)-\(date.timeIntervalSince1970)-\(selectedStat)" }
}

struct PlayerStatSummary: Codable, Equatable {
    var playerId: Int
    var statCode: String
    var statLabel: String
    var lineMode: String
    var referenceLine: Double?
    var lowerBound: Double?
    var upperBound: Double?
    var sampleSize: Int
    var proportionOver: Double?
    var proportionUnder: Double?
    var impliedOddsOver: Double?
    var impliedOddsUnder: Double?
    var proportionWithinInterval: Double?
    var proportionOutsideInterval: Double?
    var impliedOddsWithinInterval: Double?
    var impliedOddsOutsideInterval: Double?
}

struct BookmakerSummary: Codable, Equatable, Identifiable, Hashable {
    var id: Int
    var code: String
    var displayName: String
    var enabled: Bool
    var livePricingEnabled: Bool
    var sgmEligibleCount: Int
}

struct EventSummary: Codable, Equatable, Identifiable, Hashable {
    var id: Int
    var matchName: String
    var startTime: Date?
    var roundLabel: String?
    var venue: String?
    var homeTeam: TeamSummary
    var awayTeam: TeamSummary
    var availableBookmakers: [String]
}

struct MarketSummary: Codable, Equatable, Identifiable, Hashable {
    var id: Int
    var eventId: Int
    var marketTypeCode: String
    var displayName: String
    var player: PlayerSummary?
    var lineValue: Double?
    var bookmaker: String
    var availableSelectionTypes: [String]
}

struct SelectionSummary: Codable, Equatable, Identifiable, Hashable {
    var id: Int
    var marketId: Int
    var selectionType: String
    var label: String
    var decimalPrice: Double?
    var impliedProb: Double?
    var bookmaker: String
    var sgmEligible: Bool
    var edgePct: Double?
}

struct WeatherSummary: Codable, Equatable, Hashable {
    var temperatureC: Double?
    var windKph: Double?
    var precipProbability: Double?
    var precipMm: Double?
    var label: String?
    var iconCode: String?
}

struct OddsSearchResult: Codable, Equatable, Identifiable, Hashable {
    var selectionId: Int
    var marketId: Int
    var eventId: Int
    var matchName: String
    var startTime: Date?
    var bookmaker: String
    var marketTypeCode: String
    var marketDisplayName: String
    var player: PlayerSummary?
    var selectionType: String
    var label: String
    var lineValue: Double?
    var decimalPrice: Double?
    var impliedProb: Double?
    var edgePct: Double?
    var diff2025: Double?
    var diffLast10: Double?
    var playerPosition: String?
    var matchupDifficulty: String?
    var weather: WeatherSummary?
    var isBestPrice: Bool
    var nextBestProbDiff: Double?
    var sgmEligible: Bool

    var id: Int { selectionId }
}

struct PropSearchResult: Codable, Equatable, Identifiable, Hashable {
    var selectionId: Int
    var eventId: Int
    var matchName: String
    var startTime: Date?
    var bookmaker: String
    var marketTypeCode: String
    var player: PlayerSummary?
    var selectionType: String
    var label: String
    var lineValue: Double?
    var decimalPrice: Double?
    var impliedProb: Double?
    var edgePct: Double?
    var sgmEligible: Bool

    var id: Int { selectionId }
}

struct RequestedLeg: Codable, Equatable {
    var selectionId: Int
}

struct QuoteLeg: Codable, Equatable, Identifiable, Hashable {
    var selectionId: Int
    var label: String
    var marketTypeCode: String
    var selectionType: String
    var basePrice: Double

    var id: Int { selectionId }
}

struct SgmQuoteRequestPayload: Codable, Equatable {
    var bookmaker: String
    var eventId: Int
    var legs: [RequestedLeg]
    var forceRefresh: Bool
}

struct SgmQuoteResponse: Codable, Equatable, Identifiable {
    var quoteId: String
    var bookmaker: String
    var eventId: Int
    var legs: [QuoteLeg]
    var unadjustedPrice: Double
    var quotedPrice: Double
    var adjustmentFactor: Double
    var fromCache: Bool
    var quotedAt: Date
    var expiresAt: Date
    var status: String

    var id: String { quoteId }
}

struct SgmCompareRequestPayload: Codable, Equatable {
    var eventId: Int
    var selectionIds: [Int]
    var forceRefresh: Bool
}

struct SgmAgencyComparison: Codable, Equatable, Identifiable {
    var quoteId: String
    var bookmaker: String
    var eventId: Int
    var legs: [QuoteLeg]
    var unadjustedPrice: Double
    var quotedPrice: Double
    var adjustmentFactor: Double
    var fromCache: Bool
    var quotedAt: Date
    var expiresAt: Date
    var status: String

    var id: String { "\(bookmaker)-\(quoteId)" }
}

struct SgmCompareResponse: Codable, Equatable {
    var eventId: Int
    var selectionCount: Int
    var results: [SgmAgencyComparison]
}

struct CgmCompareRequestPayload: Codable, Equatable {
    var selectionIds: [Int]
}

struct CgmLegPrice: Codable, Equatable, Identifiable, Hashable {
    var selectionId: Int
    var matchName: String
    var label: String
    var marketTypeCode: String
    var selectionType: String
    var basePrice: Double

    var id: Int { selectionId }
}

struct CgmAgencyComparison: Codable, Equatable, Identifiable {
    var bookmaker: String
    var quotedPrice: Double
    var selectionCount: Int
    var legs: [CgmLegPrice]

    var id: String { bookmaker }
}

struct CgmCompareResponse: Codable, Equatable {
    var selectionCount: Int
    var results: [CgmAgencyComparison]
}

struct APIErrorEnvelope: Codable, Equatable {
    var error: APIErrorBody
}

struct APIErrorBody: Codable, Equatable {
    var code: String
    var message: String
    var retriable: Bool
    var details: [String: String]?
}
