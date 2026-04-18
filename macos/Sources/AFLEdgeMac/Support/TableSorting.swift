import Foundation

extension OddsSearchResult {
    var playerSortKey: String { player?.fullName ?? label }
    var lineSort: Double { lineValue ?? -.greatestFiniteMagnitude }
    var priceSort: Double { decimalPrice ?? 0 }
    var nextBestSort: Double { nextBestProbDiff ?? -.greatestFiniteMagnitude }
    var diffLast10Sort: Double { diffLast10 ?? -.greatestFiniteMagnitude }
    var diff2025Sort: Double { diff2025 ?? -.greatestFiniteMagnitude }
}

extension PlayerGameLogEntry {
    var roundSort: String { roundLabel ?? "" }
    var teamSort: String { team ?? "" }
    var oppositionSort: String { opposition ?? "" }
    var venueSort: String { venue ?? "" }
    var weatherSort: String { weather ?? "" }
    var selectedValueSort: Double { selectedValue ?? -.greatestFiniteMagnitude }
    var hitSort: Int { hit.map { $0 ? 1 : 0 } ?? -1 }
    var togSort: Double { tog ?? -.greatestFiniteMagnitude }
}
