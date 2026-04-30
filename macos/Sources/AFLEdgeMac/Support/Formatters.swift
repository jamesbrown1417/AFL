import Foundation

@MainActor
enum AFLFormatters {
    private static let adelaideZone = TimeZone(identifier: "Australia/Adelaide") ?? .current

    static let backendDecoder: JSONDecoder = {
        let decoder = JSONDecoder()
        decoder.keyDecodingStrategy = .convertFromSnakeCase
        decoder.dateDecodingStrategy = .custom { decoder in
            let container = try decoder.singleValueContainer()
            let value = try container.decode(String.self)
            if let date = parseBackendDate(value) {
                return date
            }
            throw DecodingError.dataCorruptedError(
                in: container,
                debugDescription: "Expected an ISO-8601 date string, got \(value)."
            )
        }
        return decoder
    }()

    static let backendEncoder: JSONEncoder = {
        let encoder = JSONEncoder()
        encoder.keyEncodingStrategy = .convertToSnakeCase
        encoder.dateEncodingStrategy = .iso8601
        return encoder
    }()

    static func dateTime(_ value: Date?) -> String {
        guard let value else { return "TBA" }
        return DateFormatter.display.string(from: value)
    }

    static func dateTimeInAdelaide(_ value: Date?) -> String {
        guard let value else { return "TBA" }
        let formatter = DateFormatter.adelaideDisplay
        formatter.timeZone = adelaideZone
        return formatter.string(from: value)
    }

    static func decimalPrice(_ value: Double?) -> String {
        guard let value else { return "--" }
        return String(format: "%.2f", value)
    }

    static func signedMetric(_ value: Double?) -> String {
        guard let value else { return "--" }
        return String(format: "%+.2f", value)
    }

    static func percent(_ value: Double?) -> String {
        guard let value else { return "--" }
        return String(format: "%.1f%%", value * 100)
    }

    static func edgePercent(_ value: Double?) -> String {
        guard let value else { return "--" }
        return String(format: "%.2f%%", value)
    }

    static func playerPositionTag(_ value: String?) -> String? {
        guard let value else { return nil }
        let normalized = value
            .trimmingCharacters(in: .whitespacesAndNewlines)
            .uppercased()
            .replacingOccurrences(of: "-", with: "_")
        guard !normalized.isEmpty else { return nil }
        return switch normalized {
        case "KEY_DEFENDER": "KDEF"
        case "MEDIUM_DEFENDER": "MDEF"
        case "KEY_FORWARD": "KFWD"
        case "MEDIUM_FORWARD": "MFWD"
        case "MIDFIELDER": "MID"
        case "MIDFIELDER_FORWARD": "MID/F"
        case "RUCK": "RUC"
        default: normalized.replacingOccurrences(of: "_", with: " ")
        }
    }

    static func matchupDifficultyTag(_ value: String?) -> String? {
        guard let value else { return nil }
        let normalized = value.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        guard !normalized.isEmpty else { return nil }
        return switch normalized {
        case "terrible": "TERR"
        case "bad": "BAD"
        case "neutral": "NEUT"
        case "good": "GOOD"
        case "excellent": "EXCL"
        default: normalized.uppercased()
        }
    }

    static func weatherTemperatureTag(_ value: Double?) -> String? {
        value.map { "\(Int($0))°" }
    }

    static func weatherRainTag(_ value: Double?) -> String? {
        guard let value, value > 0 else { return nil }
        if value >= 10 {
            return String(format: "%.0fmm", value)
        }
        return String(format: "%.1fmm", value)
    }

    static func bookmakerDisplayName(_ code: String) -> String {
        switch code.trimmingCharacters(in: .whitespacesAndNewlines).lowercased() {
        case "bet365":
            "Bet365"
        case "neds":
            "Neds"
        case "pointsbet":
            "Pointsbet"
        case "sportsbet":
            "Sportsbet"
        case "tab":
            "Tab"
        default:
            code
                .replacingOccurrences(of: "_", with: " ")
                .replacingOccurrences(of: "-", with: " ")
                .capitalized
        }
    }

    static func shortAFLMatchLabel(_ matchName: String) -> String {
        let normalized = matchName.replacingOccurrences(of: " vs ", with: " v ", options: .caseInsensitive)
        let parts = normalized.components(separatedBy: " v ")
        guard parts.count == 2,
              let home = aflTeamCode(parts[0]),
              let away = aflTeamCode(parts[1])
        else {
            return matchName
        }
        return "\(home) v \(away)"
    }

    static func aflTeamCode(_ teamName: String) -> String? {
        let normalized = teamName
            .trimmingCharacters(in: .whitespacesAndNewlines)
            .lowercased()
            .replacingOccurrences(of: ".", with: "")
        if normalized.contains("port adelaide") || normalized.hasPrefix("port ") || normalized.contains(" power") { return "PTA" }
        if normalized.contains("north melbourne") || normalized.contains("kangaroos") { return "NTH" }
        if normalized == "adelaide" || normalized.contains("adelaide crows") || normalized.hasSuffix(" crows") { return "ADE" }
        if normalized.contains("brisbane") { return "BRL" }
        if normalized.contains("carlton") { return "CAR" }
        if normalized.contains("collingwood") { return "COL" }
        if normalized.contains("essendon") { return "ESS" }
        if normalized.contains("fremantle") { return "FRE" }
        if normalized.contains("geelong") { return "GEE" }
        if normalized.contains("gold coast") { return "GCS" }
        if normalized.contains("greater western sydney") || normalized.contains("gws") { return "GWS" }
        if normalized.contains("hawthorn") { return "HAW" }
        if normalized.contains("melbourne") { return "MEL" }
        if normalized.contains("richmond") { return "RIC" }
        if normalized.contains("st kilda") { return "STK" }
        if normalized.contains("sydney") { return "SYD" }
        if normalized.contains("west coast") { return "WCE" }
        if normalized.contains("western bulldogs") || normalized.contains("bulldogs") || normalized.contains("footscray") { return "WBD" }
        return nil
    }

    nonisolated private static func parseBackendDate(_ value: String) -> Date? {
        let formatter = ISO8601DateFormatter()
        formatter.formatOptions = [.withInternetDateTime]
        if let date = formatter.date(from: value) {
            return date
        }
        let fractionalFormatter = ISO8601DateFormatter()
        fractionalFormatter.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        if let date = fractionalFormatter.date(from: value) {
            return date
        }
        for formatter in backendNaiveDateFormatters() {
            if let date = formatter.date(from: value) {
                return date
            }
        }
        return nil
    }

    nonisolated private static func backendNaiveDateFormatters() -> [DateFormatter] {
        [
            "yyyy-MM-dd'T'HH:mm:ss.SSSSSS",
            "yyyy-MM-dd'T'HH:mm:ss",
            "yyyy-MM-dd",
        ].map { format in
            let formatter = DateFormatter()
            formatter.locale = Locale(identifier: "en_US_POSIX")
            formatter.timeZone = TimeZone(secondsFromGMT: 0)
            formatter.dateFormat = format
            return formatter
        }
    }
}

extension DateFormatter {
    static let display: DateFormatter = {
        let formatter = DateFormatter()
        formatter.locale = .current
        formatter.dateFormat = "EEE d MMM, h:mm a"
        return formatter
    }()

    static let adelaideDisplay: DateFormatter = {
        let formatter = DateFormatter()
        formatter.locale = .current
        formatter.dateFormat = "EEE d MMM, h:mm a z"
        return formatter
    }()
}

extension String {
    var trimmedNonEmpty: String? {
        let trimmed = trimmingCharacters(in: .whitespacesAndNewlines)
        return trimmed.isEmpty ? nil : trimmed
    }

    var doubleValue: Double? {
        Double(trimmingCharacters(in: .whitespacesAndNewlines))
    }

    var intValue: Int? {
        Int(trimmingCharacters(in: .whitespacesAndNewlines))
    }
}
