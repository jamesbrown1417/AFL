import Foundation

protocol APITransport: Sendable {
    func data(for request: URLRequest) async throws -> (Data, URLResponse)
}

extension URLSession: APITransport {}

enum HTTPMethod: String {
    case get = "GET"
    case post = "POST"
}

enum APIClientError: Error, Equatable, LocalizedError {
    case invalidBaseURL(String)
    case invalidResponse
    case http(statusCode: Int, code: String?, message: String)
    case decoding(String)
    case transport(String)

    var errorDescription: String? {
        switch self {
        case .invalidBaseURL(let value):
            "Invalid API base URL: \(value)"
        case .invalidResponse:
            "Backend returned an invalid response."
        case .http(_, _, let message):
            message
        case .decoding(let message):
            "Failed to decode backend response: \(message)"
        case .transport(let message):
            message
        }
    }
}

@MainActor
final class AFLAPIClient {
    private let settingsProvider: () -> AppSettings
    private let transport: APITransport
    private let decoder: JSONDecoder
    private let encoder: JSONEncoder

    init(
        settingsProvider: @escaping () -> AppSettings,
        transport: APITransport = URLSession.shared,
        decoder: JSONDecoder = AFLFormatters.backendDecoder,
        encoder: JSONEncoder = AFLFormatters.backendEncoder
    ) {
        self.settingsProvider = settingsProvider
        self.transport = transport
        self.decoder = decoder
        self.encoder = encoder
    }

    func health() async throws -> HealthResponse {
        try await get("health")
    }

    func dataStatus() async throws -> DataStatusResponse {
        try await get("data/status")
    }

    func bookmakers() async throws -> [BookmakerSummary] {
        try await get("bookmakers")
    }

    func events(bookmaker: String? = nil, query: String? = nil, limit: Int = 50) async throws -> [EventSummary] {
        try await get(
            "events",
            query: [
                URLQueryItem(name: "limit", value: "\(limit)"),
                URLQueryItem.optional("bookmaker", bookmaker),
                URLQueryItem.optional("q", query),
            ].compactMap(\.self)
        )
    }

    func event(_ eventId: Int) async throws -> EventSummary {
        try await get("events/\(eventId)")
    }

    func markets(eventId: Int, bookmaker: String, playerQuery: String? = nil, limit: Int = 100) async throws -> [MarketSummary] {
        try await get(
            "events/\(eventId)/markets",
            query: [
                URLQueryItem(name: "bookmaker", value: bookmaker),
                URLQueryItem(name: "limit", value: "\(limit)"),
                URLQueryItem.optional("player_q", playerQuery),
            ].compactMap(\.self)
        )
    }

    func selections(marketId: Int, bookmaker: String) async throws -> [SelectionSummary] {
        try await get(
            "markets/\(marketId)/selections",
            query: [URLQueryItem(name: "bookmaker", value: bookmaker)]
        )
    }

    func searchPlayers(query: String, limit: Int = 50) async throws -> [PlayerSummary] {
        try await get(
            "players/search",
            query: [
                URLQueryItem(name: "q", value: query),
                URLQueryItem(name: "limit", value: "\(limit)"),
            ]
        )
    }

    func searchStatPlayers(query: String, limit: Int = 50) async throws -> [PlayerSummary] {
        try await get(
            "players/stats/search",
            query: [
                URLQueryItem(name: "q", value: query),
                URLQueryItem(name: "limit", value: "\(limit)"),
            ]
        )
    }

    func playerStatFilters(playerId: Int) async throws -> PlayerStatFilterOptions {
        try await get("players/\(playerId)/stats/filters")
    }

    func playerStatHistory(playerId: Int, filters: PlayerStatsFilters) async throws -> [PlayerGameLogEntry] {
        try await get(
            "players/\(playerId)/stats/history",
            query: playerStatsQuery(filters: filters, includeSummaryLineMode: false)
        )
    }

    func playerStatSummary(playerId: Int, filters: PlayerStatsFilters) async throws -> PlayerStatSummary {
        try await get(
            "players/\(playerId)/stats/summary",
            query: playerStatsQuery(filters: filters, includeSummaryLineMode: true)
        )
    }

    func odds(
        filters: OddsFilters,
        limit: Int = 200,
        offset: Int = 0
    ) async throws -> [OddsSearchResult] {
        let playerScoped = filters.scope == .player
        var query = [
            URLQueryItem(name: "limit", value: "\(limit)"),
            URLQueryItem(name: "offset", value: "\(offset)"),
            URLQueryItem(name: "scope", value: filters.scope.rawValue),
            URLQueryItem.optional("q", filters.query),
            URLQueryItem.optional("market_type", filters.marketTypeCode),
            URLQueryItem.optional("event_id", filters.eventId.map(String.init)),
            URLQueryItem(name: "sort_by", value: filters.sortBy),
            URLQueryItem(name: "sort_dir", value: filters.sortDirection),
        ].compactMap(\.self)
        query.append(contentsOf: filters.bookmakerCodes.map { URLQueryItem(name: "bookmaker", value: $0) })
        if playerScoped {
            query.append(contentsOf: filters.includePlayerIds.map { URLQueryItem(name: "include_player_id", value: "\($0)") })
            query.append(contentsOf: filters.excludePlayerIds.map { URLQueryItem(name: "exclude_player_id", value: "\($0)") })
            query.append(contentsOf: filters.matchupDifficulties.map { URLQueryItem(name: "matchup_difficulty", value: $0) })
            query.append(contentsOf: [
                URLQueryItem.optional("selection_type", filters.selectionType),
                URLQueryItem.optional("min_edge", filters.minEdgeText),
                URLQueryItem.optional("min_price", filters.minPriceText),
                URLQueryItem.optional("max_price", filters.maxPriceText),
                URLQueryItem(name: "min_diff_2025", value: "\(filters.minDiff2025)"),
                URLQueryItem(name: "max_diff_2025", value: "\(filters.maxDiff2025)"),
                URLQueryItem(name: "min_diff_last_10", value: "\(filters.minDiffLast10)"),
                URLQueryItem(name: "max_diff_last_10", value: "\(filters.maxDiffLast10)"),
                URLQueryItem(name: "min_next_best_prob_diff", value: "\(filters.minNextBestProbDiff)"),
                URLQueryItem(name: "max_next_best_prob_diff", value: "\(filters.maxNextBestProbDiff)"),
            ].compactMap(\.self))
            if filters.bestOnly {
                query.append(URLQueryItem(name: "best_only", value: "true"))
            }
        }
        if filters.sgmOnly {
            query.append(URLQueryItem(name: "sgm_only", value: "true"))
        }
        return try await get("odds/search", query: query)
    }

    func odds(
        bookmakers: [String],
        scope: OddsScope,
        marketType: String? = nil,
        eventId: Int? = nil,
        includePlayerIds: [Int] = [],
        excludePlayerIds: [Int] = [],
        sortBy: String,
        sortDirection: String,
        selectionType: String? = nil,
        matchupDifficulties: [String] = [],
        minPrice: Double? = nil,
        maxPrice: Double? = nil,
        minDiff2025: Double? = nil,
        maxDiff2025: Double? = nil,
        minDiffLast10: Double? = nil,
        maxDiffLast10: Double? = nil,
        minNextBestProbDiff: Double? = nil,
        maxNextBestProbDiff: Double? = nil,
        sgmOnly: Bool = false,
        bestOnly: Bool = false,
        limit: Int = 200
    ) async throws -> [OddsSearchResult] {
        var filters = OddsFilters(scope: scope)
        filters.bookmakerCodes = bookmakers
        filters.marketTypeCode = marketType
        filters.eventId = eventId
        filters.includePlayerIds = includePlayerIds
        filters.excludePlayerIds = excludePlayerIds
        filters.sortBy = sortBy
        filters.sortDirection = sortDirection
        filters.selectionType = selectionType
        filters.matchupDifficulties = matchupDifficulties
        filters.minPriceText = minPrice.map { String($0) } ?? ""
        filters.maxPriceText = maxPrice.map { String($0) } ?? ""
        filters.minDiff2025 = minDiff2025 ?? oddsDiffSliderMin
        filters.maxDiff2025 = maxDiff2025 ?? oddsDiffSliderMax
        filters.minDiffLast10 = minDiffLast10 ?? oddsDiffSliderMin
        filters.maxDiffLast10 = maxDiffLast10 ?? oddsDiffSliderMax
        filters.minNextBestProbDiff = minNextBestProbDiff ?? oddsDiffSliderMin
        filters.maxNextBestProbDiff = maxNextBestProbDiff ?? oddsDiffSliderMax
        filters.sgmOnly = sgmOnly
        filters.bestOnly = bestOnly
        return try await odds(filters: filters, limit: limit)
    }

    func props(bookmaker: String, query: String? = nil, limit: Int = 100) async throws -> [PropSearchResult] {
        try await get(
            "props/search",
            query: [
                URLQueryItem(name: "bookmaker", value: bookmaker),
                URLQueryItem(name: "limit", value: "\(limit)"),
                URLQueryItem.optional("q", query),
            ].compactMap(\.self)
        )
    }

    func quoteSgm(bookmaker: String, eventId: Int, selectionIds: [Int], forceRefresh: Bool) async throws -> SgmQuoteResponse {
        try await post(
            "pricing/sgm",
            payload: SgmQuoteRequestPayload(
                bookmaker: bookmaker,
                eventId: eventId,
                legs: selectionIds.map(RequestedLeg.init(selectionId:)),
                forceRefresh: forceRefresh
            )
        )
    }

    func compareSgm(eventId: Int, selectionIds: [Int], forceRefresh: Bool) async throws -> SgmCompareResponse {
        try await post(
            "pricing/sgm/compare",
            payload: SgmCompareRequestPayload(
                eventId: eventId,
                selectionIds: selectionIds,
                forceRefresh: forceRefresh
            )
        )
    }

    func compareCgm(selectionIds: [Int]) async throws -> CgmCompareResponse {
        try await post(
            "pricing/cgm",
            payload: CgmCompareRequestPayload(selectionIds: selectionIds)
        )
    }

    func quote(_ quoteId: String) async throws -> SgmQuoteResponse {
        try await get("quotes/\(quoteId)")
    }

    func makeRequest(
        path: String,
        query: [URLQueryItem] = [],
        method: HTTPMethod = .get,
        body: Data? = nil
    ) throws -> URLRequest {
        let settings = settingsProvider()
        let normalizedBase = AppSettingsStore.normalizedBaseURL(settings.apiBaseURL)
        guard let baseURL = URL(string: normalizedBase) else {
            throw APIClientError.invalidBaseURL(settings.apiBaseURL)
        }
        let relativePath = path.hasPrefix("/") ? String(path.dropFirst()) : path
        let endpoint = URL(string: relativePath, relativeTo: baseURL)?.absoluteURL
        guard let endpoint else {
            throw APIClientError.invalidBaseURL(normalizedBase)
        }
        var components = URLComponents(url: endpoint, resolvingAgainstBaseURL: false)
        components?.queryItems = query.filter { item in
            guard let value = item.value else { return false }
            return !value.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty
        }
        guard let url = components?.url else {
            throw APIClientError.invalidBaseURL(normalizedBase)
        }
        var request = URLRequest(url: url)
        request.httpMethod = method.rawValue
        request.setValue("application/json", forHTTPHeaderField: "Accept")
        if let token = settings.authToken.trimmedNonEmpty {
            request.setValue("Bearer \(token)", forHTTPHeaderField: "Authorization")
        }
        if let login = settings.tailscaleUserLogin.trimmedNonEmpty,
           let headerName = settings.tailscaleUserHeaderName.trimmedNonEmpty {
            request.setValue(login, forHTTPHeaderField: headerName)
        }
        if let body {
            request.httpBody = body
            request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        }
        return request
    }

    private func get<T: Decodable>(_ path: String, query: [URLQueryItem] = []) async throws -> T {
        let request = try makeRequest(path: path, query: query)
        return try await execute(request)
    }

    private func post<Payload: Encodable, Response: Decodable>(_ path: String, payload: Payload) async throws -> Response {
        let body = try encoder.encode(payload)
        let request = try makeRequest(path: path, method: .post, body: body)
        return try await execute(request)
    }

    private func execute<T: Decodable>(_ request: URLRequest) async throws -> T {
        let data: Data
        let response: URLResponse
        do {
            (data, response) = try await transport.data(for: request)
        } catch {
            AppLog.api.error("Transport failure: \(error.localizedDescription, privacy: .public)")
            throw APIClientError.transport(error.localizedDescription)
        }

        guard let httpResponse = response as? HTTPURLResponse else {
            throw APIClientError.invalidResponse
        }

        guard (200..<300).contains(httpResponse.statusCode) else {
            let envelope = try? decoder.decode(APIErrorEnvelope.self, from: data)
            let message = envelope?.error.message ?? "Backend request failed with \(httpResponse.statusCode)."
            AppLog.api.error("HTTP \(httpResponse.statusCode, privacy: .public): \(message, privacy: .public)")
            throw APIClientError.http(
                statusCode: httpResponse.statusCode,
                code: envelope?.error.code,
                message: message
            )
        }

        do {
            return try decoder.decode(T.self, from: data)
        } catch {
            let url = request.url?.absoluteString ?? "<unknown>"
            AppLog.api.error("Decode failure for \(url, privacy: .public): \(String(describing: error), privacy: .public)")
            throw APIClientError.decoding(error.localizedDescription)
        }
    }

    private func playerStatsQuery(filters: PlayerStatsFilters, includeSummaryLineMode: Bool) -> [URLQueryItem] {
        var query = [
            URLQueryItem(name: "stat", value: filters.statCode),
            URLQueryItem(name: "margin_min", value: "\(filters.marginMinText.intValue ?? -200)"),
            URLQueryItem(name: "margin_max", value: "\(filters.marginMaxText.intValue ?? 200)"),
            URLQueryItem(name: "minutes_minimum", value: "\(filters.minutesMinimumText.doubleValue ?? 0)"),
            URLQueryItem.optional("last_games", filters.lastGamesText),
        ].compactMap(\.self)
        query.append(contentsOf: filters.seasons.map { URLQueryItem(name: "seasons", value: $0) })
        query.append(contentsOf: filters.oppositions.map { URLQueryItem(name: "oppositions", value: $0) })
        query.append(contentsOf: filters.venues.map { URLQueryItem(name: "venues", value: $0) })
        query.append(contentsOf: filters.weatherCategories.map { URLQueryItem(name: "weather_categories", value: $0) })
        query.append(contentsOf: filters.homeAway.map { URLQueryItem(name: "home_away", value: $0) })
        if includeSummaryLineMode {
            query.append(URLQueryItem(name: "line_mode", value: filters.lineMode))
        } else if let lineMode = filters.resolvedHistoryLineMode {
            query.append(URLQueryItem(name: "line_mode", value: lineMode))
        }
        query.append(contentsOf: [
            URLQueryItem.optional("reference_line", filters.referenceLineText),
            URLQueryItem.optional("lower_bound", filters.lowerBoundText),
            URLQueryItem.optional("upper_bound", filters.upperBoundText),
        ].compactMap(\.self))
        return query
    }
}

private extension URLQueryItem {
    static func optional(_ name: String, _ value: String?) -> URLQueryItem? {
        guard let value = value?.trimmedNonEmpty else { return nil }
        return URLQueryItem(name: name, value: value)
    }
}
