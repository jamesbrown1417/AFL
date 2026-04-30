import Foundation
import XCTest
@testable import AFLEdgeMac

@MainActor
final class AFLAPIClientTests: XCTestCase {
    func testMakeRequestNormalizesBaseURLAndAddsAuthHeader() throws {
        let client = AFLAPIClient(
            settingsProvider: {
                AppSettings(
                apiBaseURL: "http://localhost:8000/api/v1",
                authToken: "secret",
                defaultBookmaker: "sportsbet",
                tailscaleUserLogin: "user@example.com",
                tailscaleUserHeaderName: "Tailscale-User-Login"
                )
            },
            transport: MockTransport(data: Data("{}".utf8), statusCode: 200)
        )

        let request = try client.makeRequest(
            path: "events",
            query: [
                URLQueryItem(name: "limit", value: "50"),
                URLQueryItem(name: "bookmaker", value: "sportsbet"),
            ]
        )

        XCTAssertEqual(request.url?.absoluteString, "http://localhost:8000/api/v1/events?limit=50&bookmaker=sportsbet")
        XCTAssertEqual(request.value(forHTTPHeaderField: "Accept"), "application/json")
        XCTAssertEqual(request.value(forHTTPHeaderField: "Authorization"), "Bearer secret")
        XCTAssertEqual(request.value(forHTTPHeaderField: "Tailscale-User-Login"), "user@example.com")
    }

    func testHealthDecodesSuccessfulResponse() async throws {
        let payload = """
        {
          "status": "ok",
          "database_ok": true,
          "last_successful_import_at": "2026-04-17T09:00:00.582105"
        }
        """
        let client = AFLAPIClient(
            settingsProvider: { AppSettings() },
            transport: MockTransport(data: Data(payload.utf8), statusCode: 200)
        )

        let response = try await client.health()

        XCTAssertEqual(response.status, "ok")
        XCTAssertTrue(response.databaseOk)
        XCTAssertNotNil(response.lastSuccessfulImportAt)
    }

    func testBackendErrorEnvelopeBecomesTypedHTTPError() async throws {
        let payload = """
        {
          "error": {
            "code": "unauthorized",
            "message": "Missing or invalid bearer token.",
            "retriable": false,
            "details": {}
          }
        }
        """
        let client = AFLAPIClient(
            settingsProvider: { AppSettings() },
            transport: MockTransport(data: Data(payload.utf8), statusCode: 401)
        )

        do {
            _ = try await client.health()
            XCTFail("Expected an HTTP error.")
        } catch let error as APIClientError {
            XCTAssertEqual(error, .http(statusCode: 401, code: "unauthorized", message: "Missing or invalid bearer token."))
        }
    }

    func testOddsRequestDoesNotSendDefaultMetricRangeFilters() async throws {
        let payload = "[]"
        let transport = RequestCapturingTransport(data: Data(payload.utf8), statusCode: 200)
        let client = AFLAPIClient(
            settingsProvider: { AppSettings() },
            transport: transport
        )

        _ = try await client.odds(filters: OddsFilters(scope: .player), limit: 25)

        let capturedQueryItems = await transport.lastQueryItems()
        let queryItems = try XCTUnwrap(capturedQueryItems)
        let names = Set(queryItems.map(\.name))
        XCTAssertFalse(names.contains("min_diff_2025"))
        XCTAssertFalse(names.contains("max_diff_2025"))
        XCTAssertFalse(names.contains("min_diff_last_10"))
        XCTAssertFalse(names.contains("max_diff_last_10"))
        XCTAssertFalse(names.contains("min_next_best_prob_diff"))
        XCTAssertFalse(names.contains("max_next_best_prob_diff"))
    }

    func testOddsRequestSendsChangedMetricRangeFilters() async throws {
        let payload = "[]"
        let transport = RequestCapturingTransport(data: Data(payload.utf8), statusCode: 200)
        let client = AFLAPIClient(
            settingsProvider: { AppSettings() },
            transport: transport
        )
        var filters = OddsFilters(scope: .player)
        filters.minDiffLast10 = 0.2

        _ = try await client.odds(filters: filters, limit: 25)

        let capturedQueryItems = await transport.lastQueryItems()
        let queryItems = try XCTUnwrap(capturedQueryItems)
        XCTAssertEqual(queryItems.first(where: { $0.name == "min_diff_last_10" })?.value, "0.2")
        XCTAssertNil(queryItems.first(where: { $0.name == "max_diff_last_10" }))
    }

    func testOddsResultDecodesSnakeCaseContract() throws {
        let payload = """
        {
          "selection_id": 10,
          "market_id": 20,
          "event_id": 30,
          "match_name": "Adelaide vs Port Adelaide",
          "start_time": "2026-04-17T09:00:00",
          "bookmaker": "sportsbet",
          "market_type_code": "player_disposals",
          "market_display_name": "Disposals",
          "player": {"id": 1, "full_name": "Tim English"},
          "selection_type": "over",
          "label": "Tim English Over 19.5",
          "line_value": 19.5,
          "decimal_price": 1.91,
          "implied_prob": 0.523,
          "edge_pct": 2.0,
          "diff_2025": 0.12,
          "diff_last_10": 0.18,
          "player_position": "ruck",
          "matchup_difficulty": "Good",
          "weather": {"temperature_c": 18.0, "precip_mm": 0.2, "icon_code": "rain"},
          "is_best_price": true,
          "next_best_prob_diff": 0.04,
          "sgm_eligible": true
        }
        """

        let row = try AFLFormatters.backendDecoder.decode(OddsSearchResult.self, from: Data(payload.utf8))

        XCTAssertEqual(row.selectionId, 10)
        XCTAssertEqual(row.player?.fullName, "Tim English")
        XCTAssertEqual(row.diff2025, 0.12)
        XCTAssertEqual(row.weather?.iconCode, "rain")
    }

    func testEventDecodesNaiveBackendDate() throws {
        let payload = """
        {
          "id": 1,
          "match_name": "Sydney Swans v Carlton",
          "start_time": "2026-03-05T08:30:00",
          "round_label": "Opening Round",
          "venue": "SCG",
          "home_team": {"id": 1, "name": "Sydney Swans"},
          "away_team": {"id": 2, "name": "Carlton"},
          "available_bookmakers": []
        }
        """

        let event = try AFLFormatters.backendDecoder.decode(EventSummary.self, from: Data(payload.utf8))

        XCTAssertEqual(event.matchName, "Sydney Swans v Carlton")
        XCTAssertNotNil(event.startTime)
    }

    func testRealBackendSampleShapesDecode() throws {
        try decodeSampleIfPresent("/tmp/afl_health.json", as: HealthResponse.self)
        try decodeSampleIfPresent("/tmp/afl_data_status.json", as: DataStatusResponse.self)
        try decodeSampleIfPresent("/tmp/afl_bookmakers.json", as: [BookmakerSummary].self)
        try decodeSampleIfPresent("/tmp/afl_events_limit_3.json", as: [EventSummary].self)
        try decodeSampleIfPresent("/tmp/afl_players_stats_search_q__limit_3.json", as: [PlayerSummary].self)
        try decodeSampleIfPresent("/tmp/afl_players_search_q__limit_3.json", as: [PlayerSummary].self)
        try decodeSampleIfPresent(
            "/tmp/afl_odds_search_limit_3_offset_0_scope_player_sort_by_diff_last_10_sort_dir_desc_bookmaker_sportsbet.json",
            as: [OddsSearchResult].self
        )
    }

    private func decodeSampleIfPresent<T: Decodable>(_ path: String, as type: T.Type) throws {
        guard FileManager.default.fileExists(atPath: path) else { return }
        let data = try Data(contentsOf: URL(fileURLWithPath: path))
        _ = try AFLFormatters.backendDecoder.decode(T.self, from: data)
    }
}

private struct MockTransport: APITransport {
    let data: Data
    let statusCode: Int

    func data(for request: URLRequest) async throws -> (Data, URLResponse) {
        let response = HTTPURLResponse(
            url: request.url ?? URL(string: "http://localhost")!,
            statusCode: statusCode,
            httpVersion: "HTTP/1.1",
            headerFields: ["Content-Type": "application/json"]
        )!
        return (data, response)
    }
}

private actor RequestCapturingTransport: APITransport {
    let data: Data
    let statusCode: Int
    private var requests: [URLRequest] = []

    init(data: Data, statusCode: Int) {
        self.data = data
        self.statusCode = statusCode
    }

    func lastQueryItems() -> [URLQueryItem]? {
        guard let url = requests.last?.url,
              let components = URLComponents(url: url, resolvingAgainstBaseURL: false)
        else { return nil }
        return components.queryItems
    }

    func data(for request: URLRequest) async throws -> (Data, URLResponse) {
        requests.append(request)
        let response = HTTPURLResponse(
            url: request.url ?? URL(string: "http://localhost")!,
            statusCode: statusCode,
            httpVersion: "HTTP/1.1",
            headerFields: ["Content-Type": "application/json"]
        )!
        return (data, response)
    }
}
