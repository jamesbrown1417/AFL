import Foundation
import XCTest
@testable import AFLEdgeMac

@MainActor
final class FormatterAndSettingsTests: XCTestCase {
    func testSettingsStoreNormalizesBaseURLAndDefaultBookmaker() {
        let defaults = UserDefaults(suiteName: "AFLEdgeMacTests-\(UUID().uuidString)")!
        let store = AppSettingsStore(defaults: defaults)

        store.save(
            AppSettings(
                apiBaseURL: "http://127.0.0.1:8000/api/v1",
                authToken: " token ",
                defaultBookmaker: " ",
                tailscaleUserLogin: " login@example.com ",
                tailscaleUserHeaderName: " "
            )
        )

        XCTAssertEqual(store.settings.apiBaseURL, "http://127.0.0.1:8000/api/v1/")
        XCTAssertEqual(store.settings.authToken, "token")
        XCTAssertEqual(store.settings.defaultBookmaker, "sportsbet")
        XCTAssertEqual(store.settings.tailscaleUserLogin, "login@example.com")
        XCTAssertEqual(store.settings.tailscaleUserHeaderName, "Tailscale-User-Login")
        XCTAssertTrue(AppSettingsStore.includesAPIPrefix(store.settings.apiBaseURL))
    }

    func testTeamCodeAndContextFormatters() {
        XCTAssertEqual(AFLFormatters.shortAFLMatchLabel("Adelaide Crows vs Port Adelaide"), "ADE v PTA")
        XCTAssertEqual(AFLFormatters.playerPositionTag("midfielder_forward"), "MID/F")
        XCTAssertEqual(AFLFormatters.matchupDifficultyTag("excellent"), "EXCL")
        XCTAssertEqual(AFLFormatters.weatherRainTag(0.4), "0.4mm")
    }

    func testDecimalFormatting() {
        XCTAssertEqual(AFLFormatters.decimalPrice(1.9), "1.90")
        XCTAssertEqual(AFLFormatters.signedMetric(0.125), "+0.12")
        XCTAssertEqual(AFLFormatters.percent(0.523), "52.3%")
    }
}
