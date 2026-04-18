import Foundation
import Observation

@MainActor
@Observable
final class AppSettingsStore {
    private enum Keys {
        static let apiBaseURL = "apiBaseURL"
        static let authToken = "authToken"
        static let defaultBookmaker = "defaultBookmaker"
        static let tailscaleUserLogin = "tailscaleUserLogin"
        static let tailscaleUserHeaderName = "tailscaleUserHeaderName"
        static let preferredDisplayMode = "preferredDisplayMode"
        static let denseTables = "denseTables"
    }

    private let defaults: UserDefaults

    var settings: AppSettings
    var preferredDisplayMode: BuilderDisplayMode
    var denseTables: Bool

    init(defaults: UserDefaults = .standard) {
        self.defaults = defaults
        let defaultSettings = AppSettings()
        let backendAuth = BackendEnvironment.loadAuth()
        self.settings = AppSettings(
            apiBaseURL: Self.normalizedBaseURL(Self.firstNonBlank(defaults.string(forKey: Keys.apiBaseURL), backendAuth.apiBaseURL) ?? defaultSettings.apiBaseURL),
            authToken: Self.firstNonBlank(defaults.string(forKey: Keys.authToken), backendAuth.authToken) ?? defaultSettings.authToken,
            defaultBookmaker: Self.firstNonBlank(defaults.string(forKey: Keys.defaultBookmaker)) ?? defaultSettings.defaultBookmaker,
            tailscaleUserLogin: Self.firstNonBlank(defaults.string(forKey: Keys.tailscaleUserLogin), backendAuth.tailscaleUserLogin) ?? defaultSettings.tailscaleUserLogin,
            tailscaleUserHeaderName: Self.firstNonBlank(defaults.string(forKey: Keys.tailscaleUserHeaderName), backendAuth.tailscaleUserHeaderName) ?? defaultSettings.tailscaleUserHeaderName
        )
        self.preferredDisplayMode = BuilderDisplayMode(rawValue: defaults.string(forKey: Keys.preferredDisplayMode) ?? "") ?? .row
        self.denseTables = defaults.object(forKey: Keys.denseTables) as? Bool ?? false
    }

    func save(_ next: AppSettings) {
        let normalized = AppSettings(
            apiBaseURL: Self.normalizedBaseURL(next.apiBaseURL),
            authToken: next.authToken.trimmingCharacters(in: .whitespacesAndNewlines),
            defaultBookmaker: next.defaultBookmaker.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty
                ? "sportsbet"
                : next.defaultBookmaker.trimmingCharacters(in: .whitespacesAndNewlines),
            tailscaleUserLogin: next.tailscaleUserLogin.trimmingCharacters(in: .whitespacesAndNewlines),
            tailscaleUserHeaderName: next.tailscaleUserHeaderName.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty
                ? AppSettings().tailscaleUserHeaderName
                : next.tailscaleUserHeaderName.trimmingCharacters(in: .whitespacesAndNewlines)
        )
        settings = normalized
        defaults.set(normalized.apiBaseURL, forKey: Keys.apiBaseURL)
        defaults.set(normalized.authToken, forKey: Keys.authToken)
        defaults.set(normalized.defaultBookmaker, forKey: Keys.defaultBookmaker)
        defaults.set(normalized.tailscaleUserLogin, forKey: Keys.tailscaleUserLogin)
        defaults.set(normalized.tailscaleUserHeaderName, forKey: Keys.tailscaleUserHeaderName)
        AppLog.settings.info("Settings saved for base URL \(normalized.apiBaseURL, privacy: .public)")
    }

    func setPreferredDisplayMode(_ mode: BuilderDisplayMode) {
        preferredDisplayMode = mode
        defaults.set(mode.rawValue, forKey: Keys.preferredDisplayMode)
    }

    func setDenseTables(_ isDense: Bool) {
        denseTables = isDense
        defaults.set(isDense, forKey: Keys.denseTables)
    }

    static func normalizedBaseURL(_ value: String) -> String {
        let trimmed = value.trimmingCharacters(in: .whitespacesAndNewlines)
        let candidate = trimmed.isEmpty ? AppSettings().apiBaseURL : trimmed
        return candidate.hasSuffix("/") ? candidate : "\(candidate)/"
    }

    static func includesAPIPrefix(_ value: String) -> Bool {
        normalizedBaseURL(value).contains("/api/v1/")
    }

    private static func firstNonBlank(_ values: String?...) -> String? {
        values.compactMap { $0?.trimmedNonEmpty }.first
    }
}
