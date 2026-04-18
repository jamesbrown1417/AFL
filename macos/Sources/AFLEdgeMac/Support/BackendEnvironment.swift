import Foundation

struct BackendEnvironmentAuth: Equatable {
    var authToken: String?
    var tailscaleUserLogin: String?
    var tailscaleUserHeaderName: String?
    var bindHost: String?
    var bindPort: String?

    var apiBaseURL: String? {
        guard let bindHost, let bindPort else { return nil }
        return "http://\(bindHost):\(bindPort)/api/v1/"
    }
}

enum BackendEnvironment {
    static func loadAuth() -> BackendEnvironmentAuth {
        guard let envURL = locateBackendEnv() else {
            return BackendEnvironmentAuth()
        }
        let values = parseEnvFile(envURL)
        return BackendEnvironmentAuth(
            authToken: values["AFL_AUTH_TOKEN"],
            tailscaleUserLogin: values["AFL_ALLOWED_TAILSCALE_USER_LOGINS"]?
                .split(separator: ",", omittingEmptySubsequences: true)
                .first
                .map { String($0).trimmingCharacters(in: .whitespacesAndNewlines) },
            tailscaleUserHeaderName: values["AFL_TAILSCALE_USER_HEADER_NAME"] ?? values["AFL_TAILSCALE_USER_HEADER"] ?? "Tailscale-User-Login",
            bindHost: values["AFL_BIND_HOST"] ?? "127.0.0.1",
            bindPort: values["AFL_BIND_PORT"] ?? "8000"
        )
    }

    private static func locateBackendEnv() -> URL? {
        let candidates = [
            Bundle.main.bundleURL,
            URL(fileURLWithPath: FileManager.default.currentDirectoryPath),
            URL(fileURLWithPath: CommandLine.arguments.first ?? FileManager.default.currentDirectoryPath),
        ]
        for candidate in candidates {
            if let found = walkForBackendEnv(startingAt: candidate) {
                return found
            }
        }
        return nil
    }

    private static func walkForBackendEnv(startingAt url: URL) -> URL? {
        var cursor = url.hasDirectoryPath ? url : url.deletingLastPathComponent()
        for _ in 0..<10 {
            let env = cursor.appendingPathComponent("backend/.env")
            if FileManager.default.fileExists(atPath: env.path) {
                return env
            }
            let parent = cursor.deletingLastPathComponent()
            if parent.path == cursor.path {
                return nil
            }
            cursor = parent
        }
        return nil
    }

    private static func parseEnvFile(_ url: URL) -> [String: String] {
        guard let content = try? String(contentsOf: url, encoding: .utf8) else {
            return [:]
        }
        var result: [String: String] = [:]
        for rawLine in content.components(separatedBy: .newlines) {
            let line = rawLine.trimmingCharacters(in: .whitespacesAndNewlines)
            guard !line.isEmpty, !line.hasPrefix("#"), let equals = line.firstIndex(of: "=") else {
                continue
            }
            let key = String(line[..<equals]).trimmingCharacters(in: .whitespacesAndNewlines)
            var value = String(line[line.index(after: equals)...]).trimmingCharacters(in: .whitespacesAndNewlines)
            if value.count >= 2,
               let first = value.first,
               let last = value.last,
               (first == "\"" && last == "\"") || (first == "'" && last == "'") {
                value = String(value.dropFirst().dropLast())
            }
            result[key] = value
        }
        return result
    }
}
