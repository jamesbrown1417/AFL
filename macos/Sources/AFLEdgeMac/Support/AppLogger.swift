import OSLog

enum AppLog {
    static let subsystem = "com.jamesbrown.AFLEdgeMac"

    static let app = Logger(subsystem: subsystem, category: "app")
    static let api = Logger(subsystem: subsystem, category: "api")
    static let settings = Logger(subsystem: subsystem, category: "settings")
    static let drafts = Logger(subsystem: subsystem, category: "drafts")
}
