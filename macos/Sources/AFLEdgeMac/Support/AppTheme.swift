import SwiftUI

enum AFLColor {
    static let navy950 = Color(hex: 0x0D1A2B)
    static let navy900 = Color(hex: 0x12263D)
    static let navy700 = Color(hex: 0x35516D)
    static let blue800 = Color(hex: 0x1F5C9E)
    static let blue700 = Color(hex: 0x2A72BC)
    static let blue600 = Color(hex: 0x4B90D6)
    static let blue500 = Color(hex: 0x6AABEA)
    static let blue400 = Color(hex: 0x8EC1F5)
    static let blue300 = Color(hex: 0xB7DAFA)
    static let blue200 = Color(hex: 0xD8EBFF)
    static let blue100 = Color(hex: 0xEAF5FF)
    static let blue50 = Color(hex: 0xF5FAFF)
    static let blue25 = Color(hex: 0xF9FCFF)
    static let iceWhite = Color(hex: 0xFDFEFF)
    static let orange700 = Color(hex: 0xC56612)
    static let orange600 = Color(hex: 0xD9791D)
    static let orange300 = Color(hex: 0xFFC897)
    static let orange200 = Color(hex: 0xFFE4C7)
    static let orange100 = Color(hex: 0xFFEEDB)
    static let orange50 = Color(hex: 0xFFF7EE)
    static let mint500 = Color(hex: 0x1E8A57)
    static let rose500 = Color(hex: 0xD65A5A)
    static let positiveSurface = Color(hex: 0xDDF4E5)
    static let negativeSurface = Color(hex: 0xFBE1DD)
    static let neutralSurface = Color(hex: 0xF7E9C7)
}

enum AFLTheme {
    static let primary = AFLColor.blue700
    static let primaryStrong = AFLColor.blue800
    static let accent = AFLColor.orange700
    static let accentSoft = AFLColor.orange100
    static let success = AFLColor.mint500
    static let danger = AFLColor.rose500

    static var detailBackground: some ShapeStyle {
        LinearGradient(
            colors: [
                AFLColor.blue25,
                AFLColor.iceWhite,
                AFLColor.orange50.opacity(0.72),
            ],
            startPoint: .topLeading,
            endPoint: .bottomTrailing
        )
    }

    static var cardBackground: some ShapeStyle {
        AFLColor.iceWhite.opacity(0.98)
    }

    static var paneBackground: some ShapeStyle {
        AFLColor.blue50.opacity(0.74)
    }

    static var tableBackground: some ShapeStyle {
        AFLColor.iceWhite
    }

    static var controlBackground: some ShapeStyle {
        LinearGradient(
            colors: [
                AFLColor.iceWhite.opacity(0.96),
                AFLColor.blue50.opacity(0.88),
            ],
            startPoint: .topLeading,
            endPoint: .bottomTrailing
        )
    }

    static var tableSelectionTint: Color {
        AFLColor.orange700
    }
}

extension Color {
    init(hex: UInt32, alpha: Double = 1) {
        let red = Double((hex & 0xFF0000) >> 16) / 255
        let green = Double((hex & 0x00FF00) >> 8) / 255
        let blue = Double(hex & 0x0000FF) / 255
        self.init(.sRGB, red: red, green: green, blue: blue, opacity: alpha)
    }
}

extension View {
    func aflDetailBackground() -> some View {
        background(AFLTheme.detailBackground)
    }

    func aflCard() -> some View {
        padding()
            .background(AFLTheme.cardBackground, in: .rect(cornerRadius: 8))
            .overlay(
                RoundedRectangle(cornerRadius: 8)
                    .stroke(AFLColor.blue200.opacity(0.72), lineWidth: 1)
            )
    }

    func aflPaneBackground() -> some View {
        background(AFLTheme.paneBackground)
    }

    func aflControlSurface() -> some View {
        padding(10)
            .background(AFLTheme.controlBackground, in: .rect(cornerRadius: 8))
            .overlay(
                RoundedRectangle(cornerRadius: 8)
                    .stroke(AFLColor.blue200.opacity(0.72), lineWidth: 1)
            )
    }

    func aflPanelSurface() -> some View {
        background(AFLTheme.cardBackground, in: .rect(cornerRadius: 8))
            .overlay(
                RoundedRectangle(cornerRadius: 8)
                    .stroke(AFLColor.blue200.opacity(0.72), lineWidth: 1)
            )
    }

    func aflTableSurface() -> some View {
        background(AFLTheme.tableBackground, in: .rect(cornerRadius: 8))
            .overlay(
                RoundedRectangle(cornerRadius: 8)
                    .stroke(AFLColor.blue200.opacity(0.62), lineWidth: 1)
            )
    }
}
