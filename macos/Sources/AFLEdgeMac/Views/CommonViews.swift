import AppKit
import SwiftUI

struct BrandHeader: View {
    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            if let url = Bundle.module.url(forResource: "app_logo_wordmark", withExtension: "png"),
               let image = NSImage(contentsOf: url) {
                Image(nsImage: image)
                    .resizable()
                    .scaledToFit()
                    .frame(maxWidth: 182, maxHeight: 72, alignment: .leading)
                    .clipShape(RoundedRectangle(cornerRadius: 8))
                    .accessibilityLabel("AFL Edge")
            } else {
                Text("AFL Edge")
                    .font(.title2.weight(.semibold))
            }
            Text("AFL odds and player intelligence")
                .font(.caption)
                .foregroundStyle(AFLColor.navy700)
        }
        .padding(.vertical, 8)
    }
}

struct LoadingStateView: View {
    var message: String

    var body: some View {
        HStack(spacing: 12) {
            ProgressView()
            Text(message)
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .padding()
        .background(AFLTheme.cardBackground, in: .rect(cornerRadius: 8))
        .overlay(RoundedRectangle(cornerRadius: 8).stroke(AFLColor.blue200.opacity(0.72)))
    }
}

struct EmptyStateView: View {
    var title: String
    var message: String

    var body: some View {
        VStack(alignment: .leading, spacing: 6) {
            Text(title)
                .font(.headline)
            Text(message)
                .foregroundStyle(.secondary)
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .padding()
        .background(AFLTheme.cardBackground, in: .rect(cornerRadius: 8))
        .overlay(RoundedRectangle(cornerRadius: 8).stroke(AFLColor.blue200.opacity(0.72)))
    }
}

struct ErrorStateView: View {
    var message: String

    var body: some View {
        Text(message)
            .frame(maxWidth: .infinity, alignment: .leading)
            .padding()
            .foregroundStyle(AFLTheme.danger)
            .background(AFLColor.negativeSurface.opacity(0.72), in: .rect(cornerRadius: 8))
            .overlay(RoundedRectangle(cornerRadius: 8).stroke(AFLTheme.danger.opacity(0.2)))
    }
}

struct InfoStateView: View {
    var message: String

    var body: some View {
        Text(message)
            .frame(maxWidth: .infinity, alignment: .leading)
            .padding()
            .foregroundStyle(AFLColor.navy700)
            .background(AFLColor.blue50.opacity(0.82), in: .rect(cornerRadius: 8))
            .overlay(RoundedRectangle(cornerRadius: 8).stroke(AFLColor.blue200.opacity(0.72)))
    }
}

struct MetricTile: View {
    var title: String
    var value: String
    var detail: String?

    var body: some View {
        VStack(alignment: .leading, spacing: 4) {
            Text(title)
                .font(.caption)
                .foregroundStyle(.secondary)
            Text(value)
                .font(.title2.weight(.semibold))
                .monospacedDigit()
            if let detail {
                Text(detail)
                    .font(.caption)
                    .foregroundStyle(.secondary)
                    .lineLimit(1)
            }
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .padding()
        .background(AFLTheme.cardBackground, in: .rect(cornerRadius: 8))
        .overlay(RoundedRectangle(cornerRadius: 8).stroke(AFLColor.blue200.opacity(0.72)))
    }
}

struct Pill: View {
    var label: String
    var systemImage: String?

    init(_ label: String, systemImage: String? = nil) {
        self.label = label
        self.systemImage = systemImage
    }

    var body: some View {
        Label {
            Text(label)
        } icon: {
            if let systemImage {
                Image(systemName: systemImage)
            }
        }
        .font(.caption.weight(.medium))
        .labelStyle(.titleAndIcon)
        .padding(.horizontal, 8)
        .padding(.vertical, 4)
        .foregroundStyle(AFLColor.navy700)
        .background(AFLColor.blue100.opacity(0.82), in: .rect(cornerRadius: 8))
        .overlay(RoundedRectangle(cornerRadius: 8).stroke(AFLColor.blue200.opacity(0.85)))
    }
}

struct SectionHeader: View {
    var title: String
    var subtitle: String?

    var body: some View {
        VStack(alignment: .leading, spacing: 3) {
            Text(title)
                .font(.title2.weight(.semibold))
                .foregroundStyle(AFLTheme.primaryStrong)
            if let subtitle {
                Text(subtitle)
                    .foregroundStyle(.secondary)
            }
        }
        .frame(maxWidth: .infinity, alignment: .leading)
    }
}

struct TogglePillButton: View {
    var title: String
    var isSelected: Bool
    var action: () -> Void

    var body: some View {
        Button(action: action) {
            Text(title)
                .font(.caption.weight(.medium))
                .padding(.horizontal, 10)
                .padding(.vertical, 6)
                .foregroundStyle(isSelected ? AFLColor.iceWhite : AFLColor.navy700)
                .background(isSelected ? AFLTheme.accent : AFLColor.blue100.opacity(0.88), in: .rect(cornerRadius: 8))
                .overlay(
                    RoundedRectangle(cornerRadius: 8)
                        .stroke(isSelected ? AFLColor.orange300.opacity(0.85) : AFLColor.blue200.opacity(0.85))
                )
        }
        .buttonStyle(.plain)
    }
}
