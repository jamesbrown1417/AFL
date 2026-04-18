import SwiftUI

struct DataStatusView: View {
    @Bindable var store: DataStatusStore

    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 16) {
                HStack {
                    SectionHeader(
                        title: "Data Status",
                        subtitle: "Backend health and source file freshness."
                    )
                    Spacer()
                    Button {
                        Task { await store.refresh() }
                    } label: {
                        Label("Refresh", systemImage: "arrow.clockwise")
                    }
                }

                if store.isLoading {
                    LoadingStateView(message: "Loading data status")
                }
                if let error = store.errorMessage {
                    ErrorStateView(message: error)
                }

                HealthPanel(health: store.health, response: store.response)

                if let response = store.response {
                    ForEach(response.sections) { section in
                        DataStatusSectionView(section: section)
                    }
                } else if !store.isLoading {
                    EmptyStateView(title: "No status loaded", message: "Refresh to query the backend status endpoint.")
                }
            }
            .padding()
        }
        .aflDetailBackground()
        .navigationTitle("Data Status")
        .task {
            await store.refresh()
        }
    }
}

private struct HealthPanel: View {
    var health: HealthResponse?
    var response: DataStatusResponse?

    var body: some View {
        Grid(horizontalSpacing: 12, verticalSpacing: 12) {
            GridRow {
                MetricTile(title: "Backend", value: health?.status ?? "--", detail: "Health endpoint")
                MetricTile(title: "Database", value: health?.databaseOk == true ? "Reachable" : "--", detail: nil)
                MetricTile(title: "Last Import", value: AFLFormatters.dateTimeInAdelaide(health?.lastSuccessfulImportAt), detail: nil)
                MetricTile(title: "Generated", value: AFLFormatters.dateTimeInAdelaide(response?.generatedAt), detail: nil)
            }
        }
    }
}

private struct DataStatusSectionView: View {
    var section: DataFileSection

    var body: some View {
        VStack(alignment: .leading, spacing: 10) {
            Text(section.title)
                .font(.headline)
            Table(section.files) {
                TableColumn("File") { file in
                    VStack(alignment: .leading) {
                        Text(file.fileName)
                        Text(file.relativePath)
                            .font(.caption)
                            .foregroundStyle(.secondary)
                    }
                }
                TableColumn("Modified") { file in
                    Text(AFLFormatters.dateTimeInAdelaide(file.modifiedAt))
                }
            }
            .frame(minHeight: CGFloat(max(section.files.count, 1)) * 42 + 44)
            .aflTableSurface()
        }
        .aflCard()
    }
}
