import Foundation
import Observation

@MainActor
@Observable
final class AppContainer {
    let settingsStore: AppSettingsStore
    let apiClient: AFLAPIClient
    let sgmDraftStore: SgmDraftStore
    let cgmDraftStore: CgmDraftStore

    init(settingsStore: AppSettingsStore = AppSettingsStore()) {
        self.settingsStore = settingsStore
        self.apiClient = AFLAPIClient(settingsProvider: { settingsStore.settings })
        self.sgmDraftStore = SgmDraftStore()
        self.cgmDraftStore = CgmDraftStore()
    }
}
