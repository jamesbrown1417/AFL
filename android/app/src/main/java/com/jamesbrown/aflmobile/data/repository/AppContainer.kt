package com.jamesbrown.aflmobile.data.repository

import android.content.Context
import com.jamesbrown.aflmobile.data.network.BackendApiClient
import com.jamesbrown.aflmobile.data.settings.AppSettingsStore


class AppContainer(context: Context) {
    val settingsStore = AppSettingsStore(context)
    private val apiClient = BackendApiClient(settingsStore)
    val repository = AflRepository(apiClient, settingsStore)
    val sgmDraftStore = SgmDraftStore()
}
