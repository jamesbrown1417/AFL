package com.jamesbrown.aflmobile.data.repository

import android.content.Context
import com.jamesbrown.aflmobile.data.network.BackendApiClient
import com.jamesbrown.aflmobile.data.settings.AppSettingsStore
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.SupervisorJob


class AppContainer(context: Context) {
    private val applicationScope = CoroutineScope(SupervisorJob() + Dispatchers.IO)
    val settingsStore = AppSettingsStore(context)
    private val apiClient = BackendApiClient(settingsStore)
    val repository = AflRepository(apiClient, settingsStore)
    val sgmDraftStore = SgmDraftStore(
        persistence = DataStoreSgmDraftPersistence(
            saveJson = settingsStore::saveSgmDraftJson,
            loadJson = settingsStore::sgmDraftJson,
        ),
        scope = applicationScope,
    )
}
