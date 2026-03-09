package com.jamesbrown.aflmobile.data.settings

import android.content.Context
import androidx.datastore.preferences.core.PreferenceDataStoreFactory
import androidx.datastore.preferences.core.edit
import androidx.datastore.preferences.core.stringPreferencesKey
import androidx.datastore.preferences.preferencesDataStoreFile
import com.jamesbrown.aflmobile.model.AppSettings
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.flow.map


class AppSettingsStore(context: Context) {
    private val dataStore = PreferenceDataStoreFactory.create(
        produceFile = { context.preferencesDataStoreFile("afl_edge_settings.preferences_pb") },
    )

    private object Keys {
        val ApiBaseUrl = stringPreferencesKey("api_base_url")
        val AuthToken = stringPreferencesKey("auth_token")
        val DefaultBookmaker = stringPreferencesKey("default_bookmaker")
    }

    val settingsFlow: Flow<AppSettings> = dataStore.data.map { prefs ->
        AppSettings(
            apiBaseUrl = normalizeBaseUrl(prefs[Keys.ApiBaseUrl] ?: AppSettings().apiBaseUrl),
            authToken = prefs[Keys.AuthToken].orEmpty(),
            defaultBookmaker = prefs[Keys.DefaultBookmaker] ?: AppSettings().defaultBookmaker,
        )
    }

    suspend fun current(): AppSettings = settingsFlow.first()

    suspend fun save(settings: AppSettings) {
        dataStore.edit { prefs ->
            prefs[Keys.ApiBaseUrl] = normalizeBaseUrl(settings.apiBaseUrl)
            prefs[Keys.AuthToken] = settings.authToken.trim()
            prefs[Keys.DefaultBookmaker] = settings.defaultBookmaker.trim().ifBlank { "sportsbet" }
        }
    }

    private fun normalizeBaseUrl(value: String): String {
        val trimmed = value.trim()
        if (trimmed.isEmpty()) return AppSettings().apiBaseUrl
        return if (trimmed.endsWith("/")) trimmed else "$trimmed/"
    }
}
