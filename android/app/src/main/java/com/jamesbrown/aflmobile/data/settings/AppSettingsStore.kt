package com.jamesbrown.aflmobile.data.settings

import android.content.Context
import androidx.datastore.preferences.core.PreferenceDataStoreFactory
import androidx.datastore.preferences.core.edit
import androidx.datastore.preferences.core.intPreferencesKey
import androidx.datastore.preferences.core.stringPreferencesKey
import androidx.datastore.preferences.preferencesDataStoreFile
import com.jamesbrown.aflmobile.model.AppSettings
import com.jamesbrown.aflmobile.model.AppThemeMode
import com.jamesbrown.aflmobile.model.PlayerSummary
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
        val ThemeMode = stringPreferencesKey("theme_mode")
        val LastPlayerId = intPreferencesKey("last_player_id")
        val LastPlayerName = stringPreferencesKey("last_player_name")
        val SgmDraftJson = stringPreferencesKey("sgm_draft_json")
    }

    val settingsFlow: Flow<AppSettings> = dataStore.data.map { prefs ->
        AppSettings(
            apiBaseUrl = normalizeBaseUrl(prefs[Keys.ApiBaseUrl] ?: AppSettings().apiBaseUrl),
            authToken = prefs[Keys.AuthToken].orEmpty(),
            defaultBookmaker = prefs[Keys.DefaultBookmaker] ?: AppSettings().defaultBookmaker,
            themeMode = parseThemeMode(prefs[Keys.ThemeMode]),
        )
    }

    suspend fun current(): AppSettings = settingsFlow.first()

    suspend fun save(settings: AppSettings) {
        dataStore.edit { prefs ->
            prefs[Keys.ApiBaseUrl] = normalizeBaseUrl(settings.apiBaseUrl)
            prefs[Keys.AuthToken] = settings.authToken.trim()
            prefs[Keys.DefaultBookmaker] = settings.defaultBookmaker.trim().ifBlank { "sportsbet" }
            prefs[Keys.ThemeMode] = settings.themeMode.name
        }
    }

    suspend fun saveThemeMode(themeMode: AppThemeMode) {
        dataStore.edit { prefs -> prefs[Keys.ThemeMode] = themeMode.name }
    }

    suspend fun saveLastViewedPlayer(player: PlayerSummary) {
        dataStore.edit { prefs ->
            prefs[Keys.LastPlayerId] = player.id
            prefs[Keys.LastPlayerName] = player.fullName
        }
    }

    suspend fun lastViewedPlayer(): PlayerSummary? {
        val prefs = dataStore.data.first()
        val id = prefs[Keys.LastPlayerId] ?: return null
        val name = prefs[Keys.LastPlayerName] ?: return null
        return PlayerSummary(id = id, fullName = name)
    }

    suspend fun saveSgmDraftJson(json: String) {
        dataStore.edit { prefs -> prefs[Keys.SgmDraftJson] = json }
    }

    suspend fun sgmDraftJson(): String? = dataStore.data.first()[Keys.SgmDraftJson]

    private fun normalizeBaseUrl(value: String): String {
        val trimmed = value.trim()
        if (trimmed.isEmpty()) return AppSettings().apiBaseUrl
        return if (trimmed.endsWith("/")) trimmed else "$trimmed/"
    }

    private fun parseThemeMode(value: String?): AppThemeMode =
        AppThemeMode.entries.firstOrNull { it.name == value } ?: AppSettings().themeMode
}
