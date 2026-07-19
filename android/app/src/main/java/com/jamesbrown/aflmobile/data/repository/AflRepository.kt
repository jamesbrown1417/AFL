package com.jamesbrown.aflmobile.data.repository

import com.jamesbrown.aflmobile.data.network.BackendApiClient
import com.jamesbrown.aflmobile.data.settings.AppSettingsStore
import com.jamesbrown.aflmobile.model.AppSettings
import com.jamesbrown.aflmobile.model.AppThemeMode
import com.jamesbrown.aflmobile.model.BookmakerSummary
import com.jamesbrown.aflmobile.model.CgmCompareRequestPayload
import com.jamesbrown.aflmobile.model.CgmCompareResponse
import com.jamesbrown.aflmobile.model.DataStatusResponse
import com.jamesbrown.aflmobile.model.EventSummary
import com.jamesbrown.aflmobile.model.HealthResponse
import com.jamesbrown.aflmobile.model.MarketSummary
import com.jamesbrown.aflmobile.model.OddsQuery
import com.jamesbrown.aflmobile.model.OddsSearchResult
import com.jamesbrown.aflmobile.model.PlayerGameLogEntry
import com.jamesbrown.aflmobile.model.PlayerStatFilterOptions
import com.jamesbrown.aflmobile.model.PlayerStatBundle
import com.jamesbrown.aflmobile.model.PlayerStatSummary
import com.jamesbrown.aflmobile.model.PlayerSummary
import com.jamesbrown.aflmobile.model.PlayerStatsFilters
import com.jamesbrown.aflmobile.model.PropSearchResult
import com.jamesbrown.aflmobile.model.SelectionSummary
import com.jamesbrown.aflmobile.model.SgmCompareRequestPayload
import com.jamesbrown.aflmobile.model.SgmCompareResponse
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.sync.Mutex
import kotlinx.coroutines.sync.withLock


class AflRepository(
    private val apiClient: BackendApiClient,
    private val settingsStore: AppSettingsStore,
) {
    val settingsFlow: Flow<AppSettings> = settingsStore.settingsFlow

    private val bookmakerCacheMutex = Mutex()
    private var bookmakerCache: List<BookmakerSummary>? = null
    private var bookmakerCacheAtMillis: Long = 0L

    suspend fun currentSettings(): AppSettings = settingsStore.current()

    suspend fun saveSettings(settings: AppSettings) = settingsStore.save(settings)

    suspend fun saveThemeMode(themeMode: AppThemeMode) = settingsStore.saveThemeMode(themeMode)

    suspend fun saveLastViewedPlayer(player: PlayerSummary) =
        settingsStore.saveLastViewedPlayer(player)

    suspend fun lastViewedPlayer(): PlayerSummary? = settingsStore.lastViewedPlayer()

    suspend fun health(): HealthResponse = apiClient.getHealth()

    suspend fun dataStatus(): DataStatusResponse = apiClient.getDataStatus()

    /**
     * Bookmakers change rarely but four screens request them on entry; a short
     * in-memory cache stops the repeated round-trips. Pass [forceRefresh] from
     * explicit user refreshes.
     */
    suspend fun bookmakers(forceRefresh: Boolean = false): List<BookmakerSummary> =
        bookmakerCacheMutex.withLock {
            val cached = bookmakerCache
            val fresh = System.currentTimeMillis() - bookmakerCacheAtMillis < BOOKMAKER_CACHE_TTL_MILLIS
            if (!forceRefresh && cached != null && fresh) {
                cached
            } else {
                apiClient.getBookmakers().also {
                    bookmakerCache = it
                    bookmakerCacheAtMillis = System.currentTimeMillis()
                }
            }
        }

    suspend fun events(bookmaker: String?, query: String?): List<EventSummary> =
        apiClient.getEvents(bookmaker = bookmaker, query = query)

    suspend fun event(eventId: Int): EventSummary = apiClient.getEvent(eventId)

    suspend fun markets(eventId: Int, bookmaker: String, playerQuery: String?): List<MarketSummary> =
        apiClient.getMarkets(eventId = eventId, bookmaker = bookmaker, playerQuery = playerQuery)

    suspend fun selections(marketId: Int, bookmaker: String): List<SelectionSummary> =
        apiClient.getSelections(marketId = marketId, bookmaker = bookmaker)

    suspend fun searchPlayers(query: String, limit: Int = 50): List<PlayerSummary> =
        apiClient.searchPlayers(query = query, limit = limit)

    suspend fun searchStatPlayers(query: String, limit: Int = 50): List<PlayerSummary> =
        apiClient.searchStatPlayers(query = query, limit = limit)

    suspend fun playerStatFilters(playerId: Int): PlayerStatFilterOptions =
        apiClient.getPlayerStatFilters(playerId)

    /** Venues that survive the given filters, computed server-side. */
    suspend fun playerVenueOptions(
        playerId: Int,
        filters: PlayerStatsFilters,
    ): List<String> = apiClient.getPlayerStatFiltersNarrowed(
        playerId = playerId,
        seasons = filters.seasons,
        oppositions = filters.oppositions,
        weatherCategories = filters.weatherCategories,
        homeAway = filters.homeAway,
        marginMin = filters.marginMinText.toIntOrNull() ?: -200,
        marginMax = filters.marginMaxText.toIntOrNull() ?: 200,
        lastGames = filters.lastGamesText.toIntOrNull(),
        minutesMinimum = filters.minutesMinimumText.toDoubleOrNull() ?: 0.0,
    ).venues

    suspend fun playerStatHistory(
        playerId: Int,
        filters: PlayerStatsFilters,
    ): List<PlayerGameLogEntry> = apiClient.getPlayerStatHistory(
        playerId = playerId,
        stat = filters.statCode,
        seasons = filters.seasons,
        oppositions = filters.oppositions,
        venues = filters.venues,
        weatherCategories = filters.weatherCategories,
        homeAway = filters.homeAway,
        marginMin = filters.marginMinText.toIntOrNull() ?: -200,
        marginMax = filters.marginMaxText.toIntOrNull() ?: 200,
        lastGames = filters.lastGamesText.toIntOrNull(),
        minutesMinimum = filters.minutesMinimumText.toDoubleOrNull() ?: 0.0,
        lineMode = filters.resolvedLineModeOrNull(),
        referenceLine = filters.referenceLineText.toDoubleOrNull(),
        lowerBound = filters.lowerBoundText.toDoubleOrNull(),
        upperBound = filters.upperBoundText.toDoubleOrNull(),
    )

    suspend fun playerStatSummary(
        playerId: Int,
        filters: PlayerStatsFilters,
    ): PlayerStatSummary = apiClient.getPlayerStatSummary(
        playerId = playerId,
        stat = filters.statCode,
        lineMode = filters.lineMode,
        referenceLine = filters.referenceLineText.toDoubleOrNull(),
        lowerBound = filters.lowerBoundText.toDoubleOrNull(),
        upperBound = filters.upperBoundText.toDoubleOrNull(),
        seasons = filters.seasons,
        oppositions = filters.oppositions,
        venues = filters.venues,
        weatherCategories = filters.weatherCategories,
        homeAway = filters.homeAway,
        marginMin = filters.marginMinText.toIntOrNull() ?: -200,
        marginMax = filters.marginMaxText.toIntOrNull() ?: 200,
        lastGames = filters.lastGamesText.toIntOrNull(),
        minutesMinimum = filters.minutesMinimumText.toDoubleOrNull() ?: 0.0,
    )

    suspend fun playerStatBundle(
        playerId: Int,
        filters: PlayerStatsFilters,
    ): PlayerStatBundle = apiClient.getPlayerStatBundle(
        playerId = playerId,
        stat = filters.statCode,
        seasons = filters.seasons,
        oppositions = filters.oppositions,
        venues = filters.venues,
        weatherCategories = filters.weatherCategories,
        homeAway = filters.homeAway,
        marginMin = filters.marginMinText.toIntOrNull() ?: -200,
        marginMax = filters.marginMaxText.toIntOrNull() ?: 200,
        lastGames = filters.lastGamesText.toIntOrNull(),
        minutesMinimum = filters.minutesMinimumText.toDoubleOrNull() ?: 0.0,
        lineMode = filters.resolvedLineModeOrNull(),
        referenceLine = filters.referenceLineText.toDoubleOrNull(),
        lowerBound = filters.lowerBoundText.toDoubleOrNull(),
        upperBound = filters.upperBoundText.toDoubleOrNull(),
    )

    suspend fun odds(query: OddsQuery): List<OddsSearchResult> = apiClient.searchOdds(query)

    suspend fun props(bookmaker: String, query: String?): List<PropSearchResult> =
        apiClient.searchProps(bookmaker = bookmaker, query = query)

    suspend fun compareSgm(
        eventId: Int,
        selectionIds: List<Int>,
        forceRefresh: Boolean,
    ): SgmCompareResponse = apiClient.compareSgm(
        SgmCompareRequestPayload(
            eventId = eventId,
            selectionIds = selectionIds,
            forceRefresh = forceRefresh,
        ),
    )

    suspend fun compareCgm(selectionIds: List<Int>): CgmCompareResponse =
        apiClient.compareCgm(
            CgmCompareRequestPayload(
                selectionIds = selectionIds,
            ),
        )

    private companion object {
        const val BOOKMAKER_CACHE_TTL_MILLIS = 5 * 60 * 1000L
    }
}

private fun PlayerStatsFilters.resolvedLineModeOrNull(): String? =
    when (lineMode) {
        "interval" -> if (lowerBoundText.toDoubleOrNull() != null && upperBoundText.toDoubleOrNull() != null) {
            "interval"
        } else {
            null
        }
        else -> if (referenceLineText.toDoubleOrNull() != null) "single" else null
    }
