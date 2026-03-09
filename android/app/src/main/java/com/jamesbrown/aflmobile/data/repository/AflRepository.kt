package com.jamesbrown.aflmobile.data.repository

import com.jamesbrown.aflmobile.data.network.BackendApiClient
import com.jamesbrown.aflmobile.data.settings.AppSettingsStore
import com.jamesbrown.aflmobile.model.AppSettings
import com.jamesbrown.aflmobile.model.BookmakerSummary
import com.jamesbrown.aflmobile.model.CgmCompareRequestPayload
import com.jamesbrown.aflmobile.model.CgmCompareResponse
import com.jamesbrown.aflmobile.model.EventSummary
import com.jamesbrown.aflmobile.model.HealthResponse
import com.jamesbrown.aflmobile.model.MarketSummary
import com.jamesbrown.aflmobile.model.OddsSearchResult
import com.jamesbrown.aflmobile.model.PlayerGameLogEntry
import com.jamesbrown.aflmobile.model.PlayerStatFilterOptions
import com.jamesbrown.aflmobile.model.PlayerStatSummary
import com.jamesbrown.aflmobile.model.PlayerSummary
import com.jamesbrown.aflmobile.model.PlayerStatsFilters
import com.jamesbrown.aflmobile.model.PropSearchResult
import com.jamesbrown.aflmobile.model.RequestedLeg
import com.jamesbrown.aflmobile.model.SelectionSummary
import com.jamesbrown.aflmobile.model.SgmCompareRequestPayload
import com.jamesbrown.aflmobile.model.SgmCompareResponse
import com.jamesbrown.aflmobile.model.SgmQuoteRequestPayload
import com.jamesbrown.aflmobile.model.SgmQuoteResponse
import kotlinx.coroutines.flow.Flow


class AflRepository(
    private val apiClient: BackendApiClient,
    private val settingsStore: AppSettingsStore,
) {
    val settingsFlow: Flow<AppSettings> = settingsStore.settingsFlow

    suspend fun currentSettings(): AppSettings = settingsStore.current()

    suspend fun saveSettings(settings: AppSettings) = settingsStore.save(settings)

    suspend fun health(): HealthResponse = apiClient.getHealth()

    suspend fun bookmakers(): List<BookmakerSummary> = apiClient.getBookmakers()

    suspend fun events(bookmaker: String?, query: String?): List<EventSummary> =
        apiClient.getEvents(bookmaker = bookmaker, query = query)

    suspend fun event(eventId: Int): EventSummary = apiClient.getEvent(eventId)

    suspend fun markets(eventId: Int, bookmaker: String, playerQuery: String?): List<MarketSummary> =
        apiClient.getMarkets(eventId = eventId, bookmaker = bookmaker, playerQuery = playerQuery)

    suspend fun selections(marketId: Int, bookmaker: String): List<SelectionSummary> =
        apiClient.getSelections(marketId = marketId, bookmaker = bookmaker)

    suspend fun searchPlayers(query: String, limit: Int = 50): List<PlayerSummary> =
        apiClient.searchPlayers(query = query, limit = limit)

    suspend fun playerStatFilters(playerId: Int): PlayerStatFilterOptions =
        apiClient.getPlayerStatFilters(playerId)

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

    suspend fun odds(
        bookmakers: List<String>,
        scope: String,
        query: String?,
        marketType: String?,
        eventId: Int?,
        includePlayerIds: List<Int>,
        excludePlayerIds: List<Int>,
        sortBy: String,
        sortDirection: String,
        selectionType: String?,
        minEdge: Double?,
        minPrice: Double?,
        maxPrice: Double?,
        minDiff2025: Double?,
        maxDiff2025: Double?,
        minDiffLast10: Double?,
        maxDiffLast10: Double?,
        minNextBestProbDiff: Double? = null,
        maxNextBestProbDiff: Double? = null,
        sgmOnly: Boolean,
        bestOnly: Boolean,
        limit: Int = 200,
        offset: Int = 0,
    ): List<OddsSearchResult> = apiClient.searchOdds(
        bookmakers = bookmakers,
        scope = scope,
        query = query,
        marketType = marketType,
        eventId = eventId,
        includePlayerIds = includePlayerIds,
        excludePlayerIds = excludePlayerIds,
        sortBy = sortBy,
        sortDirection = sortDirection,
        selectionType = selectionType,
        minEdge = minEdge,
        minPrice = minPrice,
        maxPrice = maxPrice,
        minDiff2025 = minDiff2025,
        maxDiff2025 = maxDiff2025,
        minDiffLast10 = minDiffLast10,
        maxDiffLast10 = maxDiffLast10,
        minNextBestProbDiff = minNextBestProbDiff,
        maxNextBestProbDiff = maxNextBestProbDiff,
        sgmOnly = sgmOnly,
        bestOnly = bestOnly,
        limit = limit,
        offset = offset,
    )

    suspend fun props(bookmaker: String, query: String?): List<PropSearchResult> =
        apiClient.searchProps(bookmaker = bookmaker, query = query)

    suspend fun quoteSgm(
        bookmaker: String,
        eventId: Int,
        selectionIds: List<Int>,
        forceRefresh: Boolean,
    ): SgmQuoteResponse = apiClient.priceSgm(
        SgmQuoteRequestPayload(
            bookmaker = bookmaker,
            eventId = eventId,
            legs = selectionIds.map { RequestedLeg(it) },
            forceRefresh = forceRefresh,
        ),
    )

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

    suspend fun quote(quoteId: String): SgmQuoteResponse = apiClient.getQuote(quoteId)
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
