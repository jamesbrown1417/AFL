package com.jamesbrown.aflmobile.data.repository

import com.jamesbrown.aflmobile.data.network.BackendApiClient
import com.jamesbrown.aflmobile.data.settings.AppSettingsStore
import com.jamesbrown.aflmobile.model.AppSettings
import com.jamesbrown.aflmobile.model.BookmakerSummary
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

    suspend fun searchPlayers(query: String): List<PlayerSummary> =
        apiClient.searchPlayers(query = query)

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
        query: String?,
        marketType: String?,
        eventId: Int?,
        selectionType: String?,
        minEdge: Double?,
        minPrice: Double?,
        maxPrice: Double?,
        sgmOnly: Boolean,
        bestOnly: Boolean,
    ): List<OddsSearchResult> = apiClient.searchOdds(
        bookmakers = bookmakers,
        query = query,
        marketType = marketType,
        eventId = eventId,
        selectionType = selectionType,
        minEdge = minEdge,
        minPrice = minPrice,
        maxPrice = maxPrice,
        sgmOnly = sgmOnly,
        bestOnly = bestOnly,
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
