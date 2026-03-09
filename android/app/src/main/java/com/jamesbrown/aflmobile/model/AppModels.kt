package com.jamesbrown.aflmobile.model

data class AppSettings(
    val apiBaseUrl: String = "http://10.0.2.2:8000/api/v1/",
    val authToken: String = "",
    val defaultBookmaker: String = "sportsbet",
)

data class OddsFilters(
    val query: String = "",
    val bookmakerCodes: List<String> = emptyList(),
    val marketTypeCode: String? = null,
    val eventId: Int? = null,
    val selectionType: String? = null,
    val minPriceText: String = "",
    val maxPriceText: String = "",
    val minEdgeText: String = "",
    val bestOnly: Boolean = false,
    val sgmOnly: Boolean = false,
)

data class PlayerStatsFilters(
    val statCode: String = "disposals",
    val seasons: List<String> = emptyList(),
    val oppositions: List<String> = emptyList(),
    val venues: List<String> = emptyList(),
    val weatherCategories: List<String> = emptyList(),
    val homeAway: List<String> = listOf("Home", "Away"),
    val marginMinText: String = "-200",
    val marginMaxText: String = "200",
    val lastGamesText: String = "",
    val minutesMinimumText: String = "0",
    val lineMode: String = "single",
    val referenceLineText: String = "19.5",
    val lowerBoundText: String = "19.5",
    val upperBoundText: String = "25.5",
)

data class DraftLeg(
    val selectionId: Int,
    val eventId: Int,
    val eventLabel: String,
    val bookmaker: String,
    val label: String,
    val marketTypeCode: String,
    val selectionType: String,
    val basePrice: Double,
)

data class SgmDraftState(
    val bookmaker: String? = null,
    val eventId: Int? = null,
    val eventLabel: String? = null,
    val legs: List<DraftLeg> = emptyList(),
    val forceRefresh: Boolean = false,
    val latestQuote: SgmQuoteResponse? = null,
    val latestError: String? = null,
)

data class DraftMutationResult(
    val applied: Boolean,
    val message: String? = null,
)
