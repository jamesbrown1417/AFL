package com.jamesbrown.aflmobile.model

data class AppSettings(
    val apiBaseUrl: String = "http://10.0.2.2:8000/api/v1/",
    val authToken: String = "",
    val defaultBookmaker: String = "sportsbet",
)

const val OddsDiffSliderMin = -1f
const val OddsDiffSliderMax = 1f
val MatchupDifficultyOptions = listOf("Terrible", "Bad", "Neutral", "Good", "Excellent")

data class OddsFilters(
    val scope: String = "player",
    val query: String = "",
    val bookmakerCodes: List<String> = emptyList(),
    val marketTypeCode: String? = null,
    val eventId: Int? = null,
    val includePlayerIds: List<Int> = emptyList(),
    val excludePlayerIds: List<Int> = emptyList(),
    val sortBy: String = "diff_last_10",
    val sortDirection: String = "desc",
    val selectionType: String? = null,
    val matchupDifficulties: List<String> = emptyList(),
    val minPriceText: String = "",
    val maxPriceText: String = "",
    val minDiff2025: Float = OddsDiffSliderMin,
    val maxDiff2025: Float = OddsDiffSliderMax,
    val minDiffLast10: Float = OddsDiffSliderMin,
    val maxDiffLast10: Float = OddsDiffSliderMax,
    val minEdgeText: String = "",
    val bestOnly: Boolean = false,
    val sgmOnly: Boolean = false,
)

data class SelectionMetricFilters(
    val matchupDifficulties: List<String> = emptyList(),
    val minPriceText: String = "",
    val maxPriceText: String = "",
    val minDiff2025: Float = OddsDiffSliderMin,
    val maxDiff2025: Float = OddsDiffSliderMax,
    val minDiffLast10: Float = OddsDiffSliderMin,
    val maxDiffLast10: Float = OddsDiffSliderMax,
    val minNextBestProbDiff: Float = OddsDiffSliderMin,
    val maxNextBestProbDiff: Float = OddsDiffSliderMax,
)

enum class BuilderDisplayMode {
    ROW,
    TILE,
}

enum class BuilderSortField {
    PLAYER,
    LINE,
    TYPE,
    NEXT_BEST,
    PRICE,
    DIFF_LAST_10,
    DIFF_2025,
}

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
    val diff2025: Double? = null,
    val diffLast10: Double? = null,
    val nextBestProbDiff: Double? = null,
    val isBestPrice: Boolean = false,
)

data class SgmDraftState(
    val bookmaker: String? = null,
    val eventId: Int? = null,
    val eventLabel: String? = null,
    val legs: List<DraftLeg> = emptyList(),
    val forceRefresh: Boolean = false,
    val latestQuote: SgmQuoteResponse? = null,
    val latestComparisons: List<SgmAgencyComparison> = emptyList(),
    val latestError: String? = null,
)

data class DraftMutationResult(
    val applied: Boolean,
    val message: String? = null,
)
