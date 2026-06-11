package com.jamesbrown.aflmobile.model


/**
 * Single parameter object for the odds search endpoint, replacing the
 * 20+-argument call chains that previously threaded through every layer.
 */
data class OddsQuery(
    val bookmakers: List<String> = emptyList(),
    val scope: String = "player",
    val marketType: String? = null,
    /** Restrict to these events; empty means all events. */
    val eventIds: List<Int> = emptyList(),
    val includePlayerIds: List<Int> = emptyList(),
    val excludePlayerIds: List<Int> = emptyList(),
    val sortBy: String = "diff_last_10",
    val sortDirection: String = "desc",
    val selectionType: String? = null,
    val matchupDifficulties: List<String> = emptyList(),
    val minPrice: Double? = null,
    val maxPrice: Double? = null,
    val minDiff2025: Double? = null,
    val maxDiff2025: Double? = null,
    val minDiffLast10: Double? = null,
    val maxDiffLast10: Double? = null,
    val minNextBestProbDiff: Double? = null,
    val maxNextBestProbDiff: Double? = null,
    val sgmOnly: Boolean = false,
    val bestOnly: Boolean = false,
    val limit: Int = 200,
    val offset: Int = 0,
)
