package com.jamesbrown.aflmobile.model

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.JsonElement


@Serializable
data class HealthResponse(
    val status: String,
    @SerialName("database_ok") val databaseOk: Boolean,
    @SerialName("last_successful_import_at") val lastSuccessfulImportAt: String? = null,
)

@Serializable
data class DataFileStatus(
    @SerialName("file_name") val fileName: String,
    @SerialName("relative_path") val relativePath: String,
    @SerialName("modified_at") val modifiedAt: String,
)

@Serializable
data class DataFileSection(
    val code: String,
    val title: String,
    val category: String,
    val files: List<DataFileStatus>,
)

@Serializable
data class DataStatusResponse(
    @SerialName("generated_at") val generatedAt: String,
    val sections: List<DataFileSection>,
)

@Serializable
data class TeamSummary(
    val id: Int,
    val name: String,
)

@Serializable
data class PlayerSummary(
    val id: Int,
    @SerialName("full_name") val fullName: String,
)

@Serializable
data class PlayerStatOption(
    val code: String,
    val label: String,
)

@Serializable
data class PlayerStatFilterOptions(
    @SerialName("player_id") val playerId: Int,
    @SerialName("player_name") val playerName: String,
    val stats: List<PlayerStatOption>,
    val seasons: List<String>,
    val oppositions: List<String>,
    val venues: List<String>,
    @SerialName("weather_categories") val weatherCategories: List<String>,
    @SerialName("home_away_options") val homeAwayOptions: List<String>,
)

@Serializable
data class PlayerGameLogEntry(
    val date: String,
    @SerialName("round_label") val roundLabel: String? = null,
    val home: String? = null,
    val venue: String? = null,
    val weather: String? = null,
    val away: String? = null,
    val team: String? = null,
    val opposition: String? = null,
    val margin: Int? = null,
    val tog: Double? = null,
    val disposals: Double? = null,
    val kicks: Double? = null,
    val handballs: Double? = null,
    val marks: Double? = null,
    val goals: Double? = null,
    val behinds: Double? = null,
    val tackles: Double? = null,
    val hitouts: Double? = null,
    @SerialName("frees_for") val freesFor: Double? = null,
    @SerialName("frees_against") val freesAgainst: Double? = null,
    val fantasy: Double? = null,
    val cba: Double? = null,
    @SerialName("game_number") val gameNumber: Int,
    @SerialName("selected_stat") val selectedStat: String,
    @SerialName("selected_value") val selectedValue: Double? = null,
    val hit: Boolean? = null,
)

@Serializable
data class PlayerStatSummary(
    @SerialName("player_id") val playerId: Int,
    @SerialName("stat_code") val statCode: String,
    @SerialName("stat_label") val statLabel: String,
    @SerialName("line_mode") val lineMode: String,
    @SerialName("reference_line") val referenceLine: Double? = null,
    @SerialName("lower_bound") val lowerBound: Double? = null,
    @SerialName("upper_bound") val upperBound: Double? = null,
    @SerialName("sample_size") val sampleSize: Int,
    @SerialName("proportion_over") val proportionOver: Double? = null,
    @SerialName("proportion_under") val proportionUnder: Double? = null,
    @SerialName("implied_odds_over") val impliedOddsOver: Double? = null,
    @SerialName("implied_odds_under") val impliedOddsUnder: Double? = null,
    @SerialName("proportion_within_interval") val proportionWithinInterval: Double? = null,
    @SerialName("proportion_outside_interval") val proportionOutsideInterval: Double? = null,
    @SerialName("implied_odds_within_interval") val impliedOddsWithinInterval: Double? = null,
    @SerialName("implied_odds_outside_interval") val impliedOddsOutsideInterval: Double? = null,
)

@Serializable
data class BookmakerSummary(
    val id: Int,
    val code: String,
    @SerialName("display_name") val displayName: String,
    val enabled: Boolean,
    @SerialName("live_pricing_enabled") val livePricingEnabled: Boolean,
    @SerialName("sgm_eligible_count") val sgmEligibleCount: Int,
)

@Serializable
data class EventSummary(
    val id: Int,
    @SerialName("match_name") val matchName: String,
    @SerialName("start_time") val startTime: String? = null,
    @SerialName("round_label") val roundLabel: String? = null,
    val venue: String? = null,
    @SerialName("home_team") val homeTeam: TeamSummary,
    @SerialName("away_team") val awayTeam: TeamSummary,
    @SerialName("available_bookmakers") val availableBookmakers: List<String>,
)

@Serializable
data class MarketSummary(
    val id: Int,
    @SerialName("event_id") val eventId: Int,
    @SerialName("market_type_code") val marketTypeCode: String,
    @SerialName("display_name") val displayName: String,
    val player: PlayerSummary? = null,
    @SerialName("line_value") val lineValue: Double? = null,
    val bookmaker: String,
    @SerialName("available_selection_types") val availableSelectionTypes: List<String>,
)

@Serializable
data class SelectionSummary(
    val id: Int,
    @SerialName("market_id") val marketId: Int,
    @SerialName("selection_type") val selectionType: String,
    val label: String,
    @SerialName("decimal_price") val decimalPrice: Double? = null,
    @SerialName("implied_prob") val impliedProb: Double? = null,
    val bookmaker: String,
    @SerialName("sgm_eligible") val sgmEligible: Boolean,
    @SerialName("edge_pct") val edgePct: Double? = null,
)

@Serializable
data class WeatherSummary(
    @SerialName("temperature_c") val temperatureC: Double? = null,
    @SerialName("wind_kph") val windKph: Double? = null,
    @SerialName("precip_probability") val precipProbability: Double? = null,
    @SerialName("precip_mm") val precipMm: Double? = null,
    val label: String? = null,
    @SerialName("icon_code") val iconCode: String? = null,
)

@Serializable
data class OddsSearchResult(
    @SerialName("selection_id") val selectionId: Int,
    @SerialName("market_id") val marketId: Int,
    @SerialName("event_id") val eventId: Int,
    @SerialName("match_name") val matchName: String,
    @SerialName("start_time") val startTime: String? = null,
    val bookmaker: String,
    @SerialName("market_type_code") val marketTypeCode: String,
    @SerialName("market_display_name") val marketDisplayName: String,
    val player: PlayerSummary? = null,
    @SerialName("selection_type") val selectionType: String,
    val label: String,
    @SerialName("line_value") val lineValue: Double? = null,
    @SerialName("decimal_price") val decimalPrice: Double? = null,
    @SerialName("implied_prob") val impliedProb: Double? = null,
    @SerialName("edge_pct") val edgePct: Double? = null,
    @SerialName("diff_2025") val diff2025: Double? = null,
    @SerialName("diff_last_10") val diffLast10: Double? = null,
    @SerialName("player_position") val playerPosition: String? = null,
    @SerialName("matchup_difficulty") val matchupDifficulty: String? = null,
    val weather: WeatherSummary? = null,
    @SerialName("is_best_price") val isBestPrice: Boolean = false,
    @SerialName("next_best_prob_diff") val nextBestProbDiff: Double? = null,
    @SerialName("sgm_eligible") val sgmEligible: Boolean,
)

@Serializable
data class PropSearchResult(
    @SerialName("selection_id") val selectionId: Int,
    @SerialName("event_id") val eventId: Int,
    @SerialName("match_name") val matchName: String,
    @SerialName("start_time") val startTime: String? = null,
    val bookmaker: String,
    @SerialName("market_type_code") val marketTypeCode: String,
    val player: PlayerSummary? = null,
    @SerialName("selection_type") val selectionType: String,
    val label: String,
    @SerialName("line_value") val lineValue: Double? = null,
    @SerialName("decimal_price") val decimalPrice: Double? = null,
    @SerialName("implied_prob") val impliedProb: Double? = null,
    @SerialName("edge_pct") val edgePct: Double? = null,
    @SerialName("sgm_eligible") val sgmEligible: Boolean,
)

@Serializable
data class QuoteLeg(
    @SerialName("selection_id") val selectionId: Int,
    val label: String,
    @SerialName("market_type_code") val marketTypeCode: String,
    @SerialName("selection_type") val selectionType: String,
    @SerialName("base_price") val basePrice: Double,
)

@Serializable
data class SgmCompareRequestPayload(
    @SerialName("event_id") val eventId: Int,
    @SerialName("selection_ids") val selectionIds: List<Int>,
    @SerialName("force_refresh") val forceRefresh: Boolean,
)

@Serializable
data class SgmAgencyComparison(
    @SerialName("quote_id") val quoteId: String,
    val bookmaker: String,
    @SerialName("event_id") val eventId: Int,
    val legs: List<QuoteLeg>,
    @SerialName("unadjusted_price") val unadjustedPrice: Double,
    @SerialName("quoted_price") val quotedPrice: Double,
    @SerialName("adjustment_factor") val adjustmentFactor: Double,
    @SerialName("from_cache") val fromCache: Boolean,
    @SerialName("quoted_at") val quotedAt: String,
    @SerialName("expires_at") val expiresAt: String,
    val status: String,
)

@Serializable
data class SgmCompareResponse(
    @SerialName("event_id") val eventId: Int,
    @SerialName("selection_count") val selectionCount: Int,
    val results: List<SgmAgencyComparison>,
)

@Serializable
data class CgmCompareRequestPayload(
    @SerialName("selection_ids") val selectionIds: List<Int>,
)

@Serializable
data class CgmLegPrice(
    @SerialName("selection_id") val selectionId: Int,
    @SerialName("match_name") val matchName: String,
    val label: String,
    @SerialName("market_type_code") val marketTypeCode: String,
    @SerialName("selection_type") val selectionType: String,
    @SerialName("base_price") val basePrice: Double,
)

@Serializable
data class CgmAgencyComparison(
    val bookmaker: String,
    @SerialName("quoted_price") val quotedPrice: Double,
    @SerialName("selection_count") val selectionCount: Int,
    val legs: List<CgmLegPrice>,
)

@Serializable
data class CgmCompareResponse(
    @SerialName("selection_count") val selectionCount: Int,
    val results: List<CgmAgencyComparison>,
)

@Serializable
data class ApiErrorEnvelope(
    val error: ApiErrorBody,
)

@Serializable
data class ApiErrorBody(
    val code: String,
    val message: String,
    val retriable: Boolean,
    val details: JsonElement? = null,
)
