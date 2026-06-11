package com.jamesbrown.aflmobile.ui.common.builder

import com.jamesbrown.aflmobile.model.BuilderSortField
import com.jamesbrown.aflmobile.model.OddsSearchResult
import com.jamesbrown.aflmobile.model.WeatherSummary
import com.jamesbrown.aflmobile.ui.common.formatLineValue
import com.jamesbrown.aflmobile.ui.common.selectionTypeLabel
import com.jamesbrown.aflmobile.ui.common.shortAflMatchLabel
import com.jamesbrown.aflmobile.ui.navigation.PlayerLaunchRequest
import java.util.Locale


/** A grouped "board" of selections for one player+market (tile mode). */
data class CandidateBoardGroup(
    val key: String,
    val title: String,
    val subtitle: String,
    val playerPosition: String?,
    val matchupDifficulty: String?,
    val weather: WeatherSummary?,
    val columns: List<CandidateLineColumn>,
)

data class CandidateLineColumn(
    val key: String,
    val label: String,
    val slots: List<CandidateSelectionSlot>,
)

data class CandidateSelectionSlot(
    val selection: OddsSearchResult,
)

const val AllMarketCode = "__all__"

private val preferredMarketOrder = listOf(
    "player_disposals",
    "player_fantasy_points",
    "player_goals",
    "player_marks",
    "player_tackles",
    "player_kicks",
    "player_handballs",
    "player_hitouts",
    "player_clearances",
    "total_points",
    "line",
    "h2h",
)

fun orderedMarketCodes(legs: List<OddsSearchResult>): List<String> {
    val orderIndex = preferredMarketOrder.withIndex().associate { it.value to it.index }
    return legs.map { it.marketTypeCode }
        .distinct()
        .sortedWith(compareBy({ orderIndex[it] ?: Int.MAX_VALUE }, { marketDisplayLabel(it) }))
}

fun marketSectionTitle(marketCode: String?): String =
    marketCode?.let {
        if (it == AllMarketCode) {
            "All player props"
        } else {
            "${marketDisplayLabel(it)} options"
        }
    } ?: "Available legs"

fun marketDisplayLabel(marketTypeCode: String): String =
    when (marketTypeCode) {
        AllMarketCode -> "All"
        "player_disposals" -> "Disposals"
        "player_fantasy_points" -> "Fantasy"
        "player_goals" -> "Goals"
        "player_marks" -> "Marks"
        "player_tackles" -> "Tackles"
        "player_kicks" -> "Kicks"
        "player_handballs" -> "Handballs"
        "player_hitouts" -> "Hitouts"
        "player_clearances" -> "Clearances"
        "total_points" -> "Totals"
        "line" -> "Line"
        "h2h" -> "H2H"
        else -> marketTypeCode.replace("_", " ").replaceFirstChar {
            if (it.isLowerCase()) it.titlecase(Locale.getDefault()) else it.toString()
        }
    }

fun buildCandidateBoard(legs: List<OddsSearchResult>): List<CandidateBoardGroup> =
    legs.groupBy { boardGroupKey(it) }
        .values
        .map { selections ->
            val first = selections.first()
            CandidateBoardGroup(
                key = boardGroupKey(first),
                title = first.player?.fullName ?: marketDisplayLabel(first.marketTypeCode),
                subtitle = buildBoardSubtitle(first),
                playerPosition = first.playerPosition,
                matchupDifficulty = first.matchupDifficulty,
                weather = first.weather,
                columns = buildLineColumns(selections, first.marketTypeCode),
            )
        }
        .sortedWith(
            compareBy<CandidateBoardGroup> { boardGroupSortBucket(it.key) }
                .thenByDescending { group ->
                    group.columns.maxOfOrNull { column ->
                        column.slots.maxOfOrNull { slot -> slot.selection.nextBestProbDiff ?: Double.NEGATIVE_INFINITY }
                            ?: Double.NEGATIVE_INFINITY
                    } ?: Double.NEGATIVE_INFINITY
                }
                .thenBy { it.title },
        )

private fun boardGroupKey(selection: OddsSearchResult): String =
    if (selection.player != null) {
        "${selection.marketTypeCode}|player|${selection.player.id}"
    } else {
        "${selection.marketTypeCode}|match"
    }

private fun boardGroupSortBucket(key: String): Int =
    if ("|player|" in key) 0 else 1

private fun buildBoardSubtitle(selection: OddsSearchResult): String =
    if (selection.player != null) {
        "${marketDisplayLabel(selection.marketTypeCode)} • ${shortAflMatchLabel(selection.matchName)}"
    } else {
        shortAflMatchLabel(selection.matchName)
    }

fun buildRowSubtitle(selection: OddsSearchResult): String =
    "${marketDisplayLabel(selection.marketTypeCode)} • ${shortAflMatchLabel(selection.matchName)}"

private fun buildLineColumns(
    selections: List<OddsSearchResult>,
    marketTypeCode: String,
): List<CandidateLineColumn> =
    selections.groupBy { lineColumnKey(it) }
        .values
        .sortedWith(compareBy({ lineColumnSortValue(it.first()) }, { lineColumnLabel(it.first(), marketTypeCode) }))
        .map { columnSelections ->
            val first = columnSelections.first()
            CandidateLineColumn(
                key = lineColumnKey(first),
                label = lineColumnLabel(first, marketTypeCode),
                slots = columnSelections
                    .sortedBy { selectionSlotSortOrder(it.selectionType) }
                    .map { CandidateSelectionSlot(selection = it) },
            )
        }

private fun lineColumnKey(selection: OddsSearchResult): String =
    when (selection.marketTypeCode) {
        "h2h" -> "win"
        "line" -> selection.marketId.toString()
        else -> selection.lineValue?.toString() ?: selection.marketId.toString()
    }

private fun lineColumnLabel(selection: OddsSearchResult, marketTypeCode: String): String =
    when (marketTypeCode) {
        "h2h" -> "Win"
        "line" -> selection.lineValue?.let(::formatLineValue)?.let { "$it line" } ?: "Line"
        "total_points" -> selection.lineValue?.let(::formatLineValue)?.let { "$it pts" } ?: "Points"
        else -> selection.lineValue?.let(::formatLineValue) ?: "Line"
    }

private fun lineColumnSortValue(selection: OddsSearchResult): Double =
    selection.lineValue ?: Double.MAX_VALUE

private fun selectionSlotSortOrder(selectionType: String): Int =
    when (selectionType) {
        "over", "home" -> 0
        "under", "away" -> 1
        else -> 99
    }

fun sortCandidateRows(
    legs: List<OddsSearchResult>,
    sortField: BuilderSortField,
    descending: Boolean,
): List<OddsSearchResult> =
    legs.sortedWith { left, right ->
        val primaryResult = when (sortField) {
            BuilderSortField.PLAYER -> compareValues(left.player?.fullName ?: left.label, right.player?.fullName ?: right.label)
            BuilderSortField.LINE -> compareValues(left.lineValue ?: Double.MAX_VALUE, right.lineValue ?: Double.MAX_VALUE)
            BuilderSortField.NEXT_BEST -> compareValues(left.nextBestProbDiff ?: Double.NEGATIVE_INFINITY, right.nextBestProbDiff ?: Double.NEGATIVE_INFINITY)
            BuilderSortField.PRICE -> compareValues(left.decimalPrice ?: Double.NEGATIVE_INFINITY, right.decimalPrice ?: Double.NEGATIVE_INFINITY)
            BuilderSortField.DIFF_LAST_10 -> compareValues(left.diffLast10 ?: Double.NEGATIVE_INFINITY, right.diffLast10 ?: Double.NEGATIVE_INFINITY)
            BuilderSortField.DIFF_2025 -> compareValues(left.diff2025 ?: Double.NEGATIVE_INFINITY, right.diff2025 ?: Double.NEGATIVE_INFINITY)
        }
        val signedResult = if (descending) -primaryResult else primaryResult
        if (signedResult != 0) {
            signedResult
        } else {
            val byPlayer = compareValues(left.player?.fullName ?: left.label, right.player?.fullName ?: right.label)
            if (byPlayer != 0) {
                byPlayer
            } else {
                val byLine = compareValues(left.lineValue ?: Double.MAX_VALUE, right.lineValue ?: Double.MAX_VALUE)
                if (byLine != 0) {
                    byLine
                } else {
                    compareValues(selectionTypeLabel(left.selectionType), selectionTypeLabel(right.selectionType))
                }
            }
        }
    }

fun defaultSortDirectionForField(field: BuilderSortField): Boolean =
    when (field) {
        BuilderSortField.PLAYER,
        BuilderSortField.LINE -> false
        BuilderSortField.NEXT_BEST,
        BuilderSortField.PRICE,
        BuilderSortField.DIFF_LAST_10,
        BuilderSortField.DIFF_2025 -> true
    }

fun compactTileLabel(columnLabel: String, selectionType: String): String =
    when (selectionType) {
        "over" -> "$columnLabel+"
        "under" -> "$columnLabel-"
        else -> columnLabel
    }

/** Compact line+side cell content, e.g. "O 19.5" / "U 19.5" / "Home". */
fun lineWithSideLabel(selection: OddsSearchResult): String {
    val line = selection.lineValue?.let(::formatLineValue)
    return when (selection.selectionType) {
        "over" -> line?.let { "O $it" } ?: "Over"
        "under" -> line?.let { "U $it" } ?: "Under"
        else -> line ?: selectionTypeLabel(selection.selectionType)
    }
}

fun OddsSearchResult.toPlayerLaunchRequest(): PlayerLaunchRequest? {
    val playerSummary = player ?: return null
    return PlayerLaunchRequest(
        requestId = System.nanoTime(),
        playerId = playerSummary.id,
        playerName = playerSummary.fullName,
        marketTypeCode = marketTypeCode,
        lineValue = lineValue,
    )
}

fun CandidateBoardGroup.toPlayerLaunchRequest(): PlayerLaunchRequest? =
    columns.asSequence()
        .flatMap { column -> column.slots.asSequence() }
        .map { slot -> slot.selection }
        .mapNotNull { selection -> selection.toPlayerLaunchRequest() }
        .firstOrNull()
