package com.jamesbrown.aflmobile

import com.jamesbrown.aflmobile.model.BuilderSortField
import com.jamesbrown.aflmobile.model.OddsSearchResult
import com.jamesbrown.aflmobile.model.PlayerSummary
import com.jamesbrown.aflmobile.ui.common.builder.lineWithSideLabel
import com.jamesbrown.aflmobile.ui.common.builder.orderedMarketCodes
import com.jamesbrown.aflmobile.ui.common.builder.sortCandidateRows
import org.junit.Assert.assertEquals
import org.junit.Test


class BuilderLogicTest {

    private fun selection(
        selectionId: Int,
        playerName: String,
        marketTypeCode: String = "player_disposals",
        selectionType: String = "over",
        lineValue: Double? = 19.5,
        price: Double? = 1.9,
        diffLast10: Double? = null,
        nextBest: Double? = null,
    ) = OddsSearchResult(
        selectionId = selectionId,
        marketId = selectionId,
        eventId = 1,
        matchName = "Collingwood v Carlton",
        bookmaker = "sportsbet",
        marketTypeCode = marketTypeCode,
        marketDisplayName = "Disposals",
        player = PlayerSummary(id = selectionId, fullName = playerName),
        selectionType = selectionType,
        label = "$playerName Over $lineValue",
        lineValue = lineValue,
        decimalPrice = price,
        diffLast10 = diffLast10,
        nextBestProbDiff = nextBest,
        sgmEligible = true,
    )

    @Test
    fun sortCandidateRows_sortsByNextBestDescendingWithNullsLast() {
        val rows = listOf(
            selection(1, "Alpha", nextBest = 0.02),
            selection(2, "Bravo", nextBest = null),
            selection(3, "Charlie", nextBest = 0.10),
        )
        val sorted = sortCandidateRows(rows, BuilderSortField.NEXT_BEST, descending = true)
        assertEquals(listOf(3, 1, 2), sorted.map { it.selectionId })
    }

    @Test
    fun sortCandidateRows_playerSortIsAlphabetical() {
        val rows = listOf(
            selection(1, "Zorko"),
            selection(2, "Anderson"),
            selection(3, "Miller"),
        )
        val sorted = sortCandidateRows(rows, BuilderSortField.PLAYER, descending = false)
        assertEquals(listOf("Anderson", "Miller", "Zorko"), sorted.map { it.player?.fullName })
    }

    @Test
    fun orderedMarketCodes_prefersKnownOrderingThenAlpha() {
        val rows = listOf(
            selection(1, "A", marketTypeCode = "player_goals"),
            selection(2, "B", marketTypeCode = "player_disposals"),
            selection(3, "C", marketTypeCode = "player_unknown_stat"),
        )
        assertEquals(
            listOf("player_disposals", "player_goals", "player_unknown_stat"),
            orderedMarketCodes(rows),
        )
    }

    @Test
    fun lineWithSideLabel_encodesSideCompactly() {
        assertEquals("O 19.5", lineWithSideLabel(selection(1, "A", selectionType = "over")))
        assertEquals("U 19.5", lineWithSideLabel(selection(1, "A", selectionType = "under")))
        assertEquals("Home", lineWithSideLabel(selection(1, "A", selectionType = "home", lineValue = null)))
    }
}
