package com.jamesbrown.aflmobile

import com.jamesbrown.aflmobile.model.OddsFilters
import com.jamesbrown.aflmobile.model.PlayerSummary
import com.jamesbrown.aflmobile.model.QuickFilterPreset
import com.jamesbrown.aflmobile.model.applyQuickFilterPreset
import com.jamesbrown.aflmobile.model.hasActiveFilters
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test


class OddsFiltersTest {
    private val defaults = listOf("sportsbet", "tab")

    @Test
    fun hasActiveFilters_falseForDefaultState() {
        val filters = OddsFilters(bookmakerCodes = defaults)
        assertFalse(filters.hasActiveFilters(defaults))
    }

    @Test
    fun hasActiveFilters_trueWhenBookmakersDiffer() {
        val filters = OddsFilters(bookmakerCodes = listOf("sportsbet"))
        assertTrue(filters.hasActiveFilters(defaults))
    }

    @Test
    fun hasActiveFilters_trueForPlayerIncludes() {
        val filters = OddsFilters(
            bookmakerCodes = defaults,
            includePlayers = listOf(PlayerSummary(1, "Test Player")),
        )
        assertTrue(filters.hasActiveFilters(defaults))
    }

    @Test
    fun quickFilterPreset_setsLast10FloorAndMatchups() {
        val filters = OddsFilters(bookmakerCodes = defaults)
            .applyQuickFilterPreset(QuickFilterPreset.LAST10_NB_AND_FAVORABLE_MATCHUP)
        assertEquals(0f, filters.minDiffLast10)
        assertEquals(0f, filters.minNextBestProbDiff)
        assertEquals(listOf("Neutral", "Good", "Excellent"), filters.matchupDifficulties)
        assertTrue(filters.hasActiveFilters(defaults))
    }
}
