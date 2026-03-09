package com.jamesbrown.aflmobile

import com.jamesbrown.aflmobile.data.repository.SgmDraftStore
import com.jamesbrown.aflmobile.model.DraftLeg
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test


class SgmDraftStoreTest {
    @Test
    fun addLeg_rejectsDifferentEvent() {
        val store = SgmDraftStore()
        val firstResult = store.addLeg(
            DraftLeg(
                selectionId = 1,
                eventId = 100,
                eventLabel = "Event A",
                bookmaker = "sportsbet",
                label = "Player A Over 20.5",
                marketTypeCode = "player_disposals",
                selectionType = "over",
                basePrice = 1.9,
            ),
        )
        val secondResult = store.addLeg(
            DraftLeg(
                selectionId = 2,
                eventId = 200,
                eventLabel = "Event B",
                bookmaker = "sportsbet",
                label = "Player B Over 15.5",
                marketTypeCode = "player_disposals",
                selectionType = "over",
                basePrice = 1.8,
            ),
        )

        assertTrue(firstResult.applied)
        assertFalse(secondResult.applied)
        assertEquals(1, store.state.value.legs.size)
    }
}
