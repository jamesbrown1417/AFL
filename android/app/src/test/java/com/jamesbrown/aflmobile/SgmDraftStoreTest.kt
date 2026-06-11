package com.jamesbrown.aflmobile

import com.jamesbrown.aflmobile.data.repository.DataStoreSgmDraftPersistence
import com.jamesbrown.aflmobile.data.repository.PersistedSgmDraft
import com.jamesbrown.aflmobile.data.repository.SgmDraftStore
import com.jamesbrown.aflmobile.model.DraftLeg
import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test


class SgmDraftStoreTest {

    private fun leg(
        selectionId: Int,
        eventId: Int = 100,
        bookmaker: String = "sportsbet",
    ) = DraftLeg(
        selectionId = selectionId,
        eventId = eventId,
        eventLabel = "Event $eventId",
        bookmaker = bookmaker,
        label = "Player $selectionId Over 20.5",
        marketTypeCode = "player_disposals",
        selectionType = "over",
        basePrice = 1.9,
    )

    @Test
    fun addLeg_rejectsDifferentEvent() {
        val store = SgmDraftStore()
        val firstResult = store.addLeg(leg(selectionId = 1, eventId = 100))
        val secondResult = store.addLeg(leg(selectionId = 2, eventId = 200))

        assertTrue(firstResult.applied)
        assertFalse(secondResult.applied)
        assertEquals(1, store.state.value.legs.size)
    }

    @Test
    fun addLeg_rejectsDuplicateSelection() {
        val store = SgmDraftStore()
        store.addLeg(leg(selectionId = 1))
        val duplicate = store.addLeg(leg(selectionId = 1))

        assertFalse(duplicate.applied)
        assertEquals(1, store.state.value.legs.size)
    }

    @Test
    fun removeLastLeg_resetsDraftEntirely() {
        val store = SgmDraftStore()
        store.addLeg(leg(selectionId = 1))
        store.removeLeg(1)

        assertEquals(null, store.state.value.bookmaker)
        assertEquals(null, store.state.value.eventId)
        assertTrue(store.state.value.legs.isEmpty())
    }

    @Test
    fun persistence_roundTripsDraftJson() = runBlocking {
        var stored: String? = null
        val persistence = DataStoreSgmDraftPersistence(
            saveJson = { stored = it },
            loadJson = { stored },
        )
        val draft = PersistedSgmDraft(
            bookmaker = "sportsbet",
            eventId = 100,
            eventLabel = "Event 100",
            forceRefresh = true,
            legs = listOf(leg(selectionId = 7)),
        )
        persistence.save(draft)
        assertEquals(draft, persistence.load())
    }

    @Test
    fun persistence_ignoresCorruptJson() = runBlocking {
        val persistence = DataStoreSgmDraftPersistence(
            saveJson = { },
            loadJson = { "{not valid json" },
        )
        assertEquals(null, persistence.load())
    }
}
