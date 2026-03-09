package com.jamesbrown.aflmobile.data.repository

import com.jamesbrown.aflmobile.model.DraftLeg
import com.jamesbrown.aflmobile.model.DraftMutationResult
import com.jamesbrown.aflmobile.model.SgmDraftState
import com.jamesbrown.aflmobile.model.SgmQuoteResponse
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.flow.update


class SgmDraftStore {
    private val _state = MutableStateFlow(SgmDraftState())
    val state: StateFlow<SgmDraftState> = _state.asStateFlow()

    fun addLeg(leg: DraftLeg): DraftMutationResult {
        val current = _state.value
        if (current.legs.any { it.selectionId == leg.selectionId }) {
            return DraftMutationResult(applied = false, message = "Selection already added.")
        }
        if (current.eventId != null && current.eventId != leg.eventId) {
            return DraftMutationResult(
                applied = false,
                message = "An SGM draft can only contain one event at a time.",
            )
        }
        if (current.bookmaker != null && current.bookmaker != leg.bookmaker) {
            return DraftMutationResult(
                applied = false,
                message = "An SGM draft can only contain one bookmaker at a time.",
            )
        }
        _state.update {
            it.copy(
                bookmaker = leg.bookmaker,
                eventId = leg.eventId,
                eventLabel = leg.eventLabel,
                legs = it.legs + leg,
                latestQuote = null,
                latestError = null,
            )
        }
        return DraftMutationResult(applied = true, message = "Leg added to SGM builder.")
    }

    fun removeLeg(selectionId: Int) {
        _state.update { current ->
            val updatedLegs = current.legs.filterNot { it.selectionId == selectionId }
            if (updatedLegs.isEmpty()) {
                SgmDraftState()
            } else {
                current.copy(legs = updatedLegs, latestQuote = null, latestError = null)
            }
        }
    }

    fun clear() {
        _state.value = SgmDraftState()
    }

    fun setForceRefresh(forceRefresh: Boolean) {
        _state.update { it.copy(forceRefresh = forceRefresh) }
    }

    fun setQuote(quote: SgmQuoteResponse) {
        _state.update { it.copy(latestQuote = quote, latestError = null) }
    }

    fun setError(message: String?) {
        _state.update { it.copy(latestError = message) }
    }
}
