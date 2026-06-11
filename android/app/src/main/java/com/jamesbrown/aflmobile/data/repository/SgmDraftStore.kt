package com.jamesbrown.aflmobile.data.repository

import com.jamesbrown.aflmobile.model.DraftLeg
import com.jamesbrown.aflmobile.model.DraftMutationResult
import com.jamesbrown.aflmobile.model.SgmAgencyComparison
import com.jamesbrown.aflmobile.model.SgmDraftState
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.flow.update
import kotlinx.coroutines.launch
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json


/** The subset of draft state worth keeping across process death. */
@Serializable
data class PersistedSgmDraft(
    val bookmaker: String? = null,
    val eventId: Int? = null,
    val eventLabel: String? = null,
    val forceRefresh: Boolean = false,
    val legs: List<DraftLeg> = emptyList(),
)

interface SgmDraftPersistence {
    suspend fun load(): PersistedSgmDraft?
    suspend fun save(draft: PersistedSgmDraft)
}

class SgmDraftStore(
    private val persistence: SgmDraftPersistence? = null,
    private val scope: CoroutineScope? = null,
) {
    private val _state = MutableStateFlow(SgmDraftState())
    val state: StateFlow<SgmDraftState> = _state.asStateFlow()

    init {
        if (persistence != null && scope != null) {
            scope.launch {
                val restored = persistence.load() ?: return@launch
                _state.update { current ->
                    if (current.legs.isNotEmpty() || restored.legs.isEmpty()) {
                        current
                    } else {
                        SgmDraftState(
                            bookmaker = restored.bookmaker,
                            eventId = restored.eventId,
                            eventLabel = restored.eventLabel,
                            forceRefresh = restored.forceRefresh,
                            legs = restored.legs,
                        )
                    }
                }
            }
        }
    }

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
                latestComparisons = emptyList(),
                latestError = null,
            )
        }
        persist()
        return DraftMutationResult(applied = true, message = "Leg added to SGM builder.")
    }

    fun removeLeg(selectionId: Int) {
        _state.update { current ->
            val updatedLegs = current.legs.filterNot { it.selectionId == selectionId }
            if (updatedLegs.isEmpty()) {
                SgmDraftState()
            } else {
                current.copy(legs = updatedLegs, latestComparisons = emptyList(), latestError = null)
            }
        }
        persist()
    }

    fun clear() {
        _state.value = SgmDraftState()
        persist()
    }

    fun setForceRefresh(forceRefresh: Boolean) {
        _state.update { it.copy(forceRefresh = forceRefresh) }
        persist()
    }

    fun setComparisons(comparisons: List<SgmAgencyComparison>) {
        _state.update { it.copy(latestComparisons = comparisons, latestError = null) }
    }

    fun setError(message: String?) {
        _state.update { it.copy(latestError = message, latestComparisons = emptyList()) }
    }

    private fun persist() {
        val persistence = persistence ?: return
        val scope = scope ?: return
        val snapshot = _state.value
        scope.launch {
            persistence.save(
                PersistedSgmDraft(
                    bookmaker = snapshot.bookmaker,
                    eventId = snapshot.eventId,
                    eventLabel = snapshot.eventLabel,
                    forceRefresh = snapshot.forceRefresh,
                    legs = snapshot.legs,
                ),
            )
        }
    }
}

/** DataStore-backed persistence, JSON-encoded alongside the app settings. */
class DataStoreSgmDraftPersistence(
    private val saveJson: suspend (String) -> Unit,
    private val loadJson: suspend () -> String?,
    private val json: Json = Json { ignoreUnknownKeys = true },
) : SgmDraftPersistence {
    override suspend fun load(): PersistedSgmDraft? =
        loadJson()?.let { raw ->
            runCatching { json.decodeFromString<PersistedSgmDraft>(raw) }.getOrNull()
        }

    override suspend fun save(draft: PersistedSgmDraft) {
        saveJson(json.encodeToString(PersistedSgmDraft.serializer(), draft))
    }
}
