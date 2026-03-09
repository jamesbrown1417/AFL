package com.jamesbrown.aflmobile.ui.screens.sgm

import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Delete
import androidx.compose.material.icons.outlined.Refresh
import androidx.compose.material3.Button
import androidx.compose.material3.Card
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Scaffold
import androidx.compose.material3.Switch
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.material3.TopAppBar
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import androidx.lifecycle.ViewModel
import androidx.lifecycle.compose.collectAsStateWithLifecycle
import androidx.lifecycle.viewModelScope
import androidx.lifecycle.viewmodel.compose.viewModel
import com.jamesbrown.aflmobile.data.repository.AflRepository
import com.jamesbrown.aflmobile.data.repository.SgmDraftStore
import com.jamesbrown.aflmobile.model.DraftLeg
import com.jamesbrown.aflmobile.model.SgmDraftState
import com.jamesbrown.aflmobile.ui.common.EmptyCard
import com.jamesbrown.aflmobile.ui.common.ErrorCard
import com.jamesbrown.aflmobile.ui.common.LoadingCard
import com.jamesbrown.aflmobile.ui.common.ScreenPadding
import com.jamesbrown.aflmobile.ui.common.formatDateTime
import com.jamesbrown.aflmobile.ui.common.formatDecimalPrice
import com.jamesbrown.aflmobile.ui.common.simpleViewModelFactory
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.flow.update
import kotlinx.coroutines.launch


data class SgmBuilderUiState(
    val draft: SgmDraftState = SgmDraftState(),
    val isLoading: Boolean = false,
    val errorMessage: String? = null,
    val infoMessage: String? = null,
)

class SgmBuilderViewModel(
    private val repository: AflRepository,
    private val draftStore: SgmDraftStore,
) : ViewModel() {
    private val _uiState = MutableStateFlow(SgmBuilderUiState())
    val uiState: StateFlow<SgmBuilderUiState> = _uiState.asStateFlow()

    init {
        viewModelScope.launch {
            draftStore.state.collect { draft ->
                _uiState.update { it.copy(draft = draft) }
            }
        }
    }

    fun removeLeg(selectionId: Int) {
        draftStore.removeLeg(selectionId)
    }

    fun clearDraft() {
        draftStore.clear()
    }

    fun setForceRefresh(forceRefresh: Boolean) {
        draftStore.setForceRefresh(forceRefresh)
    }

    fun quote() {
        val draft = uiState.value.draft
        val eventId = draft.eventId
        val bookmaker = draft.bookmaker
        if (eventId == null || bookmaker == null || draft.legs.size < 2) {
            _uiState.update { it.copy(errorMessage = "Add at least two legs from one Sportsbet event before pricing.") }
            return
        }
        viewModelScope.launch {
            _uiState.update { it.copy(isLoading = true, errorMessage = null, infoMessage = null) }
            runCatching {
                repository.quoteSgm(
                    bookmaker = bookmaker,
                    eventId = eventId,
                    selectionIds = draft.legs.map { it.selectionId },
                    forceRefresh = draft.forceRefresh,
                )
            }.onSuccess { quote ->
                draftStore.setQuote(quote)
                _uiState.update { it.copy(isLoading = false, infoMessage = "Quote updated.") }
            }.onFailure { error ->
                draftStore.setError(error.message)
                _uiState.update { it.copy(isLoading = false, errorMessage = error.message ?: "Quote failed.") }
            }
        }
    }
}

@Composable
fun SgmBuilderRoute(
    repository: AflRepository,
    draftStore: SgmDraftStore,
) {
    val viewModel: SgmBuilderViewModel = viewModel(
        factory = simpleViewModelFactory { SgmBuilderViewModel(repository, draftStore) },
    )
    val uiState by viewModel.uiState.collectAsStateWithLifecycle()
    SgmBuilderScreen(
        uiState = uiState,
        onRemoveLeg = viewModel::removeLeg,
        onClearDraft = viewModel::clearDraft,
        onForceRefreshChanged = viewModel::setForceRefresh,
        onQuote = viewModel::quote,
    )
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun SgmBuilderScreen(
    uiState: SgmBuilderUiState,
    onRemoveLeg: (Int) -> Unit,
    onClearDraft: () -> Unit,
    onForceRefreshChanged: (Boolean) -> Unit,
    onQuote: () -> Unit,
) {
    val draft = uiState.draft
    Scaffold(
        topBar = {
            TopAppBar(
                title = { Text("SGM builder") },
                actions = {
                    if (draft.legs.isNotEmpty()) {
                        IconButton(onClick = onClearDraft) {
                            Icon(Icons.Outlined.Delete, contentDescription = "Clear")
                        }
                    }
                },
            )
        },
    ) { innerPadding ->
        LazyColumn(
            modifier = Modifier
                .fillMaxSize()
                .padding(innerPadding),
            contentPadding = ScreenPadding,
            verticalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            item {
                Card {
                    Column(
                        modifier = Modifier.padding(16.dp),
                        verticalArrangement = Arrangement.spacedBy(8.dp),
                    ) {
                        Text("Live quote target", style = MaterialTheme.typography.titleMedium)
                        Text(draft.eventLabel ?: "No event selected")
                        Text("Bookmaker: ${draft.bookmaker ?: "Not set"}")
                        Row(
                            modifier = Modifier.fillMaxWidth(),
                            horizontalArrangement = Arrangement.SpaceBetween,
                            verticalAlignment = Alignment.CenterVertically,
                        ) {
                            Text("Force refresh")
                            Switch(
                                checked = draft.forceRefresh,
                                onCheckedChange = onForceRefreshChanged,
                            )
                        }
                        Button(
                            onClick = onQuote,
                            modifier = Modifier.fillMaxWidth(),
                            enabled = draft.legs.size >= 2 && !uiState.isLoading,
                        ) {
                            Icon(Icons.Outlined.Refresh, contentDescription = null)
                            Text("Request SGM quote", modifier = Modifier.padding(start = 8.dp))
                        }
                    }
                }
            }

            if (uiState.isLoading) {
                item { LoadingCard("Pricing current SGM draft") }
            }

            uiState.errorMessage?.let { message ->
                item { ErrorCard(message) }
            }

            uiState.infoMessage?.let { message ->
                item { EmptyCard("Quote status", message) }
            }

            if (draft.legs.isEmpty()) {
                item {
                    EmptyCard(
                        title = "No legs yet",
                        body = "Add eligible Sportsbet selections from the event or prop screens.",
                    )
                }
            }

            items(draft.legs, key = { it.selectionId }) { leg ->
                DraftLegCard(leg = leg, onRemove = onRemoveLeg)
            }

            draft.latestQuote?.let { quote ->
                item {
                    Card {
                        Column(
                            modifier = Modifier.padding(16.dp),
                            verticalArrangement = Arrangement.spacedBy(8.dp),
                        ) {
                            Text("Latest quote", style = MaterialTheme.typography.titleMedium)
                            Text("Quoted price: ${formatDecimalPrice(quote.quotedPrice)}")
                            Text("Base price: ${formatDecimalPrice(quote.unadjustedPrice)}")
                            Text("Adjustment factor: ${formatDecimalPrice(quote.adjustmentFactor)}")
                            Text("Cached: ${if (quote.fromCache) "yes" else "no"}")
                            Text("Quoted at: ${formatDateTime(quote.quotedAt)}")
                            Text("Expires: ${formatDateTime(quote.expiresAt)}")
                        }
                    }
                }
            }
        }
    }
}

@Composable
private fun DraftLegCard(
    leg: DraftLeg,
    onRemove: (Int) -> Unit,
) {
    Card(modifier = Modifier.fillMaxWidth()) {
        Column(
            modifier = Modifier.padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            Text(leg.label, style = MaterialTheme.typography.titleMedium)
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.SpaceBetween,
            ) {
                Text(leg.selectionType)
                Text(formatDecimalPrice(leg.basePrice))
            }
            TextButton(onClick = { onRemove(leg.selectionId) }) {
                Icon(Icons.Outlined.Delete, contentDescription = null)
                Text("Remove leg", modifier = Modifier.padding(start = 8.dp))
            }
        }
    }
}
