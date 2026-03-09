package com.jamesbrown.aflmobile.ui.screens.events

import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.automirrored.outlined.ArrowBack
import androidx.compose.material.icons.outlined.Add
import androidx.compose.material.icons.outlined.Refresh
import androidx.compose.material3.Card
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.LargeTopAppBar
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Scaffold
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import androidx.lifecycle.ViewModel
import androidx.lifecycle.compose.collectAsStateWithLifecycle
import androidx.lifecycle.viewModelScope
import androidx.lifecycle.viewmodel.compose.viewModel
import com.jamesbrown.aflmobile.data.repository.SgmDraftStore
import com.jamesbrown.aflmobile.data.repository.AflRepository
import com.jamesbrown.aflmobile.model.DraftLeg
import com.jamesbrown.aflmobile.model.SelectionSummary
import com.jamesbrown.aflmobile.ui.common.EmptyCard
import com.jamesbrown.aflmobile.ui.common.ErrorCard
import com.jamesbrown.aflmobile.ui.common.LoadingCard
import com.jamesbrown.aflmobile.ui.common.ScreenPadding
import com.jamesbrown.aflmobile.ui.common.formatDecimalPrice
import com.jamesbrown.aflmobile.ui.common.simpleViewModelFactory
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.flow.update
import kotlinx.coroutines.launch


data class MarketSelectionsUiState(
    val selections: List<SelectionSummary> = emptyList(),
    val isLoading: Boolean = true,
    val errorMessage: String? = null,
    val infoMessage: String? = null,
)

class MarketSelectionsViewModel(
    private val repository: AflRepository,
    private val draftStore: SgmDraftStore,
    private val marketId: Int,
    private val eventId: Int,
    private val bookmaker: String,
    private val eventLabel: String,
) : ViewModel() {
    private val _uiState = MutableStateFlow(MarketSelectionsUiState())
    val uiState: StateFlow<MarketSelectionsUiState> = _uiState.asStateFlow()

    init {
        refresh()
    }

    fun refresh() {
        viewModelScope.launch {
            _uiState.update { it.copy(isLoading = true, errorMessage = null, infoMessage = null) }
            runCatching { repository.selections(marketId = marketId, bookmaker = bookmaker) }
                .onSuccess { selections ->
                    _uiState.update { it.copy(selections = selections, isLoading = false) }
                }
                .onFailure { error ->
                    _uiState.update {
                        it.copy(
                            isLoading = false,
                            errorMessage = error.message ?: "Failed to load selections.",
                        )
                    }
                }
        }
    }

    fun addToDraft(selection: SelectionSummary) {
        if (bookmaker != "sportsbet") {
            _uiState.update { it.copy(infoMessage = "Live SGM pricing is only enabled for Sportsbet in this prototype.") }
            return
        }
        val basePrice = selection.decimalPrice
        if (!selection.sgmEligible || basePrice == null) {
            _uiState.update { it.copy(infoMessage = "That selection is not ready for SGM pricing.") }
            return
        }
        val result = draftStore.addLeg(
            DraftLeg(
                selectionId = selection.id,
                eventId = eventId,
                eventLabel = eventLabel,
                bookmaker = bookmaker,
                label = selection.label,
                marketTypeCode = selection.selectionType,
                selectionType = selection.selectionType,
                basePrice = basePrice,
            ),
        )
        _uiState.update { it.copy(infoMessage = result.message) }
    }
}

@Composable
fun MarketSelectionsRoute(
    repository: AflRepository,
    draftStore: SgmDraftStore,
    marketId: Int,
    eventId: Int,
    bookmaker: String,
    eventLabel: String,
    onBack: () -> Unit,
) {
    val viewModel: MarketSelectionsViewModel = viewModel(
        key = "market-$marketId-$bookmaker",
        factory = simpleViewModelFactory {
            MarketSelectionsViewModel(
                repository = repository,
                draftStore = draftStore,
                marketId = marketId,
                eventId = eventId,
                bookmaker = bookmaker,
                eventLabel = eventLabel,
            )
        },
    )
    val uiState by viewModel.uiState.collectAsStateWithLifecycle()
    MarketSelectionsScreen(
        uiState = uiState,
        bookmaker = bookmaker,
        onBack = onBack,
        onRefresh = viewModel::refresh,
        onAddToDraft = viewModel::addToDraft,
    )
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun MarketSelectionsScreen(
    uiState: MarketSelectionsUiState,
    bookmaker: String,
    onBack: () -> Unit,
    onRefresh: () -> Unit,
    onAddToDraft: (SelectionSummary) -> Unit,
) {
    Scaffold(
        topBar = {
            LargeTopAppBar(
                title = { Text("Selections") },
                navigationIcon = {
                    IconButton(onClick = onBack) {
                        Icon(Icons.AutoMirrored.Outlined.ArrowBack, contentDescription = "Back")
                    }
                },
                actions = {
                    IconButton(onClick = onRefresh) {
                        Icon(Icons.Outlined.Refresh, contentDescription = "Refresh")
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
                        verticalArrangement = Arrangement.spacedBy(6.dp),
                    ) {
                        Text("Bookmaker: $bookmaker", style = MaterialTheme.typography.titleMedium)
                        Text("Add Sportsbet-eligible rows to the SGM builder from here.")
                    }
                }
            }

            if (uiState.isLoading) {
                item { LoadingCard("Loading selections") }
            }

            uiState.errorMessage?.let { message ->
                item { ErrorCard(message) }
            }

            uiState.infoMessage?.let { message ->
                item { EmptyCard("SGM builder", message) }
            }

            if (!uiState.isLoading && uiState.selections.isEmpty()) {
                item { EmptyCard("No selections", "This market has no current selection rows.") }
            }

            items(uiState.selections, key = { it.id }) { selection ->
                SelectionCard(selection = selection, onAddToDraft = onAddToDraft)
            }
        }
    }
}

@Composable
private fun SelectionCard(
    selection: SelectionSummary,
    onAddToDraft: (SelectionSummary) -> Unit,
) {
    Card(modifier = Modifier.fillMaxWidth()) {
        Column(
            modifier = Modifier.padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            Text(selection.label, style = MaterialTheme.typography.titleMedium)
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.SpaceBetween,
            ) {
                Text("Type: ${selection.selectionType}")
                Text("Price: ${formatDecimalPrice(selection.decimalPrice)}")
            }
            selection.edgePct?.let { Text("Edge: ${String.format("%.2f%%", it)}") }
            if (selection.sgmEligible && selection.decimalPrice != null) {
                TextButton(
                    onClick = { onAddToDraft(selection) },
                    modifier = Modifier.fillMaxWidth(),
                ) {
                    Icon(Icons.Outlined.Add, contentDescription = null)
                    Text("Add to SGM builder", modifier = Modifier.padding(start = 8.dp))
                }
            }
        }
    }
}
