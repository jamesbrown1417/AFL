package com.jamesbrown.aflmobile.ui.screens.events

import androidx.compose.foundation.clickable
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
import androidx.compose.material.icons.automirrored.outlined.ArrowForward
import androidx.compose.material.icons.outlined.Refresh
import androidx.compose.material3.Card
import androidx.compose.material3.DropdownMenu
import androidx.compose.material3.DropdownMenuItem
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.ExposedDropdownMenuAnchorType
import androidx.compose.material3.ExposedDropdownMenuBox
import androidx.compose.material3.ExposedDropdownMenuDefaults
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.LargeTopAppBar
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Scaffold
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import androidx.lifecycle.ViewModel
import androidx.lifecycle.compose.collectAsStateWithLifecycle
import androidx.lifecycle.viewModelScope
import androidx.lifecycle.viewmodel.compose.viewModel
import com.jamesbrown.aflmobile.data.repository.AflRepository
import com.jamesbrown.aflmobile.model.BookmakerSummary
import com.jamesbrown.aflmobile.model.EventSummary
import com.jamesbrown.aflmobile.model.MarketSummary
import com.jamesbrown.aflmobile.ui.common.EmptyCard
import com.jamesbrown.aflmobile.ui.common.ErrorCard
import com.jamesbrown.aflmobile.ui.common.InlineChip
import com.jamesbrown.aflmobile.ui.common.LoadingCard
import com.jamesbrown.aflmobile.ui.common.ScreenPadding
import com.jamesbrown.aflmobile.ui.common.formatDateTime
import com.jamesbrown.aflmobile.ui.common.simpleViewModelFactory
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.flow.update
import kotlinx.coroutines.launch


data class EventDetailUiState(
    val event: EventSummary? = null,
    val markets: List<MarketSummary> = emptyList(),
    val bookmakers: List<BookmakerSummary> = emptyList(),
    val selectedBookmaker: String = "sportsbet",
    val playerQuery: String = "",
    val isLoading: Boolean = true,
    val errorMessage: String? = null,
)

class EventDetailViewModel(
    private val repository: AflRepository,
    private val eventId: Int,
    initialBookmaker: String,
) : ViewModel() {
    private val _uiState = MutableStateFlow(EventDetailUiState(selectedBookmaker = initialBookmaker))
    val uiState: StateFlow<EventDetailUiState> = _uiState.asStateFlow()

    init {
        refresh()
    }

    fun onBookmakerSelected(bookmaker: String) {
        _uiState.update { it.copy(selectedBookmaker = bookmaker) }
        refresh()
    }

    fun onPlayerQueryChanged(value: String) {
        _uiState.update { it.copy(playerQuery = value) }
    }

    fun refresh() {
        viewModelScope.launch {
            _uiState.update { it.copy(isLoading = true, errorMessage = null) }
            runCatching {
                Triple(
                    repository.event(eventId),
                    repository.bookmakers(),
                    repository.markets(
                        eventId = eventId,
                        bookmaker = uiState.value.selectedBookmaker,
                        playerQuery = uiState.value.playerQuery.takeIf { it.isNotBlank() },
                    ),
                )
            }.onSuccess { (event, bookmakers, markets) ->
                _uiState.update {
                    it.copy(
                        event = event,
                        bookmakers = bookmakers,
                        markets = markets,
                        isLoading = false,
                    )
                }
            }.onFailure { error ->
                _uiState.update {
                    it.copy(
                        isLoading = false,
                        errorMessage = error.message ?: "Failed to load event markets.",
                    )
                }
            }
        }
    }
}

@Composable
fun EventDetailRoute(
    repository: AflRepository,
    eventId: Int,
    initialBookmaker: String,
    onBack: () -> Unit,
    onMarketSelected: (marketId: Int, eventId: Int, bookmaker: String, eventLabel: String) -> Unit,
) {
    val viewModel: EventDetailViewModel = viewModel(
        key = "event-$eventId-$initialBookmaker",
        factory = simpleViewModelFactory {
            EventDetailViewModel(repository, eventId, initialBookmaker)
        },
    )
    val uiState by viewModel.uiState.collectAsStateWithLifecycle()
    EventDetailScreen(
        uiState = uiState,
        onBack = onBack,
        onBookmakerSelected = viewModel::onBookmakerSelected,
        onPlayerQueryChanged = viewModel::onPlayerQueryChanged,
        onRefresh = viewModel::refresh,
        onMarketSelected = onMarketSelected,
    )
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun EventDetailScreen(
    uiState: EventDetailUiState,
    onBack: () -> Unit,
    onBookmakerSelected: (String) -> Unit,
    onPlayerQueryChanged: (String) -> Unit,
    onRefresh: () -> Unit,
    onMarketSelected: (marketId: Int, eventId: Int, bookmaker: String, eventLabel: String) -> Unit,
) {
    var bookmakerExpanded by mutableStateOf(false)

    Scaffold(
        topBar = {
            LargeTopAppBar(
                title = { Text(uiState.event?.matchName ?: "Event") },
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
            uiState.event?.let { event ->
                item {
                    Card {
                        Column(
                            modifier = Modifier.padding(16.dp),
                            verticalArrangement = Arrangement.spacedBy(8.dp),
                        ) {
                            Text(formatDateTime(event.startTime), style = MaterialTheme.typography.bodyLarge)
                            event.roundLabel?.let { Text(it) }
                            event.venue?.let { Text(it) }
                            Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                                event.availableBookmakers.take(5).forEach { InlineChip(label = it) }
                            }
                        }
                    }
                }
            }

            item {
                Card {
                    Column(
                        modifier = Modifier.padding(16.dp),
                        verticalArrangement = Arrangement.spacedBy(12.dp),
                    ) {
                        ExposedDropdownMenuBox(
                            expanded = bookmakerExpanded,
                            onExpandedChange = { bookmakerExpanded = !bookmakerExpanded },
                        ) {
                            OutlinedTextField(
                                value = uiState.selectedBookmaker,
                                onValueChange = {},
                                modifier = Modifier
                                    .menuAnchor(ExposedDropdownMenuAnchorType.PrimaryNotEditable)
                                    .fillMaxWidth(),
                                readOnly = true,
                                label = { Text("Bookmaker") },
                                trailingIcon = {
                                    ExposedDropdownMenuDefaults.TrailingIcon(expanded = bookmakerExpanded)
                                },
                            )
                            DropdownMenu(
                                expanded = bookmakerExpanded,
                                onDismissRequest = { bookmakerExpanded = false },
                            ) {
                                uiState.bookmakers.forEach { bookmaker ->
                                    DropdownMenuItem(
                                        text = { Text(bookmaker.displayName) },
                                        onClick = {
                                            onBookmakerSelected(bookmaker.code)
                                            bookmakerExpanded = false
                                        },
                                    )
                                }
                            }
                        }
                        OutlinedTextField(
                            value = uiState.playerQuery,
                            onValueChange = onPlayerQueryChanged,
                            modifier = Modifier.fillMaxWidth(),
                            singleLine = true,
                            label = { Text("Filter player markets") },
                        )
                    }
                }
            }

            if (uiState.isLoading) {
                item { LoadingCard("Loading event markets") }
            }

            uiState.errorMessage?.let { message ->
                item { ErrorCard(message) }
            }

            if (!uiState.isLoading && uiState.markets.isEmpty()) {
                item {
                    EmptyCard(
                        title = "No markets yet",
                        body = "This event does not currently have markets for ${uiState.selectedBookmaker}.",
                    )
                }
            }

            items(uiState.markets, key = { it.id }) { market ->
                MarketCard(
                    market = market,
                    onClick = {
                        onMarketSelected(
                            market.id,
                            market.eventId,
                            uiState.selectedBookmaker,
                            uiState.event?.matchName ?: "Event ${market.eventId}",
                        )
                    },
                )
            }
        }
    }
}

@Composable
private fun MarketCard(
    market: MarketSummary,
    onClick: () -> Unit,
) {
    Card(
        modifier = Modifier
            .fillMaxWidth()
            .clickable(onClick = onClick),
    ) {
        Row(
            modifier = Modifier.padding(16.dp),
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.SpaceBetween,
        ) {
            Column(
                modifier = Modifier.weight(1f),
                verticalArrangement = Arrangement.spacedBy(4.dp),
            ) {
                Text(
                    text = market.player?.fullName ?: market.displayName,
                    style = MaterialTheme.typography.titleMedium,
                )
                Text(
                    text = buildString {
                        append(market.displayName)
                        market.lineValue?.let { append(" • $it") }
                    },
                    style = MaterialTheme.typography.bodyMedium,
                )
                Text(
                    text = market.availableSelectionTypes.joinToString(" / "),
                    style = MaterialTheme.typography.labelMedium,
                )
            }
            Icon(Icons.AutoMirrored.Outlined.ArrowForward, contentDescription = null)
        }
    }
}
