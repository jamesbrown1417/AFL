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


data class EventsUiState(
    val bookmakers: List<BookmakerSummary> = emptyList(),
    val selectedBookmaker: String = "sportsbet",
    val searchQuery: String = "",
    val events: List<EventSummary> = emptyList(),
    val isLoading: Boolean = true,
    val errorMessage: String? = null,
)

class EventsViewModel(
    private val repository: AflRepository,
) : ViewModel() {
    private val _uiState = MutableStateFlow(EventsUiState())
    val uiState: StateFlow<EventsUiState> = _uiState.asStateFlow()

    init {
        viewModelScope.launch {
            val settings = repository.currentSettings()
            val bookmakers = runCatching { repository.bookmakers() }.getOrDefault(emptyList())
            _uiState.update {
                it.copy(
                    selectedBookmaker = settings.defaultBookmaker,
                    bookmakers = bookmakers,
                )
            }
            refresh()
        }
    }

    fun onBookmakerSelected(bookmaker: String) {
        _uiState.update { it.copy(selectedBookmaker = bookmaker) }
        refresh()
    }

    fun onSearchQueryChanged(value: String) {
        _uiState.update { it.copy(searchQuery = value) }
    }

    fun refresh() {
        viewModelScope.launch {
            _uiState.update { it.copy(isLoading = true, errorMessage = null) }
            runCatching {
                repository.events(
                    bookmaker = uiState.value.selectedBookmaker,
                    query = uiState.value.searchQuery.takeIf { it.isNotBlank() },
                )
            }.onSuccess { events ->
                _uiState.update { it.copy(events = events, isLoading = false) }
            }.onFailure { error ->
                _uiState.update {
                    it.copy(
                        isLoading = false,
                        errorMessage = error.message ?: "Failed to load events.",
                    )
                }
            }
        }
    }
}

@Composable
fun EventsRoute(
    repository: AflRepository,
    onEventSelected: (eventId: Int, bookmaker: String) -> Unit,
) {
    val viewModel: EventsViewModel = viewModel(
        factory = simpleViewModelFactory { EventsViewModel(repository) },
    )
    val uiState by viewModel.uiState.collectAsStateWithLifecycle()
    EventsScreen(
        uiState = uiState,
        onBookmakerSelected = viewModel::onBookmakerSelected,
        onSearchQueryChanged = viewModel::onSearchQueryChanged,
        onRefresh = viewModel::refresh,
        onEventSelected = onEventSelected,
    )
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun EventsScreen(
    uiState: EventsUiState,
    onBookmakerSelected: (String) -> Unit,
    onSearchQueryChanged: (String) -> Unit,
    onRefresh: () -> Unit,
    onEventSelected: (eventId: Int, bookmaker: String) -> Unit,
) {
    var bookmakerExpanded by mutableStateOf(false)

    Scaffold(
        topBar = {
            LargeTopAppBar(
                title = { Text("Fixtures & markets") },
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
                            value = uiState.searchQuery,
                            onValueChange = onSearchQueryChanged,
                            modifier = Modifier.fillMaxWidth(),
                            singleLine = true,
                            label = { Text("Search event") },
                        )
                    }
                }
            }

            if (uiState.isLoading) {
                item { LoadingCard("Loading events") }
            }

            uiState.errorMessage?.let { message ->
                item { ErrorCard(message) }
            }

            if (!uiState.isLoading && uiState.events.isEmpty()) {
                item {
                    EmptyCard(
                        title = "No events",
                        body = "Try a different bookmaker or search term.",
                    )
                }
            }

            items(uiState.events, key = { it.id }) { event ->
                EventCard(
                    event = event,
                    onClick = { onEventSelected(event.id, uiState.selectedBookmaker) },
                )
            }
        }
    }
}

@Composable
private fun EventCard(
    event: EventSummary,
    onClick: () -> Unit,
) {
    Card(
        modifier = Modifier
            .fillMaxWidth()
            .clickable(onClick = onClick),
    ) {
        Column(
            modifier = Modifier.padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.SpaceBetween,
                verticalAlignment = Alignment.CenterVertically,
            ) {
                Column(modifier = Modifier.weight(1f), verticalArrangement = Arrangement.spacedBy(4.dp)) {
                    Text(event.matchName, style = MaterialTheme.typography.titleMedium)
                    Text(formatDateTime(event.startTime), style = MaterialTheme.typography.bodyMedium)
                    event.roundLabel?.let { Text(it, style = MaterialTheme.typography.labelMedium) }
                    event.venue?.let { Text(it, style = MaterialTheme.typography.bodySmall) }
                }
                Icon(Icons.AutoMirrored.Outlined.ArrowForward, contentDescription = null)
            }
            Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                event.availableBookmakers.take(4).forEach { bookmaker ->
                    InlineChip(label = bookmaker)
                }
            }
        }
    }
}
