package com.jamesbrown.aflmobile.ui.screens.props

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.ExperimentalLayoutApi
import androidx.compose.foundation.layout.FlowRow
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.heightIn
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Add
import androidx.compose.material.icons.outlined.FilterList
import androidx.compose.material.icons.outlined.Refresh
import androidx.compose.material3.Card
import androidx.compose.material3.DropdownMenu
import androidx.compose.material3.DropdownMenuItem
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.ExposedDropdownMenuAnchorType
import androidx.compose.material3.ExposedDropdownMenuBox
import androidx.compose.material3.ExposedDropdownMenuDefaults
import androidx.compose.material3.FilterChip
import androidx.compose.material3.FilledTonalButton
import androidx.compose.material3.HorizontalDivider
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.ModalBottomSheet
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Scaffold
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.material3.TopAppBar
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import androidx.lifecycle.ViewModel
import androidx.lifecycle.compose.collectAsStateWithLifecycle
import androidx.lifecycle.viewModelScope
import androidx.lifecycle.viewmodel.compose.viewModel
import com.jamesbrown.aflmobile.data.repository.AflRepository
import com.jamesbrown.aflmobile.data.repository.SgmDraftStore
import com.jamesbrown.aflmobile.model.BookmakerSummary
import com.jamesbrown.aflmobile.model.DraftLeg
import com.jamesbrown.aflmobile.model.EventSummary
import com.jamesbrown.aflmobile.model.OddsFilters
import com.jamesbrown.aflmobile.model.OddsSearchResult
import com.jamesbrown.aflmobile.ui.common.EmptyCard
import com.jamesbrown.aflmobile.ui.common.ErrorCard
import com.jamesbrown.aflmobile.ui.common.InlineChip
import com.jamesbrown.aflmobile.ui.common.LoadingCard
import com.jamesbrown.aflmobile.ui.common.ScreenPadding
import com.jamesbrown.aflmobile.ui.common.formatDecimalPrice
import com.jamesbrown.aflmobile.ui.common.simpleViewModelFactory
import java.util.Locale
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.flow.update
import kotlinx.coroutines.launch


private data class FilterOption(
    val code: String?,
    val label: String,
)

private val marketOptions = listOf(
    FilterOption(code = null, label = "All props"),
    FilterOption(code = "player_disposals", label = "Disposals"),
    FilterOption(code = "player_fantasy_points", label = "Fantasy"),
    FilterOption(code = "player_goals", label = "Goals"),
    FilterOption(code = "player_marks", label = "Marks"),
    FilterOption(code = "player_tackles", label = "Tackles"),
    FilterOption(code = "player_kicks", label = "Kicks"),
    FilterOption(code = "player_handballs", label = "Handballs"),
    FilterOption(code = "player_hitouts", label = "Hitouts"),
    FilterOption(code = "player_clearances", label = "Clearances"),
)

data class OddsUiState(
    val bookmakers: List<BookmakerSummary> = emptyList(),
    val events: List<EventSummary> = emptyList(),
    val filters: OddsFilters = OddsFilters(),
    val defaultBookmakerCodes: List<String> = emptyList(),
    val odds: List<OddsSearchResult> = emptyList(),
    val isLoading: Boolean = true,
    val errorMessage: String? = null,
    val infoMessage: String? = null,
)

class OddsViewModel(
    private val repository: AflRepository,
    private val draftStore: SgmDraftStore,
) : ViewModel() {
    private val _uiState = MutableStateFlow(OddsUiState())
    val uiState: StateFlow<OddsUiState> = _uiState.asStateFlow()

    init {
        viewModelScope.launch {
            val bookmakers = runCatching { repository.bookmakers() }.getOrDefault(emptyList())
            val events = runCatching { repository.events(bookmaker = null, query = null) }.getOrDefault(emptyList())
            val defaultBookmakers = bookmakers
                .filter { it.enabled }
                .map { it.code }
                .distinct()

            _uiState.update {
                it.copy(
                    bookmakers = bookmakers,
                    events = events,
                    defaultBookmakerCodes = defaultBookmakers,
                    filters = OddsFilters(bookmakerCodes = defaultBookmakers),
                )
            }
            refresh()
        }
    }

    fun applyFilters(filters: OddsFilters) {
        _uiState.update { it.copy(filters = filters, infoMessage = null) }
        refresh()
    }

    fun refresh() {
        viewModelScope.launch {
            val filters = uiState.value.filters
            _uiState.update { it.copy(isLoading = true, errorMessage = null) }
            runCatching {
                repository.odds(
                    bookmakers = filters.bookmakerCodes,
                    query = null,
                    marketType = filters.marketTypeCode,
                    eventId = filters.eventId,
                    selectionType = null,
                    minEdge = null,
                    minPrice = null,
                    maxPrice = null,
                    sgmOnly = false,
                    bestOnly = false,
                )
            }.onSuccess { odds ->
                _uiState.update { it.copy(odds = odds, isLoading = false) }
            }.onFailure { error ->
                _uiState.update {
                    it.copy(
                        isLoading = false,
                        errorMessage = error.message ?: "Failed to load odds.",
                    )
                }
            }
        }
    }

    fun addToDraft(odds: OddsSearchResult) {
        if (odds.bookmaker != "sportsbet") {
            _uiState.update { it.copy(infoMessage = "Live SGM pricing is only enabled for Sportsbet right now.") }
            return
        }
        val basePrice = odds.decimalPrice
        if (!odds.sgmEligible || basePrice == null) {
            _uiState.update { it.copy(infoMessage = "That leg is not ready for SGM pricing.") }
            return
        }
        val result = draftStore.addLeg(
            DraftLeg(
                selectionId = odds.selectionId,
                eventId = odds.eventId,
                eventLabel = odds.matchName,
                bookmaker = odds.bookmaker,
                label = odds.label,
                marketTypeCode = odds.marketTypeCode,
                selectionType = odds.selectionType,
                basePrice = basePrice,
            ),
        )
        _uiState.update { it.copy(infoMessage = result.message) }
    }
}

@Composable
fun OddsRoute(
    repository: AflRepository,
    draftStore: SgmDraftStore,
) {
    val viewModel: OddsViewModel = viewModel(
        factory = simpleViewModelFactory { OddsViewModel(repository, draftStore) },
    )
    val uiState by viewModel.uiState.collectAsStateWithLifecycle()
    OddsScreen(
        uiState = uiState,
        onApplyFilters = viewModel::applyFilters,
        onRefresh = viewModel::refresh,
        onAddToDraft = viewModel::addToDraft,
    )
}

@OptIn(ExperimentalMaterial3Api::class, ExperimentalLayoutApi::class)
@Composable
private fun OddsScreen(
    uiState: OddsUiState,
    onApplyFilters: (OddsFilters) -> Unit,
    onRefresh: () -> Unit,
    onAddToDraft: (OddsSearchResult) -> Unit,
) {
    var showFilters by remember { mutableStateOf(false) }
    var draftFilters by remember(uiState.filters) { mutableStateOf(uiState.filters) }

    LaunchedEffect(showFilters, uiState.filters) {
        if (showFilters) {
            draftFilters = uiState.filters
        }
    }

    Scaffold(
        topBar = {
            TopAppBar(
                title = { Text("Odds") },
                actions = {
                    IconButton(onClick = onRefresh) {
                        Icon(Icons.Outlined.Refresh, contentDescription = "Refresh")
                    }
                    IconButton(onClick = { showFilters = true }) {
                        Icon(Icons.Outlined.FilterList, contentDescription = "Filters")
                    }
                },
            )
        },
    ) { innerPadding ->
        Box(
            modifier = Modifier
                .fillMaxSize()
                .padding(innerPadding),
        ) {
            LazyColumn(
                modifier = Modifier.fillMaxSize(),
                contentPadding = ScreenPadding,
                verticalArrangement = Arrangement.spacedBy(12.dp),
            ) {
                item {
                    Card(modifier = Modifier.fillMaxWidth()) {
                        Row(
                            modifier = Modifier
                                .fillMaxWidth()
                                .padding(16.dp),
                            horizontalArrangement = Arrangement.SpaceBetween,
                            verticalAlignment = Alignment.CenterVertically,
                        ) {
                            Column(
                                modifier = Modifier.weight(1f),
                                verticalArrangement = Arrangement.spacedBy(6.dp),
                            ) {
                                Text("Processed prop odds", style = MaterialTheme.typography.titleLarge)
                                Text(
                                    "${uiState.odds.size} live rows from the current processed odds set.",
                                    style = MaterialTheme.typography.bodyMedium,
                                )
                            }
                            FilledTonalButton(onClick = { showFilters = true }) {
                                Icon(Icons.Outlined.FilterList, contentDescription = null)
                                Text("Filters", modifier = Modifier.padding(start = 8.dp))
                            }
                        }
                    }
                }

                item {
                    ActiveFilterRow(
                        filters = uiState.filters,
                        bookmakers = uiState.bookmakers,
                        events = uiState.events,
                    )
                }

                if (uiState.isLoading) {
                    item { LoadingCard("Loading odds") }
                }

                uiState.errorMessage?.let { message ->
                    item { ErrorCard(message) }
                }

                uiState.infoMessage?.let { message ->
                    item { EmptyCard("SGM", message) }
                }

                if (!uiState.isLoading && uiState.odds.isEmpty()) {
                    item { EmptyCard("No odds", "Change market, agency, or match filters.") }
                }

                items(
                    items = uiState.odds,
                    key = { "${it.selectionId}:${it.bookmaker}" },
                ) { odds ->
                    OddsCard(
                        odds = odds,
                        onAddToDraft = onAddToDraft,
                    )
                }
            }

            if (showFilters) {
                OddsFilterSheet(
                    filters = draftFilters,
                    bookmakers = uiState.bookmakers,
                    events = uiState.events,
                    defaultBookmakers = uiState.defaultBookmakerCodes,
                    onFiltersChanged = { draftFilters = it },
                    onApply = {
                        onApplyFilters(draftFilters)
                        showFilters = false
                    },
                    onClear = {
                        draftFilters = OddsFilters(bookmakerCodes = uiState.defaultBookmakerCodes)
                    },
                    onDismiss = { showFilters = false },
                )
            }
        }
    }
}

@OptIn(ExperimentalLayoutApi::class)
@Composable
private fun ActiveFilterRow(
    filters: OddsFilters,
    bookmakers: List<BookmakerSummary>,
    events: List<EventSummary>,
) {
    val agencyLabel = when (filters.bookmakerCodes.size) {
        0 -> "All agencies"
        1 -> bookmakers.firstOrNull { it.code == filters.bookmakerCodes.first() }?.displayName ?: "1 agency"
        else -> "${filters.bookmakerCodes.size} agencies"
    }
    val matchLabel = filters.eventId?.let { eventId ->
        events.firstOrNull { it.id == eventId }?.matchName
    } ?: "All matches"

    FlowRow(
        horizontalArrangement = Arrangement.spacedBy(8.dp),
        verticalArrangement = Arrangement.spacedBy(8.dp),
    ) {
        InlineChip("Market: ${marketLabel(filters.marketTypeCode)}")
        InlineChip("Agency: $agencyLabel")
        InlineChip("Match: $matchLabel")
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun OddsFilterSheet(
    filters: OddsFilters,
    bookmakers: List<BookmakerSummary>,
    events: List<EventSummary>,
    defaultBookmakers: List<String>,
    onFiltersChanged: (OddsFilters) -> Unit,
    onApply: () -> Unit,
    onClear: () -> Unit,
    onDismiss: () -> Unit,
) {
    var marketExpanded by mutableStateOf(false)
    var matchExpanded by mutableStateOf(false)

    ModalBottomSheet(onDismissRequest = onDismiss) {
        Column(
            modifier = Modifier
                .fillMaxWidth()
                .verticalScroll(rememberScrollState())
                .padding(horizontal = 20.dp, vertical = 8.dp),
            verticalArrangement = Arrangement.spacedBy(18.dp),
        ) {
            Text("Filters", style = MaterialTheme.typography.headlineSmall)

            ExposedDropdownMenuBox(
                expanded = marketExpanded,
                onExpandedChange = { marketExpanded = !marketExpanded },
            ) {
                OutlinedTextField(
                    value = marketLabel(filters.marketTypeCode),
                    onValueChange = {},
                    modifier = Modifier
                        .menuAnchor(ExposedDropdownMenuAnchorType.PrimaryNotEditable)
                        .fillMaxWidth(),
                    readOnly = true,
                    label = { Text("Market") },
                    trailingIcon = {
                        ExposedDropdownMenuDefaults.TrailingIcon(expanded = marketExpanded)
                    },
                )
                DropdownMenu(
                    expanded = marketExpanded,
                    onDismissRequest = { marketExpanded = false },
                ) {
                    marketOptions.forEach { option ->
                        DropdownMenuItem(
                            text = { Text(option.label) },
                            onClick = {
                                onFiltersChanged(filters.copy(marketTypeCode = option.code))
                                marketExpanded = false
                            },
                        )
                    }
                }
            }

            Column(verticalArrangement = Arrangement.spacedBy(10.dp)) {
                Text("Agency", style = MaterialTheme.typography.titleMedium)
                FlowRow(
                    horizontalArrangement = Arrangement.spacedBy(8.dp),
                    verticalArrangement = Arrangement.spacedBy(8.dp),
                ) {
                    bookmakers.forEach { bookmaker ->
                        FilterChip(
                            selected = filters.bookmakerCodes.contains(bookmaker.code),
                            onClick = {
                                onFiltersChanged(
                                    filters.copy(
                                        bookmakerCodes = toggleBookmaker(
                                            current = filters.bookmakerCodes.ifEmpty { defaultBookmakers },
                                            bookmakerCode = bookmaker.code,
                                        ),
                                    ),
                                )
                            },
                            label = { Text(bookmaker.displayName) },
                        )
                    }
                }
            }

            ExposedDropdownMenuBox(
                expanded = matchExpanded,
                onExpandedChange = { matchExpanded = !matchExpanded },
            ) {
                OutlinedTextField(
                    value = selectedMatchLabel(filters.eventId, events),
                    onValueChange = {},
                    modifier = Modifier
                        .menuAnchor(ExposedDropdownMenuAnchorType.PrimaryNotEditable)
                        .fillMaxWidth(),
                    readOnly = true,
                    label = { Text("Match") },
                    trailingIcon = {
                        ExposedDropdownMenuDefaults.TrailingIcon(expanded = matchExpanded)
                    },
                )
                DropdownMenu(
                    expanded = matchExpanded,
                    onDismissRequest = { matchExpanded = false },
                    modifier = Modifier.heightIn(max = 360.dp),
                ) {
                    DropdownMenuItem(
                        text = { Text("All matches") },
                        onClick = {
                            onFiltersChanged(filters.copy(eventId = null))
                            matchExpanded = false
                        },
                    )
                    events.forEach { event ->
                        DropdownMenuItem(
                            text = { Text(event.matchName) },
                            onClick = {
                                onFiltersChanged(filters.copy(eventId = event.id))
                                matchExpanded = false
                            },
                        )
                    }
                }
            }

            HorizontalDivider()

            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.spacedBy(12.dp),
            ) {
                TextButton(
                    onClick = onClear,
                    modifier = Modifier.weight(1f),
                ) {
                    Text("Clear")
                }
                FilledTonalButton(
                    onClick = onApply,
                    modifier = Modifier.weight(1f),
                ) {
                    Text("Apply")
                }
            }
        }
    }
}

@Composable
private fun OddsCard(
    odds: OddsSearchResult,
    onAddToDraft: (OddsSearchResult) -> Unit,
) {
    Card(modifier = Modifier.fillMaxWidth()) {
        Column(
            modifier = Modifier.padding(14.dp),
            verticalArrangement = Arrangement.spacedBy(10.dp),
        ) {
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.SpaceBetween,
                verticalAlignment = Alignment.Top,
            ) {
                Column(
                    modifier = Modifier.weight(1f),
                    verticalArrangement = Arrangement.spacedBy(4.dp),
                ) {
                    Text(
                        text = odds.player?.fullName ?: odds.label,
                        style = MaterialTheme.typography.titleMedium,
                        fontWeight = FontWeight.SemiBold,
                    )
                    Text(
                        text = odds.marketDisplayName,
                        style = MaterialTheme.typography.labelLarge,
                        color = MaterialTheme.colorScheme.primary,
                    )
                    Text(
                        text = odds.matchName,
                        style = MaterialTheme.typography.bodySmall,
                    )
                }
                if (odds.bookmaker == "sportsbet" && odds.sgmEligible && odds.decimalPrice != null) {
                    TextButton(
                        onClick = { onAddToDraft(odds) },
                        modifier = Modifier.padding(start = 8.dp),
                    ) {
                        Icon(Icons.Outlined.Add, contentDescription = null)
                        Text("+SGM", modifier = Modifier.padding(start = 4.dp))
                    }
                }
            }

            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.spacedBy(8.dp),
            ) {
                DenseStatCell(
                    label = "LINE",
                    value = odds.lineValue?.let(::formatLineValue) ?: "-",
                    modifier = Modifier.weight(1f),
                )
                DenseStatCell(
                    label = "SIDE",
                    value = odds.selectionType.uppercase(Locale.getDefault()),
                    modifier = Modifier.weight(1f),
                )
                DenseStatCell(
                    label = "PRICE",
                    value = formatDecimalPrice(odds.decimalPrice),
                    modifier = Modifier.weight(1f),
                )
                DenseStatCell(
                    label = "AGENCY",
                    value = bookmakerLabel(odds.bookmaker),
                    modifier = Modifier.weight(1f),
                )
            }

            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.spacedBy(8.dp),
            ) {
                DeltaStatCell(
                    label = "DIFF 2025",
                    value = odds.diff2025,
                    modifier = Modifier.weight(1f),
                )
                DeltaStatCell(
                    label = "DIFF L10",
                    value = odds.diffLast10,
                    modifier = Modifier.weight(1f),
                )
            }
        }
    }
}

@Composable
private fun DenseStatCell(
    label: String,
    value: String,
    modifier: Modifier = Modifier,
) {
    Column(
        modifier = modifier
            .background(
                color = MaterialTheme.colorScheme.surfaceContainerHighest,
                shape = RoundedCornerShape(14.dp),
            )
            .padding(horizontal = 10.dp, vertical = 10.dp),
        verticalArrangement = Arrangement.spacedBy(4.dp),
    ) {
        Text(
            text = label,
            style = MaterialTheme.typography.labelSmall,
            color = MaterialTheme.colorScheme.onSurfaceVariant,
        )
        Text(
            text = value,
            style = MaterialTheme.typography.titleSmall,
            fontWeight = FontWeight.Bold,
        )
    }
}

@Composable
private fun DeltaStatCell(
    label: String,
    value: Double?,
    modifier: Modifier = Modifier,
) {
    val display = value?.let(::formatDelta) ?: "-"
    val tone = when {
        value == null -> MaterialTheme.colorScheme.onSurface
        value > 0 -> Color(0xFF1B7F46)
        value < 0 -> MaterialTheme.colorScheme.error
        else -> MaterialTheme.colorScheme.onSurface
    }
    Column(
        modifier = modifier
            .background(
                color = MaterialTheme.colorScheme.surface,
                shape = RoundedCornerShape(14.dp),
            )
            .padding(horizontal = 10.dp, vertical = 10.dp),
        verticalArrangement = Arrangement.spacedBy(4.dp),
    ) {
        Text(
            text = label,
            style = MaterialTheme.typography.labelSmall,
            color = MaterialTheme.colorScheme.onSurfaceVariant,
        )
        Text(
            text = display,
            style = MaterialTheme.typography.titleSmall,
            color = tone,
            fontWeight = FontWeight.Bold,
        )
    }
}

private fun toggleBookmaker(current: List<String>, bookmakerCode: String): List<String> =
    if (current.contains(bookmakerCode)) {
        current.filterNot { it == bookmakerCode }
    } else {
        current + bookmakerCode
    }

private fun marketLabel(marketTypeCode: String?): String =
    marketOptions.firstOrNull { it.code == marketTypeCode }?.label ?: "All props"

private fun selectedMatchLabel(eventId: Int?, events: List<EventSummary>): String {
    if (eventId == null) return "All matches"
    return events.firstOrNull { it.id == eventId }?.matchName ?: "All matches"
}

private fun bookmakerLabel(bookmakerCode: String): String =
    bookmakerCode.replaceFirstChar { if (it.isLowerCase()) it.titlecase(Locale.getDefault()) else it.toString() }

private fun formatLineValue(value: Double): String =
    if (value % 1.0 == 0.0) {
        String.format(Locale.getDefault(), "%.0f", value)
    } else {
        String.format(Locale.getDefault(), "%.1f", value)
    }

private fun formatDelta(value: Double): String =
    String.format(Locale.getDefault(), "%+.2f", value)
