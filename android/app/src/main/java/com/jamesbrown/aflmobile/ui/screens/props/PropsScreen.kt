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
import androidx.compose.material3.PrimaryTabRow
import androidx.compose.material3.Scaffold
import androidx.compose.material3.Tab
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
import com.jamesbrown.aflmobile.model.BookmakerSummary
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

private const val OddsScopeMatch = "match"
private const val OddsScopePlayer = "player"

private val playerMarketOptions = listOf(
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

private val matchMarketOptions = listOf(
    FilterOption(code = null, label = "All match markets"),
    FilterOption(code = "h2h", label = "H2H"),
    FilterOption(code = "line", label = "Line"),
    FilterOption(code = "total_points", label = "Totals"),
)

private data class SortOption(
    val sortBy: String,
    val sortDirection: String,
    val label: String,
)

private val playerSortOptions = listOf(
    SortOption(sortBy = "diff_last_10", sortDirection = "desc", label = "Diff L10"),
    SortOption(sortBy = "diff_2025", sortDirection = "desc", label = "Diff 2025"),
    SortOption(sortBy = "price", sortDirection = "desc", label = "Price high-low"),
    SortOption(sortBy = "price", sortDirection = "asc", label = "Price low-high"),
    SortOption(sortBy = "player", sortDirection = "asc", label = "Player A-Z"),
    SortOption(sortBy = "match", sortDirection = "asc", label = "Match"),
)

private val matchSortOptions = listOf(
    SortOption(sortBy = "start_time", sortDirection = "asc", label = "Start time"),
    SortOption(sortBy = "price", sortDirection = "desc", label = "Price high-low"),
    SortOption(sortBy = "price", sortDirection = "asc", label = "Price low-high"),
    SortOption(sortBy = "market", sortDirection = "asc", label = "Market"),
    SortOption(sortBy = "match", sortDirection = "asc", label = "Match"),
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
                    filters = OddsFilters(
                        scope = OddsScopePlayer,
                        bookmakerCodes = defaultBookmakers,
                    ),
                )
            }
            refresh()
        }
    }

    fun setScope(scope: String) {
        val currentFilters = uiState.value.filters
        val defaultSort = defaultSortForScope(scope)
        val nextMarketType = currentFilters.marketTypeCode
            ?.takeIf { marketOptionsForScope(scope).any { option -> option.code == it } }
        _uiState.update {
            it.copy(
                filters = currentFilters.copy(
                    scope = scope,
                    marketTypeCode = nextMarketType,
                    sortBy = defaultSort.sortBy,
                    sortDirection = defaultSort.sortDirection,
                ),
                infoMessage = null,
            )
        }
        refresh()
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
                    scope = filters.scope,
                    query = null,
                    marketType = filters.marketTypeCode,
                    eventId = filters.eventId,
                    sortBy = filters.sortBy,
                    sortDirection = filters.sortDirection,
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

}

@Composable
fun OddsRoute(
    repository: AflRepository,
) {
    val viewModel: OddsViewModel = viewModel(
        factory = simpleViewModelFactory { OddsViewModel(repository) },
    )
    val uiState by viewModel.uiState.collectAsStateWithLifecycle()
    OddsScreen(
        uiState = uiState,
        onScopeSelected = viewModel::setScope,
        onApplyFilters = viewModel::applyFilters,
        onRefresh = viewModel::refresh,
    )
}

@OptIn(ExperimentalMaterial3Api::class, ExperimentalLayoutApi::class)
@Composable
private fun OddsScreen(
    uiState: OddsUiState,
    onScopeSelected: (String) -> Unit,
    onApplyFilters: (OddsFilters) -> Unit,
    onRefresh: () -> Unit,
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
                    PrimaryTabRow(selectedTabIndex = if (uiState.filters.scope == OddsScopeMatch) 0 else 1) {
                        Tab(
                            selected = uiState.filters.scope == OddsScopeMatch,
                            onClick = { onScopeSelected(OddsScopeMatch) },
                            text = { Text("Match") },
                        )
                        Tab(
                            selected = uiState.filters.scope == OddsScopePlayer,
                            onClick = { onScopeSelected(OddsScopePlayer) },
                            text = { Text("Player") },
                        )
                    }
                }

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
                                Text(
                                    if (uiState.filters.scope == OddsScopeMatch) {
                                        "Processed match odds"
                                    } else {
                                        "Processed player props"
                                    },
                                    style = MaterialTheme.typography.titleLarge,
                                )
                                Text(
                                    if (uiState.filters.scope == OddsScopeMatch) {
                                        "${uiState.odds.size} match-market rows across H2H, line, and totals."
                                    } else {
                                        "${uiState.odds.size} player prop rows from the current processed odds set."
                                    },
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

                if (!uiState.isLoading && uiState.odds.isEmpty()) {
                    item {
                        EmptyCard(
                            "No odds",
                            if (uiState.filters.scope == OddsScopeMatch) {
                                "Change the match-market, agency, or match filters."
                            } else {
                                "Change the player market, agency, or match filters."
                            },
                        )
                    }
                }

                items(
                    items = uiState.odds,
                    key = { "${it.selectionId}:${it.bookmaker}" },
                ) { odds ->
                    OddsCard(
                        odds = odds,
                        scope = uiState.filters.scope,
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
                        val defaultSort = defaultSortForScope(draftFilters.scope)
                        draftFilters = OddsFilters(
                            scope = draftFilters.scope,
                            bookmakerCodes = uiState.defaultBookmakerCodes,
                            sortBy = defaultSort.sortBy,
                            sortDirection = defaultSort.sortDirection,
                        )
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
        InlineChip("Scope: ${filters.scope.replaceFirstChar { it.titlecase(Locale.getDefault()) }}")
        InlineChip("Market: ${marketLabel(filters.marketTypeCode, filters.scope)}")
        InlineChip("Agency: $agencyLabel")
        InlineChip("Match: $matchLabel")
        InlineChip("Sort: ${sortLabel(filters)}")
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
    var marketExpanded by remember { mutableStateOf(false) }
    var matchExpanded by remember { mutableStateOf(false) }
    var sortExpanded by remember { mutableStateOf(false) }

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
                    value = marketLabel(filters.marketTypeCode, filters.scope),
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
                    marketOptionsForScope(filters.scope).forEach { option ->
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
                expanded = sortExpanded,
                onExpandedChange = { sortExpanded = !sortExpanded },
            ) {
                OutlinedTextField(
                    value = sortLabel(filters),
                    onValueChange = {},
                    modifier = Modifier
                        .menuAnchor(ExposedDropdownMenuAnchorType.PrimaryNotEditable)
                        .fillMaxWidth(),
                    readOnly = true,
                    label = { Text("Sort") },
                    trailingIcon = {
                        ExposedDropdownMenuDefaults.TrailingIcon(expanded = sortExpanded)
                    },
                )
                DropdownMenu(
                    expanded = sortExpanded,
                    onDismissRequest = { sortExpanded = false },
                ) {
                    sortOptionsForScope(filters.scope).forEach { option ->
                        DropdownMenuItem(
                            text = { Text(option.label) },
                            onClick = {
                                onFiltersChanged(
                                    filters.copy(
                                        sortBy = option.sortBy,
                                        sortDirection = option.sortDirection,
                                    ),
                                )
                                sortExpanded = false
                            },
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
    scope: String,
) {
    if (scope == OddsScopeMatch) {
        MatchOddsCard(odds = odds)
        return
    }
    PlayerOddsCard(odds = odds)
}

@Composable
private fun PlayerOddsCard(
    odds: OddsSearchResult,
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
private fun MatchOddsCard(
    odds: OddsSearchResult,
) {
    Card(modifier = Modifier.fillMaxWidth()) {
        Column(
            modifier = Modifier.padding(14.dp),
            verticalArrangement = Arrangement.spacedBy(10.dp),
        ) {
            Column(verticalArrangement = Arrangement.spacedBy(4.dp)) {
                Text(
                    text = odds.label,
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

            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.spacedBy(8.dp),
            ) {
                DenseStatCell(
                    label = "SIDE",
                    value = odds.selectionType.uppercase(Locale.getDefault()),
                    modifier = Modifier.weight(1f),
                )
                DenseStatCell(
                    label = "LINE",
                    value = odds.lineValue?.let(::formatLineValue) ?: "-",
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

private fun marketOptionsForScope(scope: String): List<FilterOption> =
    if (scope == OddsScopeMatch) matchMarketOptions else playerMarketOptions

private fun sortOptionsForScope(scope: String): List<SortOption> =
    if (scope == OddsScopeMatch) matchSortOptions else playerSortOptions

private fun defaultSortForScope(scope: String): SortOption =
    sortOptionsForScope(scope).first()

private fun marketLabel(marketTypeCode: String?, scope: String): String =
    marketOptionsForScope(scope).firstOrNull { it.code == marketTypeCode }?.label
        ?: marketOptionsForScope(scope).first().label

private fun sortLabel(filters: OddsFilters): String =
    sortOptionsForScope(filters.scope).firstOrNull {
        it.sortBy == filters.sortBy && it.sortDirection == filters.sortDirection
    }?.label ?: defaultSortForScope(filters.scope).label

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
