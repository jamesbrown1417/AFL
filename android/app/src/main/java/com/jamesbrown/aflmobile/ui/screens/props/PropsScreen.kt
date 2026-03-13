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
import androidx.compose.material.icons.automirrored.outlined.Sort
import androidx.compose.material.icons.outlined.FilterList
import androidx.compose.material.icons.outlined.Menu
import androidx.compose.material.icons.outlined.Refresh
import androidx.compose.material3.Checkbox
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
import androidx.compose.material3.RangeSlider
import androidx.compose.material3.Scaffold
import androidx.compose.material3.Switch
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
import com.jamesbrown.aflmobile.model.OddsDiffSliderMax
import com.jamesbrown.aflmobile.model.OddsDiffSliderMin
import com.jamesbrown.aflmobile.model.OddsFilters
import com.jamesbrown.aflmobile.model.OddsSearchResult
import com.jamesbrown.aflmobile.model.PlayerSummary
import com.jamesbrown.aflmobile.ui.common.EmptyCard
import com.jamesbrown.aflmobile.ui.common.ErrorCard
import com.jamesbrown.aflmobile.ui.common.InlineChip
import com.jamesbrown.aflmobile.ui.common.LoadingCard
import com.jamesbrown.aflmobile.ui.common.ScreenPadding
import com.jamesbrown.aflmobile.ui.common.formatDecimalPrice
import com.jamesbrown.aflmobile.ui.common.simpleViewModelFactory
import com.jamesbrown.aflmobile.ui.theme.appCardColors
import com.jamesbrown.aflmobile.ui.theme.appGlassBorder
import com.jamesbrown.aflmobile.ui.theme.appTopBarColors
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

private val playerSelectionOptions = listOf(
    FilterOption(code = null, label = "All sides"),
    FilterOption(code = "over", label = "Overs"),
    FilterOption(code = "under", label = "Unders"),
)

private data class SortOption(
    val sortBy: String,
    val sortDirection: String,
    val label: String,
)

private val playerSortOptions = listOf(
    SortOption(sortBy = "diff_last_10", sortDirection = "desc", label = "Diff L10"),
    SortOption(sortBy = "next_best_prob_diff", sortDirection = "desc", label = "Next best diff"),
    SortOption(sortBy = "diff_2025", sortDirection = "desc", label = "Diff 2025"),
    SortOption(sortBy = "price", sortDirection = "desc", label = "Price high-low"),
    SortOption(sortBy = "price", sortDirection = "asc", label = "Price low-high"),
    SortOption(sortBy = "player", sortDirection = "asc", label = "Player A-Z"),
    SortOption(sortBy = "match", sortDirection = "asc", label = "Match"),
)

private val matchSortOptions = listOf(
    SortOption(sortBy = "start_time", sortDirection = "asc", label = "Start time"),
    SortOption(sortBy = "next_best_prob_diff", sortDirection = "desc", label = "Next best diff"),
    SortOption(sortBy = "price", sortDirection = "desc", label = "Price high-low"),
    SortOption(sortBy = "price", sortDirection = "asc", label = "Price low-high"),
    SortOption(sortBy = "market", sortDirection = "asc", label = "Market"),
    SortOption(sortBy = "match", sortDirection = "asc", label = "Match"),
)

private const val OddsPageSize = 50

data class OddsUiState(
    val bookmakers: List<BookmakerSummary> = emptyList(),
    val events: List<EventSummary> = emptyList(),
    val allPlayers: List<PlayerSummary> = emptyList(),
    val filters: OddsFilters = OddsFilters(),
    val defaultBookmakerCodes: List<String> = emptyList(),
    val odds: List<OddsSearchResult> = emptyList(),
    val visibleCount: Int = OddsPageSize,
    val hasMore: Boolean = false,
    val alternateUndersTarget: OddsSearchResult? = null,
    val alternateUnders: List<OddsSearchResult> = emptyList(),
    val isLoadingAlternateUnders: Boolean = false,
    val alternateUndersError: String? = null,
    val isLoading: Boolean = true,
    val isLoadingMore: Boolean = false,
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
            val players = runCatching { repository.searchPlayers("", limit = 5000) }.getOrDefault(emptyList())
            val defaultBookmakers = bookmakers
                .filter { it.enabled }
                .map { it.code }
                .distinct()

            _uiState.update {
                it.copy(
                    bookmakers = bookmakers,
                    events = events,
                    allPlayers = players,
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
        val scopedFilters = if (scope == OddsScopePlayer) {
            currentFilters
        } else {
            currentFilters.copy(
                includePlayerIds = emptyList(),
                excludePlayerIds = emptyList(),
                selectionType = null,
                minPriceText = "",
                maxPriceText = "",
                minDiff2025 = OddsDiffSliderMin,
                maxDiff2025 = OddsDiffSliderMax,
                minDiffLast10 = OddsDiffSliderMin,
                maxDiffLast10 = OddsDiffSliderMax,
                bestOnly = false,
            )
        }
        _uiState.update {
            it.copy(
                filters = scopedFilters.copy(
                    scope = scope,
                    marketTypeCode = nextMarketType,
                    sortBy = defaultSort.sortBy,
                    sortDirection = defaultSort.sortDirection,
                ),
                infoMessage = null,
            )
        }
        refresh(resetVisibleCount = true)
    }

    fun applyFilters(filters: OddsFilters) {
        _uiState.update {
            it.copy(
                filters = filters,
                visibleCount = OddsPageSize,
                infoMessage = null,
            )
        }
        refresh(resetVisibleCount = true)
    }

    fun refresh(resetVisibleCount: Boolean = false) {
        viewModelScope.launch {
            val currentState = uiState.value
            val filters = currentState.filters
            val visibleCount = if (resetVisibleCount) OddsPageSize else currentState.visibleCount
            val playerScoped = filters.scope == OddsScopePlayer
            _uiState.update {
                it.copy(
                    visibleCount = visibleCount,
                    isLoading = true,
                    isLoadingMore = false,
                    errorMessage = null,
                )
            }
            runCatching {
                repository.odds(
                    bookmakers = filters.bookmakerCodes,
                    scope = filters.scope,
                    query = null,
                    marketType = filters.marketTypeCode,
                    eventId = filters.eventId,
                    includePlayerIds = if (playerScoped) filters.includePlayerIds else emptyList(),
                    excludePlayerIds = if (playerScoped) filters.excludePlayerIds else emptyList(),
                    sortBy = filters.sortBy,
                    sortDirection = filters.sortDirection,
                    selectionType = if (playerScoped) filters.selectionType else null,
                    minEdge = null,
                    minPrice = if (playerScoped) filters.minPriceText.toDoubleOrNull() else null,
                    maxPrice = if (playerScoped) filters.maxPriceText.toDoubleOrNull() else null,
                    minDiff2025 = if (playerScoped) filters.minDiff2025.toDouble() else null,
                    maxDiff2025 = if (playerScoped) filters.maxDiff2025.toDouble() else null,
                    minDiffLast10 = if (playerScoped) filters.minDiffLast10.toDouble() else null,
                    maxDiffLast10 = if (playerScoped) filters.maxDiffLast10.toDouble() else null,
                    sgmOnly = false,
                    bestOnly = if (playerScoped) filters.bestOnly else false,
                    limit = visibleCount + 1,
                )
            }.onSuccess { odds ->
                _uiState.update {
                    it.copy(
                        odds = odds.take(visibleCount),
                        hasMore = odds.size > visibleCount,
                        isLoading = false,
                        isLoadingMore = false,
                    )
                }
            }.onFailure { error ->
                _uiState.update {
                    it.copy(
                        isLoading = false,
                        isLoadingMore = false,
                        errorMessage = error.message ?: "Failed to load odds.",
                    )
                }
            }
        }
    }

    fun loadMore() {
        val currentState = uiState.value
        if (currentState.isLoading || currentState.isLoadingMore || !currentState.hasMore) {
            return
        }
        val nextVisibleCount = currentState.visibleCount + OddsPageSize
        viewModelScope.launch {
            val filters = currentState.filters
            val playerScoped = filters.scope == OddsScopePlayer
            _uiState.update { it.copy(isLoadingMore = true, errorMessage = null) }
            runCatching {
                repository.odds(
                    bookmakers = filters.bookmakerCodes,
                    scope = filters.scope,
                    query = null,
                    marketType = filters.marketTypeCode,
                    eventId = filters.eventId,
                    includePlayerIds = if (playerScoped) filters.includePlayerIds else emptyList(),
                    excludePlayerIds = if (playerScoped) filters.excludePlayerIds else emptyList(),
                    sortBy = filters.sortBy,
                    sortDirection = filters.sortDirection,
                    selectionType = if (playerScoped) filters.selectionType else null,
                    minEdge = null,
                    minPrice = if (playerScoped) filters.minPriceText.toDoubleOrNull() else null,
                    maxPrice = if (playerScoped) filters.maxPriceText.toDoubleOrNull() else null,
                    minDiff2025 = if (playerScoped) filters.minDiff2025.toDouble() else null,
                    maxDiff2025 = if (playerScoped) filters.maxDiff2025.toDouble() else null,
                    minDiffLast10 = if (playerScoped) filters.minDiffLast10.toDouble() else null,
                    maxDiffLast10 = if (playerScoped) filters.maxDiffLast10.toDouble() else null,
                    sgmOnly = false,
                    bestOnly = if (playerScoped) filters.bestOnly else false,
                    limit = nextVisibleCount + 1,
                )
            }.onSuccess { odds ->
                _uiState.update {
                    it.copy(
                        odds = odds.take(nextVisibleCount),
                        visibleCount = nextVisibleCount,
                        hasMore = odds.size > nextVisibleCount,
                        isLoadingMore = false,
                    )
                }
            }.onFailure { error ->
                _uiState.update {
                    it.copy(
                        isLoadingMore = false,
                        errorMessage = error.message ?: "Failed to load more odds.",
                    )
                }
            }
        }
    }

    fun openAlternateUnders(odds: OddsSearchResult) {
        val player = odds.player ?: return
        val currentState = uiState.value
        val bookmakers = currentState.filters.bookmakerCodes.ifEmpty { currentState.defaultBookmakerCodes }.ifEmpty { listOf(odds.bookmaker) }
        viewModelScope.launch {
            _uiState.update {
                it.copy(
                    alternateUndersTarget = odds,
                    alternateUnders = emptyList(),
                    isLoadingAlternateUnders = true,
                    alternateUndersError = null,
                )
            }
            runCatching {
                repository.odds(
                    bookmakers = bookmakers,
                    scope = OddsScopePlayer,
                    query = null,
                    marketType = odds.marketTypeCode,
                    eventId = odds.eventId,
                    includePlayerIds = listOf(player.id),
                    excludePlayerIds = emptyList(),
                    sortBy = "price",
                    sortDirection = "asc",
                    selectionType = "under",
                    minEdge = null,
                    minPrice = null,
                    maxPrice = null,
                    minDiff2025 = null,
                    maxDiff2025 = null,
                    minDiffLast10 = null,
                    maxDiffLast10 = null,
                    sgmOnly = false,
                    bestOnly = false,
                    limit = 200,
                )
            }.onSuccess { rows ->
                val sortedRows = rows.sortedWith(
                    compareByDescending<OddsSearchResult> { it.lineValue ?: Double.NEGATIVE_INFINITY }
                        .thenByDescending { it.decimalPrice ?: Double.NEGATIVE_INFINITY }
                        .thenBy { it.bookmaker },
                )
                _uiState.update {
                    it.copy(
                        alternateUnders = sortedRows,
                        isLoadingAlternateUnders = false,
                    )
                }
            }.onFailure { error ->
                _uiState.update {
                    it.copy(
                        isLoadingAlternateUnders = false,
                        alternateUndersError = error.message ?: "Failed to load alternate under lines.",
                    )
                }
            }
        }
    }

    fun closeAlternateUnders() {
        _uiState.update {
            it.copy(
                alternateUndersTarget = null,
                alternateUnders = emptyList(),
                isLoadingAlternateUnders = false,
                alternateUndersError = null,
            )
        }
    }
}

@Composable
fun OddsRoute(
    repository: AflRepository,
    onOpenNavigation: () -> Unit,
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
        onLoadMore = viewModel::loadMore,
        onOpenAlternateUnders = viewModel::openAlternateUnders,
        onDismissAlternateUnders = viewModel::closeAlternateUnders,
        onOpenNavigation = onOpenNavigation,
    )
}

@OptIn(ExperimentalMaterial3Api::class, ExperimentalLayoutApi::class)
@Composable
private fun OddsScreen(
    uiState: OddsUiState,
    onScopeSelected: (String) -> Unit,
    onApplyFilters: (OddsFilters) -> Unit,
    onRefresh: () -> Unit,
    onLoadMore: () -> Unit,
    onOpenAlternateUnders: (OddsSearchResult) -> Unit,
    onDismissAlternateUnders: () -> Unit,
    onOpenNavigation: () -> Unit,
) {
    var showFilters by remember { mutableStateOf(false) }
    var showSort by remember { mutableStateOf(false) }
    var draftFilters by remember(uiState.filters) { mutableStateOf(uiState.filters) }

    LaunchedEffect(showFilters, uiState.filters) {
        if (showFilters) {
            draftFilters = uiState.filters
        }
    }

    Scaffold(
        containerColor = Color.Transparent,
        topBar = {
            TopAppBar(
                title = { Text("Odds") },
                colors = appTopBarColors(),
                navigationIcon = {
                    IconButton(onClick = onOpenNavigation) {
                        Icon(Icons.Outlined.Menu, contentDescription = "Open navigation")
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
                    PrimaryTabRow(
                        selectedTabIndex = if (uiState.filters.scope == OddsScopeMatch) 0 else 1,
                        containerColor = MaterialTheme.colorScheme.surface.copy(alpha = 0.82f),
                        contentColor = MaterialTheme.colorScheme.primary,
                    ) {
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
                    Card(
                        modifier = Modifier.fillMaxWidth(),
                        colors = appCardColors(),
                        border = appGlassBorder(),
                    ) {
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
                                        "Showing ${uiState.odds.size} match-market rows across H2H, line, and totals."
                                    } else {
                                        "Showing ${uiState.odds.size} player prop rows from the current processed odds set."
                                    },
                                    style = MaterialTheme.typography.bodyMedium,
                                )
                            }
                            Column(
                                horizontalAlignment = Alignment.End,
                                verticalArrangement = Arrangement.spacedBy(6.dp),
                            ) {
                                FilledTonalButton(onClick = { showFilters = true }) {
                                    Icon(Icons.Outlined.FilterList, contentDescription = null)
                                    Text("Filters", modifier = Modifier.padding(start = 8.dp))
                                }
                                TextButton(onClick = { showSort = true }) {
                                    Icon(Icons.AutoMirrored.Outlined.Sort, contentDescription = null)
                                    Text("Sort", modifier = Modifier.padding(start = 8.dp))
                                }
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
                        onOpenAlternateUnders = onOpenAlternateUnders,
                    )
                }

                if (uiState.isLoadingMore) {
                    item { LoadingCard("Loading more odds") }
                } else if (uiState.hasMore && uiState.odds.isNotEmpty()) {
                    item {
                        Card(
                            modifier = Modifier.fillMaxWidth(),
                            colors = appCardColors(),
                            border = appGlassBorder(),
                        ) {
                            Column(
                                modifier = Modifier
                                    .fillMaxWidth()
                                    .padding(16.dp),
                                horizontalAlignment = Alignment.CenterHorizontally,
                                verticalArrangement = Arrangement.spacedBy(10.dp),
                            ) {
                                Text(
                                    "More rows are available for the current filter.",
                                    style = MaterialTheme.typography.bodyMedium,
                                    textAlign = TextAlign.Center,
                                )
                                FilledTonalButton(onClick = onLoadMore) {
                                    Text("Load 50 more")
                                }
                            }
                        }
                    }
                }
            }

            if (showFilters) {
                OddsFilterSheet(
                    filters = draftFilters,
                    bookmakers = uiState.bookmakers,
                    events = uiState.events,
                    players = uiState.allPlayers,
                    defaultBookmakers = uiState.defaultBookmakerCodes,
                    onFiltersChanged = { draftFilters = it },
                    onApply = {
                        onApplyFilters(draftFilters)
                        showFilters = false
                    },
                    onClear = {
                        draftFilters = OddsFilters(
                            scope = draftFilters.scope,
                            bookmakerCodes = uiState.defaultBookmakerCodes,
                            sortBy = uiState.filters.sortBy,
                            sortDirection = uiState.filters.sortDirection,
                        )
                    },
                    onDismiss = { showFilters = false },
                )
            }

            if (showSort) {
                OddsSortSheet(
                    filters = uiState.filters,
                    onApply = { option ->
                        onApplyFilters(
                            uiState.filters.copy(
                                sortBy = option.sortBy,
                                sortDirection = option.sortDirection,
                            ),
                        )
                        showSort = false
                    },
                    onDismiss = { showSort = false },
                )
            }

            uiState.alternateUndersTarget?.let { target ->
                AlternateUndersSheet(
                    target = target,
                    rows = uiState.alternateUnders,
                    isLoading = uiState.isLoadingAlternateUnders,
                    errorMessage = uiState.alternateUndersError,
                    onDismiss = onDismissAlternateUnders,
                )
            }
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun OddsSortSheet(
    filters: OddsFilters,
    onApply: (SortOption) -> Unit,
    onDismiss: () -> Unit,
) {
    ModalBottomSheet(
        onDismissRequest = onDismiss,
        containerColor = MaterialTheme.colorScheme.surface.copy(alpha = 0.98f),
        scrimColor = MaterialTheme.colorScheme.scrim.copy(alpha = 0.26f),
    ) {
        Column(
            modifier = Modifier
                .fillMaxWidth()
                .padding(horizontal = 20.dp, vertical = 8.dp),
            verticalArrangement = Arrangement.spacedBy(14.dp),
        ) {
            Text("Sort", style = MaterialTheme.typography.headlineSmall)
            sortOptionsForScope(filters.scope).forEach { option ->
                val selected = option.sortBy == filters.sortBy && option.sortDirection == filters.sortDirection
                FilledTonalButton(
                    onClick = { onApply(option) },
                    modifier = Modifier.fillMaxWidth(),
                ) {
                    Row(
                        modifier = Modifier.fillMaxWidth(),
                        horizontalArrangement = Arrangement.SpaceBetween,
                        verticalAlignment = Alignment.CenterVertically,
                    ) {
                        Text(option.label)
                        if (selected) {
                            Text(
                                "Selected",
                                style = MaterialTheme.typography.labelMedium,
                                fontWeight = FontWeight.SemiBold,
                            )
                        }
                    }
                }
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
        if (filters.scope == OddsScopePlayer && filters.selectionType != null) {
            InlineChip("Side: ${filters.selectionType.replaceFirstChar { it.titlecase(Locale.getDefault()) }}")
        }
        if (filters.scope == OddsScopePlayer && filters.includePlayerIds.isNotEmpty()) {
            InlineChip("Include: ${filters.includePlayerIds.size}")
        }
        if (filters.scope == OddsScopePlayer && filters.excludePlayerIds.isNotEmpty()) {
            InlineChip("Exclude: ${filters.excludePlayerIds.size}")
        }
        if (filters.minPriceText.isNotBlank() || filters.maxPriceText.isNotBlank()) {
            InlineChip("Odds: ${filters.minPriceText.ifBlank { "-" }} to ${filters.maxPriceText.ifBlank { "-" }}")
        }
        if (!isDefaultDiffRange(filters.minDiffLast10, filters.maxDiffLast10)) {
            InlineChip("L10: ${formatSliderValue(filters.minDiffLast10)} to ${formatSliderValue(filters.maxDiffLast10)}")
        }
        if (!isDefaultDiffRange(filters.minDiff2025, filters.maxDiff2025)) {
            InlineChip("2025: ${formatSliderValue(filters.minDiff2025)} to ${formatSliderValue(filters.maxDiff2025)}")
        }
        if (filters.scope == OddsScopePlayer && filters.bestOnly) {
            InlineChip("Best price only")
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun OddsFilterSheet(
    filters: OddsFilters,
    bookmakers: List<BookmakerSummary>,
    events: List<EventSummary>,
    players: List<PlayerSummary>,
    defaultBookmakers: List<String>,
    onFiltersChanged: (OddsFilters) -> Unit,
    onApply: () -> Unit,
    onClear: () -> Unit,
    onDismiss: () -> Unit,
) {
    var marketExpanded by remember { mutableStateOf(false) }
    var matchExpanded by remember { mutableStateOf(false) }
    var includeExpanded by remember { mutableStateOf(false) }
    var excludeExpanded by remember { mutableStateOf(false) }
    var includeQuery by remember(filters.scope) { mutableStateOf("") }
    var excludeQuery by remember(filters.scope) { mutableStateOf("") }

    ModalBottomSheet(
        onDismissRequest = onDismiss,
        containerColor = MaterialTheme.colorScheme.surface.copy(alpha = 0.98f),
        scrimColor = MaterialTheme.colorScheme.scrim.copy(alpha = 0.26f),
    ) {
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

            if (filters.scope == OddsScopePlayer) {
                Column(verticalArrangement = Arrangement.spacedBy(10.dp)) {
                    Text("Side", style = MaterialTheme.typography.titleMedium)
                    FlowRow(
                        horizontalArrangement = Arrangement.spacedBy(8.dp),
                        verticalArrangement = Arrangement.spacedBy(8.dp),
                    ) {
                        playerSelectionOptions.forEach { option ->
                            FilterChip(
                                selected = filters.selectionType == option.code,
                                onClick = { onFiltersChanged(filters.copy(selectionType = option.code)) },
                                label = { Text(option.label) },
                            )
                        }
                    }
                }

                PlayerMultiSelectDropdown(
                    label = "Player include",
                    query = includeQuery,
                    onQueryChanged = {
                        includeQuery = it
                        includeExpanded = true
                    },
                    expanded = includeExpanded,
                    onExpandedChange = { includeExpanded = !includeExpanded },
                    players = filterPlayersByQuery(players, includeQuery),
                    selectedIds = filters.includePlayerIds,
                    onTogglePlayer = { player ->
                        onFiltersChanged(
                            filters.copy(
                                includePlayerIds = togglePlayer(filters.includePlayerIds, player.id),
                                excludePlayerIds = filters.excludePlayerIds.filterNot { it == player.id },
                            ),
                        )
                    },
                )

                SelectedPlayerChipRow(
                    label = "Including",
                    players = players.filter { filters.includePlayerIds.contains(it.id) },
                    onRemove = { playerId ->
                        onFiltersChanged(filters.copy(includePlayerIds = filters.includePlayerIds.filterNot { it == playerId }))
                    },
                )

                PlayerMultiSelectDropdown(
                    label = "Player exclude",
                    query = excludeQuery,
                    onQueryChanged = {
                        excludeQuery = it
                        excludeExpanded = true
                    },
                    expanded = excludeExpanded,
                    onExpandedChange = { excludeExpanded = !excludeExpanded },
                    players = filterPlayersByQuery(players, excludeQuery),
                    selectedIds = filters.excludePlayerIds,
                    onTogglePlayer = { player ->
                        onFiltersChanged(
                            filters.copy(
                                excludePlayerIds = togglePlayer(filters.excludePlayerIds, player.id),
                                includePlayerIds = filters.includePlayerIds.filterNot { it == player.id },
                            ),
                        )
                    },
                )

                SelectedPlayerChipRow(
                    label = "Excluding",
                    players = players.filter { filters.excludePlayerIds.contains(it.id) },
                    onRemove = { playerId ->
                        onFiltersChanged(filters.copy(excludePlayerIds = filters.excludePlayerIds.filterNot { it == playerId }))
                    },
                )
            }

            if (filters.scope == OddsScopePlayer) {
                Row(
                    modifier = Modifier.fillMaxWidth(),
                    horizontalArrangement = Arrangement.spacedBy(12.dp),
                ) {
                    OutlinedTextField(
                        value = filters.minPriceText,
                        onValueChange = { onFiltersChanged(filters.copy(minPriceText = it)) },
                        modifier = Modifier.weight(1f),
                        singleLine = true,
                        label = { Text("Min odds") },
                    )
                    OutlinedTextField(
                        value = filters.maxPriceText,
                        onValueChange = { onFiltersChanged(filters.copy(maxPriceText = it)) },
                        modifier = Modifier.weight(1f),
                        singleLine = true,
                        label = { Text("Max odds") },
                    )
                }

                Row(
                    modifier = Modifier.fillMaxWidth(),
                    horizontalArrangement = Arrangement.SpaceBetween,
                    verticalAlignment = Alignment.CenterVertically,
                ) {
                    Column(
                        modifier = Modifier.weight(1f),
                        verticalArrangement = Arrangement.spacedBy(2.dp),
                    ) {
                        Text("Best market price only", style = MaterialTheme.typography.titleMedium)
                        Text(
                            "Only show rows where the selected agency is currently best in market.",
                            style = MaterialTheme.typography.bodySmall,
                            color = MaterialTheme.colorScheme.onSurfaceVariant,
                        )
                    }
                    Switch(
                        checked = filters.bestOnly,
                        onCheckedChange = { onFiltersChanged(filters.copy(bestOnly = it)) },
                    )
                }

                DiffRangeSection(
                    title = "Diff last 10",
                    range = filters.minDiffLast10..filters.maxDiffLast10,
                    onRangeChange = { range ->
                        onFiltersChanged(
                            filters.copy(
                                minDiffLast10 = range.start,
                                maxDiffLast10 = range.endInclusive,
                            ),
                        )
                    },
                )

                DiffRangeSection(
                    title = "Diff 2025",
                    range = filters.minDiff2025..filters.maxDiff2025,
                    onRangeChange = { range ->
                        onFiltersChanged(
                            filters.copy(
                                minDiff2025 = range.start,
                                maxDiff2025 = range.endInclusive,
                            ),
                        )
                    },
                )
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

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun PlayerMultiSelectDropdown(
    label: String,
    query: String,
    onQueryChanged: (String) -> Unit,
    expanded: Boolean,
    onExpandedChange: () -> Unit,
    players: List<PlayerSummary>,
    selectedIds: List<Int>,
    onTogglePlayer: (PlayerSummary) -> Unit,
) {
    ExposedDropdownMenuBox(
        expanded = expanded,
        onExpandedChange = { onExpandedChange() },
    ) {
        OutlinedTextField(
            value = query,
            onValueChange = onQueryChanged,
            modifier = Modifier
                .menuAnchor(ExposedDropdownMenuAnchorType.PrimaryEditable)
                .fillMaxWidth(),
            singleLine = true,
            label = { Text(label) },
            placeholder = { Text("Type to filter players") },
            trailingIcon = {
                ExposedDropdownMenuDefaults.TrailingIcon(expanded = expanded)
            },
        )
        DropdownMenu(
            expanded = expanded,
            onDismissRequest = onExpandedChange,
            modifier = Modifier.heightIn(max = 360.dp),
        ) {
            players.take(75).forEach { player ->
                DropdownMenuItem(
                    text = {
                        Row(
                            verticalAlignment = Alignment.CenterVertically,
                            horizontalArrangement = Arrangement.spacedBy(10.dp),
                        ) {
                            Checkbox(
                                checked = selectedIds.contains(player.id),
                                onCheckedChange = null,
                            )
                            Text(player.fullName)
                        }
                    },
                    onClick = { onTogglePlayer(player) },
                )
            }
            if (players.isEmpty()) {
                DropdownMenuItem(
                    text = { Text("No matching players") },
                    onClick = {},
                    enabled = false,
                )
            }
        }
    }
}

@Composable
private fun SelectedPlayerChipRow(
    label: String,
    players: List<PlayerSummary>,
    onRemove: (Int) -> Unit,
) {
    if (players.isEmpty()) {
        return
    }
    Column(verticalArrangement = Arrangement.spacedBy(8.dp)) {
        Text(label, style = MaterialTheme.typography.labelLarge, color = MaterialTheme.colorScheme.onSurfaceVariant)
        FlowRow(
            horizontalArrangement = Arrangement.spacedBy(8.dp),
            verticalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            players.forEach { player ->
                FilterChip(
                    selected = true,
                    onClick = { onRemove(player.id) },
                    label = { Text(player.fullName) },
                )
            }
        }
    }
}

@Composable
private fun DiffRangeSection(
    title: String,
    range: ClosedFloatingPointRange<Float>,
    onRangeChange: (ClosedFloatingPointRange<Float>) -> Unit,
) {
    Column(verticalArrangement = Arrangement.spacedBy(8.dp)) {
        Text(title, style = MaterialTheme.typography.titleMedium)
        Text(
            "${formatSliderValue(range.start)} to ${formatSliderValue(range.endInclusive)}",
            style = MaterialTheme.typography.bodySmall,
            color = MaterialTheme.colorScheme.onSurfaceVariant,
        )
        RangeSlider(
            value = range.start..range.endInclusive,
            onValueChange = { onRangeChange(it.start..it.endInclusive) },
            valueRange = OddsDiffSliderMin..OddsDiffSliderMax,
            steps = 39,
        )
    }
}

@Composable
private fun OddsCard(
    odds: OddsSearchResult,
    scope: String,
    onOpenAlternateUnders: (OddsSearchResult) -> Unit,
) {
    if (scope == OddsScopeMatch) {
        MatchOddsCard(odds = odds)
        return
    }
    PlayerOddsCard(
        odds = odds,
        onOpenAlternateUnders = onOpenAlternateUnders,
    )
}

@Composable
private fun PlayerOddsCard(
    odds: OddsSearchResult,
    onOpenAlternateUnders: (OddsSearchResult) -> Unit,
) {
    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appGlassBorder(),
    ) {
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
                    if (odds.isBestPrice) {
                        InlineChip("Best market price")
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
                DeltaStatCell(
                    label = if (odds.isBestPrice) "NEXT BEST" else "BEST GAP",
                    value = odds.nextBestProbDiff,
                    modifier = Modifier.weight(1f),
                )
            }

            if (odds.selectionType == "under" && odds.player != null) {
                Row(
                    modifier = Modifier.fillMaxWidth(),
                    horizontalArrangement = Arrangement.End,
                ) {
                    TextButton(onClick = { onOpenAlternateUnders(odds) }) {
                        Text("Other unders")
                    }
                }
            }
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun AlternateUndersSheet(
    target: OddsSearchResult,
    rows: List<OddsSearchResult>,
    isLoading: Boolean,
    errorMessage: String?,
    onDismiss: () -> Unit,
) {
    val lineGroups = remember(rows) {
        rows.groupBy { it.lineValue }
            .toList()
            .sortedByDescending { it.first ?: Double.NEGATIVE_INFINITY }
    }

    ModalBottomSheet(
        onDismissRequest = onDismiss,
        containerColor = MaterialTheme.colorScheme.surface.copy(alpha = 0.98f),
        scrimColor = MaterialTheme.colorScheme.scrim.copy(alpha = 0.26f),
    ) {
        Column(
            modifier = Modifier
                .fillMaxWidth()
                .padding(horizontal = 20.dp, vertical = 8.dp),
            verticalArrangement = Arrangement.spacedBy(14.dp),
        ) {
            Text("All under lines", style = MaterialTheme.typography.headlineSmall)
            Text(
                "${target.player?.fullName ?: target.label} • ${target.marketDisplayName}",
                style = MaterialTheme.typography.titleMedium,
                fontWeight = FontWeight.SemiBold,
            )
            Text(
                target.matchName,
                style = MaterialTheme.typography.bodySmall,
                color = MaterialTheme.colorScheme.onSurfaceVariant,
            )

            Card(
                modifier = Modifier.fillMaxWidth(),
                colors = appCardColors(),
                border = appGlassBorder(),
            ) {
                Row(
                    modifier = Modifier
                        .fillMaxWidth()
                        .padding(14.dp),
                    horizontalArrangement = Arrangement.spacedBy(10.dp),
                ) {
                    DenseStatCell(
                        label = "CURRENT LINE",
                        value = target.lineValue?.let(::formatLineValue)?.let { "U $it" } ?: "Under",
                        modifier = Modifier.weight(1f),
                    )
                    DenseStatCell(
                        label = "CURRENT PRICE",
                        value = formatDecimalPrice(target.decimalPrice),
                        modifier = Modifier.weight(1f),
                    )
                }
            }

            when {
                isLoading -> LoadingCard("Loading alternate under lines")
                errorMessage != null -> ErrorCard(errorMessage)
                lineGroups.isEmpty() -> EmptyCard("No under lines", "No under lines were found for this player and market.")
                else -> {
                    LazyColumn(
                        modifier = Modifier.fillMaxWidth(),
                        verticalArrangement = Arrangement.spacedBy(10.dp),
                    ) {
                        items(lineGroups, key = { (lineValue, _) -> "line:${lineValue ?: "null"}" }) { (lineValue, groupedRows) ->
                            Card(
                                modifier = Modifier.fillMaxWidth(),
                                colors = appCardColors(),
                                border = appGlassBorder(),
                            ) {
                                Column(
                                    modifier = Modifier.padding(14.dp),
                                    verticalArrangement = Arrangement.spacedBy(10.dp),
                                ) {
                                    Text(
                                        lineValue?.let(::formatLineValue)?.let { "Under $it" } ?: "Under",
                                        style = MaterialTheme.typography.titleMedium,
                                        fontWeight = FontWeight.SemiBold,
                                    )
                                    groupedRows.forEach { row ->
                                        Column(
                                            verticalArrangement = Arrangement.spacedBy(8.dp),
                                        ) {
                                            Row(
                                                modifier = Modifier.fillMaxWidth(),
                                                horizontalArrangement = Arrangement.SpaceBetween,
                                                verticalAlignment = Alignment.CenterVertically,
                                            ) {
                                                Text(
                                                    bookmakerLabel(row.bookmaker),
                                                    style = MaterialTheme.typography.titleSmall,
                                                    fontWeight = FontWeight.SemiBold,
                                                )
                                                Text(
                                                    formatDecimalPrice(row.decimalPrice),
                                                    style = MaterialTheme.typography.titleSmall,
                                                    fontWeight = FontWeight.Bold,
                                                )
                                            }
                                            Row(
                                                modifier = Modifier.fillMaxWidth(),
                                                horizontalArrangement = Arrangement.spacedBy(8.dp),
                                            ) {
                                                DeltaStatCell(
                                                    label = "DIFF 2025",
                                                    value = row.diff2025,
                                                    modifier = Modifier.weight(1f),
                                                )
                                                DeltaStatCell(
                                                    label = "DIFF L10",
                                                    value = row.diffLast10,
                                                    modifier = Modifier.weight(1f),
                                                )
                                                DeltaStatCell(
                                                    label = if (row.isBestPrice) "NEXT BEST" else "BEST GAP",
                                                    value = row.nextBestProbDiff,
                                                    modifier = Modifier.weight(1f),
                                                )
                                            }
                                            if (row.selectionId == target.selectionId) {
                                                InlineChip("Current row")
                                            }
                                        }
                                        if (row != groupedRows.last()) {
                                            HorizontalDivider()
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

@Composable
private fun MatchOddsCard(
    odds: OddsSearchResult,
) {
    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appGlassBorder(),
    ) {
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
                color = MaterialTheme.colorScheme.surfaceContainerHigh.copy(alpha = 0.92f),
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
                color = MaterialTheme.colorScheme.surfaceContainer.copy(alpha = 0.92f),
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

private fun togglePlayer(current: List<Int>, playerId: Int): List<Int> =
    if (current.contains(playerId)) {
        current.filterNot { it == playerId }
    } else {
        current + playerId
    }

private fun filterPlayersByQuery(players: List<PlayerSummary>, query: String): List<PlayerSummary> {
    val normalized = query.trim().lowercase(Locale.getDefault())
    if (normalized.isBlank()) return players
    return players.filter { it.fullName.lowercase(Locale.getDefault()).contains(normalized) }
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

private fun isDefaultDiffRange(min: Float, max: Float): Boolean =
    min == OddsDiffSliderMin && max == OddsDiffSliderMax

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

private fun formatSliderValue(value: Float): String =
    String.format(Locale.getDefault(), "%+.2f", value)
