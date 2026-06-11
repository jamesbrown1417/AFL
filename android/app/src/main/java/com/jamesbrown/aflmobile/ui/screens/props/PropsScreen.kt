package com.jamesbrown.aflmobile.ui.screens.props

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.ExperimentalLayoutApi
import androidx.compose.foundation.layout.FlowRow
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.heightIn
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.lazy.rememberLazyListState
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.automirrored.outlined.HelpOutline
import androidx.compose.material.icons.outlined.FilterList
import androidx.compose.material.icons.outlined.SwapVert
import androidx.compose.material3.Badge
import androidx.compose.material3.BadgedBox
import androidx.compose.material3.ButtonDefaults
import androidx.compose.material3.Card
import androidx.compose.material3.Checkbox
import androidx.compose.material3.DropdownMenu
import androidx.compose.material3.DropdownMenuItem
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.ExposedDropdownMenuAnchorType
import androidx.compose.material3.ExposedDropdownMenuBox
import androidx.compose.material3.ExposedDropdownMenuDefaults
import androidx.compose.material3.FilledTonalButton
import androidx.compose.material3.FilterChip
import androidx.compose.material3.FilterChipDefaults
import androidx.compose.material3.HorizontalDivider
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.ModalBottomSheet
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.PrimaryTabRow
import androidx.compose.material3.Scaffold
import androidx.compose.material3.Switch
import androidx.compose.material3.Tab
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.material3.TopAppBar
import androidx.compose.material3.pulltorefresh.PullToRefreshBox
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.runtime.snapshotFlow
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.semantics.heading
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.KeyboardType
import androidx.compose.ui.unit.dp
import androidx.lifecycle.ViewModel
import androidx.lifecycle.compose.collectAsStateWithLifecycle
import androidx.lifecycle.viewModelScope
import androidx.lifecycle.viewmodel.compose.viewModel
import com.jamesbrown.aflmobile.core.runCatchingCancellable
import com.jamesbrown.aflmobile.core.toUserMessage
import com.jamesbrown.aflmobile.data.repository.AflRepository
import com.jamesbrown.aflmobile.model.BookmakerSummary
import com.jamesbrown.aflmobile.model.EventSummary
import com.jamesbrown.aflmobile.model.MatchupDifficultyOptions
import com.jamesbrown.aflmobile.model.OddsDiffSliderMax
import com.jamesbrown.aflmobile.model.OddsDiffSliderMin
import com.jamesbrown.aflmobile.model.OddsFilters
import com.jamesbrown.aflmobile.model.OddsQuery
import com.jamesbrown.aflmobile.model.OddsSearchResult
import com.jamesbrown.aflmobile.model.PlayerSummary
import com.jamesbrown.aflmobile.model.QuickFilterPreset
import com.jamesbrown.aflmobile.model.applyQuickFilterPreset
import com.jamesbrown.aflmobile.model.hasActiveFilters
import com.jamesbrown.aflmobile.ui.common.DiffRangeSection
import com.jamesbrown.aflmobile.ui.common.EmptyCard
import com.jamesbrown.aflmobile.ui.common.ErrorCard
import com.jamesbrown.aflmobile.ui.common.InlineChip
import com.jamesbrown.aflmobile.ui.common.LoadingCard
import com.jamesbrown.aflmobile.ui.common.PlayerContextTags
import com.jamesbrown.aflmobile.ui.common.QuickFilterActionSection
import com.jamesbrown.aflmobile.ui.common.ScreenPadding
import com.jamesbrown.aflmobile.ui.common.WeatherContextTags
import com.jamesbrown.aflmobile.ui.common.appScreenInsets
import com.jamesbrown.aflmobile.ui.common.bookmakerLabel
import com.jamesbrown.aflmobile.ui.common.builder.MetricGlossarySheet
import com.jamesbrown.aflmobile.ui.common.builder.toPlayerLaunchRequest
import com.jamesbrown.aflmobile.ui.common.formatDecimalPrice
import com.jamesbrown.aflmobile.ui.common.formatLineValue
import com.jamesbrown.aflmobile.ui.common.formatSignedDelta
import com.jamesbrown.aflmobile.ui.common.simpleViewModelFactory
import com.jamesbrown.aflmobile.ui.navigation.PlayerLaunchRequest
import com.jamesbrown.aflmobile.ui.theme.AppTheme
import com.jamesbrown.aflmobile.ui.theme.appCardBorder
import com.jamesbrown.aflmobile.ui.theme.appCardColors
import com.jamesbrown.aflmobile.ui.theme.appTopBarColors
import com.jamesbrown.aflmobile.ui.theme.tabular
import java.util.Locale
import kotlinx.coroutines.Job
import kotlinx.coroutines.delay
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
    SortOption(sortBy = "diff_last_10", sortDirection = "desc", label = "Last-10 diff"),
    SortOption(sortBy = "next_best_prob_diff", sortDirection = "desc", label = "Next best diff"),
    SortOption(sortBy = "diff_2025", sortDirection = "desc", label = "Season diff"),
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
    val filters: OddsFilters = OddsFilters(),
    val defaultBookmakerCodes: List<String> = emptyList(),
    val odds: List<OddsSearchResult> = emptyList(),
    val hasMore: Boolean = false,
    val playerSearchResults: List<PlayerSummary> = emptyList(),
    val alternateUndersTarget: OddsSearchResult? = null,
    val alternateUnders: List<OddsSearchResult> = emptyList(),
    val isLoadingAlternateUnders: Boolean = false,
    val alternateUndersError: String? = null,
    val isLoading: Boolean = true,
    val isLoadingMore: Boolean = false,
    val errorMessage: String? = null,
)

class OddsViewModel(
    private val repository: AflRepository,
) : ViewModel() {
    private val _uiState = MutableStateFlow(OddsUiState())
    val uiState: StateFlow<OddsUiState> = _uiState.asStateFlow()
    private var loadJob: Job? = null
    private var playerSearchJob: Job? = null

    init {
        viewModelScope.launch {
            val bookmakers = runCatchingCancellable { repository.bookmakers() }.getOrNull()
            val events = runCatchingCancellable { repository.events(bookmaker = null, query = null) }
                .getOrDefault(emptyList())
            if (bookmakers == null) {
                _uiState.update {
                    it.copy(
                        isLoading = false,
                        errorMessage = "Couldn't load bookmakers from the backend. Pull down to retry.",
                    )
                }
                return@launch
            }
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
        val scopedFilters = if (scope == OddsScopePlayer) {
            currentFilters
        } else {
            currentFilters.copy(
                includePlayers = emptyList(),
                excludePlayers = emptyList(),
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
            )
        }
        refresh()
    }

    fun applyFilters(filters: OddsFilters) {
        _uiState.update { it.copy(filters = filters) }
        refresh()
    }

    fun refresh() {
        loadJob?.cancel()
        loadJob = viewModelScope.launch {
            val filters = uiState.value.filters
            _uiState.update {
                it.copy(
                    isLoading = true,
                    isLoadingMore = false,
                    errorMessage = null,
                )
            }
            runCatchingCancellable {
                repository.odds(filters.toQuery(limit = OddsPageSize + 1, offset = 0))
            }.onSuccess { odds ->
                _uiState.update {
                    it.copy(
                        odds = odds.take(OddsPageSize),
                        hasMore = odds.size > OddsPageSize,
                        isLoading = false,
                        isLoadingMore = false,
                    )
                }
            }.onFailure { error ->
                _uiState.update {
                    it.copy(
                        isLoading = false,
                        isLoadingMore = false,
                        errorMessage = error.toUserMessage("Failed to load odds."),
                    )
                }
            }
        }
    }

    /** Appends the next page using offset paging — earlier pages are never refetched. */
    fun loadMore() {
        val currentState = uiState.value
        if (currentState.isLoading || currentState.isLoadingMore || !currentState.hasMore) {
            return
        }
        _uiState.update { it.copy(isLoadingMore = true, errorMessage = null) }
        viewModelScope.launch {
            val filters = currentState.filters
            runCatchingCancellable {
                repository.odds(
                    filters.toQuery(limit = OddsPageSize + 1, offset = currentState.odds.size),
                )
            }.onSuccess { nextPage ->
                _uiState.update { state ->
                    val seen = state.odds.map { it.selectionId to it.bookmaker }.toSet()
                    val appended = nextPage
                        .take(OddsPageSize)
                        .filterNot { (it.selectionId to it.bookmaker) in seen }
                    state.copy(
                        odds = state.odds + appended,
                        hasMore = nextPage.size > OddsPageSize,
                        isLoadingMore = false,
                    )
                }
            }.onFailure { error ->
                _uiState.update {
                    it.copy(
                        isLoadingMore = false,
                        errorMessage = error.toUserMessage("Failed to load more odds."),
                    )
                }
            }
        }
    }

    /** Server-side, debounced player lookup for the include/exclude pickers. */
    fun searchFilterPlayers(query: String) {
        playerSearchJob?.cancel()
        playerSearchJob = viewModelScope.launch {
            delay(250)
            runCatchingCancellable { repository.searchPlayers(query, limit = 50) }
                .onSuccess { players ->
                    _uiState.update { it.copy(playerSearchResults = players) }
                }
        }
    }

    fun openAlternateUnders(odds: OddsSearchResult) {
        val player = odds.player ?: return
        val currentState = uiState.value
        val bookmakers = currentState.filters.bookmakerCodes
            .ifEmpty { currentState.defaultBookmakerCodes }
            .ifEmpty { listOf(odds.bookmaker) }
        viewModelScope.launch {
            _uiState.update {
                it.copy(
                    alternateUndersTarget = odds,
                    alternateUnders = emptyList(),
                    isLoadingAlternateUnders = true,
                    alternateUndersError = null,
                )
            }
            runCatchingCancellable {
                repository.odds(
                    OddsQuery(
                        bookmakers = bookmakers,
                        scope = OddsScopePlayer,
                        marketType = odds.marketTypeCode,
                        eventIds = listOf(odds.eventId),
                        includePlayerIds = listOf(player.id),
                        sortBy = "price",
                        sortDirection = "asc",
                        selectionType = "under",
                        limit = 200,
                    ),
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
                        alternateUndersError = error.toUserMessage("Failed to load alternate under lines."),
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

    private fun OddsFilters.toQuery(limit: Int, offset: Int): OddsQuery {
        val playerScoped = scope == OddsScopePlayer
        return OddsQuery(
            bookmakers = bookmakerCodes,
            scope = scope,
            marketType = marketTypeCode,
            eventIds = listOfNotNull(eventId),
            includePlayerIds = if (playerScoped) includePlayers.map { it.id } else emptyList(),
            excludePlayerIds = if (playerScoped) excludePlayers.map { it.id } else emptyList(),
            sortBy = sortBy,
            sortDirection = sortDirection,
            selectionType = if (playerScoped) selectionType else null,
            matchupDifficulties = if (playerScoped) matchupDifficulties else emptyList(),
            minPrice = if (playerScoped) minPriceText.toDoubleOrNull() else null,
            maxPrice = if (playerScoped) maxPriceText.toDoubleOrNull() else null,
            minDiff2025 = if (playerScoped) minDiff2025.toDouble() else null,
            maxDiff2025 = if (playerScoped) maxDiff2025.toDouble() else null,
            minDiffLast10 = if (playerScoped) minDiffLast10.toDouble() else null,
            maxDiffLast10 = if (playerScoped) maxDiffLast10.toDouble() else null,
            minNextBestProbDiff = if (playerScoped) minNextBestProbDiff.toDouble() else null,
            maxNextBestProbDiff = if (playerScoped) maxNextBestProbDiff.toDouble() else null,
            bestOnly = if (playerScoped) bestOnly else false,
            limit = limit,
            offset = offset,
        )
    }
}

@Composable
fun OddsRoute(
    repository: AflRepository,
    onOpenPlayerRequest: (PlayerLaunchRequest) -> Unit,
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
        onSearchFilterPlayers = viewModel::searchFilterPlayers,
        onOpenAlternateUnders = viewModel::openAlternateUnders,
        onDismissAlternateUnders = viewModel::closeAlternateUnders,
        onOpenPlayerRequest = onOpenPlayerRequest,
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
    onSearchFilterPlayers: (String) -> Unit,
    onOpenAlternateUnders: (OddsSearchResult) -> Unit,
    onDismissAlternateUnders: () -> Unit,
    onOpenPlayerRequest: (PlayerLaunchRequest) -> Unit,
) {
    var showFilters by remember { mutableStateOf(false) }
    var showSort by remember { mutableStateOf(false) }
    var showGlossary by remember { mutableStateOf(false) }
    var draftFilters by remember(uiState.filters) { mutableStateOf(uiState.filters) }
    val listState = rememberLazyListState()

    LaunchedEffect(showFilters, uiState.filters) {
        if (showFilters) {
            draftFilters = uiState.filters
        }
    }

    // Infinite scroll: request the next page when the user nears the end.
    LaunchedEffect(listState, uiState.hasMore, uiState.isLoading, uiState.isLoadingMore) {
        snapshotFlow {
            val layoutInfo = listState.layoutInfo
            val lastVisible = layoutInfo.visibleItemsInfo.lastOrNull()?.index ?: 0
            lastVisible to layoutInfo.totalItemsCount
        }.collect { (lastVisible, total) ->
            if (uiState.hasMore && !uiState.isLoading && !uiState.isLoadingMore && total > 0 && lastVisible >= total - 5) {
                onLoadMore()
            }
        }
    }

    Scaffold(
        containerColor = MaterialTheme.colorScheme.background,
        contentWindowInsets = appScreenInsets(),
        topBar = {
            TopAppBar(
                title = { Text("Odds") },
                colors = appTopBarColors(),
                actions = {
                    IconButton(onClick = { showGlossary = true }) {
                        Icon(Icons.AutoMirrored.Outlined.HelpOutline, contentDescription = "Metric glossary")
                    }
                    BadgedBox(
                        badge = {
                            if (uiState.filters.hasActiveFilters(uiState.defaultBookmakerCodes)) {
                                Badge()
                            }
                        },
                    ) {
                        IconButton(onClick = { showFilters = true }) {
                            Icon(Icons.Outlined.FilterList, contentDescription = "Open filters")
                        }
                    }
                    IconButton(onClick = { showSort = true }) {
                        Icon(Icons.Outlined.SwapVert, contentDescription = "Sort")
                    }
                },
            )
        },
    ) { innerPadding ->
        PullToRefreshBox(
            isRefreshing = uiState.isLoading,
            onRefresh = onRefresh,
            modifier = Modifier
                .fillMaxSize()
                .padding(innerPadding),
        ) {
            LazyColumn(
                state = listState,
                modifier = Modifier.fillMaxSize(),
                contentPadding = ScreenPadding,
                verticalArrangement = Arrangement.spacedBy(12.dp),
            ) {
                item {
                    PrimaryTabRow(
                        selectedTabIndex = if (uiState.filters.scope == OddsScopePlayer) 0 else 1,
                        containerColor = MaterialTheme.colorScheme.surface,
                        contentColor = MaterialTheme.colorScheme.tertiary,
                    ) {
                        Tab(
                            selected = uiState.filters.scope == OddsScopePlayer,
                            onClick = { onScopeSelected(OddsScopePlayer) },
                            selectedContentColor = MaterialTheme.colorScheme.tertiary,
                            unselectedContentColor = MaterialTheme.colorScheme.primary,
                            text = { Text("Player") },
                        )
                        Tab(
                            selected = uiState.filters.scope == OddsScopeMatch,
                            onClick = { onScopeSelected(OddsScopeMatch) },
                            selectedContentColor = MaterialTheme.colorScheme.tertiary,
                            unselectedContentColor = MaterialTheme.colorScheme.primary,
                            text = { Text("Match") },
                        )
                    }
                }

                item {
                    Row(
                        modifier = Modifier.fillMaxWidth(),
                        horizontalArrangement = Arrangement.SpaceBetween,
                        verticalAlignment = Alignment.CenterVertically,
                    ) {
                        Text(
                            if (uiState.filters.scope == OddsScopeMatch) "Match markets" else "Player props",
                            modifier = Modifier.semantics { heading() },
                            style = MaterialTheme.typography.titleMedium,
                            fontWeight = FontWeight.SemiBold,
                        )
                        Text(
                            "${uiState.odds.size}${if (uiState.hasMore) "+" else ""} rows",
                            style = MaterialTheme.typography.labelMedium.tabular,
                            color = MaterialTheme.colorScheme.onSurfaceVariant,
                        )
                    }
                }

                item {
                    ActiveFilterRow(
                        filters = uiState.filters,
                        bookmakers = uiState.bookmakers,
                        events = uiState.events,
                    )
                }

                uiState.errorMessage?.let { message ->
                    item { ErrorCard(message, onRetry = onRefresh) }
                }

                if (!uiState.isLoading && uiState.odds.isEmpty() && uiState.errorMessage == null) {
                    item {
                        EmptyCard(
                            title = "No odds",
                            body = if (uiState.filters.scope == OddsScopeMatch) {
                                "Change the match-market, agency, or match filters."
                            } else {
                                "Change the player market, agency, or match filters."
                            },
                            actionLabel = "Clear filters",
                            onAction = {
                                onApplyFilters(
                                    OddsFilters(
                                        scope = uiState.filters.scope,
                                        bookmakerCodes = uiState.defaultBookmakerCodes,
                                        sortBy = uiState.filters.sortBy,
                                        sortDirection = uiState.filters.sortDirection,
                                    ),
                                )
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
                        onOpenPlayerRequest = onOpenPlayerRequest,
                        modifier = Modifier.animateItem(),
                    )
                }

                if (uiState.isLoadingMore) {
                    item { LoadingCard("Loading more odds") }
                }
            }
        }

        if (showFilters) {
            OddsFilterSheet(
                filters = draftFilters,
                bookmakers = uiState.bookmakers,
                events = uiState.events,
                playerSearchResults = uiState.playerSearchResults,
                defaultBookmakers = uiState.defaultBookmakerCodes,
                onFiltersChanged = { draftFilters = it },
                onSearchPlayers = onSearchFilterPlayers,
                onApply = {
                    onApplyFilters(draftFilters)
                    showFilters = false
                },
                onApplyQuickFilter = { preset ->
                    draftFilters = preset
                    onApplyFilters(preset)
                    showFilters = false
                },
                onClearAll = {
                    val cleared = OddsFilters(
                        scope = draftFilters.scope,
                        bookmakerCodes = uiState.defaultBookmakerCodes,
                        sortBy = uiState.filters.sortBy,
                        sortDirection = uiState.filters.sortDirection,
                    )
                    draftFilters = cleared
                    onApplyFilters(cleared)
                    showFilters = false
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

        if (showGlossary) {
            MetricGlossarySheet(onDismiss = { showGlossary = false })
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

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun OddsSortSheet(
    filters: OddsFilters,
    onApply: (SortOption) -> Unit,
    onDismiss: () -> Unit,
) {
    ModalBottomSheet(
        onDismissRequest = onDismiss,
        containerColor = MaterialTheme.colorScheme.surface,
    ) {
        Column(
            modifier = Modifier
                .fillMaxWidth()
                .padding(horizontal = 20.dp, vertical = 8.dp),
            verticalArrangement = Arrangement.spacedBy(10.dp),
        ) {
            Text(
                "Sort by",
                modifier = Modifier.semantics { heading() },
                style = MaterialTheme.typography.headlineSmall,
            )
            sortOptionsForScope(filters.scope).forEach { option ->
                val selected = option.sortBy == filters.sortBy && option.sortDirection == filters.sortDirection
                FilledTonalButton(
                    onClick = { onApply(option) },
                    modifier = Modifier.fillMaxWidth(),
                    colors = ButtonDefaults.filledTonalButtonColors(
                        containerColor = if (selected) {
                            MaterialTheme.colorScheme.tertiaryContainer
                        } else {
                            MaterialTheme.colorScheme.secondaryContainer
                        },
                        contentColor = if (selected) {
                            MaterialTheme.colorScheme.tertiary
                        } else {
                            MaterialTheme.colorScheme.primary
                        },
                    ),
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
            androidx.compose.foundation.layout.Spacer(modifier = Modifier.heightIn(min = 8.dp))
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
        InlineChip("Market: ${marketLabel(filters.marketTypeCode, filters.scope)}")
        InlineChip("Agency: $agencyLabel")
        InlineChip("Match: $matchLabel")
        InlineChip("Sort: ${sortLabel(filters)}")
        if (filters.scope == OddsScopePlayer && filters.selectionType != null) {
            InlineChip("Side: ${filters.selectionType.replaceFirstChar { it.titlecase(Locale.getDefault()) }}")
        }
        if (filters.scope == OddsScopePlayer && filters.matchupDifficulties.isNotEmpty()) {
            InlineChip("Matchup: ${filters.matchupDifficulties.joinToString("/")}")
        }
        if (filters.scope == OddsScopePlayer && filters.includePlayers.isNotEmpty()) {
            InlineChip("Include: ${filters.includePlayers.size}")
        }
        if (filters.scope == OddsScopePlayer && filters.excludePlayers.isNotEmpty()) {
            InlineChip("Exclude: ${filters.excludePlayers.size}")
        }
        if (filters.minPriceText.isNotBlank() || filters.maxPriceText.isNotBlank()) {
            InlineChip("Odds: ${filters.minPriceText.ifBlank { "-" }} to ${filters.maxPriceText.ifBlank { "-" }}")
        }
        if (!isDefaultDiffRange(filters.minDiffLast10, filters.maxDiffLast10)) {
            InlineChip("L10: ${formatSliderValue(filters.minDiffLast10)} to ${formatSliderValue(filters.maxDiffLast10)}")
        }
        if (!isDefaultDiffRange(filters.minDiff2025, filters.maxDiff2025)) {
            InlineChip("Season: ${formatSliderValue(filters.minDiff2025)} to ${formatSliderValue(filters.maxDiff2025)}")
        }
        if (!isDefaultDiffRange(filters.minNextBestProbDiff, filters.maxNextBestProbDiff)) {
            InlineChip("NB: ${formatSliderValue(filters.minNextBestProbDiff)} to ${formatSliderValue(filters.maxNextBestProbDiff)}")
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
    playerSearchResults: List<PlayerSummary>,
    defaultBookmakers: List<String>,
    onFiltersChanged: (OddsFilters) -> Unit,
    onSearchPlayers: (String) -> Unit,
    onApply: () -> Unit,
    onApplyQuickFilter: (OddsFilters) -> Unit,
    onClearAll: () -> Unit,
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
        containerColor = MaterialTheme.colorScheme.surface,
    ) {
        Column(
            modifier = Modifier
                .fillMaxWidth()
                .verticalScroll(rememberScrollState())
                .padding(horizontal = 20.dp, vertical = 8.dp),
            verticalArrangement = Arrangement.spacedBy(18.dp),
        ) {
            Text(
                "Filters",
                modifier = Modifier.semantics { heading() },
                style = MaterialTheme.typography.headlineSmall,
            )

            if (filters.scope == OddsScopePlayer) {
                QuickFilterActionSection(
                    onSelectPreset = { preset ->
                        onApplyQuickFilter(filters.applyQuickFilterPreset(preset))
                    },
                )
            }

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
                            colors = oddsFilterChipColors(),
                            border = oddsFilterChipBorder(filters.bookmakerCodes.contains(bookmaker.code)),
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
                                colors = oddsFilterChipColors(),
                                border = oddsFilterChipBorder(filters.selectionType == option.code),
                            )
                        }
                    }
                }

                Column(verticalArrangement = Arrangement.spacedBy(10.dp)) {
                    Text("Matchup difficulty", style = MaterialTheme.typography.titleMedium)
                    FlowRow(
                        horizontalArrangement = Arrangement.spacedBy(8.dp),
                        verticalArrangement = Arrangement.spacedBy(8.dp),
                    ) {
                        MatchupDifficultyOptions.forEach { difficulty ->
                            val selected = filters.matchupDifficulties.contains(difficulty)
                            FilterChip(
                                selected = selected,
                                onClick = {
                                    onFiltersChanged(
                                        filters.copy(
                                            matchupDifficulties = if (selected) {
                                                filters.matchupDifficulties - difficulty
                                            } else {
                                                filters.matchupDifficulties + difficulty
                                            },
                                        ),
                                    )
                                },
                                label = { Text(difficulty) },
                                colors = oddsFilterChipColors(),
                                border = oddsFilterChipBorder(selected),
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
                        onSearchPlayers(it)
                    },
                    expanded = includeExpanded,
                    onExpandedChange = { includeExpanded = !includeExpanded },
                    players = playerSearchResults,
                    selectedIds = filters.includePlayers.map { it.id },
                    onTogglePlayer = { player ->
                        onFiltersChanged(
                            filters.copy(
                                includePlayers = togglePlayer(filters.includePlayers, player),
                                excludePlayers = filters.excludePlayers.filterNot { it.id == player.id },
                            ),
                        )
                    },
                )

                SelectedPlayerChipRow(
                    label = "Including",
                    players = filters.includePlayers,
                    onRemove = { playerId ->
                        onFiltersChanged(filters.copy(includePlayers = filters.includePlayers.filterNot { it.id == playerId }))
                    },
                )

                PlayerMultiSelectDropdown(
                    label = "Player exclude",
                    query = excludeQuery,
                    onQueryChanged = {
                        excludeQuery = it
                        excludeExpanded = true
                        onSearchPlayers(it)
                    },
                    expanded = excludeExpanded,
                    onExpandedChange = { excludeExpanded = !excludeExpanded },
                    players = playerSearchResults,
                    selectedIds = filters.excludePlayers.map { it.id },
                    onTogglePlayer = { player ->
                        onFiltersChanged(
                            filters.copy(
                                excludePlayers = togglePlayer(filters.excludePlayers, player),
                                includePlayers = filters.includePlayers.filterNot { it.id == player.id },
                            ),
                        )
                    },
                )

                SelectedPlayerChipRow(
                    label = "Excluding",
                    players = filters.excludePlayers,
                    onRemove = { playerId ->
                        onFiltersChanged(filters.copy(excludePlayers = filters.excludePlayers.filterNot { it.id == playerId }))
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
                        keyboardOptions = KeyboardOptions(keyboardType = KeyboardType.Decimal),
                    )
                    OutlinedTextField(
                        value = filters.maxPriceText,
                        onValueChange = { onFiltersChanged(filters.copy(maxPriceText = it)) },
                        modifier = Modifier.weight(1f),
                        singleLine = true,
                        label = { Text("Max odds") },
                        keyboardOptions = KeyboardOptions(keyboardType = KeyboardType.Decimal),
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
                    title = "Season diff",
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

                DiffRangeSection(
                    title = "Next best diff",
                    range = filters.minNextBestProbDiff..filters.maxNextBestProbDiff,
                    onRangeChange = { range ->
                        onFiltersChanged(
                            filters.copy(
                                minNextBestProbDiff = range.start,
                                maxNextBestProbDiff = range.endInclusive,
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
                    onClick = onClearAll,
                    modifier = Modifier.weight(1f),
                ) {
                    Text("Clear all")
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
            placeholder = { Text("Type to search players") },
            trailingIcon = {
                ExposedDropdownMenuDefaults.TrailingIcon(expanded = expanded)
            },
        )
        DropdownMenu(
            expanded = expanded,
            onDismissRequest = onExpandedChange,
            modifier = Modifier.heightIn(max = 360.dp),
        ) {
            players.forEach { player ->
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
                    text = { Text(if (query.isBlank()) "Type to search players" else "No matching players") },
                    onClick = {},
                    enabled = false,
                )
            }
        }
    }
}

@OptIn(ExperimentalLayoutApi::class)
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
                    colors = oddsFilterChipColors(),
                    border = oddsFilterChipBorder(true),
                )
            }
        }
    }
}

@Composable
private fun OddsCard(
    odds: OddsSearchResult,
    scope: String,
    onOpenAlternateUnders: (OddsSearchResult) -> Unit,
    onOpenPlayerRequest: (PlayerLaunchRequest) -> Unit,
    modifier: Modifier = Modifier,
) {
    if (scope == OddsScopeMatch) {
        MatchOddsCard(odds = odds, modifier = modifier)
        return
    }
    PlayerOddsCard(
        odds = odds,
        onOpenAlternateUnders = onOpenAlternateUnders,
        onOpenPlayerRequest = onOpenPlayerRequest,
        modifier = modifier,
    )
}

@Composable
private fun PlayerOddsCard(
    odds: OddsSearchResult,
    onOpenAlternateUnders: (OddsSearchResult) -> Unit,
    onOpenPlayerRequest: (PlayerLaunchRequest) -> Unit,
    modifier: Modifier = Modifier,
) {
    Card(
        modifier = modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appCardBorder(),
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
                        modifier = Modifier.clickable(enabled = odds.player != null) {
                            odds.toPlayerLaunchRequest()?.let(onOpenPlayerRequest)
                        },
                        style = MaterialTheme.typography.titleMedium,
                        fontWeight = FontWeight.SemiBold,
                    )
                    Text(
                        text = odds.marketDisplayName,
                        style = MaterialTheme.typography.labelLarge,
                        color = MaterialTheme.colorScheme.primary,
                    )
                    PlayerContextTags(
                        position = odds.playerPosition,
                        matchupDifficulty = odds.matchupDifficulty,
                    )
                    WeatherContextTags(weather = odds.weather)
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
                    label = "SEASON",
                    value = odds.diff2025,
                    modifier = Modifier.weight(1f),
                )
                DeltaStatCell(
                    label = "LAST 10",
                    value = odds.diffLast10,
                    modifier = Modifier.weight(1f),
                )
                DeltaStatCell(
                    label = "NEXT BEST",
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
        containerColor = MaterialTheme.colorScheme.surface,
    ) {
        Column(
            modifier = Modifier
                .fillMaxWidth()
                .padding(horizontal = 20.dp, vertical = 8.dp),
            verticalArrangement = Arrangement.spacedBy(14.dp),
        ) {
            Text(
                "All under lines",
                modifier = Modifier.semantics { heading() },
                style = MaterialTheme.typography.headlineSmall,
            )
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
                border = appCardBorder(),
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
                                border = appCardBorder(),
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
                                                    style = MaterialTheme.typography.titleSmall.tabular,
                                                    fontWeight = FontWeight.Bold,
                                                )
                                            }
                                            Row(
                                                modifier = Modifier.fillMaxWidth(),
                                                horizontalArrangement = Arrangement.spacedBy(8.dp),
                                            ) {
                                                DeltaStatCell(
                                                    label = "SEASON",
                                                    value = row.diff2025,
                                                    modifier = Modifier.weight(1f),
                                                )
                                                DeltaStatCell(
                                                    label = "LAST 10",
                                                    value = row.diffLast10,
                                                    modifier = Modifier.weight(1f),
                                                )
                                                DeltaStatCell(
                                                    label = "NEXT BEST",
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
    modifier: Modifier = Modifier,
) {
    Card(
        modifier = modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appCardBorder(),
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
                WeatherContextTags(weather = odds.weather)
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
                color = MaterialTheme.colorScheme.surfaceContainerHigh,
                shape = MaterialTheme.shapes.small,
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
            style = MaterialTheme.typography.titleSmall.tabular,
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
    val colors = AppTheme.colors
    val display = value?.let(::formatSignedDelta) ?: "-"
    val tone = when {
        value == null -> MaterialTheme.colorScheme.onSurface
        value > 0 -> colors.positive
        value < 0 -> colors.negative
        else -> MaterialTheme.colorScheme.onSurface
    }
    Column(
        modifier = modifier
            .background(
                color = MaterialTheme.colorScheme.surfaceContainer,
                shape = MaterialTheme.shapes.small,
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
            style = MaterialTheme.typography.titleSmall.tabular,
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

private fun togglePlayer(current: List<PlayerSummary>, player: PlayerSummary): List<PlayerSummary> =
    if (current.any { it.id == player.id }) {
        current.filterNot { it.id == player.id }
    } else {
        current + player
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

private fun formatSliderValue(value: Float): String =
    String.format(Locale.getDefault(), "%+.2f", value)

@Composable
private fun oddsFilterChipColors() = FilterChipDefaults.filterChipColors(
    containerColor = MaterialTheme.colorScheme.secondaryContainer,
    labelColor = MaterialTheme.colorScheme.primary,
    selectedContainerColor = MaterialTheme.colorScheme.tertiary,
    selectedLabelColor = MaterialTheme.colorScheme.onTertiary,
)

@Composable
private fun oddsFilterChipBorder(selected: Boolean) = FilterChipDefaults.filterChipBorder(
    enabled = true,
    selected = selected,
    borderColor = MaterialTheme.colorScheme.outlineVariant,
    selectedBorderColor = MaterialTheme.colorScheme.tertiary,
)
