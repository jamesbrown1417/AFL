package com.jamesbrown.aflmobile.ui.screens.props

import androidx.compose.foundation.Canvas
import androidx.compose.foundation.background
import androidx.compose.foundation.gestures.detectTapGestures
import androidx.compose.foundation.horizontalScroll
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.ExperimentalLayoutApi
import androidx.compose.foundation.layout.FlowRow
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.heightIn
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.text.KeyboardActions
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Close
import androidx.compose.material.icons.outlined.FilterList
import androidx.compose.material3.Badge
import androidx.compose.material3.BadgedBox
import androidx.compose.material3.Card
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
import androidx.compose.material3.Scaffold
import androidx.compose.material3.SegmentedButton
import androidx.compose.material3.SegmentedButtonDefaults
import androidx.compose.material3.SingleChoiceSegmentedButtonRow
import androidx.compose.material3.SnackbarHost
import androidx.compose.material3.SnackbarHostState
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.material3.TopAppBar
import androidx.compose.material3.pulltorefresh.PullToRefreshBox
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.Path
import androidx.compose.ui.graphics.PathEffect
import androidx.compose.ui.graphics.drawscope.DrawScope
import androidx.compose.ui.graphics.drawscope.Stroke
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.layout.onSizeChanged
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.platform.LocalFocusManager
import androidx.compose.ui.semantics.contentDescription
import androidx.compose.ui.semantics.heading
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.text.TextMeasurer
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.drawText
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.ImeAction
import androidx.compose.ui.text.input.KeyboardCapitalization
import androidx.compose.ui.text.rememberTextMeasurer
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.IntSize
import androidx.compose.ui.unit.dp
import androidx.lifecycle.ViewModel
import androidx.lifecycle.compose.collectAsStateWithLifecycle
import androidx.lifecycle.viewModelScope
import androidx.lifecycle.viewmodel.compose.viewModel
import com.jamesbrown.aflmobile.core.runCatchingCancellable
import com.jamesbrown.aflmobile.core.toUserMessage
import com.jamesbrown.aflmobile.data.repository.AflRepository
import com.jamesbrown.aflmobile.model.PlayerGameLogEntry
import com.jamesbrown.aflmobile.model.PlayerStatFilterOptions
import com.jamesbrown.aflmobile.model.PlayerStatSummary
import com.jamesbrown.aflmobile.model.PlayerStatsFilters
import com.jamesbrown.aflmobile.model.PlayerSummary
import com.jamesbrown.aflmobile.ui.common.EmptyCard
import com.jamesbrown.aflmobile.ui.common.ErrorCard
import com.jamesbrown.aflmobile.ui.common.InlineChip
import com.jamesbrown.aflmobile.ui.common.LoadingCard
import com.jamesbrown.aflmobile.ui.common.ScreenPadding
import com.jamesbrown.aflmobile.ui.common.StepperField
import com.jamesbrown.aflmobile.ui.common.appScreenInsets
import com.jamesbrown.aflmobile.ui.common.formatDateTime
import com.jamesbrown.aflmobile.ui.common.formatDecimalPrice
import com.jamesbrown.aflmobile.ui.common.formatPercentage
import com.jamesbrown.aflmobile.ui.common.simpleViewModelFactory
import com.jamesbrown.aflmobile.ui.navigation.PlayerLaunchRequest
import com.jamesbrown.aflmobile.ui.theme.AppTheme
import com.jamesbrown.aflmobile.ui.theme.appCardBorder
import com.jamesbrown.aflmobile.ui.theme.appCardColors
import com.jamesbrown.aflmobile.ui.theme.appTopBarColors
import com.jamesbrown.aflmobile.ui.theme.tabular
import java.time.LocalDateTime
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import java.util.Locale
import kotlinx.coroutines.Job
import kotlinx.coroutines.async
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.flow.update
import kotlinx.coroutines.launch
import kotlin.math.roundToInt


data class ComparisonScenarioState(
    val filters: PlayerStatsFilters = PlayerStatsFilters(),
    val history: List<PlayerGameLogEntry> = emptyList(),
    val summary: PlayerStatSummary? = null,
    val isLoading: Boolean = false,
    val errorMessage: String? = null,
    val infoMessage: String? = null,
    /** Filters the loaded history/summary correspond to; used to skip redundant loads. */
    val loadedFilters: PlayerStatsFilters? = null,
)

data class PlayerStatsUiState(
    val searchQuery: String = "",
    val searchResults: List<PlayerSummary> = emptyList(),
    val selectedPlayer: PlayerSummary? = null,
    val filterOptions: PlayerStatFilterOptions? = null,
    val filters: PlayerStatsFilters = PlayerStatsFilters(),
    val history: List<PlayerGameLogEntry> = emptyList(),
    val summary: PlayerStatSummary? = null,
    /** Backend-narrowed venue options for the filter sheet; null = use the full list. */
    val availableVenues: List<String>? = null,
    val scenarioA: ComparisonScenarioState = ComparisonScenarioState(),
    val scenarioB: ComparisonScenarioState = ComparisonScenarioState(),
    val isLoading: Boolean = true,
    val errorMessage: String? = null,
    val infoMessage: String? = null,
)

class PlayerStatsViewModel(
    private val repository: AflRepository,
) : ViewModel() {
    private val _uiState = MutableStateFlow(PlayerStatsUiState())
    val uiState: StateFlow<PlayerStatsUiState> = _uiState.asStateFlow()
    private var pendingLaunchRequest: PlayerLaunchRequest? = null
    private var searchJob: Job? = null
    private var loadJob: Job? = null
    private var comparisonJob: Job? = null
    private var venueOptionsJob: Job? = null
    private var bootstrapped = false

    init {
        bootstrap()
    }

    private fun bootstrap() {
        viewModelScope.launch {
            val lastViewed = runCatchingCancellable { repository.lastViewedPlayer() }.getOrNull()
            val initialPlayers = runCatchingCancellable { repository.searchStatPlayers("", limit = 50) }
                .getOrDefault(emptyList())
            val selected = lastViewed ?: initialPlayers.firstOrNull()
            bootstrapped = true
            _uiState.update {
                it.copy(
                    searchQuery = selected?.fullName.orEmpty(),
                    searchResults = initialPlayers,
                    selectedPlayer = selected,
                )
            }
            pendingLaunchRequest?.let { request ->
                pendingLaunchRequest = null
                handleLaunchRequest(request)
                return@launch
            }
            if (selected != null) {
                loadPlayer(selected)
            } else {
                _uiState.update {
                    it.copy(
                        isLoading = false,
                        errorMessage = "Couldn't load any players from the backend. Pull down to retry.",
                    )
                }
            }
        }
    }

    fun updateSearchQuery(query: String) {
        _uiState.update { state ->
            state.copy(searchQuery = query, errorMessage = null)
        }
        searchJob?.cancel()
        searchJob = viewModelScope.launch {
            delay(250)
            runCatchingCancellable { repository.searchStatPlayers(query, limit = 50) }
                .onSuccess { players ->
                    _uiState.update { it.copy(searchResults = players) }
                }
        }
    }

    fun selectPlayer(player: PlayerSummary) {
        _uiState.update {
            it.copy(
                selectedPlayer = player,
                searchQuery = player.fullName,
            )
        }
        viewModelScope.launch {
            runCatchingCancellable { repository.saveLastViewedPlayer(player) }
        }
        loadPlayer(player)
    }

    fun handleLaunchRequest(request: PlayerLaunchRequest) {
        if (!bootstrapped) {
            pendingLaunchRequest = request
            return
        }
        val player = PlayerSummary(id = request.playerId, fullName = request.playerName)
        _uiState.update {
            it.copy(
                selectedPlayer = player,
                searchQuery = player.fullName,
            )
        }
        viewModelScope.launch {
            runCatchingCancellable { repository.saveLastViewedPlayer(player) }
        }
        loadPlayer(player, request)
    }

    private fun loadPlayer(player: PlayerSummary, launchRequest: PlayerLaunchRequest? = null) {
        loadJob?.cancel()
        loadJob = viewModelScope.launch {
            _uiState.update {
                it.copy(
                    isLoading = true,
                    errorMessage = null,
                    infoMessage = null,
                    summary = null,
                    history = emptyList(),
                    availableVenues = null,
                )
            }
            runCatchingCancellable { repository.playerStatFilters(player.id) }
                .onSuccess { options ->
                    val defaults = defaultPlayerStatsFilters(options)
                    val filters = filtersForLaunchRequest(
                        options = options,
                        defaults = defaults,
                        request = launchRequest,
                    )
                    _uiState.update {
                        it.copy(
                            filterOptions = options,
                            filters = filters,
                            scenarioA = ComparisonScenarioState(filters = filters),
                            scenarioB = ComparisonScenarioState(filters = filters),
                        )
                    }
                    loadHistoryAndSummary(player.id, filters)
                }
                .onFailure { error ->
                    _uiState.update {
                        it.copy(
                            isLoading = false,
                            errorMessage = error.toUserMessage("Failed to load player filters."),
                        )
                    }
                }
        }
    }

    fun applyFilters(filters: PlayerStatsFilters) {
        _uiState.update { it.copy(filters = filters) }
        refresh()
    }

    fun refresh() {
        val player = uiState.value.selectedPlayer ?: return
        val filters = uiState.value.filters
        loadJob?.cancel()
        loadJob = viewModelScope.launch {
            _uiState.update { it.copy(isLoading = true, errorMessage = null) }
            loadHistoryAndSummary(player.id, filters)
        }
    }

    private suspend fun loadHistoryAndSummary(playerId: Int, filters: PlayerStatsFilters) {
        val historyResult = runCatchingCancellable { repository.playerStatHistory(playerId, filters) }
        val summaryResult = if (filters.canRequestSummary()) {
            runCatchingCancellable { repository.playerStatSummary(playerId, filters) }
        } else {
            Result.success(null)
        }

        historyResult
            .onSuccess { history ->
                _uiState.update {
                    it.copy(
                        history = history,
                        summary = summaryResult.getOrNull(),
                        isLoading = false,
                        infoMessage = playerSummaryInfoMessage(filters),
                    )
                }
            }
            .onFailure { error ->
                _uiState.update {
                    it.copy(
                        isLoading = false,
                        errorMessage = error.toUserMessage("Failed to load player history."),
                    )
                }
            }
    }

    /**
     * Refreshes the venue option list for the filter sheet from the backend,
     * debounced because it re-runs as the user edits other filters.
     */
    fun refreshVenueOptions(filters: PlayerStatsFilters) {
        val playerId = uiState.value.selectedPlayer?.id ?: return
        venueOptionsJob?.cancel()
        venueOptionsJob = viewModelScope.launch {
            delay(300)
            runCatchingCancellable { repository.playerVenueOptions(playerId, filters) }
                .onSuccess { venues ->
                    _uiState.update {
                        it.copy(availableVenues = venues.takeIf { list -> list.isNotEmpty() })
                    }
                }
        }
    }

    fun setScenarioFilters(scenario: PlayerComparisonFocus, filters: PlayerStatsFilters) {
        _uiState.update {
            when (scenario) {
                PlayerComparisonFocus.ScenarioA -> it.copy(scenarioA = it.scenarioA.copy(filters = filters))
                PlayerComparisonFocus.ScenarioB -> it.copy(scenarioB = it.scenarioB.copy(filters = filters))
            }
        }
        loadComparison(force = false)
    }

    fun applySharedComparisonControls(shared: PlayerStatsFilters) {
        _uiState.update {
            it.copy(
                scenarioA = it.scenarioA.copy(filters = mergeSharedComparisonFilters(it.scenarioA.filters, shared)),
                scenarioB = it.scenarioB.copy(filters = mergeSharedComparisonFilters(it.scenarioB.filters, shared)),
            )
        }
        loadComparison(force = false)
    }

    /**
     * Loads both scenarios in parallel. Skipped when the loaded data already
     * matches the current filters (e.g. re-entering the tab), unless [force].
     */
    fun loadComparison(force: Boolean) {
        val playerId = uiState.value.selectedPlayer?.id ?: return
        val state = uiState.value
        val scenarioAStale = force || state.scenarioA.loadedFilters != state.scenarioA.filters
        val scenarioBStale = force || state.scenarioB.loadedFilters != state.scenarioB.filters
        if (!scenarioAStale && !scenarioBStale) return

        comparisonJob?.cancel()
        comparisonJob = viewModelScope.launch {
            _uiState.update {
                it.copy(
                    scenarioA = if (scenarioAStale) it.scenarioA.copy(isLoading = true, errorMessage = null) else it.scenarioA,
                    scenarioB = if (scenarioBStale) it.scenarioB.copy(isLoading = true, errorMessage = null) else it.scenarioB,
                )
            }
            coroutineScope {
                val filtersA = uiState.value.scenarioA.filters
                val filtersB = uiState.value.scenarioB.filters
                val deferredA = if (scenarioAStale) async { loadScenario(playerId, filtersA) } else null
                val deferredB = if (scenarioBStale) async { loadScenario(playerId, filtersB) } else null
                deferredA?.await()?.let { result ->
                    _uiState.update { it.copy(scenarioA = result) }
                }
                deferredB?.await()?.let { result ->
                    _uiState.update { it.copy(scenarioB = result) }
                }
            }
        }
    }

    private suspend fun loadScenario(
        playerId: Int,
        filters: PlayerStatsFilters,
    ): ComparisonScenarioState {
        val historyResult = runCatchingCancellable { repository.playerStatHistory(playerId, filters) }
        val summaryResult = if (filters.canRequestSummary()) {
            runCatchingCancellable { repository.playerStatSummary(playerId, filters) }
        } else {
            Result.success(null)
        }
        return historyResult.fold(
            onSuccess = { history ->
                ComparisonScenarioState(
                    filters = filters,
                    history = history,
                    summary = summaryResult.getOrNull(),
                    infoMessage = playerSummaryInfoMessage(filters),
                    loadedFilters = filters,
                )
            },
            onFailure = { error ->
                ComparisonScenarioState(
                    filters = filters,
                    errorMessage = error.toUserMessage("Failed to load comparison scenario."),
                    infoMessage = playerSummaryInfoMessage(filters),
                    loadedFilters = filters,
                )
            },
        )
    }
}

private fun playerSummaryInfoMessage(filters: PlayerStatsFilters): String? = when {
    filters.lineMode == "interval" && !filters.canRequestSummary() ->
        "Set both interval bounds to calculate implied prices."
    filters.lineMode != "interval" && !filters.canRequestSummary() ->
        "Set a reference line to calculate over/under implied prices."
    else -> null
}

private enum class PlayerViewMode {
    Table,
    Graph,
}

private enum class PlayerSubtab {
    Stats,
    Comparison,
}

private enum class PlayerFilterTarget {
    Stats,
    ScenarioA,
    ScenarioB,
}

private enum class PlayerComparisonViewMode {
    Table,
    Graph,
    GameLog,
}

enum class PlayerComparisonFocus {
    ScenarioA,
    ScenarioB,
}

@Composable
fun PlayerStatsRoute(
    repository: AflRepository,
    launchRequest: PlayerLaunchRequest?,
) {
    val viewModel: PlayerStatsViewModel = viewModel(
        factory = simpleViewModelFactory { PlayerStatsViewModel(repository) },
    )
    val uiState by viewModel.uiState.collectAsStateWithLifecycle()
    LaunchedEffect(launchRequest?.requestId) {
        launchRequest?.let(viewModel::handleLaunchRequest)
    }
    PlayerStatsScreen(
        uiState = uiState,
        onSearchQueryChanged = viewModel::updateSearchQuery,
        onSelectPlayer = viewModel::selectPlayer,
        onApplyFilters = viewModel::applyFilters,
        onSetScenarioFilters = viewModel::setScenarioFilters,
        onApplySharedComparisonControls = viewModel::applySharedComparisonControls,
        onLoadComparison = viewModel::loadComparison,
        onRefreshVenueOptions = viewModel::refreshVenueOptions,
        onRefresh = viewModel::refresh,
    )
}

@OptIn(ExperimentalMaterial3Api::class, ExperimentalLayoutApi::class)
@Composable
private fun PlayerStatsScreen(
    uiState: PlayerStatsUiState,
    onSearchQueryChanged: (String) -> Unit,
    onSelectPlayer: (PlayerSummary) -> Unit,
    onApplyFilters: (PlayerStatsFilters) -> Unit,
    onSetScenarioFilters: (PlayerComparisonFocus, PlayerStatsFilters) -> Unit,
    onApplySharedComparisonControls: (PlayerStatsFilters) -> Unit,
    onLoadComparison: (Boolean) -> Unit,
    onRefreshVenueOptions: (PlayerStatsFilters) -> Unit,
    onRefresh: () -> Unit,
) {
    var activeTab by rememberSaveable { mutableStateOf(PlayerSubtab.Stats.name) }
    var activeFilterTarget by remember { mutableStateOf<PlayerFilterTarget?>(null) }
    var draftFilters by remember(uiState.filters) { mutableStateOf(uiState.filters) }
    var viewMode by rememberSaveable { mutableStateOf(PlayerViewMode.Table.name) }
    var comparisonViewMode by rememberSaveable { mutableStateOf(PlayerComparisonViewMode.Table.name) }
    var comparisonFocus by rememberSaveable { mutableStateOf(PlayerComparisonFocus.ScenarioA.name) }
    val snackbarHostState = remember { SnackbarHostState() }
    val isStatsTab = PlayerSubtab.valueOf(activeTab) == PlayerSubtab.Stats
    val scenarioA = uiState.scenarioA
    val scenarioB = uiState.scenarioB
    val activeFilterCount = remember(uiState.filters, uiState.filterOptions) {
        activePlayerFilterCount(uiState.filters, uiState.filterOptions)
    }

    LaunchedEffect(activeFilterTarget, uiState.filters, scenarioA.filters, scenarioB.filters) {
        draftFilters = when (activeFilterTarget) {
            PlayerFilterTarget.Stats -> uiState.filters
            PlayerFilterTarget.ScenarioA -> scenarioA.filters
            PlayerFilterTarget.ScenarioB -> scenarioB.filters
            null -> draftFilters
        }
    }

    LaunchedEffect(activeTab, uiState.selectedPlayer?.id) {
        if (!isStatsTab) {
            onLoadComparison(false)
        }
    }

    // While a filter sheet is open, keep the venue options in sync with the
    // other draft filters. The backend computes the narrowed list with the
    // same pipeline as the history endpoint; the call itself is debounced in
    // the ViewModel.
    LaunchedEffect(activeFilterTarget, draftFilters) {
        if (activeFilterTarget != null) {
            onRefreshVenueOptions(draftFilters)
        }
    }

    LaunchedEffect(uiState.infoMessage) {
        uiState.infoMessage?.let { snackbarHostState.showSnackbar(it) }
    }

    Scaffold(
        containerColor = MaterialTheme.colorScheme.background,
        contentWindowInsets = appScreenInsets(),
        snackbarHost = { SnackbarHost(snackbarHostState) },
        topBar = {
            TopAppBar(
                title = { Text("Player stats") },
                colors = appTopBarColors(),
                actions = {
                    if (isStatsTab) {
                        IconButton(onClick = { activeFilterTarget = PlayerFilterTarget.Stats }) {
                            BadgedBox(
                                badge = {
                                    if (activeFilterCount > 0) {
                                        Badge { Text(activeFilterCount.toString()) }
                                    }
                                },
                            ) {
                                Icon(Icons.Outlined.FilterList, contentDescription = "Filters")
                            }
                        }
                    }
                },
            )
        },
    ) { innerPadding ->
        PullToRefreshBox(
            isRefreshing = if (isStatsTab) uiState.isLoading else (scenarioA.isLoading || scenarioB.isLoading),
            onRefresh = {
                if (isStatsTab) onRefresh() else onLoadComparison(true)
            },
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
                    PlayerSearchCard(
                        uiState = uiState,
                        onSearchQueryChanged = onSearchQueryChanged,
                        onSelectPlayer = onSelectPlayer,
                    )
                }

                item {
                    SegmentedToggle(
                        options = listOf("Stats", "Comparison"),
                        selectedIndex = if (isStatsTab) 0 else 1,
                        onSelected = { index ->
                            activeTab = if (index == 0) PlayerSubtab.Stats.name else PlayerSubtab.Comparison.name
                        },
                    )
                }

                uiState.selectedPlayer?.let { selectedPlayer ->
                    if (isStatsTab) {
                        item {
                            PlayerStatsFilterSummary(
                                playerName = selectedPlayer.fullName,
                                filters = uiState.filters,
                                filterOptions = uiState.filterOptions,
                                onEditFilters = { activeFilterTarget = PlayerFilterTarget.Stats },
                                onApplyFilters = onApplyFilters,
                            )
                        }
                    } else {
                        item {
                            ComparisonSharedControlsCard(
                                filterOptions = uiState.filterOptions,
                                filters = scenarioA.filters,
                                onFiltersChanged = onApplySharedComparisonControls,
                            )
                        }
                    }
                }

                if (isStatsTab && uiState.isLoading) {
                    item { LoadingCard("Loading player history") }
                }

                uiState.errorMessage?.takeIf { isStatsTab }?.let { message ->
                    item { ErrorCard(message, onRetry = onRefresh) }
                }

                if (!uiState.isLoading && uiState.selectedPlayer != null) {
                    if (isStatsTab) {
                        item {
                            PlayerSummaryCard(summary = uiState.summary)
                        }
                        item {
                            SegmentedToggle(
                                options = listOf("Table", "Graph"),
                                selectedIndex = if (PlayerViewMode.valueOf(viewMode) == PlayerViewMode.Table) 0 else 1,
                                onSelected = { index ->
                                    viewMode = if (index == 0) PlayerViewMode.Table.name else PlayerViewMode.Graph.name
                                },
                            )
                        }
                        item {
                            if (uiState.history.isEmpty()) {
                                EmptyCard(
                                    title = "No history",
                                    body = "Adjust the player filters or widen the season range.",
                                    actionLabel = "Open filters",
                                    onAction = { activeFilterTarget = PlayerFilterTarget.Stats },
                                )
                            } else if (PlayerViewMode.valueOf(viewMode) == PlayerViewMode.Graph) {
                                PlayerHistoryGraph(
                                    history = uiState.history,
                                    filters = uiState.filters,
                                )
                            } else {
                                PlayerHistoryTable(uiState.history)
                            }
                        }
                    } else {
                        item {
                            PlayerComparisonContent(
                                playerName = uiState.selectedPlayer.fullName,
                                filterOptions = uiState.filterOptions,
                                viewMode = PlayerComparisonViewMode.valueOf(comparisonViewMode),
                                focus = PlayerComparisonFocus.valueOf(comparisonFocus),
                                scenarioA = scenarioA,
                                scenarioB = scenarioB,
                                onViewModeChanged = { comparisonViewMode = it.name },
                                onFocusChanged = { comparisonFocus = it.name },
                                onEditScenarioA = { activeFilterTarget = PlayerFilterTarget.ScenarioA },
                                onEditScenarioB = { activeFilterTarget = PlayerFilterTarget.ScenarioB },
                            )
                        }
                    }
                }
            }
        }

        if (activeFilterTarget != null) {
            val filterTarget = activeFilterTarget ?: PlayerFilterTarget.Stats
            val filterOptionsForSheet = uiState.filterOptions?.let { options ->
                val narrowed = uiState.availableVenues ?: options.venues
                // Keep already-selected venues visible even when the narrowed
                // list no longer contains them, so they can be deselected.
                options.copy(venues = (narrowed + draftFilters.venues).distinct().sorted())
            }
            PlayerStatsFilterSheet(
                title = when (filterTarget) {
                    PlayerFilterTarget.Stats -> "Player filters"
                    PlayerFilterTarget.ScenarioA -> "Scenario A filters"
                    PlayerFilterTarget.ScenarioB -> "Scenario B filters"
                },
                filterOptions = filterOptionsForSheet,
                filters = draftFilters,
                showStatAndLineControls = filterTarget == PlayerFilterTarget.Stats,
                onFiltersChanged = { draftFilters = it },
                onApply = {
                    when (filterTarget) {
                        PlayerFilterTarget.Stats -> onApplyFilters(draftFilters)
                        PlayerFilterTarget.ScenarioA -> onSetScenarioFilters(PlayerComparisonFocus.ScenarioA, draftFilters)
                        PlayerFilterTarget.ScenarioB -> onSetScenarioFilters(PlayerComparisonFocus.ScenarioB, draftFilters)
                    }
                    activeFilterTarget = null
                },
                onClear = {
                    draftFilters = uiState.filterOptions?.let(::defaultPlayerStatsFilters) ?: PlayerStatsFilters()
                },
                onDismiss = { activeFilterTarget = null },
            )
        }
    }
}

/** A naked segmented control — replaces the old chips-inside-cards toggles. */
@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun SegmentedToggle(
    options: List<String>,
    selectedIndex: Int,
    onSelected: (Int) -> Unit,
) {
    SingleChoiceSegmentedButtonRow(modifier = Modifier.fillMaxWidth()) {
        options.forEachIndexed { index, option ->
            SegmentedButton(
                selected = index == selectedIndex,
                onClick = { onSelected(index) },
                shape = SegmentedButtonDefaults.itemShape(index = index, count = options.size),
                colors = SegmentedButtonDefaults.colors(
                    activeContainerColor = MaterialTheme.colorScheme.tertiary,
                    activeContentColor = MaterialTheme.colorScheme.onTertiary,
                    inactiveContainerColor = MaterialTheme.colorScheme.surfaceContainerHigh,
                    inactiveContentColor = MaterialTheme.colorScheme.onSurface,
                ),
                label = { Text(option, maxLines = 1) },
            )
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun PlayerSearchCard(
    uiState: PlayerStatsUiState,
    onSearchQueryChanged: (String) -> Unit,
    onSelectPlayer: (PlayerSummary) -> Unit,
) {
    var expanded by remember { mutableStateOf(false) }
    val focusManager = LocalFocusManager.current

    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appCardBorder(),
    ) {
        Column(
            modifier = Modifier.padding(14.dp),
            verticalArrangement = Arrangement.spacedBy(10.dp),
        ) {
            ExposedDropdownMenuBox(
                expanded = expanded,
                onExpandedChange = { expanded = !expanded },
            ) {
                OutlinedTextField(
                    value = uiState.searchQuery,
                    onValueChange = {
                        onSearchQueryChanged(it)
                        // Typing should surface suggestions immediately.
                        expanded = true
                    },
                    modifier = Modifier
                        .menuAnchor(ExposedDropdownMenuAnchorType.PrimaryEditable)
                        .fillMaxWidth(),
                    singleLine = true,
                    label = { Text("Search players") },
                    keyboardOptions = KeyboardOptions(
                        autoCorrectEnabled = false,
                        capitalization = KeyboardCapitalization.Words,
                        imeAction = ImeAction.Search,
                    ),
                    keyboardActions = KeyboardActions(
                        onSearch = { focusManager.clearFocus() },
                        onDone = { focusManager.clearFocus() },
                    ),
                    trailingIcon = {
                        if (uiState.searchQuery.isNotEmpty()) {
                            IconButton(onClick = { onSearchQueryChanged("") }) {
                                Icon(Icons.Outlined.Close, contentDescription = "Clear search")
                            }
                        } else {
                            ExposedDropdownMenuDefaults.TrailingIcon(expanded = expanded)
                        }
                    },
                )
                // ExposedDropdownMenu (unlike DropdownMenu) doesn't take focus,
                // so the keyboard stays up and typing is never interrupted.
                ExposedDropdownMenu(
                    expanded = expanded && uiState.searchResults.isNotEmpty(),
                    onDismissRequest = { expanded = false },
                    modifier = Modifier.heightIn(max = 360.dp),
                ) {
                    uiState.searchResults.forEach { player ->
                        DropdownMenuItem(
                            text = { Text(player.fullName) },
                            onClick = {
                                onSelectPlayer(player)
                                expanded = false
                                focusManager.clearFocus()
                            },
                        )
                    }
                }
            }
        }
    }
}

@OptIn(ExperimentalLayoutApi::class)
@Composable
private fun PlayerFilterChipFlow(
    filters: PlayerStatsFilters,
    filterOptions: PlayerStatFilterOptions?,
    showStatAndLine: Boolean = true,
) {
    val statLabel = filterOptions?.stats?.firstOrNull { it.code == filters.statCode }?.label ?: filters.statCode
    val lineLabel = playerLineLabel(filters)
    val seasonLabel = summarizeFilterValues(filters.seasons)
    val homeAwayLabel = summarizeFilterValues(filters.homeAway)
    val oppositionLabel = summarizeFilterValues(filters.oppositions, filterOptions?.oppositions.orEmpty())
    val venueLabel = summarizeFilterValues(filters.venues, filterOptions?.venues.orEmpty())
    val weatherLabel = summarizeFilterValues(filters.weatherCategories, filterOptions?.weatherCategories.orEmpty())

    FlowRow(
        horizontalArrangement = Arrangement.spacedBy(8.dp),
        verticalArrangement = Arrangement.spacedBy(8.dp),
    ) {
        if (showStatAndLine) {
            InlineChip("Stat: $statLabel")
        }
        seasonLabel?.let { InlineChip("Seasons: $it") }
        if (showStatAndLine) {
            InlineChip("Line: $lineLabel")
        }
        homeAwayLabel?.let { InlineChip("Home/Away: $it") }
        oppositionLabel?.let { InlineChip("Opp: $it") }
        venueLabel?.let { InlineChip("Venue: $it") }
        weatherLabel?.let { InlineChip("Weather: $it") }
        if (filters.marginMinText != "-200" || filters.marginMaxText != "200") {
            InlineChip("Margin: ${filters.marginMinText} to ${filters.marginMaxText}")
        }
        if (filters.lastGamesText.isNotBlank()) {
            InlineChip("Last: ${filters.lastGamesText} games")
        }
        if (filters.minutesMinimumText != "0") {
            InlineChip("TOG >= ${filters.minutesMinimumText}%")
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun ComparisonSharedControlsCard(
    filterOptions: PlayerStatFilterOptions?,
    filters: PlayerStatsFilters,
    onFiltersChanged: (PlayerStatsFilters) -> Unit,
) {
    var statExpanded by remember { mutableStateOf(false) }

    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appCardBorder(),
    ) {
        Column(
            modifier = Modifier.padding(14.dp),
            verticalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            Text(
                "Comparison setup",
                modifier = Modifier.semantics { heading() },
                style = MaterialTheme.typography.titleMedium,
                fontWeight = FontWeight.SemiBold,
            )
            if (filterOptions == null) {
                LoadingCard("Loading player filters")
            } else {
                ExposedDropdownMenuBox(
                    expanded = statExpanded,
                    onExpandedChange = { statExpanded = !statExpanded },
                ) {
                    OutlinedTextField(
                        value = filterOptions.stats.firstOrNull { it.code == filters.statCode }?.label ?: filters.statCode,
                        onValueChange = {},
                        modifier = Modifier
                            .menuAnchor(ExposedDropdownMenuAnchorType.PrimaryNotEditable)
                            .fillMaxWidth(),
                        readOnly = true,
                        label = { Text("Market") },
                        trailingIcon = {
                            ExposedDropdownMenuDefaults.TrailingIcon(expanded = statExpanded)
                        },
                    )
                    DropdownMenu(
                        expanded = statExpanded,
                        onDismissRequest = { statExpanded = false },
                    ) {
                        filterOptions.stats.forEach { option ->
                            DropdownMenuItem(
                                text = { Text(option.label) },
                                onClick = {
                                    onFiltersChanged(filters.copy(statCode = option.code))
                                    statExpanded = false
                                },
                            )
                        }
                    }
                }

                LineModeControls(
                    filters = filters,
                    onFiltersChanged = onFiltersChanged,
                )
            }
        }
    }
}

@Composable
private fun LineModeControls(
    filters: PlayerStatsFilters,
    onFiltersChanged: (PlayerStatsFilters) -> Unit,
) {
    Row(
        modifier = Modifier.fillMaxWidth(),
        horizontalArrangement = Arrangement.spacedBy(12.dp),
    ) {
        FilterChip(
            selected = filters.lineMode == "single",
            onClick = { onFiltersChanged(filters.copy(lineMode = "single")) },
            label = { Text("Single line") },
            colors = playerAccentFilterChipColors(),
            border = playerAccentFilterChipBorder(filters.lineMode == "single"),
        )
        FilterChip(
            selected = filters.lineMode == "interval",
            onClick = { onFiltersChanged(filters.copy(lineMode = "interval")) },
            label = { Text("Interval") },
            colors = playerAccentFilterChipColors(),
            border = playerAccentFilterChipBorder(filters.lineMode == "interval"),
        )
    }

    if (filters.lineMode == "interval") {
        Row(
            modifier = Modifier.fillMaxWidth(),
            horizontalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            StepperField(
                value = filters.lowerBoundText,
                onValueChange = { onFiltersChanged(filters.copy(lowerBoundText = it)) },
                label = "Lower",
                modifier = Modifier.weight(1f),
                step = 0.5,
                minValue = 0.0,
            )
            StepperField(
                value = filters.upperBoundText,
                onValueChange = { onFiltersChanged(filters.copy(upperBoundText = it)) },
                label = "Upper",
                modifier = Modifier.weight(1f),
                step = 0.5,
                minValue = 0.0,
            )
        }
    } else {
        StepperField(
            value = filters.referenceLineText,
            onValueChange = { onFiltersChanged(filters.copy(referenceLineText = it)) },
            label = "Reference line",
            modifier = Modifier.fillMaxWidth(),
            step = 0.5,
            minValue = 0.0,
        )
    }
}

@OptIn(ExperimentalLayoutApi::class)
@Composable
private fun PlayerStatsFilterSummary(
    playerName: String,
    filters: PlayerStatsFilters,
    filterOptions: PlayerStatFilterOptions?,
    onEditFilters: () -> Unit,
    onApplyFilters: (PlayerStatsFilters) -> Unit,
) {
    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appCardBorder(),
    ) {
        Column(
            modifier = Modifier.padding(14.dp),
            verticalArrangement = Arrangement.spacedBy(10.dp),
        ) {
            Row(
                modifier = Modifier.fillMaxWidth(),
                verticalAlignment = Alignment.CenterVertically,
                horizontalArrangement = Arrangement.spacedBy(10.dp),
            ) {
                Text(
                    playerName,
                    modifier = Modifier
                        .weight(1f)
                        .semantics { heading() },
                    style = MaterialTheme.typography.titleMedium,
                    fontWeight = FontWeight.SemiBold,
                    maxLines = 1,
                    overflow = TextOverflow.Ellipsis,
                )
                TextButton(onClick = onEditFilters) {
                    Icon(Icons.Outlined.FilterList, contentDescription = null)
                    Text("Edit", modifier = Modifier.padding(start = 6.dp))
                }
            }
            PlayerFilterChipFlow(
                filters = filters,
                filterOptions = filterOptions,
            )
            QuickPlayerFilterRow(
                filters = filters,
                filterOptions = filterOptions,
                onApplyFilters = onApplyFilters,
            )
        }
    }
}

@Composable
private fun QuickPlayerFilterRow(
    filters: PlayerStatsFilters,
    filterOptions: PlayerStatFilterOptions?,
    onApplyFilters: (PlayerStatsFilters) -> Unit,
) {
    val latestSeason = filterOptions?.seasons?.firstOrNull()
    val defaultFilters = filterOptions?.let(::defaultPlayerStatsFilters)
    val homeAwayOptions = filterOptions?.homeAwayOptions.orEmpty()
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .horizontalScroll(rememberScrollState()),
        horizontalArrangement = Arrangement.spacedBy(8.dp),
    ) {
        QuickFilterChip(
            label = "Last 5",
            selected = filters.lastGamesText == "5",
            onClick = { onApplyFilters(filters.copy(lastGamesText = if (filters.lastGamesText == "5") "" else "5")) },
        )
        QuickFilterChip(
            label = "Last 10",
            selected = filters.lastGamesText == "10",
            onClick = { onApplyFilters(filters.copy(lastGamesText = if (filters.lastGamesText == "10") "" else "10")) },
        )
        if (latestSeason != null) {
            QuickFilterChip(
                label = "$latestSeason only",
                selected = filters.seasons == listOf(latestSeason),
                onClick = {
                    onApplyFilters(
                        filters.copy(
                            seasons = if (filters.seasons == listOf(latestSeason)) {
                                defaultFilters?.seasons ?: filters.seasons
                            } else {
                                listOf(latestSeason)
                            },
                        ),
                    )
                },
            )
        }
        if (homeAwayOptions.contains("Home")) {
            QuickFilterChip(
                label = "Home",
                selected = filters.homeAway == listOf("Home"),
                onClick = {
                    onApplyFilters(
                        filters.copy(
                            homeAway = if (filters.homeAway == listOf("Home")) {
                                defaultFilters?.homeAway ?: homeAwayOptions
                            } else {
                                listOf("Home")
                            },
                        ),
                    )
                },
            )
        }
        if (homeAwayOptions.contains("Away")) {
            QuickFilterChip(
                label = "Away",
                selected = filters.homeAway == listOf("Away"),
                onClick = {
                    onApplyFilters(
                        filters.copy(
                            homeAway = if (filters.homeAway == listOf("Away")) {
                                defaultFilters?.homeAway ?: homeAwayOptions
                            } else {
                                listOf("Away")
                            },
                        ),
                    )
                },
            )
        }
        if (defaultFilters != null && filters != defaultFilters) {
            QuickFilterChip(
                label = "Reset",
                selected = false,
                onClick = { onApplyFilters(defaultFilters) },
            )
        }
    }
}

@Composable
private fun QuickFilterChip(
    label: String,
    selected: Boolean,
    onClick: () -> Unit,
) {
    FilterChip(
        selected = selected,
        onClick = onClick,
        label = { Text(label, maxLines = 1) },
        colors = playerAccentFilterChipColors(),
        border = playerAccentFilterChipBorder(selected),
    )
}

private fun summarizeFilterValues(
    selected: List<String>,
    allValues: List<String> = emptyList(),
    maxVisible: Int = 3,
): String? {
    if (selected.isEmpty()) return null
    if (allValues.isNotEmpty() && selected.toSet() == allValues.toSet()) return "All"
    val visible = selected.take(maxVisible)
    val suffix = if (selected.size > maxVisible) " +${selected.size - maxVisible}" else ""
    return visible.joinToString(", ") + suffix
}

@Composable
private fun PlayerSummaryCard(summary: PlayerStatSummary?) {
    if (summary == null) {
        EmptyCard(
            title = "Line summary",
            body = "Add a reference line in Filters to calculate over/under and implied prices.",
        )
        return
    }
    val primary = primarySummaryOutcome(summary)
    val secondary = secondarySummaryOutcome(summary)
    val primaryHits = primary.probability?.let { (it * summary.sampleSize).roundToInt() }
    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appCardBorder(),
    ) {
        Column(
            modifier = Modifier.padding(14.dp),
            verticalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            Text(
                "${summary.statLabel} summary",
                modifier = Modifier.semantics { heading() },
                style = MaterialTheme.typography.titleMedium,
                fontWeight = FontWeight.SemiBold,
            )
            Row(
                modifier = Modifier.fillMaxWidth(),
                verticalAlignment = Alignment.Bottom,
                horizontalArrangement = Arrangement.spacedBy(14.dp),
            ) {
                Column(modifier = Modifier.weight(1f)) {
                    Text(
                        text = "${primary.label} hit rate",
                        style = MaterialTheme.typography.labelLarge,
                        color = MaterialTheme.colorScheme.onSurfaceVariant,
                    )
                    Text(
                        text = formatPercentage(primary.probability),
                        style = MaterialTheme.typography.headlineMedium.tabular,
                        fontWeight = FontWeight.Bold,
                    )
                    Text(
                        text = if (primaryHits != null) {
                            "$primaryHits of ${summary.sampleSize} games ${primary.caption.lowercase(Locale.getDefault())}"
                        } else {
                            "Across ${summary.sampleSize} games"
                        },
                        style = MaterialTheme.typography.bodySmall,
                        color = MaterialTheme.colorScheme.onSurfaceVariant,
                    )
                }
                DenseSummaryCell(
                    label = "${primary.label.uppercase(Locale.getDefault())} PRICE",
                    value = formatDecimalPrice(primary.price),
                    modifier = Modifier.width(118.dp),
                )
            }
            Text(
                text = summaryLineDescription(summary),
                style = MaterialTheme.typography.bodyMedium,
                color = MaterialTheme.colorScheme.onSurfaceVariant,
            )
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.spacedBy(8.dp),
            ) {
                DenseSummaryCell(
                    label = "${secondary.label.uppercase(Locale.getDefault())} RATE",
                    value = formatPercentage(secondary.probability),
                    modifier = Modifier.weight(1f),
                )
                DenseSummaryCell(
                    label = "${secondary.label.uppercase(Locale.getDefault())} PRICE",
                    value = formatDecimalPrice(secondary.price),
                    modifier = Modifier.weight(1f),
                )
            }
        }
    }
}

private data class SummaryOutcome(
    val label: String,
    val caption: String,
    val probability: Double?,
    val price: Double?,
)

private fun primarySummaryOutcome(summary: PlayerStatSummary): SummaryOutcome =
    if (summary.lineMode == "interval") {
        SummaryOutcome(
            label = "Within",
            caption = "inside the interval",
            probability = summary.proportionWithinInterval,
            price = summary.impliedOddsWithinInterval,
        )
    } else {
        SummaryOutcome(
            label = "Over",
            caption = "over the line",
            probability = summary.proportionOver,
            price = summary.impliedOddsOver,
        )
    }

private fun secondarySummaryOutcome(summary: PlayerStatSummary): SummaryOutcome =
    if (summary.lineMode == "interval") {
        SummaryOutcome(
            label = "Outside",
            caption = "outside the interval",
            probability = summary.proportionOutsideInterval,
            price = summary.impliedOddsOutsideInterval,
        )
    } else {
        SummaryOutcome(
            label = "Under",
            caption = "under the line",
            probability = summary.proportionUnder,
            price = summary.impliedOddsUnder,
        )
    }

private fun summaryLineDescription(summary: PlayerStatSummary): String =
    if (summary.lineMode == "interval") {
        "Interval ${summary.lowerBound} to ${summary.upperBound} across ${summary.sampleSize} games."
    } else {
        "Line ${summary.referenceLine} across ${summary.sampleSize} games."
    }

@Composable
private fun PlayerComparisonContent(
    playerName: String,
    filterOptions: PlayerStatFilterOptions?,
    viewMode: PlayerComparisonViewMode,
    focus: PlayerComparisonFocus,
    scenarioA: ComparisonScenarioState,
    scenarioB: ComparisonScenarioState,
    onViewModeChanged: (PlayerComparisonViewMode) -> Unit,
    onFocusChanged: (PlayerComparisonFocus) -> Unit,
    onEditScenarioA: () -> Unit,
    onEditScenarioB: () -> Unit,
) {
    Column(
        verticalArrangement = Arrangement.spacedBy(12.dp),
    ) {
        ComparisonScenarioCard(
            title = "Scenario A",
            playerName = playerName,
            state = scenarioA,
            filterOptions = filterOptions,
            onEdit = onEditScenarioA,
        )
        ComparisonScenarioCard(
            title = "Scenario B",
            playerName = playerName,
            state = scenarioB,
            filterOptions = filterOptions,
            onEdit = onEditScenarioB,
        )
        SegmentedToggle(
            options = listOf("Table", "Graph", "Game log"),
            selectedIndex = when (viewMode) {
                PlayerComparisonViewMode.Table -> 0
                PlayerComparisonViewMode.Graph -> 1
                PlayerComparisonViewMode.GameLog -> 2
            },
            onSelected = { index ->
                onViewModeChanged(
                    when (index) {
                        0 -> PlayerComparisonViewMode.Table
                        1 -> PlayerComparisonViewMode.Graph
                        else -> PlayerComparisonViewMode.GameLog
                    },
                )
            },
        )
        if (viewMode == PlayerComparisonViewMode.Table) {
            ComparisonSummaryCard(
                scenarioA = scenarioA,
                scenarioB = scenarioB,
            )
        } else if (viewMode == PlayerComparisonViewMode.Graph) {
            when {
                scenarioA.isLoading || scenarioB.isLoading -> LoadingCard("Loading comparison graph")
                scenarioA.history.isEmpty() && scenarioB.history.isEmpty() ->
                    EmptyCard("No graph", "Adjust the scenario filters to load game history.")
                else ->
                    ComparisonHistoryGraph(
                        scenarioA = scenarioA,
                        scenarioB = scenarioB,
                    )
            }
        } else {
            SegmentedToggle(
                options = listOf("Scenario A", "Scenario B"),
                selectedIndex = if (focus == PlayerComparisonFocus.ScenarioA) 0 else 1,
                onSelected = { index ->
                    onFocusChanged(
                        if (index == 0) PlayerComparisonFocus.ScenarioA else PlayerComparisonFocus.ScenarioB,
                    )
                },
            )
            val focusedScenario = if (focus == PlayerComparisonFocus.ScenarioA) scenarioA else scenarioB
            val focusedLabel = if (focus == PlayerComparisonFocus.ScenarioA) "Scenario A" else "Scenario B"
            when {
                focusedScenario.isLoading -> LoadingCard("Loading $focusedLabel")
                focusedScenario.errorMessage != null -> ErrorCard(focusedScenario.errorMessage)
                focusedScenario.history.isEmpty() ->
                    EmptyCard("No game log", "Adjust the $focusedLabel filters to load game history.")
                else ->
                    PlayerHistoryTable(focusedScenario.history)
            }
        }
    }
}

@Composable
private fun ComparisonScenarioCard(
    title: String,
    playerName: String,
    state: ComparisonScenarioState,
    filterOptions: PlayerStatFilterOptions?,
    onEdit: () -> Unit,
) {
    Card(
        modifier = Modifier.fillMaxWidth(),
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
                verticalAlignment = Alignment.CenterVertically,
            ) {
                Column(verticalArrangement = Arrangement.spacedBy(2.dp)) {
                    Text(
                        title,
                        modifier = Modifier.semantics { heading() },
                        style = MaterialTheme.typography.titleMedium,
                        fontWeight = FontWeight.SemiBold,
                    )
                    Text(
                        playerName,
                        style = MaterialTheme.typography.bodySmall,
                        color = MaterialTheme.colorScheme.onSurfaceVariant,
                    )
                }
                TextButton(onClick = onEdit) {
                    Text("Edit")
                }
            }
            PlayerFilterChipFlow(
                filters = state.filters,
                filterOptions = filterOptions,
                showStatAndLine = false,
            )
            when {
                state.isLoading -> Text(
                    "Refreshing scenario...",
                    style = MaterialTheme.typography.bodySmall,
                    color = MaterialTheme.colorScheme.onSurfaceVariant,
                )
                state.errorMessage != null -> Text(
                    state.errorMessage,
                    style = MaterialTheme.typography.bodySmall,
                    color = MaterialTheme.colorScheme.error,
                )
                state.infoMessage != null -> Text(
                    state.infoMessage,
                    style = MaterialTheme.typography.bodySmall,
                    color = MaterialTheme.colorScheme.onSurfaceVariant,
                )
            }
        }
    }
}

@Composable
private fun ComparisonSummaryCard(
    scenarioA: ComparisonScenarioState,
    scenarioB: ComparisonScenarioState,
) {
    val outcomeLabels = comparisonOutcomeLabels(scenarioA.filters, scenarioB.filters)
    val rows = listOf(
        ComparisonMetricRow(
            label = "Games",
            scenarioAValue = comparisonGameCount(scenarioA),
            scenarioBValue = comparisonGameCount(scenarioB),
        ),
        ComparisonMetricRow(
            label = "Average",
            scenarioAValue = comparisonAverage(scenarioA.history),
            scenarioBValue = comparisonAverage(scenarioB.history),
        ),
        ComparisonMetricRow(
            label = outcomeLabels.first,
            scenarioAValue = comparisonOutcomeValue(scenarioA.summary, primary = true, showLabelPrefix = outcomeLabels.first == "Outcome 1"),
            scenarioBValue = comparisonOutcomeValue(scenarioB.summary, primary = true, showLabelPrefix = outcomeLabels.first == "Outcome 1"),
        ),
        ComparisonMetricRow(
            label = outcomeLabels.second,
            scenarioAValue = comparisonOutcomeValue(scenarioA.summary, primary = false, showLabelPrefix = outcomeLabels.second == "Outcome 2"),
            scenarioBValue = comparisonOutcomeValue(scenarioB.summary, primary = false, showLabelPrefix = outcomeLabels.second == "Outcome 2"),
        ),
    )

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
                "Scenario comparison",
                modifier = Modifier.semantics { heading() },
                style = MaterialTheme.typography.titleMedium,
                fontWeight = FontWeight.SemiBold,
            )
            ComparisonTableHeader(
                scenarioALabel = playerLineLabel(scenarioA.filters),
                scenarioBLabel = playerLineLabel(scenarioB.filters),
            )
            rows.forEachIndexed { index, row ->
                if (index > 0) {
                    HorizontalDivider()
                }
                ComparisonTableRow(row = row)
            }
        }
    }
}

private data class ComparisonMetricRow(
    val label: String,
    val scenarioAValue: String,
    val scenarioBValue: String,
)

@Composable
private fun ComparisonTableHeader(
    scenarioALabel: String,
    scenarioBLabel: String,
) {
    Row(
        modifier = Modifier.fillMaxWidth(),
        horizontalArrangement = Arrangement.spacedBy(10.dp),
        verticalAlignment = Alignment.CenterVertically,
    ) {
        Text(
            "Metric",
            modifier = Modifier.weight(0.9f),
            style = MaterialTheme.typography.labelMedium,
            color = MaterialTheme.colorScheme.onSurfaceVariant,
            fontWeight = FontWeight.SemiBold,
        )
        Text(
            "A\n$scenarioALabel",
            modifier = Modifier.weight(1f),
            style = MaterialTheme.typography.labelMedium,
            color = MaterialTheme.colorScheme.onSurfaceVariant,
            fontWeight = FontWeight.SemiBold,
        )
        Text(
            "B\n$scenarioBLabel",
            modifier = Modifier.weight(1f),
            style = MaterialTheme.typography.labelMedium,
            color = MaterialTheme.colorScheme.onSurfaceVariant,
            fontWeight = FontWeight.SemiBold,
        )
    }
}

@Composable
private fun ComparisonTableRow(row: ComparisonMetricRow) {
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .padding(vertical = 10.dp),
        horizontalArrangement = Arrangement.spacedBy(10.dp),
        verticalAlignment = Alignment.Top,
    ) {
        Text(
            row.label,
            modifier = Modifier.weight(0.9f),
            style = MaterialTheme.typography.bodyMedium,
            fontWeight = FontWeight.SemiBold,
        )
        Text(
            row.scenarioAValue,
            modifier = Modifier.weight(1f),
            style = MaterialTheme.typography.bodySmall.tabular,
        )
        Text(
            row.scenarioBValue,
            modifier = Modifier.weight(1f),
            style = MaterialTheme.typography.bodySmall.tabular,
        )
    }
}

@Composable
private fun DenseSummaryCell(
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
private fun PlayerHistoryTable(history: List<PlayerGameLogEntry>) {
    val colors = AppTheme.colors
    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appCardBorder(),
    ) {
        Column(
            modifier = Modifier.padding(12.dp),
            verticalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            CompactHistoryHeader()
            history.forEachIndexed { index, entry ->
                CompactHistoryRow(
                    entry = entry,
                    index = index,
                    colors = colors,
                )
            }
        }
    }
}

@Composable
private fun CompactHistoryHeader() {
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .padding(horizontal = 8.dp, vertical = 2.dp),
        horizontalArrangement = Arrangement.spacedBy(10.dp),
        verticalAlignment = Alignment.CenterVertically,
    ) {
        Text(
            "Date",
            modifier = Modifier.weight(0.95f),
            style = MaterialTheme.typography.labelMedium,
            fontWeight = FontWeight.Bold,
            color = MaterialTheme.colorScheme.onSurfaceVariant,
        )
        Text(
            "Opponent",
            modifier = Modifier.weight(1.35f),
            style = MaterialTheme.typography.labelMedium,
            fontWeight = FontWeight.Bold,
            color = MaterialTheme.colorScheme.onSurfaceVariant,
        )
        Text(
            "Venue",
            modifier = Modifier.weight(1f),
            style = MaterialTheme.typography.labelMedium,
            fontWeight = FontWeight.Bold,
            color = MaterialTheme.colorScheme.onSurfaceVariant,
        )
        Text(
            "Value",
            modifier = Modifier.width(48.dp),
            style = MaterialTheme.typography.labelMedium,
            fontWeight = FontWeight.Bold,
            color = MaterialTheme.colorScheme.onSurfaceVariant,
            textAlign = TextAlign.End,
        )
    }
}

@Composable
private fun CompactHistoryRow(
    entry: PlayerGameLogEntry,
    index: Int,
    colors: com.jamesbrown.aflmobile.ui.theme.AppColors,
) {
    val rowTint = when (entry.hit) {
        true -> colors.positiveContainer.copy(alpha = 0.62f)
        false -> colors.negativeContainer.copy(alpha = 0.62f)
        null -> if (index % 2 == 1) {
            MaterialTheme.colorScheme.surfaceContainerLow
        } else {
            MaterialTheme.colorScheme.surfaceContainerLowest
        }
    }
    val hitLabel = when (entry.hit) {
        true -> "Hit"
        false -> "Miss"
        null -> null
    }
    Column(
        modifier = Modifier
            .fillMaxWidth()
            .background(color = rowTint, shape = MaterialTheme.shapes.small)
            .padding(horizontal = 8.dp, vertical = 9.dp),
        verticalArrangement = Arrangement.spacedBy(4.dp),
    ) {
        Row(
            modifier = Modifier.fillMaxWidth(),
            horizontalArrangement = Arrangement.spacedBy(10.dp),
            verticalAlignment = Alignment.CenterVertically,
        ) {
            Column(modifier = Modifier.weight(0.95f)) {
                Text(
                    text = formatGameDate(entry.date),
                    style = MaterialTheme.typography.bodySmall.tabular,
                    fontWeight = FontWeight.SemiBold,
                    maxLines = 1,
                )
                Text(
                    text = entry.roundLabel ?: "--",
                    style = MaterialTheme.typography.labelSmall,
                    color = MaterialTheme.colorScheme.onSurfaceVariant,
                    maxLines = 1,
                    overflow = TextOverflow.Ellipsis,
                )
            }
            Text(
                text = entry.opposition ?: "--",
                modifier = Modifier.weight(1.35f),
                style = MaterialTheme.typography.bodySmall,
                maxLines = 2,
                overflow = TextOverflow.Ellipsis,
            )
            Text(
                text = entry.venue ?: "--",
                modifier = Modifier.weight(1f),
                style = MaterialTheme.typography.bodySmall,
                maxLines = 1,
                overflow = TextOverflow.Ellipsis,
            )
            Text(
                text = formatNumber(entry.selectedValue),
                modifier = Modifier.width(48.dp),
                style = MaterialTheme.typography.titleSmall.tabular,
                fontWeight = FontWeight.Bold,
                textAlign = TextAlign.End,
            )
        }
        Row(
            modifier = Modifier.fillMaxWidth(),
            horizontalArrangement = Arrangement.spacedBy(8.dp),
            verticalAlignment = Alignment.CenterVertically,
        ) {
            InlineChip(entry.selectedStat.replace('_', ' ').replaceFirstChar { it.titlecase(Locale.getDefault()) })
            hitLabel?.let {
                InlineChip(it)
            }
            Text(
                text = compactGameMeta(entry),
                modifier = Modifier.weight(1f),
                style = MaterialTheme.typography.labelSmall,
                color = MaterialTheme.colorScheme.onSurfaceVariant,
                maxLines = 1,
                overflow = TextOverflow.Ellipsis,
                textAlign = TextAlign.End,
            )
        }
    }
}

// ---------------------------------------------------------------------------
// Charts
// ---------------------------------------------------------------------------

private const val ChartHeightDp = 280
private const val ChartLeftPaddingDp = 44f
private const val ChartRightPaddingDp = 10f
private const val ChartTopPaddingDp = 14f
private const val ChartBottomPaddingDp = 28f

private data class ChartAxis(
    val min: Double,
    val max: Double,
    val ticks: List<Double>,
)

private fun computeChartAxis(
    values: List<Double>,
    guides: List<Double>,
): ChartAxis {
    val dataMin = values.minOrNull() ?: 0.0
    val dataMax = values.maxOrNull() ?: 1.0
    val dataSpan = (dataMax - dataMin).takeIf { it > 0.0 } ?: 1.0
    val paddingValue = when {
        dataSpan <= 2.0 -> 0.5
        dataSpan <= 8.0 -> 1.0
        else -> roundUpToHalf(dataSpan * 0.06)
    }
    var rangeMin = dataMin - paddingValue
    var rangeMax = dataMax + paddingValue
    guides.forEach { guide ->
        if (guide < rangeMin && rangeMin - guide <= dataSpan * 0.75) {
            rangeMin = guide - 0.5
        }
        if (guide > rangeMax && guide - rangeMax <= dataSpan * 0.75) {
            rangeMax = guide + 0.5
        }
    }
    val axisMin = roundDownToHalf(rangeMin)
    val axisStep = roundUpToHalf(((roundUpToHalf(rangeMax) - axisMin) / 4.0).coerceAtLeast(0.5))
    val axisMax = axisMin + (axisStep * 4.0)
    val ticks = List(5) { index -> axisMin + (axisStep * index.toDouble()) }
    return ChartAxis(min = axisMin, max = axisMax, ticks = ticks)
}

private fun DrawScope.drawChartFrame(
    axis: ChartAxis,
    textMeasurer: TextMeasurer,
    labelStyle: TextStyle,
    labelColor: Color,
    gridColor: Color,
    axisColor: Color,
) {
    val left = ChartLeftPaddingDp.dp.toPx()
    val right = size.width - ChartRightPaddingDp.dp.toPx()
    val top = ChartTopPaddingDp.dp.toPx()
    val bottom = size.height - ChartBottomPaddingDp.dp.toPx()
    val chartHeight = (bottom - top).coerceAtLeast(1f)

    axis.ticks.forEach { tick ->
        val fraction = ((tick - axis.min) / (axis.max - axis.min)).toFloat()
        val y = bottom - (fraction * chartHeight)
        drawLine(
            color = gridColor,
            start = Offset(left, y),
            end = Offset(right, y),
            strokeWidth = 1.dp.toPx(),
        )
        val measured = textMeasurer.measure(formatGraphValue(tick), labelStyle)
        drawText(
            textLayoutResult = measured,
            color = labelColor,
            topLeft = Offset(
                x = left - measured.size.width - 6.dp.toPx(),
                y = y - measured.size.height / 2f,
            ),
        )
    }
    drawLine(
        color = axisColor,
        start = Offset(left, bottom),
        end = Offset(right, bottom),
        strokeWidth = 1.2.dp.toPx(),
    )
}

private fun DrawScope.drawXAxisLabels(
    firstLabel: String?,
    lastLabel: String?,
    textMeasurer: TextMeasurer,
    labelStyle: TextStyle,
    labelColor: Color,
) {
    val left = ChartLeftPaddingDp.dp.toPx()
    val right = size.width - ChartRightPaddingDp.dp.toPx()
    val bottom = size.height - ChartBottomPaddingDp.dp.toPx()
    firstLabel?.let {
        val measured = textMeasurer.measure(it, labelStyle)
        drawText(
            textLayoutResult = measured,
            color = labelColor,
            topLeft = Offset(left, bottom + 6.dp.toPx()),
        )
    }
    lastLabel?.let {
        val measured = textMeasurer.measure(it, labelStyle)
        drawText(
            textLayoutResult = measured,
            color = labelColor,
            topLeft = Offset(right - measured.size.width, bottom + 6.dp.toPx()),
        )
    }
}

@Composable
private fun PlayerHistoryGraph(
    history: List<PlayerGameLogEntry>,
    filters: PlayerStatsFilters,
) {
    val orderedHistory = remember(history) { history.sortedBy { it.gameNumber } }
    var chartSize by remember { mutableStateOf(IntSize.Zero) }
    var selectedPointIndex by remember(orderedHistory, filters) { mutableStateOf<Int?>(null) }
    val density = LocalDensity.current
    val textMeasurer = rememberTextMeasurer()
    val selectedValues = orderedHistory.mapNotNull { it.selectedValue }
    if (selectedValues.isEmpty()) {
        EmptyCard("No graph", "No selected stat values are available for the current filter set.")
        return
    }

    val singleLine = filters.referenceLineText.toDoubleOrNull()
    val lowerBound = filters.lowerBoundText.toDoubleOrNull()
    val upperBound = filters.upperBoundText.toDoubleOrNull()
    val axis = remember(selectedValues, singleLine, lowerBound, upperBound) {
        computeChartAxis(
            values = selectedValues,
            guides = listOfNotNull(singleLine, lowerBound, upperBound),
        )
    }
    val averageValue = remember(selectedValues) { selectedValues.average() }
    val colors = AppTheme.colors
    val outlineColor = MaterialTheme.colorScheme.outline
    val primaryColor = MaterialTheme.colorScheme.primary
    val labelColor = MaterialTheme.colorScheme.onSurfaceVariant
    val axisColor = MaterialTheme.colorScheme.onSurfaceVariant.copy(alpha = 0.35f)
    val labelStyle = MaterialTheme.typography.labelSmall
    val plottedPoints = remember(orderedHistory, chartSize, axis, density) {
        if (chartSize == IntSize.Zero) {
            emptyList()
        } else {
            val left = with(density) { ChartLeftPaddingDp.dp.toPx() }
            val right = chartSize.width.toFloat() - with(density) { ChartRightPaddingDp.dp.toPx() }
            val top = with(density) { ChartTopPaddingDp.dp.toPx() }
            val bottom = chartSize.height.toFloat() - with(density) { ChartBottomPaddingDp.dp.toPx() }
            val chartWidth = (right - left).coerceAtLeast(1f)
            val chartHeight = (bottom - top).coerceAtLeast(1f)
            orderedHistory.mapIndexedNotNull { index, entry ->
                val value = entry.selectedValue ?: return@mapIndexedNotNull null
                val x = if (orderedHistory.size == 1) {
                    left + (chartWidth / 2f)
                } else {
                    left + (index.toFloat() / orderedHistory.lastIndex.toFloat()) * chartWidth
                }
                val normalized = ((value - axis.min) / (axis.max - axis.min)).toFloat()
                val y = bottom - (normalized * chartHeight)
                IndexedOffset(index = index, offset = Offset(x, y))
            }
        }
    }
    val selectedEntry = selectedPointIndex?.let { orderedHistory.getOrNull(it) }
    val hitRadiusPx = with(density) { 28.dp.toPx() }
    val hitCount = orderedHistory.count { it.hit == true }
    val statLabel = orderedHistory.first().selectedStat
    val chartDescription =
        "$statLabel over ${orderedHistory.size} games, oldest to latest. " +
            "Average ${formatGraphValue(averageValue)}." +
            if (orderedHistory.any { it.hit != null }) {
                " $hitCount of ${orderedHistory.size} games hit the line."
            } else {
                ""
            }

    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appCardBorder(),
    ) {
        Column(
            modifier = Modifier.padding(14.dp),
            verticalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            Text(
                "$statLabel graph",
                modifier = Modifier.semantics { heading() },
                style = MaterialTheme.typography.titleMedium,
                fontWeight = FontWeight.SemiBold,
            )
            Canvas(
                modifier = Modifier
                    .fillMaxWidth()
                    .height(ChartHeightDp.dp)
                    .onSizeChanged { chartSize = it }
                    .semantics { contentDescription = chartDescription }
                    .pointerInput(plottedPoints) {
                        detectTapGestures { tapOffset ->
                            val nearest = plottedPoints
                                .map { point -> point.index to point.offset.getDistanceSquared(tapOffset) }
                                .minByOrNull { it.second }
                            selectedPointIndex = nearest
                                ?.takeIf { it.second <= (hitRadiusPx * hitRadiusPx) }
                                ?.first
                        }
                    },
            ) {
                val left = ChartLeftPaddingDp.dp.toPx()
                val right = size.width - ChartRightPaddingDp.dp.toPx()
                val top = ChartTopPaddingDp.dp.toPx()
                val bottom = size.height - ChartBottomPaddingDp.dp.toPx()
                val chartWidth = (right - left).coerceAtLeast(1f)
                val chartHeight = (bottom - top).coerceAtLeast(1f)

                fun xFor(index: Int): Float =
                    if (orderedHistory.size == 1) {
                        left + (chartWidth / 2f)
                    } else {
                        left + (index.toFloat() / orderedHistory.lastIndex.toFloat()) * chartWidth
                    }

                fun yFor(value: Double): Float {
                    val normalized = ((value - axis.min) / (axis.max - axis.min)).toFloat()
                    return bottom - (normalized * chartHeight)
                }

                drawChartFrame(
                    axis = axis,
                    textMeasurer = textMeasurer,
                    labelStyle = labelStyle,
                    labelColor = labelColor,
                    gridColor = outlineColor.copy(alpha = 0.2f),
                    axisColor = axisColor,
                )
                drawXAxisLabels(
                    firstLabel = orderedHistory.firstOrNull()?.let { formatGameDate(it.date) },
                    lastLabel = orderedHistory.lastOrNull()?.let { formatGameDate(it.date) },
                    textMeasurer = textMeasurer,
                    labelStyle = labelStyle,
                    labelColor = labelColor,
                )

                if (filters.lineMode == "interval" && lowerBound != null && upperBound != null) {
                    val topBand = yFor(upperBound)
                    val bottomBand = yFor(lowerBound)
                    drawRect(
                        color = colors.positive.copy(alpha = 0.15f),
                        topLeft = Offset(left, topBand),
                        size = androidx.compose.ui.geometry.Size(chartWidth, bottomBand - topBand),
                    )
                    drawLine(
                        color = primaryColor,
                        start = Offset(left, topBand),
                        end = Offset(right, topBand),
                        strokeWidth = 2.dp.toPx(),
                        pathEffect = PathEffect.dashPathEffect(floatArrayOf(14f, 10f)),
                    )
                    drawLine(
                        color = primaryColor,
                        start = Offset(left, bottomBand),
                        end = Offset(right, bottomBand),
                        strokeWidth = 2.dp.toPx(),
                        pathEffect = PathEffect.dashPathEffect(floatArrayOf(14f, 10f)),
                    )
                } else if (singleLine != null) {
                    val referenceY = yFor(singleLine)
                    drawLine(
                        color = primaryColor,
                        start = Offset(left, referenceY),
                        end = Offset(right, referenceY),
                        strokeWidth = 2.dp.toPx(),
                        pathEffect = PathEffect.dashPathEffect(floatArrayOf(14f, 10f)),
                    )
                }

                // Rolling context: a faint flat line at the filtered average.
                val averageY = yFor(averageValue)
                drawLine(
                    color = labelColor.copy(alpha = 0.7f),
                    start = Offset(left, averageY),
                    end = Offset(right, averageY),
                    strokeWidth = 1.5.dp.toPx(),
                    pathEffect = PathEffect.dashPathEffect(floatArrayOf(4f, 6f)),
                )
                val avgLabel = textMeasurer.measure("avg ${formatGraphValue(averageValue)}", labelStyle)
                drawText(
                    textLayoutResult = avgLabel,
                    color = labelColor,
                    topLeft = Offset(
                        x = right - avgLabel.size.width,
                        y = (averageY - avgLabel.size.height - 2.dp.toPx()).coerceAtLeast(top),
                    ),
                )

                val trendPath = Path()
                orderedHistory.forEachIndexed { index, entry ->
                    val value = entry.selectedValue ?: return@forEachIndexed
                    val point = Offset(xFor(index), yFor(value))
                    if (trendPath.isEmpty) {
                        trendPath.moveTo(point.x, point.y)
                    } else {
                        trendPath.lineTo(point.x, point.y)
                    }
                }
                drawPath(
                    path = trendPath,
                    color = outlineColor,
                    style = Stroke(width = 2.dp.toPx()),
                )

                orderedHistory.forEachIndexed { index, entry ->
                    val value = entry.selectedValue ?: return@forEachIndexed
                    val pointColor = when (entry.hit) {
                        true -> colors.positive
                        false -> colors.negative
                        null -> primaryColor
                    }
                    val radius = if (selectedPointIndex == index) 7.dp.toPx() else 5.dp.toPx()
                    drawCircle(
                        color = pointColor,
                        radius = radius,
                        center = Offset(xFor(index), yFor(value)),
                    )
                    if (selectedPointIndex == index) {
                        drawCircle(
                            color = Color.White,
                            radius = radius + 2.dp.toPx(),
                            center = Offset(xFor(index), yFor(value)),
                            style = Stroke(width = 2.dp.toPx()),
                        )
                    }
                }
            }

            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.spacedBy(16.dp),
                verticalAlignment = Alignment.CenterVertically,
            ) {
                GraphLegendDot(color = colors.positive, label = "Hit")
                GraphLegendDot(color = colors.negative, label = "Miss")
                GraphLegendDot(color = labelColor, label = "Average")
            }

            selectedEntry?.let { entry ->
                SelectedGraphPointCard(
                    entry = entry,
                    filters = filters,
                )
            }
        }
    }
}

@Composable
private fun ComparisonHistoryGraph(
    scenarioA: ComparisonScenarioState,
    scenarioB: ComparisonScenarioState,
) {
    val orderedHistoryA = remember(scenarioA.history) { scenarioA.history.sortedBy { it.gameNumber } }
    val orderedHistoryB = remember(scenarioB.history) { scenarioB.history.sortedBy { it.gameNumber } }
    val allValues = orderedHistoryA.mapNotNull { it.selectedValue } + orderedHistoryB.mapNotNull { it.selectedValue }
    if (allValues.isEmpty()) {
        EmptyCard("No graph", "Adjust the scenario filters to load game history.")
        return
    }

    val scenarioAColor = MaterialTheme.colorScheme.tertiary
    val scenarioBColor = MaterialTheme.colorScheme.primary
    var chartSize by remember { mutableStateOf(IntSize.Zero) }
    var selectedPoint by remember(orderedHistoryA, orderedHistoryB, scenarioA.filters, scenarioB.filters) {
        mutableStateOf<ComparisonPlotPoint?>(null)
    }
    val density = LocalDensity.current
    val textMeasurer = rememberTextMeasurer()
    val allGuides = listOfNotNull(
        scenarioA.filters.referenceLineText.toDoubleOrNull(),
        scenarioA.filters.lowerBoundText.toDoubleOrNull(),
        scenarioA.filters.upperBoundText.toDoubleOrNull(),
        scenarioB.filters.referenceLineText.toDoubleOrNull(),
        scenarioB.filters.lowerBoundText.toDoubleOrNull(),
        scenarioB.filters.upperBoundText.toDoubleOrNull(),
    )
    val axis = remember(allValues, allGuides) {
        computeChartAxis(values = allValues, guides = allGuides)
    }
    val outlineColor = MaterialTheme.colorScheme.outline
    val labelColor = MaterialTheme.colorScheme.onSurfaceVariant
    val axisColor = MaterialTheme.colorScheme.onSurfaceVariant.copy(alpha = 0.35f)
    val labelStyle = MaterialTheme.typography.labelSmall
    val plottedPoints = remember(orderedHistoryA, orderedHistoryB, chartSize, axis, density, scenarioAColor, scenarioBColor) {
        buildComparisonPlotPoints(
            historyA = orderedHistoryA,
            historyB = orderedHistoryB,
            chartSize = chartSize,
            axisMin = axis.min,
            axisMax = axis.max,
            density = density,
            colorA = scenarioAColor,
            colorB = scenarioBColor,
        )
    }
    val hitRadiusPx = with(density) { 28.dp.toPx() }
    val chartDescription =
        "Comparison of scenario A (${orderedHistoryA.size} games) and scenario B (${orderedHistoryB.size} games), oldest to latest."

    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appCardBorder(),
    ) {
        Column(
            modifier = Modifier.padding(14.dp),
            verticalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            Text(
                "Scenario comparison graph",
                modifier = Modifier.semantics { heading() },
                style = MaterialTheme.typography.titleMedium,
                fontWeight = FontWeight.SemiBold,
            )
            Canvas(
                modifier = Modifier
                    .fillMaxWidth()
                    .height(ChartHeightDp.dp)
                    .onSizeChanged { chartSize = it }
                    .semantics { contentDescription = chartDescription }
                    .pointerInput(plottedPoints) {
                        detectTapGestures { tapOffset ->
                            val nearest = plottedPoints
                                .map { point -> point to point.offset.getDistanceSquared(tapOffset) }
                                .minByOrNull { it.second }
                            selectedPoint = nearest
                                ?.takeIf { it.second <= (hitRadiusPx * hitRadiusPx) }
                                ?.first
                        }
                    },
            ) {
                val left = ChartLeftPaddingDp.dp.toPx()
                val right = size.width - ChartRightPaddingDp.dp.toPx()
                val top = ChartTopPaddingDp.dp.toPx()
                val bottom = size.height - ChartBottomPaddingDp.dp.toPx()
                val chartHeight = (bottom - top).coerceAtLeast(1f)

                fun yFor(value: Double): Float {
                    val normalized = ((value - axis.min) / (axis.max - axis.min)).toFloat()
                    return bottom - (normalized * chartHeight)
                }

                drawChartFrame(
                    axis = axis,
                    textMeasurer = textMeasurer,
                    labelStyle = labelStyle,
                    labelColor = labelColor,
                    gridColor = outlineColor.copy(alpha = 0.2f),
                    axisColor = axisColor,
                )
                drawXAxisLabels(
                    firstLabel = "Oldest",
                    lastLabel = "Latest",
                    textMeasurer = textMeasurer,
                    labelStyle = labelStyle,
                    labelColor = labelColor,
                )

                drawComparisonGuides(
                    filters = scenarioA.filters,
                    left = left,
                    right = right,
                    yFor = ::yFor,
                    color = scenarioAColor.copy(alpha = 0.8f),
                    strokeWidthPx = 2.dp.toPx(),
                )
                drawComparisonGuides(
                    filters = scenarioB.filters,
                    left = left,
                    right = right,
                    yFor = ::yFor,
                    color = scenarioBColor.copy(alpha = 0.8f),
                    strokeWidthPx = 2.dp.toPx(),
                )

                drawScenarioPath(
                    history = orderedHistoryA,
                    color = scenarioAColor,
                    axisMin = axis.min,
                    axisMax = axis.max,
                    chartSize = chartSize,
                )
                drawScenarioPath(
                    history = orderedHistoryB,
                    color = scenarioBColor,
                    axisMin = axis.min,
                    axisMax = axis.max,
                    chartSize = chartSize,
                )

                plottedPoints.forEach { point ->
                    val radius = if (selectedPoint == point) 7.dp.toPx() else 5.dp.toPx()
                    drawCircle(
                        color = point.color,
                        radius = radius,
                        center = point.offset,
                    )
                    if (selectedPoint == point) {
                        drawCircle(
                            color = Color.White,
                            radius = radius + 2.dp.toPx(),
                            center = point.offset,
                            style = Stroke(width = 2.dp.toPx()),
                        )
                    }
                }
            }

            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.spacedBy(16.dp),
                verticalAlignment = Alignment.CenterVertically,
            ) {
                GraphLegendDot(color = scenarioAColor, label = "Scenario A")
                GraphLegendDot(color = scenarioBColor, label = "Scenario B")
            }

            selectedPoint?.let { point ->
                SelectedComparisonPointCard(
                    point = point,
                )
            }
        }
    }
}

@Composable
private fun GraphLegendDot(
    color: Color,
    label: String,
) {
    Row(
        horizontalArrangement = Arrangement.spacedBy(6.dp),
        verticalAlignment = Alignment.CenterVertically,
    ) {
        Box(
            modifier = Modifier
                .width(10.dp)
                .height(10.dp)
                .background(color = color, shape = MaterialTheme.shapes.extraLarge),
        )
        Text(label, style = MaterialTheme.typography.labelSmall)
    }
}

@OptIn(ExperimentalLayoutApi::class)
@Composable
private fun SelectedGraphPointCard(
    entry: PlayerGameLogEntry,
    filters: PlayerStatsFilters,
) {
    val lineLabel = if (filters.lineMode == "interval") {
        "${filters.lowerBoundText} - ${filters.upperBoundText}"
    } else {
        filters.referenceLineText
    }
    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appCardBorder(),
    ) {
        Column(
            modifier = Modifier.padding(12.dp),
            verticalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            Text(
                formatGameDate(entry.date),
                style = MaterialTheme.typography.titleSmall,
                fontWeight = FontWeight.SemiBold,
            )
            FlowRow(
                horizontalArrangement = Arrangement.spacedBy(8.dp),
                verticalArrangement = Arrangement.spacedBy(8.dp),
            ) {
                InlineChip("Round: ${entry.roundLabel ?: "--"}")
                InlineChip("Opp: ${entry.opposition ?: "--"}")
                InlineChip("Venue: ${entry.venue ?: "--"}")
                InlineChip("Value: ${formatNumber(entry.selectedValue)}")
                InlineChip("Line: $lineLabel")
                InlineChip(
                    when (entry.hit) {
                        true -> "Result: Hit"
                        false -> "Result: Miss"
                        null -> "Result: --"
                    },
                )
            }
        }
    }
}

private fun formatGraphValue(value: Double): String =
    String.format(Locale.getDefault(), "%.1f", value)

private fun roundDownToHalf(value: Double): Double = kotlin.math.floor(value * 2.0) / 2.0

private fun roundUpToHalf(value: Double): Double = kotlin.math.ceil(value * 2.0) / 2.0

private data class IndexedOffset(
    val index: Int,
    val offset: Offset,
)

private fun Offset.getDistanceSquared(other: Offset): Float {
    val dx = x - other.x
    val dy = y - other.y
    return (dx * dx) + (dy * dy)
}

@OptIn(ExperimentalMaterial3Api::class, ExperimentalLayoutApi::class)
@Composable
private fun PlayerStatsFilterSheet(
    title: String,
    filterOptions: PlayerStatFilterOptions?,
    filters: PlayerStatsFilters,
    showStatAndLineControls: Boolean,
    onFiltersChanged: (PlayerStatsFilters) -> Unit,
    onApply: () -> Unit,
    onClear: () -> Unit,
    onDismiss: () -> Unit,
) {
    var statExpanded by remember { mutableStateOf(false) }

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
                title,
                modifier = Modifier.semantics { heading() },
                style = MaterialTheme.typography.headlineSmall,
            )

            if (filterOptions == null) {
                LoadingCard("Loading player filters")
            } else {
                if (showStatAndLineControls) {
                    ExposedDropdownMenuBox(
                        expanded = statExpanded,
                        onExpandedChange = { statExpanded = !statExpanded },
                    ) {
                        OutlinedTextField(
                            value = filterOptions.stats.firstOrNull { it.code == filters.statCode }?.label ?: filters.statCode,
                            onValueChange = {},
                            modifier = Modifier
                                .menuAnchor(ExposedDropdownMenuAnchorType.PrimaryNotEditable)
                                .fillMaxWidth(),
                            readOnly = true,
                            label = { Text("Statistic") },
                            trailingIcon = {
                                ExposedDropdownMenuDefaults.TrailingIcon(expanded = statExpanded)
                            },
                        )
                        DropdownMenu(
                            expanded = statExpanded,
                            onDismissRequest = { statExpanded = false },
                        ) {
                            filterOptions.stats.forEach { option ->
                                DropdownMenuItem(
                                    text = { Text(option.label) },
                                    onClick = {
                                        onFiltersChanged(filters.copy(statCode = option.code))
                                        statExpanded = false
                                    },
                                )
                            }
                        }
                    }

                    LineModeControls(
                        filters = filters,
                        onFiltersChanged = onFiltersChanged,
                    )
                }

                Row(
                    modifier = Modifier.fillMaxWidth(),
                    horizontalArrangement = Arrangement.spacedBy(12.dp),
                ) {
                    StepperField(
                        value = filters.marginMinText,
                        onValueChange = { onFiltersChanged(filters.copy(marginMinText = it)) },
                        label = "Margin min",
                        modifier = Modifier.weight(1f),
                        step = 6.0,
                        allowDecimal = false,
                        allowNegative = true,
                    )
                    StepperField(
                        value = filters.marginMaxText,
                        onValueChange = { onFiltersChanged(filters.copy(marginMaxText = it)) },
                        label = "Margin max",
                        modifier = Modifier.weight(1f),
                        step = 6.0,
                        allowDecimal = false,
                        allowNegative = true,
                    )
                }

                Row(
                    modifier = Modifier.fillMaxWidth(),
                    horizontalArrangement = Arrangement.spacedBy(12.dp),
                ) {
                    StepperField(
                        value = filters.lastGamesText,
                        onValueChange = { onFiltersChanged(filters.copy(lastGamesText = it)) },
                        label = "Last N games",
                        modifier = Modifier.weight(1f),
                        step = 1.0,
                        minValue = 1.0,
                        allowDecimal = false,
                    )
                    StepperField(
                        value = filters.minutesMinimumText,
                        onValueChange = { onFiltersChanged(filters.copy(minutesMinimumText = it)) },
                        label = "Min TOG %",
                        modifier = Modifier.weight(1f),
                        step = 5.0,
                        minValue = 0.0,
                        maxValue = 100.0,
                    )
                }

                ToggleChipGroup(
                    title = "Seasons",
                    options = filterOptions.seasons,
                    selected = filters.seasons,
                    onToggle = { value ->
                        onFiltersChanged(filters.copy(seasons = toggleSelection(filters.seasons, value)))
                    },
                )

                ToggleChipGroup(
                    title = "Home / Away",
                    options = filterOptions.homeAwayOptions,
                    selected = filters.homeAway,
                    onToggle = { value ->
                        onFiltersChanged(filters.copy(homeAway = toggleSelection(filters.homeAway, value)))
                    },
                )

                ToggleChipGroup(
                    title = "Opposition",
                    options = filterOptions.oppositions,
                    selected = filters.oppositions,
                    onToggle = { value ->
                        onFiltersChanged(filters.copy(oppositions = toggleSelection(filters.oppositions, value)))
                    },
                )

                ToggleChipGroup(
                    title = "Venue",
                    options = filterOptions.venues,
                    selected = filters.venues,
                    onToggle = { value ->
                        onFiltersChanged(filters.copy(venues = toggleSelection(filters.venues, value)))
                    },
                )

                ToggleChipGroup(
                    title = "Weather",
                    options = filterOptions.weatherCategories,
                    selected = filters.weatherCategories,
                    onToggle = { value ->
                        onFiltersChanged(filters.copy(weatherCategories = toggleSelection(filters.weatherCategories, value)))
                    },
                )
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
                    Text("Reset")
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

/**
 * Option chips with an expandable overflow instead of silently clipping long
 * lists (some venue lists run past 20 entries).
 */
@OptIn(ExperimentalLayoutApi::class)
@Composable
private fun ToggleChipGroup(
    title: String,
    options: List<String>,
    selected: List<String>,
    onToggle: (String) -> Unit,
    collapsedCount: Int = 12,
) {
    var showAll by rememberSaveable(title) { mutableStateOf(false) }
    // Selected options always stay visible, even when collapsed.
    val visibleOptions = if (showAll || options.size <= collapsedCount) {
        options
    } else {
        val head = options.take(collapsedCount)
        (head + selected.filter { it in options }).distinct()
    }
    Column(verticalArrangement = Arrangement.spacedBy(10.dp)) {
        Text(title, style = MaterialTheme.typography.titleMedium)
        FlowRow(
            horizontalArrangement = Arrangement.spacedBy(8.dp),
            verticalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            visibleOptions.forEach { option ->
                FilterChip(
                    selected = selected.contains(option),
                    onClick = { onToggle(option) },
                    label = { Text(option) },
                    colors = playerAccentFilterChipColors(),
                    border = playerAccentFilterChipBorder(selected.contains(option)),
                )
            }
        }
        if (options.size > collapsedCount) {
            TextButton(onClick = { showAll = !showAll }) {
                Text(if (showAll) "Show fewer" else "Show all ${options.size}")
            }
        }
    }
}

private fun defaultPlayerStatsFilters(options: PlayerStatFilterOptions): PlayerStatsFilters {
    val defaultStat = options.stats.firstOrNull { it.code == "disposals" }?.code
        ?: options.stats.firstOrNull()?.code
        ?: "disposals"
    val defaultSeasons = options.seasons.take(2).ifEmpty { options.seasons.take(1) }
    val defaultHomeAway = options.homeAwayOptions.ifEmpty { listOf("Home", "Away") }
    return PlayerStatsFilters(
        statCode = defaultStat,
        seasons = defaultSeasons,
        homeAway = defaultHomeAway,
    )
}

private fun activePlayerFilterCount(
    filters: PlayerStatsFilters,
    filterOptions: PlayerStatFilterOptions?,
): Int {
    val defaults = filterOptions?.let(::defaultPlayerStatsFilters) ?: PlayerStatsFilters()
    var count = 0
    if (filters.statCode != defaults.statCode) count += 1
    if (filters.lineMode != defaults.lineMode ||
        filters.referenceLineText != defaults.referenceLineText ||
        filters.lowerBoundText != defaults.lowerBoundText ||
        filters.upperBoundText != defaults.upperBoundText
    ) {
        count += 1
    }
    if (filters.seasons.toSet() != defaults.seasons.toSet()) count += 1
    if (filters.homeAway.toSet() != defaults.homeAway.toSet()) count += 1
    if (filters.oppositions.isNotEmpty()) count += 1
    if (filters.venues.isNotEmpty()) count += 1
    if (filters.weatherCategories.isNotEmpty()) count += 1
    if (filters.marginMinText != defaults.marginMinText || filters.marginMaxText != defaults.marginMaxText) count += 1
    if (filters.lastGamesText.isNotBlank()) count += 1
    if (filters.minutesMinimumText != defaults.minutesMinimumText) count += 1
    return count
}

private fun filtersForLaunchRequest(
    options: PlayerStatFilterOptions,
    defaults: PlayerStatsFilters,
    request: PlayerLaunchRequest?,
): PlayerStatsFilters {
    if (request == null) {
        return defaults
    }
    val requestedStatCode = when (request.marketTypeCode) {
        "player_disposals" -> "disposals"
        "player_fantasy_points" -> "fantasy_points"
        "player_tackles" -> "tackles"
        "player_marks" -> "marks"
        "player_goals" -> "goals"
        "player_kicks" -> "kicks"
        "player_handballs" -> "handballs"
        "player_hitouts" -> "hitouts"
        else -> defaults.statCode
    }
    val resolvedStatCode = options.stats.firstOrNull { it.code == requestedStatCode }?.code ?: defaults.statCode
    return defaults.copy(
        statCode = resolvedStatCode,
        lineMode = "single",
        referenceLineText = request.lineValue?.let { formatLineForPrefill(it) } ?: defaults.referenceLineText,
        lowerBoundText = "",
        upperBoundText = "",
    )
}

private fun formatLineForPrefill(line: Double): String =
    if (line % 1.0 == 0.0) {
        line.toInt().toString()
    } else {
        String.format(Locale.getDefault(), "%.1f", line)
    }

private fun PlayerStatsFilters.canRequestSummary(): Boolean =
    when (lineMode) {
        "interval" -> {
            val lower = lowerBoundText.toDoubleOrNull()
            val upper = upperBoundText.toDoubleOrNull()
            lower != null && upper != null && lower < upper
        }
        else -> referenceLineText.toDoubleOrNull() != null
    }

private fun toggleSelection(current: List<String>, value: String): List<String> =
    if (current.contains(value)) {
        current.filterNot { it == value }
    } else {
        current + value
    }

private fun mergeSharedComparisonFilters(
    current: PlayerStatsFilters,
    shared: PlayerStatsFilters,
): PlayerStatsFilters = current.copy(
    statCode = shared.statCode,
    lineMode = shared.lineMode,
    referenceLineText = shared.referenceLineText,
    lowerBoundText = shared.lowerBoundText,
    upperBoundText = shared.upperBoundText,
)

private fun playerLineLabel(filters: PlayerStatsFilters): String =
    if (filters.lineMode == "interval") {
        "${filters.lowerBoundText} - ${filters.upperBoundText}"
    } else {
        filters.referenceLineText
    }

private data class ComparisonPlotPoint(
    val scenarioLabel: String,
    val entry: PlayerGameLogEntry,
    val color: Color,
    val offset: Offset,
)

private fun buildComparisonPlotPoints(
    historyA: List<PlayerGameLogEntry>,
    historyB: List<PlayerGameLogEntry>,
    chartSize: IntSize,
    axisMin: Double,
    axisMax: Double,
    density: androidx.compose.ui.unit.Density,
    colorA: Color,
    colorB: Color,
): List<ComparisonPlotPoint> {
    if (chartSize == IntSize.Zero) return emptyList()
    val left = with(density) { ChartLeftPaddingDp.dp.toPx() }
    val right = chartSize.width.toFloat() - with(density) { ChartRightPaddingDp.dp.toPx() }
    val top = with(density) { ChartTopPaddingDp.dp.toPx() }
    val bottom = chartSize.height.toFloat() - with(density) { ChartBottomPaddingDp.dp.toPx() }
    val chartWidth = (right - left).coerceAtLeast(1f)
    val chartHeight = (bottom - top).coerceAtLeast(1f)

    fun xFor(index: Int, historySize: Int): Float {
        return if (historySize <= 1) {
            left + (chartWidth / 2f)
        } else {
            left + (index.toFloat() / (historySize - 1).toFloat()) * chartWidth
        }
    }

    fun yFor(value: Double): Float {
        val normalized = ((value - axisMin) / (axisMax - axisMin)).toFloat()
        return bottom - (normalized * chartHeight)
    }

    val pointsA = historyA.mapIndexedNotNull { index, entry ->
        val value = entry.selectedValue ?: return@mapIndexedNotNull null
        ComparisonPlotPoint(
            scenarioLabel = "Scenario A",
            entry = entry,
            color = colorA,
            offset = Offset(xFor(index, historyA.size), yFor(value)),
        )
    }
    val pointsB = historyB.mapIndexedNotNull { index, entry ->
        val value = entry.selectedValue ?: return@mapIndexedNotNull null
        ComparisonPlotPoint(
            scenarioLabel = "Scenario B",
            entry = entry,
            color = colorB,
            offset = Offset(xFor(index, historyB.size), yFor(value)),
        )
    }
    return pointsA + pointsB
}

private fun DrawScope.drawComparisonGuides(
    filters: PlayerStatsFilters,
    left: Float,
    right: Float,
    yFor: (Double) -> Float,
    color: Color,
    strokeWidthPx: Float,
) {
    if (filters.lineMode == "interval") {
        filters.lowerBoundText.toDoubleOrNull()?.let { lower ->
            drawLine(
                color = color,
                start = Offset(left, yFor(lower)),
                end = Offset(right, yFor(lower)),
                strokeWidth = strokeWidthPx,
                pathEffect = PathEffect.dashPathEffect(floatArrayOf(14f, 10f)),
            )
        }
        filters.upperBoundText.toDoubleOrNull()?.let { upper ->
            drawLine(
                color = color,
                start = Offset(left, yFor(upper)),
                end = Offset(right, yFor(upper)),
                strokeWidth = strokeWidthPx,
                pathEffect = PathEffect.dashPathEffect(floatArrayOf(14f, 10f)),
            )
        }
    } else {
        filters.referenceLineText.toDoubleOrNull()?.let { line ->
            drawLine(
                color = color,
                start = Offset(left, yFor(line)),
                end = Offset(right, yFor(line)),
                strokeWidth = strokeWidthPx,
                pathEffect = PathEffect.dashPathEffect(floatArrayOf(14f, 10f)),
            )
        }
    }
}

private fun DrawScope.drawScenarioPath(
    history: List<PlayerGameLogEntry>,
    color: Color,
    axisMin: Double,
    axisMax: Double,
    chartSize: IntSize,
) {
    if (history.isEmpty() || chartSize == IntSize.Zero) return
    val left = ChartLeftPaddingDp.dp.toPx()
    val right = chartSize.width.toFloat() - ChartRightPaddingDp.dp.toPx()
    val top = ChartTopPaddingDp.dp.toPx()
    val bottom = chartSize.height.toFloat() - ChartBottomPaddingDp.dp.toPx()
    val chartWidth = (right - left).coerceAtLeast(1f)
    val chartHeight = (bottom - top).coerceAtLeast(1f)

    fun xFor(index: Int): Float {
        return if (history.size <= 1) {
            left + (chartWidth / 2f)
        } else {
            left + (index.toFloat() / history.lastIndex.toFloat()) * chartWidth
        }
    }

    fun yFor(value: Double): Float {
        val normalized = ((value - axisMin) / (axisMax - axisMin)).toFloat()
        return bottom - (normalized * chartHeight)
    }

    val trendPath = Path()
    history.forEachIndexed { index, entry ->
        val value = entry.selectedValue ?: return@forEachIndexed
        val point = Offset(xFor(index), yFor(value))
        if (trendPath.isEmpty) {
            trendPath.moveTo(point.x, point.y)
        } else {
            trendPath.lineTo(point.x, point.y)
        }
    }
    drawPath(
        path = trendPath,
        color = color,
        style = Stroke(width = 2.dp.toPx()),
    )
}

@OptIn(ExperimentalLayoutApi::class)
@Composable
private fun SelectedComparisonPointCard(
    point: ComparisonPlotPoint,
) {
    val entry = point.entry
    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appCardBorder(),
    ) {
        Column(
            modifier = Modifier.padding(12.dp),
            verticalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            Text(
                "${point.scenarioLabel} • ${formatGameDate(entry.date)}",
                style = MaterialTheme.typography.titleSmall,
                fontWeight = FontWeight.SemiBold,
            )
            FlowRow(
                horizontalArrangement = Arrangement.spacedBy(8.dp),
                verticalArrangement = Arrangement.spacedBy(8.dp),
            ) {
                InlineChip("Round: ${entry.roundLabel ?: "--"}")
                InlineChip("Opp: ${entry.opposition ?: "--"}")
                InlineChip("Venue: ${entry.venue ?: "--"}")
                InlineChip("Value: ${formatNumber(entry.selectedValue)}")
                InlineChip(
                    when (entry.hit) {
                        true -> "Result: Hit"
                        false -> "Result: Miss"
                        null -> "Result: --"
                    },
                )
            }
        }
    }
}

private fun comparisonGameCount(state: ComparisonScenarioState): String =
    state.summary?.sampleSize?.toString() ?: state.history.size.toString()

private fun comparisonAverage(history: List<PlayerGameLogEntry>): String {
    val values = history.mapNotNull { it.selectedValue }
    if (values.isEmpty()) return "--"
    return String.format(Locale.getDefault(), "%.1f", values.average())
}

private fun comparisonOutcomeLabels(
    filtersA: PlayerStatsFilters,
    filtersB: PlayerStatsFilters,
): Pair<String, String> =
    if (filtersA.lineMode == filtersB.lineMode) {
        if (filtersA.lineMode == "interval") {
            "Within" to "Outside"
        } else {
            "Over" to "Under"
        }
    } else {
        "Outcome 1" to "Outcome 2"
    }

private fun comparisonOutcomeValue(
    summary: PlayerStatSummary?,
    primary: Boolean,
    showLabelPrefix: Boolean,
): String {
    if (summary == null) return "--"
    val (label, probability, price) = if (summary.lineMode == "interval") {
        if (primary) {
            Triple("In", summary.proportionWithinInterval, summary.impliedOddsWithinInterval)
        } else {
            Triple("Out", summary.proportionOutsideInterval, summary.impliedOddsOutsideInterval)
        }
    } else if (primary) {
        Triple("Over", summary.proportionOver, summary.impliedOddsOver)
    } else {
        Triple("Under", summary.proportionUnder, summary.impliedOddsUnder)
    }
    val value = "${formatPercentage(probability)} / ${formatDecimalPrice(price)}"
    return if (showLabelPrefix) "$label $value" else value
}

@Composable
private fun playerAccentFilterChipColors() = FilterChipDefaults.filterChipColors(
    containerColor = MaterialTheme.colorScheme.secondaryContainer,
    labelColor = MaterialTheme.colorScheme.primary,
    selectedContainerColor = MaterialTheme.colorScheme.tertiary,
    selectedLabelColor = MaterialTheme.colorScheme.onTertiary,
)

@Composable
private fun playerAccentFilterChipBorder(selected: Boolean) = FilterChipDefaults.filterChipBorder(
    enabled = true,
    selected = selected,
    borderColor = MaterialTheme.colorScheme.outlineVariant,
    selectedBorderColor = MaterialTheme.colorScheme.tertiary,
)

private fun formatNumber(value: Double?): String =
    value?.let {
        if (it % 1.0 == 0.0) {
            String.format(Locale.getDefault(), "%.0f", it)
        } else {
            String.format(Locale.getDefault(), "%.1f", it)
        }
    } ?: "--"

private fun compactGameMeta(entry: PlayerGameLogEntry): String =
    listOfNotNull(
        entry.weather,
        entry.margin?.let { "Margin $it" },
        entry.tog?.let { "TOG ${formatNumber(it)}" },
    ).joinToString("  •  ").ifBlank { " " }

private fun formatGameDate(value: String): String =
    runCatching {
        OffsetDateTime.parse(value, DateTimeFormatter.ISO_OFFSET_DATE_TIME)
            .format(CompactDateFormatter)
    }.recoverCatching {
        LocalDateTime.parse(value, DateTimeFormatter.ISO_LOCAL_DATE_TIME)
            .format(CompactDateFormatter)
    }.getOrElse { formatDateTime(value) }

private val CompactDateFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("d MMM", Locale.getDefault())
