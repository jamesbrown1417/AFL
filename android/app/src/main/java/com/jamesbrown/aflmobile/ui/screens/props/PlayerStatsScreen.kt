package com.jamesbrown.aflmobile.ui.screens.props

import androidx.compose.foundation.background
import androidx.compose.foundation.Canvas
import androidx.compose.foundation.horizontalScroll
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.BoxWithConstraints
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.ExperimentalLayoutApi
import androidx.compose.foundation.layout.FlowRow
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.heightIn
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.verticalScroll
import androidx.compose.foundation.gestures.detectTapGestures
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
import androidx.compose.material3.FilterChipDefaults
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
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.Path
import androidx.compose.ui.graphics.PathEffect
import androidx.compose.ui.input.pointer.pointerInput
import androidx.compose.ui.platform.LocalFocusManager
import androidx.compose.ui.layout.onSizeChanged
import androidx.compose.ui.text.input.ImeAction
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.foundation.text.KeyboardActions
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.platform.LocalDensity
import androidx.compose.ui.unit.IntSize
import androidx.compose.ui.unit.dp
import androidx.lifecycle.ViewModel
import androidx.lifecycle.compose.collectAsStateWithLifecycle
import androidx.lifecycle.viewModelScope
import androidx.lifecycle.viewmodel.compose.viewModel
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
import com.jamesbrown.aflmobile.ui.common.DataStatusNavigationIcons
import com.jamesbrown.aflmobile.ui.common.formatDateTime
import com.jamesbrown.aflmobile.ui.common.formatDecimalPrice
import com.jamesbrown.aflmobile.ui.common.formatPercentage
import com.jamesbrown.aflmobile.ui.common.simpleViewModelFactory
import com.jamesbrown.aflmobile.ui.theme.Blue100
import com.jamesbrown.aflmobile.ui.theme.Blue200
import com.jamesbrown.aflmobile.ui.theme.Blue700
import com.jamesbrown.aflmobile.ui.theme.IceWhite
import com.jamesbrown.aflmobile.ui.theme.Orange300
import com.jamesbrown.aflmobile.ui.theme.Orange700
import com.jamesbrown.aflmobile.ui.theme.appCardColors
import com.jamesbrown.aflmobile.ui.theme.appGlassBorder
import com.jamesbrown.aflmobile.ui.theme.appTopBarColors
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import java.util.Locale
import kotlinx.coroutines.async
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.flow.update
import kotlinx.coroutines.launch


data class PlayerStatsUiState(
    val searchQuery: String = "Tim English",
    val allPlayers: List<PlayerSummary> = emptyList(),
    val searchResults: List<PlayerSummary> = emptyList(),
    val selectedPlayer: PlayerSummary? = null,
    val filterOptions: PlayerStatFilterOptions? = null,
    val filters: PlayerStatsFilters = PlayerStatsFilters(),
    val history: List<PlayerGameLogEntry> = emptyList(),
    val summary: PlayerStatSummary? = null,
    val isLoading: Boolean = true,
    val errorMessage: String? = null,
    val infoMessage: String? = null,
)

class PlayerStatsViewModel(
    private val repository: AflRepository,
) : ViewModel() {
    private val _uiState = MutableStateFlow(PlayerStatsUiState())
    val uiState: StateFlow<PlayerStatsUiState> = _uiState.asStateFlow()

    init {
        bootstrap()
    }

    private fun bootstrap() {
        viewModelScope.launch {
            val players = runCatching { repository.searchPlayers("", limit = 5000) }.getOrDefault(emptyList())
            val selected = players.firstOrNull { it.fullName.equals("Tim English", ignoreCase = true) } ?: players.firstOrNull()
            _uiState.update {
                it.copy(
                    searchQuery = selected?.fullName ?: "Tim English",
                    allPlayers = players,
                    searchResults = filterPlayers(players, selected?.fullName ?: "Tim English"),
                    selectedPlayer = selected,
                )
            }
            if (selected != null) {
                loadPlayer(selected)
            } else {
                _uiState.update { it.copy(isLoading = false, errorMessage = "Could not find the default player.") }
            }
        }
    }

    fun updateSearchQuery(query: String) {
        _uiState.update { state ->
            state.copy(
                searchQuery = query,
                searchResults = filterPlayers(state.allPlayers, query),
                errorMessage = null,
            )
        }
    }

    fun selectPlayer(player: PlayerSummary) {
        _uiState.update {
            it.copy(
                selectedPlayer = player,
                searchQuery = player.fullName,
                searchResults = filterPlayers(it.allPlayers, player.fullName),
            )
        }
        loadPlayer(player)
    }

    private fun loadPlayer(player: PlayerSummary) {
        viewModelScope.launch {
            _uiState.update {
                it.copy(
                    isLoading = true,
                    errorMessage = null,
                    infoMessage = null,
                    summary = null,
                    history = emptyList(),
                )
            }
            runCatching { repository.playerStatFilters(player.id) }
                .onSuccess { options ->
                    val filters = defaultPlayerStatsFilters(options)
                    _uiState.update {
                        it.copy(
                            filterOptions = options,
                            filters = filters,
                        )
                    }
                    refresh()
                }
                .onFailure { error ->
                    _uiState.update {
                        it.copy(
                            isLoading = false,
                            errorMessage = error.message ?: "Failed to load player filters.",
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
        viewModelScope.launch {
            _uiState.update { it.copy(isLoading = true, errorMessage = null) }
            val historyResult = runCatching { repository.playerStatHistory(player.id, filters) }
            val summaryResult = if (filters.canRequestSummary()) {
                runCatching { repository.playerStatSummary(player.id, filters) }
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
                            errorMessage = error.message ?: "Failed to load player history.",
                        )
                    }
                }
        }
    }
}

private fun filterPlayers(players: List<PlayerSummary>, query: String): List<PlayerSummary> {
    val normalized = query.trim().lowercase(Locale.getDefault())
    if (players.isEmpty()) return emptyList()
    if (normalized.isBlank()) return players
    return players.filter { it.fullName.lowercase(Locale.getDefault()).contains(normalized) }
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

private enum class PlayerComparisonFocus {
    ScenarioA,
    ScenarioB,
}

private data class PlayerComparisonScenarioState(
    val filters: PlayerStatsFilters,
    val history: List<PlayerGameLogEntry> = emptyList(),
    val summary: PlayerStatSummary? = null,
    val isLoading: Boolean = false,
    val errorMessage: String? = null,
    val infoMessage: String? = null,
)

private val PlayerAccent = Orange700
private val PlayerAccentBorder = Orange300

@Composable
fun PlayerStatsRoute(
    repository: AflRepository,
    onOpenNavigation: () -> Unit,
) {
    val viewModel: PlayerStatsViewModel = viewModel(
        factory = simpleViewModelFactory { PlayerStatsViewModel(repository) },
    )
    val uiState by viewModel.uiState.collectAsStateWithLifecycle()
    PlayerStatsScreen(
        repository = repository,
        uiState = uiState,
        onSearchQueryChanged = viewModel::updateSearchQuery,
        onSelectPlayer = viewModel::selectPlayer,
        onApplyFilters = viewModel::applyFilters,
        onRefresh = viewModel::refresh,
        onOpenNavigation = onOpenNavigation,
    )
}

@OptIn(ExperimentalMaterial3Api::class, ExperimentalLayoutApi::class)
@Composable
private fun PlayerStatsScreen(
    repository: AflRepository,
    uiState: PlayerStatsUiState,
    onSearchQueryChanged: (String) -> Unit,
    onSelectPlayer: (PlayerSummary) -> Unit,
    onApplyFilters: (PlayerStatsFilters) -> Unit,
    onRefresh: () -> Unit,
    onOpenNavigation: () -> Unit,
) {
    var activeTab by rememberSaveable { mutableStateOf(PlayerSubtab.Stats.name) }
    var activeFilterTarget by remember { mutableStateOf<PlayerFilterTarget?>(null) }
    var draftFilters by remember(uiState.filters) { mutableStateOf(uiState.filters) }
    var viewMode by rememberSaveable { mutableStateOf(PlayerViewMode.Table.name) }
    var comparisonViewMode by rememberSaveable { mutableStateOf(PlayerComparisonViewMode.Table.name) }
    var comparisonFocus by rememberSaveable { mutableStateOf(PlayerComparisonFocus.ScenarioA.name) }
    var comparisonRefreshToken by remember { mutableStateOf(0) }
    var availabilityHistory by remember(uiState.filterOptions?.playerId) { mutableStateOf<List<PlayerGameLogEntry>>(emptyList()) }
    var scenarioA by remember(uiState.filterOptions?.playerId) {
        mutableStateOf(PlayerComparisonScenarioState(filters = uiState.filters))
    }
    var scenarioB by remember(uiState.filterOptions?.playerId) {
        mutableStateOf(PlayerComparisonScenarioState(filters = uiState.filters))
    }

    LaunchedEffect(activeFilterTarget, uiState.filters, scenarioA.filters, scenarioB.filters) {
        draftFilters = when (activeFilterTarget) {
            PlayerFilterTarget.Stats -> uiState.filters
            PlayerFilterTarget.ScenarioA -> scenarioA.filters
            PlayerFilterTarget.ScenarioB -> scenarioB.filters
            null -> draftFilters
        }
    }

    LaunchedEffect(uiState.selectedPlayer?.id, uiState.filterOptions?.playerId) {
        val playerId = uiState.selectedPlayer?.id ?: return@LaunchedEffect
        val filterOptions = uiState.filterOptions ?: return@LaunchedEffect
        availabilityHistory = runCatching {
            repository.playerStatHistory(
                playerId = playerId,
                filters = availabilityFilters(filterOptions),
            )
        }.getOrDefault(emptyList())
    }

    LaunchedEffect(activeTab, uiState.selectedPlayer?.id, scenarioA.filters, scenarioB.filters, comparisonRefreshToken) {
        if (PlayerSubtab.valueOf(activeTab) != PlayerSubtab.Comparison) return@LaunchedEffect
        val playerId = uiState.selectedPlayer?.id ?: return@LaunchedEffect
        val filtersA = scenarioA.filters
        val filtersB = scenarioB.filters
        scenarioA = scenarioA.copy(isLoading = true, errorMessage = null, infoMessage = null)
        scenarioB = scenarioB.copy(isLoading = true, errorMessage = null, infoMessage = null)
        coroutineScope {
            val scenarioADeferred = async { loadComparisonScenario(repository, playerId, filtersA) }
            val scenarioBDeferred = async { loadComparisonScenario(repository, playerId, filtersB) }
            val resultA = scenarioADeferred.await()
            val resultB = scenarioBDeferred.await()
            scenarioA = scenarioA.copy(
                filters = filtersA,
                history = resultA.history,
                summary = resultA.summary,
                isLoading = false,
                errorMessage = resultA.errorMessage,
                infoMessage = resultA.infoMessage,
            )
            scenarioB = scenarioB.copy(
                filters = filtersB,
                history = resultB.history,
                summary = resultB.summary,
                isLoading = false,
                errorMessage = resultB.errorMessage,
                infoMessage = resultB.infoMessage,
            )
        }
    }

    fun applyComparisonSharedControls(updatedFilters: PlayerStatsFilters) {
        scenarioA = scenarioA.copy(filters = mergeSharedComparisonFilters(scenarioA.filters, updatedFilters))
        scenarioB = scenarioB.copy(filters = mergeSharedComparisonFilters(scenarioB.filters, updatedFilters))
    }

    Scaffold(
        containerColor = Color.Transparent,
        topBar = {
            TopAppBar(
                title = { Text("Player") },
                colors = appTopBarColors(),
                navigationIcon = {
                    DataStatusNavigationIcons(repository = repository, onOpenNavigation = onOpenNavigation)
                },
                actions = {
                    if (PlayerSubtab.valueOf(activeTab) == PlayerSubtab.Stats) {
                        IconButton(onClick = { activeFilterTarget = PlayerFilterTarget.Stats }) {
                            Icon(Icons.Outlined.FilterList, contentDescription = "Filters")
                        }
                    }
                    IconButton(
                        onClick = {
                            if (PlayerSubtab.valueOf(activeTab) == PlayerSubtab.Stats) {
                                onRefresh()
                            } else {
                                comparisonRefreshToken += 1
                            }
                        },
                    ) {
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
            androidx.compose.foundation.lazy.LazyColumn(
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
                    PlayerSubtabToggle(
                        selected = PlayerSubtab.valueOf(activeTab),
                        onSelected = { activeTab = it.name },
                    )
                }

                uiState.selectedPlayer?.let { selectedPlayer ->
                    if (PlayerSubtab.valueOf(activeTab) == PlayerSubtab.Stats) {
                        item {
                            PlayerStatsFilterSummary(
                                playerName = selectedPlayer.fullName,
                                filters = uiState.filters,
                                filterOptions = uiState.filterOptions,
                            )
                        }
                    } else {
                        item {
                            ComparisonSharedControlsCard(
                                filterOptions = uiState.filterOptions,
                                filters = scenarioA.filters,
                                onFiltersChanged = ::applyComparisonSharedControls,
                            )
                        }
                    }
                }

                if (PlayerSubtab.valueOf(activeTab) == PlayerSubtab.Stats && uiState.isLoading) {
                    item { LoadingCard("Loading player history") }
                }

                uiState.errorMessage?.takeIf { PlayerSubtab.valueOf(activeTab) == PlayerSubtab.Stats }?.let { message ->
                    item { ErrorCard(message) }
                }

                uiState.infoMessage?.takeIf { PlayerSubtab.valueOf(activeTab) == PlayerSubtab.Stats }?.let { message ->
                    item { EmptyCard("Line", message) }
                }

                if (!uiState.isLoading && uiState.selectedPlayer != null) {
                    if (PlayerSubtab.valueOf(activeTab) == PlayerSubtab.Stats) {
                        item {
                            PlayerSummaryCard(summary = uiState.summary)
                        }
                        item {
                            PlayerViewModeToggle(
                                selected = PlayerViewMode.valueOf(viewMode),
                                onSelected = { viewMode = it.name },
                            )
                        }
                        item {
                            if (uiState.history.isEmpty()) {
                                EmptyCard(
                                    title = "No history",
                                    body = "Adjust the player filters or widen the season range.",
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

            if (activeFilterTarget != null) {
                val filterTarget = activeFilterTarget ?: PlayerFilterTarget.Stats
                val availableVenues = deriveAvailableVenues(
                    history = availabilityHistory,
                    filters = draftFilters,
                    fallback = uiState.filterOptions?.venues.orEmpty(),
                )
                val filterOptionsForSheet = uiState.filterOptions?.copy(venues = availableVenues)
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
                            PlayerFilterTarget.ScenarioA -> scenarioA = scenarioA.copy(filters = draftFilters)
                            PlayerFilterTarget.ScenarioB -> scenarioB = scenarioB.copy(filters = draftFilters)
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
    val dropdownPlayers = remember(uiState.allPlayers, uiState.searchResults, uiState.searchQuery, uiState.selectedPlayer, expanded) {
        if (expanded && uiState.selectedPlayer?.fullName == uiState.searchQuery) {
            uiState.allPlayers
        } else {
            uiState.searchResults
        }
    }

    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appGlassBorder(),
    ) {
        Column(
            modifier = Modifier.padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            Text("Player Stats", style = MaterialTheme.typography.titleLarge)
            ExposedDropdownMenuBox(
                expanded = expanded,
                onExpandedChange = { },
            ) {
                OutlinedTextField(
                    value = uiState.searchQuery,
                    onValueChange = {
                        onSearchQueryChanged(it)
                        expanded = false
                    },
                    modifier = Modifier
                        .menuAnchor(ExposedDropdownMenuAnchorType.PrimaryEditable)
                        .fillMaxWidth(),
                    singleLine = true,
                    label = { Text("Select player") },
                    keyboardOptions = KeyboardOptions(imeAction = ImeAction.Search),
                    keyboardActions = KeyboardActions(
                        onSearch = {
                            focusManager.clearFocus()
                            expanded = dropdownPlayers.isNotEmpty()
                        },
                        onDone = {
                            focusManager.clearFocus()
                            expanded = dropdownPlayers.isNotEmpty()
                        },
                    ),
                    trailingIcon = {
                        IconButton(onClick = { expanded = !expanded }) {
                            ExposedDropdownMenuDefaults.TrailingIcon(expanded = expanded)
                        }
                    },
                )
                DropdownMenu(
                    expanded = expanded,
                    onDismissRequest = { expanded = false },
                    modifier = Modifier.heightIn(max = 360.dp),
                ) {
                    dropdownPlayers.forEach { player ->
                        DropdownMenuItem(
                            text = { Text(player.fullName) },
                            onClick = {
                                onSelectPlayer(player)
                                expanded = false
                            },
                        )
                    }
                }
            }
            uiState.selectedPlayer?.let { player ->
                InlineChip("Selected: ${player.fullName}")
            }
            if (uiState.allPlayers.isNotEmpty()) {
                Text(
                    "${dropdownPlayers.size} matching players",
                    style = MaterialTheme.typography.bodySmall,
                    color = MaterialTheme.colorScheme.onSurfaceVariant,
                )
            }
        }
    }
}

@Composable
private fun PlayerSubtabToggle(
    selected: PlayerSubtab,
    onSelected: (PlayerSubtab) -> Unit,
) {
    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appGlassBorder(),
    ) {
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .padding(12.dp),
            horizontalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            FilterChip(
                selected = selected == PlayerSubtab.Stats,
                onClick = { onSelected(PlayerSubtab.Stats) },
                label = { Text("Stats") },
                colors = playerAccentFilterChipColors(),
                border = playerAccentFilterChipBorder(selected == PlayerSubtab.Stats),
            )
            FilterChip(
                selected = selected == PlayerSubtab.Comparison,
                onClick = { onSelected(PlayerSubtab.Comparison) },
                label = { Text("Comparison") },
                colors = playerAccentFilterChipColors(),
                border = playerAccentFilterChipBorder(selected == PlayerSubtab.Comparison),
            )
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
        border = appGlassBorder(),
    ) {
        Column(
            modifier = Modifier.padding(14.dp),
            verticalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            Text(
                "Comparison setup",
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
                        OutlinedTextField(
                            value = filters.lowerBoundText,
                            onValueChange = { onFiltersChanged(filters.copy(lowerBoundText = it)) },
                            modifier = Modifier.weight(1f),
                            label = { Text("Lower") },
                            singleLine = true,
                        )
                        OutlinedTextField(
                            value = filters.upperBoundText,
                            onValueChange = { onFiltersChanged(filters.copy(upperBoundText = it)) },
                            modifier = Modifier.weight(1f),
                            label = { Text("Upper") },
                            singleLine = true,
                        )
                    }
                } else {
                    OutlinedTextField(
                        value = filters.referenceLineText,
                        onValueChange = { onFiltersChanged(filters.copy(referenceLineText = it)) },
                        modifier = Modifier.fillMaxWidth(),
                        label = { Text("Reference line") },
                        singleLine = true,
                    )
                }
            }
        }
    }
}

@OptIn(ExperimentalLayoutApi::class)
@Composable
private fun PlayerStatsFilterSummary(
    playerName: String,
    filters: PlayerStatsFilters,
    filterOptions: PlayerStatFilterOptions?,
) {
    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appGlassBorder(),
    ) {
        Column(
            modifier = Modifier.padding(14.dp),
            verticalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            Text(playerName, style = MaterialTheme.typography.titleMedium, fontWeight = FontWeight.SemiBold)
            PlayerFilterChipFlow(
                filters = filters,
                filterOptions = filterOptions,
            )
        }
    }
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
                "${summary.statLabel} summary",
                style = MaterialTheme.typography.titleMedium,
                fontWeight = FontWeight.SemiBold,
            )
            Text(
                if (summary.lineMode == "interval") {
                    "Interval ${summary.lowerBound} to ${summary.upperBound} across ${summary.sampleSize} games."
                } else {
                    "Line ${summary.referenceLine} across ${summary.sampleSize} games."
                },
                style = MaterialTheme.typography.bodyMedium,
            )
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.spacedBy(8.dp),
            ) {
                DenseSummaryCell(
                    label = if (summary.lineMode == "interval") "WITHIN" else "OVER",
                    value = formatPercentage(summary.proportionWithinInterval ?: summary.proportionOver),
                    modifier = Modifier.weight(1f),
                )
                DenseSummaryCell(
                    label = if (summary.lineMode == "interval") "OUTSIDE" else "UNDER",
                    value = formatPercentage(summary.proportionOutsideInterval ?: summary.proportionUnder),
                    modifier = Modifier.weight(1f),
                )
            }
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.spacedBy(8.dp),
            ) {
                DenseSummaryCell(
                    label = if (summary.lineMode == "interval") "IMPLIED IN" else "IMPLIED O",
                    value = formatDecimalPrice(summary.impliedOddsWithinInterval ?: summary.impliedOddsOver),
                    modifier = Modifier.weight(1f),
                )
                DenseSummaryCell(
                    label = if (summary.lineMode == "interval") "IMPLIED OUT" else "IMPLIED U",
                    value = formatDecimalPrice(summary.impliedOddsOutsideInterval ?: summary.impliedOddsUnder),
                    modifier = Modifier.weight(1f),
                )
            }
        }
    }
}

@Composable
private fun PlayerComparisonContent(
    playerName: String,
    filterOptions: PlayerStatFilterOptions?,
    viewMode: PlayerComparisonViewMode,
    focus: PlayerComparisonFocus,
    scenarioA: PlayerComparisonScenarioState,
    scenarioB: PlayerComparisonScenarioState,
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
        PlayerComparisonViewModeToggle(
            selected = viewMode,
            onSelected = onViewModeChanged,
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
            ComparisonScenarioFocusToggle(
                selected = focus,
                onSelected = onFocusChanged,
            )
            val focusedScenario = if (focus == PlayerComparisonFocus.ScenarioA) scenarioA else scenarioB
            val focusedLabel = if (focus == PlayerComparisonFocus.ScenarioA) "Scenario A" else "Scenario B"
            when {
                focusedScenario.isLoading -> LoadingCard("Loading $focusedLabel")
                focusedScenario.errorMessage != null -> ErrorCard(focusedScenario.errorMessage)
                viewMode == PlayerComparisonViewMode.GameLog && focusedScenario.history.isEmpty() ->
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
    state: PlayerComparisonScenarioState,
    filterOptions: PlayerStatFilterOptions?,
    onEdit: () -> Unit,
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
                verticalAlignment = Alignment.CenterVertically,
            ) {
                Column(verticalArrangement = Arrangement.spacedBy(2.dp)) {
                    Text(title, style = MaterialTheme.typography.titleMedium, fontWeight = FontWeight.SemiBold)
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
private fun PlayerComparisonViewModeToggle(
    selected: PlayerComparisonViewMode,
    onSelected: (PlayerComparisonViewMode) -> Unit,
) {
    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appGlassBorder(),
    ) {
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .padding(12.dp),
            horizontalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            FilterChip(
                selected = selected == PlayerComparisonViewMode.Table,
                onClick = { onSelected(PlayerComparisonViewMode.Table) },
                label = { Text("Table") },
                colors = playerAccentFilterChipColors(),
                border = playerAccentFilterChipBorder(selected == PlayerComparisonViewMode.Table),
            )
            FilterChip(
                selected = selected == PlayerComparisonViewMode.Graph,
                onClick = { onSelected(PlayerComparisonViewMode.Graph) },
                label = { Text("Graph") },
                colors = playerAccentFilterChipColors(),
                border = playerAccentFilterChipBorder(selected == PlayerComparisonViewMode.Graph),
            )
            FilterChip(
                selected = selected == PlayerComparisonViewMode.GameLog,
                onClick = { onSelected(PlayerComparisonViewMode.GameLog) },
                label = { Text("Game log") },
                colors = playerAccentFilterChipColors(),
                border = playerAccentFilterChipBorder(selected == PlayerComparisonViewMode.GameLog),
            )
        }
    }
}

@Composable
private fun ComparisonScenarioFocusToggle(
    selected: PlayerComparisonFocus,
    onSelected: (PlayerComparisonFocus) -> Unit,
) {
    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appGlassBorder(),
    ) {
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .padding(12.dp),
            horizontalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            FilterChip(
                selected = selected == PlayerComparisonFocus.ScenarioA,
                onClick = { onSelected(PlayerComparisonFocus.ScenarioA) },
                label = { Text("Scenario A") },
                colors = playerAccentFilterChipColors(),
                border = playerAccentFilterChipBorder(selected == PlayerComparisonFocus.ScenarioA),
            )
            FilterChip(
                selected = selected == PlayerComparisonFocus.ScenarioB,
                onClick = { onSelected(PlayerComparisonFocus.ScenarioB) },
                label = { Text("Scenario B") },
                colors = playerAccentFilterChipColors(),
                border = playerAccentFilterChipBorder(selected == PlayerComparisonFocus.ScenarioB),
            )
        }
    }
}

@Composable
private fun ComparisonHistoryGraph(
    scenarioA: PlayerComparisonScenarioState,
    scenarioB: PlayerComparisonScenarioState,
) {
    val orderedHistoryA = remember(scenarioA.history) { scenarioA.history.sortedBy { it.gameNumber } }
    val orderedHistoryB = remember(scenarioB.history) { scenarioB.history.sortedBy { it.gameNumber } }
    val allValues = orderedHistoryA.mapNotNull { it.selectedValue } + orderedHistoryB.mapNotNull { it.selectedValue }
    if (allValues.isEmpty()) {
        EmptyCard("No graph", "Adjust the scenario filters to load game history.")
        return
    }

    val scenarioAColor = Orange700
    val scenarioBColor = Blue700
    var chartSize by remember { mutableStateOf(IntSize.Zero) }
    var selectedPoint by remember(orderedHistoryA, orderedHistoryB, scenarioA.filters, scenarioB.filters) {
        mutableStateOf<ComparisonPlotPoint?>(null)
    }
    val density = LocalDensity.current
    val allGuides = listOfNotNull(
        scenarioA.filters.referenceLineText.toDoubleOrNull(),
        scenarioA.filters.lowerBoundText.toDoubleOrNull(),
        scenarioA.filters.upperBoundText.toDoubleOrNull(),
        scenarioB.filters.referenceLineText.toDoubleOrNull(),
        scenarioB.filters.lowerBoundText.toDoubleOrNull(),
        scenarioB.filters.upperBoundText.toDoubleOrNull(),
    )
    val dataMin = allValues.minOrNull() ?: 0.0
    val dataMax = allValues.maxOrNull() ?: 1.0
    val dataSpan = (dataMax - dataMin).takeIf { it > 0.0 } ?: 1.0
    val paddingValue = when {
        dataSpan <= 2.0 -> 0.5
        dataSpan <= 8.0 -> 1.0
        else -> roundUpToHalf(dataSpan * 0.06)
    }
    val visibleRange = remember(dataMin, dataMax, dataSpan, paddingValue, allGuides) {
        var rangeMin = dataMin - paddingValue
        var rangeMax = dataMax + paddingValue
        allGuides.forEach { guide ->
            if (guide < rangeMin && rangeMin - guide <= dataSpan * 0.75) {
                rangeMin = guide - 0.5
            }
            if (guide > rangeMax && guide - rangeMax <= dataSpan * 0.75) {
                rangeMax = guide + 0.5
            }
        }
        roundDownToHalf(rangeMin) to roundUpToHalf(rangeMax)
    }
    val axisMin = visibleRange.first
    val axisStep = remember(visibleRange) {
        roundUpToHalf(((visibleRange.second - visibleRange.first) / 4.0).coerceAtLeast(0.5))
    }
    val axisMax = axisMin + (axisStep * 4.0)
    val outlineColor = MaterialTheme.colorScheme.outline
    val axisColor = MaterialTheme.colorScheme.onSurfaceVariant.copy(alpha = 0.35f)
    val yTickValues = remember(axisMin, axisMax, axisStep) {
        List(5) { index -> axisMax - (axisStep * index.toDouble()) }
    }
    val plottedPoints = remember(orderedHistoryA, orderedHistoryB, chartSize, axisMin, axisMax, density) {
        buildComparisonPlotPoints(
            historyA = orderedHistoryA,
            historyB = orderedHistoryB,
            chartSize = chartSize,
            axisMin = axisMin,
            axisMax = axisMax,
            density = density,
        )
    }
    val hitRadiusPx = with(density) { 28.dp.toPx() }

    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appGlassBorder(),
    ) {
        Column(
            modifier = Modifier.padding(14.dp),
            verticalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            Text(
                "Scenario comparison graph",
                style = MaterialTheme.typography.titleMedium,
                fontWeight = FontWeight.SemiBold,
            )
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.spacedBy(10.dp),
                verticalAlignment = Alignment.Bottom,
            ) {
                Column(
                    modifier = Modifier.height(280.dp),
                    verticalArrangement = Arrangement.SpaceBetween,
                    horizontalAlignment = Alignment.End,
                ) {
                    yTickValues.forEach { tick ->
                        Text(
                            text = formatGraphValue(tick),
                            style = MaterialTheme.typography.labelSmall,
                            color = MaterialTheme.colorScheme.onSurfaceVariant,
                            textAlign = TextAlign.End,
                        )
                    }
                }
                Canvas(
                    modifier = Modifier
                        .weight(1f)
                        .height(280.dp)
                        .onSizeChanged { chartSize = it }
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
                    val left = 24.dp.toPx()
                    val right = size.width - 10.dp.toPx()
                    val top = 14.dp.toPx()
                    val bottom = size.height - 24.dp.toPx()
                    val chartWidth = (right - left).coerceAtLeast(1f)
                    val chartHeight = (bottom - top).coerceAtLeast(1f)

                    fun yFor(value: Double): Float {
                        val normalized = ((value - axisMin) / (axisMax - axisMin)).toFloat()
                        return bottom - (normalized * chartHeight)
                    }

                    repeat(5) { step ->
                        val fraction = step / 4f
                        val y = top + (fraction * chartHeight)
                        drawLine(
                            color = outlineColor.copy(alpha = 0.2f),
                            start = Offset(left, y),
                            end = Offset(right, y),
                            strokeWidth = 1.dp.toPx(),
                        )
                    }

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
                        axisMin = axisMin,
                        axisMax = axisMax,
                        chartSize = chartSize,
                    )
                    drawScenarioPath(
                        history = orderedHistoryB,
                        color = scenarioBColor,
                        axisMin = axisMin,
                        axisMax = axisMax,
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
                                style = androidx.compose.ui.graphics.drawscope.Stroke(width = 2.dp.toPx()),
                            )
                        }
                    }

                    drawLine(
                        color = axisColor,
                        start = Offset(left, bottom),
                        end = Offset(right, bottom),
                        strokeWidth = 1.2.dp.toPx(),
                    )
                }
            }

            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.SpaceBetween,
                verticalAlignment = Alignment.CenterVertically,
            ) {
                Text("Oldest", style = MaterialTheme.typography.labelSmall, color = MaterialTheme.colorScheme.onSurfaceVariant)
                GraphLegendDot(color = scenarioAColor, label = "Scenario A")
                GraphLegendDot(color = scenarioBColor, label = "Scenario B")
                Text("Latest", style = MaterialTheme.typography.labelSmall, color = MaterialTheme.colorScheme.onSurfaceVariant)
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
private fun ComparisonSummaryCard(
    scenarioA: PlayerComparisonScenarioState,
    scenarioB: PlayerComparisonScenarioState,
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
        border = appGlassBorder(),
    ) {
        Column(
            modifier = Modifier.padding(14.dp),
            verticalArrangement = Arrangement.spacedBy(10.dp),
        ) {
            Text(
                "Scenario comparison",
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
            style = MaterialTheme.typography.bodySmall,
        )
        Text(
            row.scenarioBValue,
            modifier = Modifier.weight(1f),
            style = MaterialTheme.typography.bodySmall,
        )
    }
}

@Composable
private fun PlayerViewModeToggle(
    selected: PlayerViewMode,
    onSelected: (PlayerViewMode) -> Unit,
) {
    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appGlassBorder(),
    ) {
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .padding(12.dp),
            horizontalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            FilterChip(
                selected = selected == PlayerViewMode.Table,
                onClick = { onSelected(PlayerViewMode.Table) },
                label = { Text("Table") },
                colors = playerAccentFilterChipColors(),
                border = playerAccentFilterChipBorder(selected == PlayerViewMode.Table),
            )
            FilterChip(
                selected = selected == PlayerViewMode.Graph,
                onClick = { onSelected(PlayerViewMode.Graph) },
                label = { Text("Graph") },
                colors = playerAccentFilterChipColors(),
                border = playerAccentFilterChipBorder(selected == PlayerViewMode.Graph),
            )
        }
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
private fun PlayerHistoryTable(history: List<PlayerGameLogEntry>) {
    val scrollState = rememberScrollState()
    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appGlassBorder(),
    ) {
        Column(
            modifier = Modifier
                .horizontalScroll(scrollState)
                .padding(12.dp),
        ) {
            HistoryHeaderRow()
            history.forEachIndexed { index, entry ->
                if (index > 0) {
                    HorizontalDivider(modifier = Modifier.fillMaxWidth())
                }
                HistoryDataRow(entry)
            }
        }
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
    val selectedValues = orderedHistory.mapNotNull { it.selectedValue }
    if (selectedValues.isEmpty()) {
        EmptyCard("No graph", "No selected stat values are available for the current filter set.")
        return
    }

    val singleLine = filters.referenceLineText.toDoubleOrNull()
    val lowerBound = filters.lowerBoundText.toDoubleOrNull()
    val upperBound = filters.upperBoundText.toDoubleOrNull()
    val dataMin = selectedValues.minOrNull() ?: 0.0
    val dataMax = selectedValues.maxOrNull() ?: 1.0
    val dataSpan = (dataMax - dataMin).takeIf { it > 0.0 } ?: 1.0
    val paddingValue = when {
        dataSpan <= 2.0 -> 0.5
        dataSpan <= 8.0 -> 1.0
        else -> roundUpToHalf(dataSpan * 0.06)
    }
    val visibleRange = remember(dataMin, dataMax, dataSpan, paddingValue, singleLine, lowerBound, upperBound) {
        var rangeMin = dataMin - paddingValue
        var rangeMax = dataMax + paddingValue
        listOfNotNull(singleLine, lowerBound, upperBound).forEach { guide ->
            if (guide < rangeMin && rangeMin - guide <= dataSpan * 0.75) {
                rangeMin = guide - 0.5
            }
            if (guide > rangeMax && guide - rangeMax <= dataSpan * 0.75) {
                rangeMax = guide + 0.5
            }
        }
        roundDownToHalf(rangeMin) to roundUpToHalf(rangeMax)
    }
    val axisMin = visibleRange.first
    val axisStep = remember(visibleRange) {
        roundUpToHalf(((visibleRange.second - visibleRange.first) / 4.0).coerceAtLeast(0.5))
    }
    val axisMax = axisMin + (axisStep * 4.0)
    val outlineColor = MaterialTheme.colorScheme.outline
    val primaryColor = MaterialTheme.colorScheme.primary
    val axisColor = MaterialTheme.colorScheme.onSurfaceVariant.copy(alpha = 0.35f)
    val yTickValues = remember(axisMin, axisMax, axisStep) {
        List(5) { index -> axisMax - (axisStep * index.toDouble()) }
    }
    val plottedPoints = remember(orderedHistory, chartSize, axisMin, axisMax, density) {
        if (chartSize == IntSize.Zero) {
            emptyList()
        } else {
            val left = with(density) { 24.dp.toPx() }
            val right = chartSize.width.toFloat() - with(density) { 10.dp.toPx() }
            val top = with(density) { 14.dp.toPx() }
            val bottom = chartSize.height.toFloat() - with(density) { 24.dp.toPx() }
            val chartWidth = (right - left).coerceAtLeast(1f)
            val chartHeight = (bottom - top).coerceAtLeast(1f)
            orderedHistory.mapIndexedNotNull { index, entry ->
                val value = entry.selectedValue ?: return@mapIndexedNotNull null
                val x = if (orderedHistory.size == 1) {
                    left + (chartWidth / 2f)
                } else {
                    left + (index.toFloat() / orderedHistory.lastIndex.toFloat()) * chartWidth
                }
                val normalized = ((value - axisMin) / (axisMax - axisMin)).toFloat()
                val y = bottom - (normalized * chartHeight)
                IndexedOffset(index = index, offset = Offset(x, y))
            }
        }
    }
    val selectedEntry = selectedPointIndex?.let { orderedHistory.getOrNull(it) }
    val hitRadiusPx = with(density) { 28.dp.toPx() }

    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appGlassBorder(),
    ) {
        Column(
            modifier = Modifier.padding(14.dp),
            verticalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            Text(
                "${orderedHistory.first().selectedStat} graph",
                style = MaterialTheme.typography.titleMedium,
                fontWeight = FontWeight.SemiBold,
            )
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.spacedBy(10.dp),
                verticalAlignment = Alignment.Bottom,
            ) {
                Column(
                    modifier = Modifier.height(280.dp),
                    verticalArrangement = Arrangement.SpaceBetween,
                    horizontalAlignment = Alignment.End,
                ) {
                    yTickValues.forEach { tick ->
                        Text(
                            text = formatGraphValue(tick),
                            style = MaterialTheme.typography.labelSmall,
                            color = MaterialTheme.colorScheme.onSurfaceVariant,
                            textAlign = TextAlign.End,
                        )
                    }
                }
                Canvas(
                    modifier = Modifier
                        .weight(1f)
                        .height(280.dp)
                        .onSizeChanged { chartSize = it }
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
                    val left = 24.dp.toPx()
                    val right = size.width - 10.dp.toPx()
                    val top = 14.dp.toPx()
                    val bottom = size.height - 24.dp.toPx()
                    val chartWidth = (right - left).coerceAtLeast(1f)
                    val chartHeight = (bottom - top).coerceAtLeast(1f)

                    fun xFor(index: Int): Float =
                        if (orderedHistory.size == 1) {
                            left + (chartWidth / 2f)
                        } else {
                            left + (index.toFloat() / orderedHistory.lastIndex.toFloat()) * chartWidth
                        }

                    fun yFor(value: Double): Float {
                        val normalized = ((value - axisMin) / (axisMax - axisMin)).toFloat()
                        return bottom - (normalized * chartHeight)
                    }

                    repeat(5) { step ->
                        val fraction = step / 4f
                        val y = top + (fraction * chartHeight)
                        drawLine(
                            color = outlineColor.copy(alpha = 0.2f),
                            start = Offset(left, y),
                            end = Offset(right, y),
                            strokeWidth = 1.dp.toPx(),
                        )
                    }

                    if (filters.lineMode == "interval" && lowerBound != null && upperBound != null) {
                        val topBand = yFor(upperBound)
                        val bottomBand = yFor(lowerBound)
                        drawRect(
                            color = Color(0x2624A148),
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
                        style = androidx.compose.ui.graphics.drawscope.Stroke(width = 2.dp.toPx()),
                    )

                    orderedHistory.forEachIndexed { index, entry ->
                        val value = entry.selectedValue ?: return@forEachIndexed
                        val pointColor = when (entry.hit) {
                            true -> Color(0xFF1B7F46)
                            false -> Color(0xFFD14343)
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
                                style = androidx.compose.ui.graphics.drawscope.Stroke(width = 2.dp.toPx()),
                            )
                        }
                    }

                    drawLine(
                        color = axisColor,
                        start = Offset(left, bottom),
                        end = Offset(right, bottom),
                        strokeWidth = 1.2.dp.toPx(),
                    )
                }
            }

            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.SpaceBetween,
                verticalAlignment = Alignment.CenterVertically,
            ) {
                Text("Oldest", style = MaterialTheme.typography.labelSmall, color = MaterialTheme.colorScheme.onSurfaceVariant)
                GraphLegendDot(color = Color(0xFF1B7F46), label = "Hit")
                GraphLegendDot(color = Color(0xFFD14343), label = "Miss")
                Text("Latest", style = MaterialTheme.typography.labelSmall, color = MaterialTheme.colorScheme.onSurfaceVariant)
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
                .background(color = color, shape = RoundedCornerShape(100.dp)),
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
        border = appGlassBorder(),
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

@Composable
private fun HistoryHeaderRow() {
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .padding(bottom = 8.dp),
        horizontalArrangement = Arrangement.spacedBy(8.dp),
    ) {
        HistoryCell("Date", 112.dp, header = true)
        HistoryCell("Round", 78.dp, header = true)
        HistoryCell("Opp", 104.dp, header = true)
        HistoryCell("Venue", 120.dp, header = true)
        HistoryCell("H/A", 52.dp, header = true)
        HistoryCell("Weather", 110.dp, header = true)
        HistoryCell("Margin", 64.dp, header = true)
        HistoryCell("TOG", 60.dp, header = true)
        HistoryCell("Disp", 60.dp, header = true)
        HistoryCell("Fantasy", 72.dp, header = true)
        HistoryCell("Marks", 64.dp, header = true)
        HistoryCell("Goals", 64.dp, header = true)
        HistoryCell("Tackles", 72.dp, header = true)
        HistoryCell("Hitouts", 72.dp, header = true)
        HistoryCell("Selected", 72.dp, header = true)
        HistoryCell("Hit", 56.dp, header = true)
    }
}

@Composable
private fun HistoryDataRow(entry: PlayerGameLogEntry) {
    val rowTint = when (entry.hit) {
        true -> Color(0xFFDBF5E4)
        false -> Color(0xFFF9DFDF)
        null -> Color.Transparent
    }
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .background(
                color = rowTint,
                shape = RoundedCornerShape(10.dp),
            )
            .padding(vertical = 8.dp),
        horizontalArrangement = Arrangement.spacedBy(8.dp),
    ) {
        HistoryCell(formatGameDate(entry.date), 112.dp)
        HistoryCell(entry.roundLabel ?: "--", 78.dp)
        HistoryCell(entry.opposition ?: "--", 104.dp)
        HistoryCell(entry.venue ?: "--", 120.dp)
        HistoryCell(
            when {
                entry.team != null && entry.home != null && entry.team == entry.home -> "H"
                entry.team != null -> "A"
                else -> "--"
            },
            52.dp,
        )
        HistoryCell(entry.weather ?: "--", 110.dp)
        HistoryCell(entry.margin?.toString() ?: "--", 64.dp)
        HistoryCell(formatNumber(entry.tog), 60.dp)
        HistoryCell(formatNumber(entry.disposals), 60.dp)
        HistoryCell(formatNumber(entry.fantasy), 72.dp)
        HistoryCell(formatNumber(entry.marks), 64.dp)
        HistoryCell(formatNumber(entry.goals), 64.dp)
        HistoryCell(formatNumber(entry.tackles), 72.dp)
        HistoryCell(formatNumber(entry.hitouts), 72.dp)
        HistoryCell(formatNumber(entry.selectedValue), 72.dp, highlighted = true)
        HistoryCell(
            when (entry.hit) {
                true -> "Yes"
                false -> "No"
                null -> "--"
            },
            56.dp,
            color = when (entry.hit) {
                true -> Color(0xFF1B7F46)
                false -> MaterialTheme.colorScheme.error
                null -> MaterialTheme.colorScheme.onSurface
            },
        )
    }
}

@Composable
private fun HistoryCell(
    text: String,
    width: androidx.compose.ui.unit.Dp,
    header: Boolean = false,
    highlighted: Boolean = false,
    color: Color = MaterialTheme.colorScheme.onSurface,
) {
    Text(
        text = text,
        modifier = Modifier.width(width),
        style = if (header) MaterialTheme.typography.labelMedium else MaterialTheme.typography.bodySmall,
        fontWeight = when {
            header -> FontWeight.Bold
            highlighted -> FontWeight.SemiBold
            else -> FontWeight.Normal
        },
        color = if (header) MaterialTheme.colorScheme.onSurfaceVariant else color,
    )
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
            Text(title, style = MaterialTheme.typography.headlineSmall)

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
                            OutlinedTextField(
                                value = filters.lowerBoundText,
                                onValueChange = { onFiltersChanged(filters.copy(lowerBoundText = it)) },
                                modifier = Modifier.weight(1f),
                                label = { Text("Lower") },
                                singleLine = true,
                            )
                            OutlinedTextField(
                                value = filters.upperBoundText,
                                onValueChange = { onFiltersChanged(filters.copy(upperBoundText = it)) },
                                modifier = Modifier.weight(1f),
                                label = { Text("Upper") },
                                singleLine = true,
                            )
                        }
                    } else {
                        OutlinedTextField(
                            value = filters.referenceLineText,
                            onValueChange = { onFiltersChanged(filters.copy(referenceLineText = it)) },
                            modifier = Modifier.fillMaxWidth(),
                            label = { Text("Reference line") },
                            singleLine = true,
                        )
                    }
                }

                Row(
                    modifier = Modifier.fillMaxWidth(),
                    horizontalArrangement = Arrangement.spacedBy(12.dp),
                ) {
                    OutlinedTextField(
                        value = filters.marginMinText,
                        onValueChange = { onFiltersChanged(filters.copy(marginMinText = it)) },
                        modifier = Modifier.weight(1f),
                        label = { Text("Margin min") },
                        singleLine = true,
                    )
                    OutlinedTextField(
                        value = filters.marginMaxText,
                        onValueChange = { onFiltersChanged(filters.copy(marginMaxText = it)) },
                        modifier = Modifier.weight(1f),
                        label = { Text("Margin max") },
                        singleLine = true,
                    )
                }

                Row(
                    modifier = Modifier.fillMaxWidth(),
                    horizontalArrangement = Arrangement.spacedBy(12.dp),
                ) {
                    OutlinedTextField(
                        value = filters.lastGamesText,
                        onValueChange = { onFiltersChanged(filters.copy(lastGamesText = it)) },
                        modifier = Modifier.weight(1f),
                        label = { Text("Last N games") },
                        singleLine = true,
                    )
                    OutlinedTextField(
                        value = filters.minutesMinimumText,
                        onValueChange = { onFiltersChanged(filters.copy(minutesMinimumText = it)) },
                        modifier = Modifier.weight(1f),
                        label = { Text("Min TOG %") },
                        singleLine = true,
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

@OptIn(ExperimentalLayoutApi::class)
@Composable
private fun ToggleChipGroup(
    title: String,
    options: List<String>,
    selected: List<String>,
    onToggle: (String) -> Unit,
) {
    Column(verticalArrangement = Arrangement.spacedBy(10.dp)) {
        Text(title, style = MaterialTheme.typography.titleMedium)
        FlowRow(
            horizontalArrangement = Arrangement.spacedBy(8.dp),
            verticalArrangement = Arrangement.spacedBy(8.dp),
            modifier = Modifier.heightIn(max = 220.dp),
        ) {
            options.forEach { option ->
                FilterChip(
                    selected = selected.contains(option),
                    onClick = { onToggle(option) },
                    label = { Text(option) },
                    colors = playerAccentFilterChipColors(),
                    border = playerAccentFilterChipBorder(selected.contains(option)),
                )
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

private fun availabilityFilters(options: PlayerStatFilterOptions): PlayerStatsFilters =
    PlayerStatsFilters(
        statCode = options.stats.firstOrNull { it.code == "disposals" }?.code
            ?: options.stats.firstOrNull()?.code
            ?: "disposals",
        seasons = emptyList(),
        oppositions = emptyList(),
        venues = emptyList(),
        weatherCategories = emptyList(),
        homeAway = emptyList(),
        marginMinText = "-200",
        marginMaxText = "200",
        lastGamesText = "",
        minutesMinimumText = "0",
        lineMode = "single",
        referenceLineText = "",
        lowerBoundText = "",
        upperBoundText = "",
    )

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

private fun deriveAvailableVenues(
    history: List<PlayerGameLogEntry>,
    filters: PlayerStatsFilters,
    fallback: List<String>,
): List<String> {
    if (history.isEmpty()) return fallback
    val minMargin = filters.marginMinText.toIntOrNull() ?: -200
    val maxMargin = filters.marginMaxText.toIntOrNull() ?: 200
    val minTog = filters.minutesMinimumText.toDoubleOrNull() ?: 0.0
    val filtered = history
        .filter { entry ->
            val season = entry.date.take(4)
            (filters.seasons.isEmpty() || filters.seasons.contains(season)) &&
                (filters.homeAway.isEmpty() || filters.homeAway.contains(resolveHomeAway(entry))) &&
                ((entry.margin ?: 0) in minMargin..maxMargin) &&
                ((entry.tog ?: 0.0) >= minTog)
        }
        .let { rows ->
            val lastGames = filters.lastGamesText.toIntOrNull()
            if (lastGames != null) rows.take(lastGames) else rows
        }
        .filter { entry ->
            (filters.oppositions.isEmpty() || filters.oppositions.contains(entry.opposition)) &&
                (filters.weatherCategories.isEmpty() || filters.weatherCategories.contains(entry.weather))
        }
    return (filtered.mapNotNull { it.venue } + filters.venues)
        .distinct()
        .sorted()
        .ifEmpty { fallback }
}

private fun resolveHomeAway(entry: PlayerGameLogEntry): String =
    when {
        entry.team != null && entry.home != null && entry.team == entry.home -> "Home"
        entry.team != null -> "Away"
        else -> ""
    }

private suspend fun loadComparisonScenario(
    repository: AflRepository,
    playerId: Int,
    filters: PlayerStatsFilters,
): PlayerComparisonScenarioState {
    val historyResult = runCatching { repository.playerStatHistory(playerId, filters) }
    val summaryResult = if (filters.canRequestSummary()) {
        runCatching { repository.playerStatSummary(playerId, filters) }
    } else {
        Result.success(null)
    }
    return historyResult.fold(
        onSuccess = { history ->
            PlayerComparisonScenarioState(
                filters = filters,
                history = history,
                summary = summaryResult.getOrNull(),
                errorMessage = null,
                infoMessage = playerSummaryInfoMessage(filters),
            )
        },
        onFailure = { error ->
            PlayerComparisonScenarioState(
                filters = filters,
                history = emptyList(),
                summary = null,
                errorMessage = error.message ?: "Failed to load comparison scenario.",
                infoMessage = playerSummaryInfoMessage(filters),
            )
        },
    )
}

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
): List<ComparisonPlotPoint> {
    if (chartSize == IntSize.Zero) return emptyList()
    val left = with(density) { 24.dp.toPx() }
    val right = chartSize.width.toFloat() - with(density) { 10.dp.toPx() }
    val top = with(density) { 14.dp.toPx() }
    val bottom = chartSize.height.toFloat() - with(density) { 24.dp.toPx() }
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
            color = Orange700,
            offset = Offset(xFor(index, historyA.size), yFor(value)),
        )
    }
    val pointsB = historyB.mapIndexedNotNull { index, entry ->
        val value = entry.selectedValue ?: return@mapIndexedNotNull null
        ComparisonPlotPoint(
            scenarioLabel = "Scenario B",
            entry = entry,
            color = Blue700,
            offset = Offset(xFor(index, historyB.size), yFor(value)),
        )
    }
    return pointsA + pointsB
}

private fun androidx.compose.ui.graphics.drawscope.DrawScope.drawComparisonGuides(
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

private fun androidx.compose.ui.graphics.drawscope.DrawScope.drawScenarioPath(
    history: List<PlayerGameLogEntry>,
    color: Color,
    axisMin: Double,
    axisMax: Double,
    chartSize: IntSize,
) {
    if (history.isEmpty() || chartSize == IntSize.Zero) return
    val left = 24.dp.toPx()
    val right = chartSize.width.toFloat() - 10.dp.toPx()
    val top = 14.dp.toPx()
    val bottom = chartSize.height.toFloat() - 24.dp.toPx()
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
        style = androidx.compose.ui.graphics.drawscope.Stroke(width = 2.dp.toPx()),
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
        border = appGlassBorder(),
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

private fun comparisonGameCount(state: PlayerComparisonScenarioState): String =
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
    containerColor = Blue100,
    labelColor = Blue700,
    selectedContainerColor = PlayerAccent,
    selectedLabelColor = IceWhite,
)

@Composable
private fun playerAccentFilterChipBorder(selected: Boolean) = FilterChipDefaults.filterChipBorder(
    enabled = true,
    selected = selected,
    borderColor = Blue200,
    selectedBorderColor = PlayerAccentBorder,
)

private fun formatNumber(value: Double?): String =
    value?.let {
        if (it % 1.0 == 0.0) {
            String.format(Locale.getDefault(), "%.0f", it)
        } else {
            String.format(Locale.getDefault(), "%.1f", it)
        }
    } ?: "--"

private fun formatGameDate(value: String): String =
    runCatching {
        OffsetDateTime.parse(value, DateTimeFormatter.ISO_OFFSET_DATE_TIME)
            .format(DateTimeFormatter.ofPattern("d MMM yy", Locale.getDefault()))
    }.getOrElse { formatDateTime(value) }
