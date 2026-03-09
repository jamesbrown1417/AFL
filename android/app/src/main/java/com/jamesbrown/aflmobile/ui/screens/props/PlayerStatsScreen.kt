package com.jamesbrown.aflmobile.ui.screens.props

import androidx.compose.foundation.background
import androidx.compose.foundation.horizontalScroll
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
import com.jamesbrown.aflmobile.ui.common.formatDateTime
import com.jamesbrown.aflmobile.ui.common.formatDecimalPrice
import com.jamesbrown.aflmobile.ui.common.formatPercentage
import com.jamesbrown.aflmobile.ui.common.simpleViewModelFactory
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import java.util.Locale
import kotlinx.coroutines.Job
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.flow.update
import kotlinx.coroutines.launch


data class PlayerStatsUiState(
    val searchQuery: String = "Tim English",
    val searchResults: List<PlayerSummary> = emptyList(),
    val selectedPlayer: PlayerSummary? = null,
    val filterOptions: PlayerStatFilterOptions? = null,
    val filters: PlayerStatsFilters = PlayerStatsFilters(),
    val history: List<PlayerGameLogEntry> = emptyList(),
    val summary: PlayerStatSummary? = null,
    val isSearchingPlayers: Boolean = false,
    val isLoading: Boolean = true,
    val errorMessage: String? = null,
    val infoMessage: String? = null,
)

class PlayerStatsViewModel(
    private val repository: AflRepository,
) : ViewModel() {
    private val _uiState = MutableStateFlow(PlayerStatsUiState())
    val uiState: StateFlow<PlayerStatsUiState> = _uiState.asStateFlow()

    private var searchJob: Job? = null

    init {
        bootstrap()
    }

    private fun bootstrap() {
        viewModelScope.launch {
            val players = runCatching { repository.searchPlayers("Tim English") }.getOrDefault(emptyList())
            val selected = players.firstOrNull()
            _uiState.update {
                it.copy(
                    searchQuery = selected?.fullName ?: "Tim English",
                    searchResults = players,
                    selectedPlayer = selected,
                    isSearchingPlayers = false,
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
        _uiState.update { it.copy(searchQuery = query) }
        searchJob?.cancel()
        if (query.length < 2) {
            _uiState.update { it.copy(searchResults = emptyList(), isSearchingPlayers = false) }
            return
        }
        _uiState.update { it.copy(isSearchingPlayers = true) }
        searchJob = viewModelScope.launch {
            delay(250)
            runCatching { repository.searchPlayers(query) }
                .onSuccess { results ->
                    _uiState.update { state ->
                        state.copy(
                            searchResults = results,
                            isSearchingPlayers = false,
                            errorMessage = null,
                        )
                    }
                }
                .onFailure { error ->
                    _uiState.update { state ->
                        state.copy(
                            isSearchingPlayers = false,
                            errorMessage = error.message ?: "Failed to search players.",
                        )
                    }
                }
        }
    }

    fun selectPlayer(player: PlayerSummary) {
        _uiState.update {
            it.copy(
                selectedPlayer = player,
                searchQuery = player.fullName,
                searchResults = emptyList(),
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
                            infoMessage = summaryInfoMessage(filters),
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

    private fun summaryInfoMessage(filters: PlayerStatsFilters): String? = when {
        filters.lineMode == "interval" && !filters.canRequestSummary() ->
            "Set both interval bounds to calculate implied prices."
        filters.lineMode != "interval" && !filters.canRequestSummary() ->
            "Set a reference line to calculate over/under implied prices."
        else -> null
    }
}

@Composable
fun PlayerStatsRoute(
    repository: AflRepository,
) {
    val viewModel: PlayerStatsViewModel = viewModel(
        factory = simpleViewModelFactory { PlayerStatsViewModel(repository) },
    )
    val uiState by viewModel.uiState.collectAsStateWithLifecycle()
    PlayerStatsScreen(
        uiState = uiState,
        onSearchQueryChanged = viewModel::updateSearchQuery,
        onSelectPlayer = viewModel::selectPlayer,
        onApplyFilters = viewModel::applyFilters,
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
                title = { Text("Player") },
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

                uiState.selectedPlayer?.let { selectedPlayer ->
                    item {
                        PlayerStatsFilterSummary(
                            playerName = selectedPlayer.fullName,
                            filters = uiState.filters,
                            filterOptions = uiState.filterOptions,
                            onOpenFilters = { showFilters = true },
                        )
                    }
                }

                if (uiState.isLoading) {
                    item { LoadingCard("Loading player history") }
                }

                uiState.errorMessage?.let { message ->
                    item { ErrorCard(message) }
                }

                uiState.infoMessage?.let { message ->
                    item { EmptyCard("Line", message) }
                }

                if (!uiState.isLoading && uiState.selectedPlayer != null) {
                    item {
                        PlayerSummaryCard(summary = uiState.summary)
                    }
                    item {
                        if (uiState.history.isEmpty()) {
                            EmptyCard(
                                title = "No history",
                                body = "Adjust the player filters or widen the season range.",
                            )
                        } else {
                            PlayerHistoryTable(uiState.history)
                        }
                    }
                }
            }

            if (showFilters) {
                PlayerStatsFilterSheet(
                    filterOptions = uiState.filterOptions,
                    filters = draftFilters,
                    onFiltersChanged = { draftFilters = it },
                    onApply = {
                        onApplyFilters(draftFilters)
                        showFilters = false
                    },
                    onClear = {
                        draftFilters = uiState.filterOptions?.let(::defaultPlayerStatsFilters) ?: PlayerStatsFilters()
                    },
                    onDismiss = { showFilters = false },
                )
            }
        }
    }
}

@Composable
private fun PlayerSearchCard(
    uiState: PlayerStatsUiState,
    onSearchQueryChanged: (String) -> Unit,
    onSelectPlayer: (PlayerSummary) -> Unit,
) {
    Card(modifier = Modifier.fillMaxWidth()) {
        Column(
            modifier = Modifier.padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            Text("Player Stats", style = MaterialTheme.typography.titleLarge)
            OutlinedTextField(
                value = uiState.searchQuery,
                onValueChange = onSearchQueryChanged,
                modifier = Modifier.fillMaxWidth(),
                singleLine = true,
                label = { Text("Select player") },
            )
            uiState.selectedPlayer?.let { player ->
                InlineChip("Selected: ${player.fullName}")
            }
            if (uiState.isSearchingPlayers) {
                Text("Searching players...", style = MaterialTheme.typography.bodySmall)
            }
            if (uiState.searchResults.isNotEmpty()) {
                Column(
                    verticalArrangement = Arrangement.spacedBy(4.dp),
                ) {
                    uiState.searchResults.take(6).forEach { player ->
                        TextButton(
                            onClick = { onSelectPlayer(player) },
                            modifier = Modifier.fillMaxWidth(),
                        ) {
                            Text(player.fullName, modifier = Modifier.fillMaxWidth())
                        }
                    }
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
    onOpenFilters: () -> Unit,
) {
    val statLabel = filterOptions?.stats?.firstOrNull { it.code == filters.statCode }?.label ?: filters.statCode
    val lineLabel = if (filters.lineMode == "interval") {
        "${filters.lowerBoundText} - ${filters.upperBoundText}"
    } else {
        filters.referenceLineText
    }
    Card(modifier = Modifier.fillMaxWidth()) {
        Column(
            modifier = Modifier.padding(14.dp),
            verticalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.SpaceBetween,
                verticalAlignment = Alignment.CenterVertically,
            ) {
                Text(playerName, style = MaterialTheme.typography.titleMedium, fontWeight = FontWeight.SemiBold)
                FilledTonalButton(onClick = onOpenFilters) {
                    Icon(Icons.Outlined.FilterList, contentDescription = null)
                    Text("Filters", modifier = Modifier.padding(start = 6.dp))
                }
            }
            FlowRow(
                horizontalArrangement = Arrangement.spacedBy(8.dp),
                verticalArrangement = Arrangement.spacedBy(8.dp),
            ) {
                InlineChip("Stat: $statLabel")
                InlineChip("Seasons: ${filters.seasons.joinToString(", ")}")
                InlineChip("Line: $lineLabel")
                if (filters.lastGamesText.isNotBlank()) {
                    InlineChip("Last: ${filters.lastGamesText} games")
                }
                if (filters.minutesMinimumText != "0") {
                    InlineChip("TOG >= ${filters.minutesMinimumText}%")
                }
            }
        }
    }
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
    Card(modifier = Modifier.fillMaxWidth()) {
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
private fun DenseSummaryCell(
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
private fun PlayerHistoryTable(history: List<PlayerGameLogEntry>) {
    val scrollState = rememberScrollState()
    Card(modifier = Modifier.fillMaxWidth()) {
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
    filterOptions: PlayerStatFilterOptions?,
    filters: PlayerStatsFilters,
    onFiltersChanged: (PlayerStatsFilters) -> Unit,
    onApply: () -> Unit,
    onClear: () -> Unit,
    onDismiss: () -> Unit,
) {
    var statExpanded by remember { mutableStateOf(false) }

    ModalBottomSheet(onDismissRequest = onDismiss) {
        Column(
            modifier = Modifier
                .fillMaxWidth()
                .verticalScroll(rememberScrollState())
                .padding(horizontal = 20.dp, vertical = 8.dp),
            verticalArrangement = Arrangement.spacedBy(18.dp),
        ) {
            Text("Player filters", style = MaterialTheme.typography.headlineSmall)

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
                    )
                    FilterChip(
                        selected = filters.lineMode == "interval",
                        onClick = { onFiltersChanged(filters.copy(lineMode = "interval")) },
                        label = { Text("Interval") },
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
