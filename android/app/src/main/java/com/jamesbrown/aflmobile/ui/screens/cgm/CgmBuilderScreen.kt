package com.jamesbrown.aflmobile.ui.screens.cgm

import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.ExperimentalFoundationApi
import androidx.compose.foundation.horizontalScroll
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.heightIn
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.automirrored.outlined.HelpOutline
import androidx.compose.material.icons.outlined.FilterList
import androidx.compose.material.icons.outlined.MoreVert
import androidx.compose.material.icons.outlined.Refresh
import androidx.compose.material3.AlertDialog
import androidx.compose.material3.BottomSheetScaffold
import androidx.compose.material3.Button
import androidx.compose.material3.ButtonDefaults
import androidx.compose.material3.Card
import androidx.compose.material3.CardDefaults
import androidx.compose.material3.Checkbox
import androidx.compose.material3.DropdownMenu
import androidx.compose.material3.DropdownMenuItem
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.ExposedDropdownMenuAnchorType
import androidx.compose.material3.ExposedDropdownMenuBox
import androidx.compose.material3.ExposedDropdownMenuDefaults
import androidx.compose.material3.FilterChip
import androidx.compose.material3.FilterChipDefaults
import androidx.compose.material3.HorizontalDivider
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.ModalBottomSheet
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.SheetValue
import androidx.compose.material3.SnackbarHost
import androidx.compose.material3.SnackbarHostState
import androidx.compose.material3.Switch
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.material3.TopAppBar
import androidx.compose.material3.pulltorefresh.PullToRefreshBox
import androidx.compose.material3.rememberBottomSheetScaffoldState
import androidx.compose.material3.rememberStandardBottomSheetState
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.semantics.heading
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.lifecycle.ViewModel
import androidx.lifecycle.compose.collectAsStateWithLifecycle
import androidx.lifecycle.viewModelScope
import androidx.lifecycle.viewmodel.compose.viewModel
import com.jamesbrown.aflmobile.core.runCatchingCancellable
import com.jamesbrown.aflmobile.core.toUserMessage
import com.jamesbrown.aflmobile.data.repository.AflRepository
import com.jamesbrown.aflmobile.model.BookmakerSummary
import com.jamesbrown.aflmobile.model.BuilderDisplayMode
import com.jamesbrown.aflmobile.model.BuilderSortField
import com.jamesbrown.aflmobile.model.CgmAgencyComparison
import com.jamesbrown.aflmobile.model.DraftLeg
import com.jamesbrown.aflmobile.model.EventSummary
import com.jamesbrown.aflmobile.model.OddsQuery
import com.jamesbrown.aflmobile.model.OddsSearchResult
import com.jamesbrown.aflmobile.model.SelectionMetricFilters
import com.jamesbrown.aflmobile.ui.common.BuilderDisplayModeSegmented
import com.jamesbrown.aflmobile.ui.common.BuilderSupportText
import com.jamesbrown.aflmobile.ui.common.EmptyCard
import com.jamesbrown.aflmobile.ui.common.ErrorCard
import com.jamesbrown.aflmobile.ui.common.LoadingCard
import com.jamesbrown.aflmobile.ui.common.ScreenPadding
import com.jamesbrown.aflmobile.ui.common.SelectionMetricFilterSheet
import com.jamesbrown.aflmobile.ui.common.bookmakerLabel
import com.jamesbrown.aflmobile.ui.common.builder.AllMarketCode
import com.jamesbrown.aflmobile.ui.common.builder.CandidateBoardCard
import com.jamesbrown.aflmobile.ui.common.builder.CandidateRowHeader
import com.jamesbrown.aflmobile.ui.common.builder.CandidateSelectionRow
import com.jamesbrown.aflmobile.ui.common.builder.DraftLegCard
import com.jamesbrown.aflmobile.ui.common.builder.DraftPeekBar
import com.jamesbrown.aflmobile.ui.common.builder.MetricGlossarySheet
import com.jamesbrown.aflmobile.ui.common.builder.buildCandidateBoard
import com.jamesbrown.aflmobile.ui.common.builder.defaultSortDirectionForField
import com.jamesbrown.aflmobile.ui.common.builder.marketDisplayLabel
import com.jamesbrown.aflmobile.ui.common.builder.marketSectionTitle
import com.jamesbrown.aflmobile.ui.common.builder.orderedMarketCodes
import com.jamesbrown.aflmobile.ui.common.builder.sortCandidateRows
import com.jamesbrown.aflmobile.ui.common.formatDecimalPrice
import com.jamesbrown.aflmobile.ui.common.simpleViewModelFactory
import com.jamesbrown.aflmobile.ui.navigation.PlayerLaunchRequest
import com.jamesbrown.aflmobile.ui.theme.appTopBarColors
import com.jamesbrown.aflmobile.ui.theme.tabular
import kotlinx.coroutines.Job
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.flow.update
import kotlinx.coroutines.launch


data class CgmBuilderUiState(
    val bookmakers: List<BookmakerSummary> = emptyList(),
    val events: List<EventSummary> = emptyList(),
    val selectedBookmaker: String? = null,
    val selectedEventIds: Set<Int> = emptySet(),
    val bestOnly: Boolean = false,
    val metricFilters: SelectionMetricFilters = SelectionMetricFilters(),
    val candidateLegs: List<OddsSearchResult> = emptyList(),
    val selectedLegs: List<DraftLeg> = emptyList(),
    val comparisonResults: List<CgmAgencyComparison> = emptyList(),
    val isLoadingOptions: Boolean = true,
    val isComparing: Boolean = false,
    val errorMessage: String? = null,
    val infoMessage: String? = null,
)

class CgmBuilderViewModel(
    private val repository: AflRepository,
) : ViewModel() {
    private val _uiState = MutableStateFlow(CgmBuilderUiState())
    val uiState: StateFlow<CgmBuilderUiState> = _uiState.asStateFlow()
    private var loadJob: Job? = null

    init {
        refresh()
    }

    fun refresh() {
        loadJob?.cancel()
        loadJob = viewModelScope.launch {
            _uiState.update { it.copy(isLoadingOptions = true, errorMessage = null) }
            runCatchingCancellable {
                repository.bookmakers(forceRefresh = true)
            }.onSuccess { bookmakers ->
                val defaultBookmaker = repository.currentSettings().defaultBookmaker
                val selectedBookmaker = uiState.value.selectedBookmaker
                    ?.takeIf { selected -> bookmakers.any { it.code == selected && it.enabled } }
                    ?: bookmakers.firstOrNull { it.code == defaultBookmaker && it.enabled }?.code
                    ?: bookmakers.firstOrNull { it.enabled }?.code
                _uiState.update {
                    it.copy(
                        bookmakers = bookmakers,
                        selectedBookmaker = selectedBookmaker,
                    )
                }
                if (selectedBookmaker == null) {
                    _uiState.update { it.copy(isLoadingOptions = false, candidateLegs = emptyList(), events = emptyList()) }
                } else {
                    loadBookmakerData(selectedBookmaker)
                }
            }.onFailure { error ->
                _uiState.update {
                    it.copy(
                        isLoadingOptions = false,
                        errorMessage = error.toUserMessage("Failed to load agencies."),
                    )
                }
            }
        }
    }

    fun selectBookmaker(bookmakerCode: String) {
        _uiState.update {
            it.copy(
                selectedBookmaker = bookmakerCode,
                selectedEventIds = emptySet(),
                selectedLegs = emptyList(),
                comparisonResults = emptyList(),
                events = emptyList(),
                candidateLegs = emptyList(),
                isLoadingOptions = true,
                errorMessage = null,
            )
        }
        loadJob?.cancel()
        loadJob = viewModelScope.launch {
            loadBookmakerData(bookmakerCode)
        }
    }

    fun toggleEventSelection(eventId: Int) {
        _uiState.update { current ->
            val updatedEventIds = current.selectedEventIds.toMutableSet().also { selected ->
                if (!selected.add(eventId)) {
                    selected.remove(eventId)
                }
            }.toSet()
            val filteredLegs = if (updatedEventIds.isEmpty()) {
                current.selectedLegs
            } else {
                current.selectedLegs.filter { it.eventId in updatedEventIds }
            }
            current.copy(
                selectedEventIds = updatedEventIds,
                selectedLegs = filteredLegs,
                comparisonResults = emptyList(),
                infoMessage = if (filteredLegs.size != current.selectedLegs.size) "Draft trimmed to selected matches." else current.infoMessage,
            )
        }
        reloadForEventSelection()
    }

    fun clearEventSelection() {
        _uiState.update {
            it.copy(
                selectedEventIds = emptySet(),
                comparisonResults = emptyList(),
            )
        }
        reloadForEventSelection()
    }

    /**
     * Event selection now filters server-side, so the app no longer downloads
     * the full cross-game board when only a few matches are in play. Debounced
     * so rapidly ticking checkboxes results in a single request.
     */
    private fun reloadForEventSelection() {
        val selectedBookmaker = uiState.value.selectedBookmaker ?: return
        loadJob?.cancel()
        loadJob = viewModelScope.launch {
            delay(400)
            _uiState.update { it.copy(isLoadingOptions = true) }
            loadBookmakerData(selectedBookmaker)
        }
    }

    fun setBestOnly(bestOnly: Boolean) {
        _uiState.update {
            it.copy(
                bestOnly = bestOnly,
                isLoadingOptions = it.selectedBookmaker != null,
                comparisonResults = emptyList(),
                errorMessage = null,
            )
        }
        val selectedBookmaker = uiState.value.selectedBookmaker ?: return
        loadJob?.cancel()
        loadJob = viewModelScope.launch {
            loadBookmakerData(selectedBookmaker)
        }
    }

    fun applyMetricFilters(metricFilters: SelectionMetricFilters) {
        _uiState.update {
            it.copy(
                metricFilters = metricFilters,
                isLoadingOptions = it.selectedBookmaker != null,
                errorMessage = null,
            )
        }
        val selectedBookmaker = uiState.value.selectedBookmaker ?: return
        loadJob?.cancel()
        loadJob = viewModelScope.launch {
            loadBookmakerData(selectedBookmaker)
        }
    }

    fun toggleLeg(leg: OddsSearchResult) {
        val decimalPrice = leg.decimalPrice
        if (decimalPrice == null) {
            _uiState.update { it.copy(errorMessage = "That leg does not have a current price.") }
            return
        }
        _uiState.update { current ->
            val existing = current.selectedLegs.any { it.selectionId == leg.selectionId }
            if (existing) {
                current.copy(
                    selectedLegs = current.selectedLegs.filterNot { it.selectionId == leg.selectionId },
                    comparisonResults = emptyList(),
                    errorMessage = null,
                    infoMessage = "Leg removed.",
                )
            } else if (current.selectedLegs.any { it.eventId == leg.eventId }) {
                current.copy(
                    errorMessage = "Cross-game multis allow one leg per match. Pick a different game.",
                )
            } else {
                val updatedEventIds = current.selectedEventIds - leg.eventId
                current.copy(
                    selectedLegs = current.selectedLegs + DraftLeg(
                        selectionId = leg.selectionId,
                        eventId = leg.eventId,
                        eventLabel = leg.matchName,
                        bookmaker = leg.bookmaker,
                        label = leg.label,
                        marketTypeCode = leg.marketTypeCode,
                        selectionType = leg.selectionType,
                        basePrice = decimalPrice,
                        diff2025 = leg.diff2025,
                        diffLast10 = leg.diffLast10,
                        nextBestProbDiff = leg.nextBestProbDiff,
                        isBestPrice = leg.isBestPrice,
                    ),
                    selectedEventIds = updatedEventIds,
                    comparisonResults = emptyList(),
                    errorMessage = null,
                    infoMessage = if (current.selectedEventIds.isEmpty()) {
                        "Leg added to CGM builder."
                    } else {
                        "Leg added. Its match has been removed from the remaining CGM options."
                    },
                )
            }
        }
    }

    fun removeLeg(selectionId: Int) {
        _uiState.update {
            it.copy(
                selectedLegs = it.selectedLegs.filterNot { leg -> leg.selectionId == selectionId },
                comparisonResults = emptyList(),
                errorMessage = null,
                infoMessage = "Leg removed.",
            )
        }
    }

    fun clearDraft() {
        _uiState.update {
            it.copy(
                selectedLegs = emptyList(),
                comparisonResults = emptyList(),
                errorMessage = null,
                infoMessage = "Draft cleared.",
            )
        }
    }

    fun compare() {
        val selectedLegs = uiState.value.selectedLegs
        if (selectedLegs.size < 2) {
            _uiState.update { it.copy(errorMessage = "Choose at least two legs before comparing.") }
            return
        }
        viewModelScope.launch {
            _uiState.update { it.copy(isComparing = true, errorMessage = null) }
            runCatchingCancellable {
                repository.compareCgm(selectedLegs.map { it.selectionId })
            }.onSuccess { comparison ->
                _uiState.update {
                    it.copy(
                        comparisonResults = comparison.results,
                        isComparing = false,
                        infoMessage = if (comparison.results.isEmpty()) {
                            "No agency currently offers the full combination."
                        } else {
                            "Comparison updated."
                        },
                    )
                }
            }.onFailure { error ->
                _uiState.update {
                    it.copy(
                        isComparing = false,
                        errorMessage = error.toUserMessage("Failed to compare CGM prices."),
                    )
                }
            }
        }
    }

    fun consumeInfoMessage() {
        _uiState.update { it.copy(infoMessage = null) }
    }

    fun consumeErrorMessage() {
        _uiState.update { it.copy(errorMessage = null) }
    }

    private suspend fun loadBookmakerData(bookmakerCode: String) {
        val currentBestOnly = uiState.value.bestOnly
        val metricFilters = uiState.value.metricFilters
        val previousEventIds = uiState.value.selectedEventIds
        runCatchingCancellable {
            val events = repository.events(bookmaker = bookmakerCode, query = null)
            val odds = repository.odds(
                OddsQuery(
                    bookmakers = listOf(bookmakerCode),
                    scope = "player",
                    eventIds = previousEventIds.sorted(),
                    sortBy = "next_best_prob_diff",
                    sortDirection = "desc",
                    matchupDifficulties = metricFilters.matchupDifficulties,
                    minPrice = metricFilters.minPriceText.toDoubleOrNull(),
                    maxPrice = metricFilters.maxPriceText.toDoubleOrNull(),
                    minDiff2025 = metricFilters.minDiff2025.toDouble(),
                    maxDiff2025 = metricFilters.maxDiff2025.toDouble(),
                    minDiffLast10 = metricFilters.minDiffLast10.toDouble(),
                    maxDiffLast10 = metricFilters.maxDiffLast10.toDouble(),
                    minNextBestProbDiff = metricFilters.minNextBestProbDiff.toDouble(),
                    maxNextBestProbDiff = metricFilters.maxNextBestProbDiff.toDouble(),
                    bestOnly = currentBestOnly,
                    limit = 5000,
                ),
            )
            events to odds
        }.onSuccess { (events, odds) ->
            val validEventIds = events.map { it.id }.toSet()
            _uiState.update {
                it.copy(
                    events = events,
                    selectedEventIds = previousEventIds.intersect(validEventIds),
                    candidateLegs = odds.filter { leg -> leg.marketTypeCode.startsWith("player_") },
                    selectedLegs = it.selectedLegs.filter { leg -> leg.bookmaker == bookmakerCode },
                    isLoadingOptions = false,
                    errorMessage = null,
                )
            }
        }.onFailure { error ->
            _uiState.update {
                it.copy(
                    isLoadingOptions = false,
                    candidateLegs = emptyList(),
                    errorMessage = error.toUserMessage("Failed to load CGM legs."),
                )
            }
        }
    }
}

@Composable
fun CgmBuilderRoute(
    repository: AflRepository,
    onOpenPlayerRequest: (PlayerLaunchRequest) -> Unit,
) {
    val viewModel: CgmBuilderViewModel = viewModel(
        factory = simpleViewModelFactory { CgmBuilderViewModel(repository) },
    )
    val uiState by viewModel.uiState.collectAsStateWithLifecycle()
    CgmBuilderScreen(
        uiState = uiState,
        onSelectBookmaker = viewModel::selectBookmaker,
        onToggleEvent = viewModel::toggleEventSelection,
        onClearEventSelection = viewModel::clearEventSelection,
        onToggleLeg = viewModel::toggleLeg,
        onRemoveLeg = viewModel::removeLeg,
        onClearDraft = viewModel::clearDraft,
        onBestOnlyChanged = viewModel::setBestOnly,
        onApplyMetricFilters = viewModel::applyMetricFilters,
        onCompare = viewModel::compare,
        onRefresh = viewModel::refresh,
        onInfoMessageShown = viewModel::consumeInfoMessage,
        onErrorMessageShown = viewModel::consumeErrorMessage,
        onOpenPlayerRequest = onOpenPlayerRequest,
    )
}

@OptIn(ExperimentalMaterial3Api::class, ExperimentalFoundationApi::class)
@Composable
private fun CgmBuilderScreen(
    uiState: CgmBuilderUiState,
    onSelectBookmaker: (String) -> Unit,
    onToggleEvent: (Int) -> Unit,
    onClearEventSelection: () -> Unit,
    onToggleLeg: (OddsSearchResult) -> Unit,
    onRemoveLeg: (Int) -> Unit,
    onClearDraft: () -> Unit,
    onBestOnlyChanged: (Boolean) -> Unit,
    onApplyMetricFilters: (SelectionMetricFilters) -> Unit,
    onCompare: () -> Unit,
    onRefresh: () -> Unit,
    onInfoMessageShown: () -> Unit,
    onErrorMessageShown: () -> Unit,
    onOpenPlayerRequest: (PlayerLaunchRequest) -> Unit,
) {
    val selectedEventIds = uiState.selectedEventIds
    val selectedSelectionIds = remember(uiState.selectedLegs) { uiState.selectedLegs.map { it.selectionId }.toSet() }
    var showFilters by remember { mutableStateOf(false) }
    var showControls by remember { mutableStateOf(false) }
    var showGlossary by remember { mutableStateOf(false) }
    var pendingBookmaker by remember { mutableStateOf<String?>(null) }
    var draftMetricFilters by remember(uiState.metricFilters) { mutableStateOf(uiState.metricFilters) }
    var displayMode by rememberSaveable { mutableStateOf(BuilderDisplayMode.ROW) }
    var rowSortField by rememberSaveable { mutableStateOf(BuilderSortField.NEXT_BEST) }
    var rowSortDescending by rememberSaveable { mutableStateOf(true) }
    val coroutineScope = rememberCoroutineScope()
    val snackbarHostState = remember { SnackbarHostState() }
    val scaffoldState = rememberBottomSheetScaffoldState(
        bottomSheetState = rememberStandardBottomSheetState(
            initialValue = SheetValue.PartiallyExpanded,
        ),
    )
    val selectedDraftEventIds = remember(uiState.selectedLegs) {
        uiState.selectedLegs.map { it.eventId }.toSet()
    }
    val availableEvents = remember(uiState.events, selectedDraftEventIds) {
        uiState.events.filterNot { it.id in selectedDraftEventIds }
    }
    val filteredByEvent = remember(uiState.candidateLegs, selectedEventIds, selectedDraftEventIds) {
        if (selectedEventIds.isEmpty()) {
            uiState.candidateLegs.filterNot { it.eventId in selectedDraftEventIds }
        } else {
            uiState.candidateLegs.filter { it.eventId in selectedEventIds && it.eventId !in selectedDraftEventIds }
        }
    }
    val marketCodes = remember(filteredByEvent) { orderedMarketCodes(filteredByEvent) }
    var selectedMarketCode by rememberSaveable(uiState.selectedBookmaker, selectedEventIds.hashCode()) {
        mutableStateOf(if (marketCodes.isNotEmpty()) AllMarketCode else null)
    }
    LaunchedEffect(marketCodes) {
        val allowedCodes = listOf(AllMarketCode) + marketCodes
        if (selectedMarketCode !in allowedCodes) {
            selectedMarketCode = if (marketCodes.isNotEmpty()) AllMarketCode else null
        }
    }
    val visibleLegs = remember(filteredByEvent, selectedMarketCode) {
        filteredByEvent.filter { leg ->
            selectedMarketCode == null || selectedMarketCode == AllMarketCode || leg.marketTypeCode == selectedMarketCode
        }
    }
    val groupedLegs = remember(visibleLegs) {
        buildCandidateBoard(legs = visibleLegs)
    }
    val rowLegs = remember(visibleLegs, rowSortField, rowSortDescending) {
        sortCandidateRows(legs = visibleLegs, sortField = rowSortField, descending = rowSortDescending)
    }
    LaunchedEffect(uiState.isComparing, uiState.comparisonResults.size) {
        if (uiState.isComparing || uiState.comparisonResults.isNotEmpty()) {
            scaffoldState.bottomSheetState.expand()
        }
    }
    LaunchedEffect(showFilters, uiState.metricFilters) {
        if (showFilters) {
            draftMetricFilters = uiState.metricFilters
        }
    }
    LaunchedEffect(uiState.infoMessage) {
        uiState.infoMessage?.let { message ->
            snackbarHostState.showSnackbar(message)
            onInfoMessageShown()
        }
    }

    BottomSheetScaffold(
        scaffoldState = scaffoldState,
        snackbarHost = { SnackbarHost(snackbarHostState) },
        sheetPeekHeight = if (uiState.selectedLegs.isNotEmpty()) 76.dp else 0.dp,
        sheetContainerColor = MaterialTheme.colorScheme.surfaceContainerLow,
        sheetContentColor = MaterialTheme.colorScheme.onSurface,
        sheetShadowElevation = if (uiState.selectedLegs.isNotEmpty()) 10.dp else 0.dp,
        sheetShape = RoundedCornerShape(topStart = 28.dp, topEnd = 28.dp),
        sheetDragHandle = null,
        sheetContent = {
            if (uiState.selectedLegs.isNotEmpty()) {
                CgmDraftSheet(
                    selectedBookmaker = uiState.selectedBookmaker,
                    selectedLegs = uiState.selectedLegs,
                    comparisonResults = uiState.comparisonResults,
                    isComparing = uiState.isComparing,
                    onExpand = {
                        coroutineScope.launch {
                            scaffoldState.bottomSheetState.expand()
                        }
                    },
                    onCompare = {
                        coroutineScope.launch {
                            scaffoldState.bottomSheetState.expand()
                        }
                        onCompare()
                    },
                    onRemoveLeg = onRemoveLeg,
                    onClearDraft = onClearDraft,
                )
            } else {
                Spacer(modifier = Modifier.height(1.dp))
            }
        },
        containerColor = MaterialTheme.colorScheme.background,
        topBar = {
            TopAppBar(
                title = { Text("CGM builder") },
                colors = appTopBarColors(),
                actions = {
                    IconButton(onClick = { showGlossary = true }) {
                        Icon(Icons.AutoMirrored.Outlined.HelpOutline, contentDescription = "Metric glossary")
                    }
                    IconButton(onClick = { showFilters = true }) {
                        Icon(Icons.Outlined.FilterList, contentDescription = "Open filters")
                    }
                    IconButton(onClick = { showControls = true }) {
                        Icon(Icons.Outlined.MoreVert, contentDescription = "Open options")
                    }
                },
            )
        },
    ) { innerPadding ->
        PullToRefreshBox(
            isRefreshing = uiState.isLoadingOptions,
            onRefresh = onRefresh,
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
                    CgmControlCard(
                        bookmakers = uiState.bookmakers,
                        events = availableEvents,
                        selectedBookmaker = uiState.selectedBookmaker,
                        selectedEventIds = uiState.selectedEventIds,
                        bestOnly = uiState.bestOnly,
                        onSelectBookmaker = { code ->
                            if (uiState.selectedLegs.isNotEmpty() && uiState.selectedBookmaker != code) {
                                pendingBookmaker = code
                            } else {
                                onSelectBookmaker(code)
                            }
                        },
                        onToggleEvent = onToggleEvent,
                        onClearEventSelection = onClearEventSelection,
                        onBestOnlyChanged = onBestOnlyChanged,
                    )
                }

                if (marketCodes.isNotEmpty()) {
                    item {
                        CgmMarketSelectorRow(
                            marketCodes = listOf(AllMarketCode) + marketCodes,
                            selectedMarketCode = selectedMarketCode,
                            onSelected = { selectedMarketCode = it },
                        )
                    }
                }

                uiState.errorMessage?.let { message ->
                    item {
                        ErrorCard(
                            message = message,
                            onRetry = {
                                onErrorMessageShown()
                                onRefresh()
                            },
                        )
                    }
                }

                if (!uiState.isLoadingOptions && visibleLegs.isEmpty() && uiState.errorMessage == null) {
                    item {
                        EmptyCard(
                            title = "No player props",
                            body = "No player props match the current source agency, match selection, best-price filter, and one-leg-per-game rule.",
                            actionLabel = "Reset filters",
                            onAction = { onApplyMetricFilters(SelectionMetricFilters()) },
                        )
                    }
                } else if (visibleLegs.isNotEmpty()) {
                    item {
                        Text(
                            marketSectionTitle(selectedMarketCode),
                            modifier = Modifier.semantics { heading() },
                            style = MaterialTheme.typography.titleMedium,
                            fontWeight = FontWeight.SemiBold,
                        )
                    }
                    if (displayMode == BuilderDisplayMode.ROW) {
                        stickyHeader {
                            CandidateRowHeader(
                                sortField = rowSortField,
                                descending = rowSortDescending,
                                onSortSelected = { selectedField ->
                                    if (rowSortField == selectedField) {
                                        rowSortDescending = !rowSortDescending
                                    } else {
                                        rowSortField = selectedField
                                        rowSortDescending = defaultSortDirectionForField(selectedField)
                                    }
                                },
                            )
                        }
                        items(rowLegs, key = { it.selectionId }) { leg ->
                            CandidateSelectionRow(
                                selection = leg,
                                selected = leg.selectionId in selectedSelectionIds,
                                enabled = leg.decimalPrice != null,
                                onOpenPlayerRequest = onOpenPlayerRequest,
                                onToggleLeg = onToggleLeg,
                                modifier = Modifier.animateItem(),
                            )
                        }
                    } else {
                        items(groupedLegs, key = { it.key }) { group ->
                            CandidateBoardCard(
                                group = group,
                                selectedSelectionIds = selectedSelectionIds,
                                isSelectionEnabled = { it.decimalPrice != null },
                                onOpenPlayerRequest = onOpenPlayerRequest,
                                onToggleLeg = onToggleLeg,
                                modifier = Modifier.animateItem(),
                            )
                        }
                    }
                }
            }
        }

        if (showFilters) {
            SelectionMetricFilterSheet(
                filters = draftMetricFilters,
                onFiltersChanged = { draftMetricFilters = it },
                onApply = {
                    onApplyMetricFilters(draftMetricFilters)
                    showFilters = false
                },
                onApplyQuickFilter = { preset ->
                    draftMetricFilters = preset
                    onApplyMetricFilters(preset)
                    showFilters = false
                },
                onClear = { draftMetricFilters = SelectionMetricFilters() },
                onDismiss = { showFilters = false },
            )
        }
        if (showControls) {
            CgmControlsSheet(
                displayMode = displayMode,
                onDisplayModeChanged = { displayMode = it },
                onDismiss = { showControls = false },
            )
        }
        if (showGlossary) {
            MetricGlossarySheet(onDismiss = { showGlossary = false })
        }
        pendingBookmaker?.let { candidate ->
            AlertDialog(
                onDismissRequest = { pendingBookmaker = null },
                title = { Text("Clear draft?") },
                text = {
                    Text(
                        "Switching the agency clears your current draft. You currently have " +
                            "${uiState.selectedLegs.size} leg${if (uiState.selectedLegs.size == 1) "" else "s"} selected.",
                    )
                },
                confirmButton = {
                    TextButton(
                        onClick = {
                            onSelectBookmaker(candidate)
                            pendingBookmaker = null
                        },
                    ) {
                        Text("Clear and switch")
                    }
                },
                dismissButton = {
                    TextButton(onClick = { pendingBookmaker = null }) {
                        Text("Keep draft")
                    }
                },
            )
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun CgmControlCard(
    bookmakers: List<BookmakerSummary>,
    events: List<EventSummary>,
    selectedBookmaker: String?,
    selectedEventIds: Set<Int>,
    bestOnly: Boolean,
    onSelectBookmaker: (String) -> Unit,
    onToggleEvent: (Int) -> Unit,
    onClearEventSelection: () -> Unit,
    onBestOnlyChanged: (Boolean) -> Unit,
) {
    var bookmakerExpanded by remember { mutableStateOf(false) }
    var eventExpanded by remember { mutableStateOf(false) }
    Card(
        colors = CardDefaults.cardColors(containerColor = MaterialTheme.colorScheme.surface),
        border = BorderStroke(1.dp, MaterialTheme.colorScheme.outlineVariant),
    ) {
        Column(
            modifier = Modifier.padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            Text(
                "Agency and matches",
                modifier = Modifier.semantics { heading() },
                style = MaterialTheme.typography.titleMedium,
                fontWeight = FontWeight.SemiBold,
            )
            BuilderSupportText("Choose the source agency and the remaining matches you want to include.")

            ExposedDropdownMenuBox(
                expanded = bookmakerExpanded,
                onExpandedChange = { bookmakerExpanded = !bookmakerExpanded },
            ) {
                OutlinedTextField(
                    value = selectedBookmaker?.let(::bookmakerLabel) ?: "Choose agency",
                    onValueChange = {},
                    modifier = Modifier
                        .menuAnchor(ExposedDropdownMenuAnchorType.PrimaryNotEditable)
                        .fillMaxWidth(),
                    readOnly = true,
                    label = { Text("Source agency") },
                    trailingIcon = {
                        ExposedDropdownMenuDefaults.TrailingIcon(expanded = bookmakerExpanded)
                    },
                )
                DropdownMenu(
                    expanded = bookmakerExpanded,
                    onDismissRequest = { bookmakerExpanded = false },
                ) {
                    bookmakers.filter { it.enabled }.forEach { bookmaker ->
                        DropdownMenuItem(
                            text = { Text(bookmaker.displayName) },
                            onClick = {
                                onSelectBookmaker(bookmaker.code)
                                bookmakerExpanded = false
                            },
                        )
                    }
                }
            }

            ExposedDropdownMenuBox(
                expanded = eventExpanded,
                onExpandedChange = { eventExpanded = !eventExpanded },
            ) {
                OutlinedTextField(
                    value = selectedMatchesLabel(events, selectedEventIds),
                    onValueChange = {},
                    modifier = Modifier
                        .menuAnchor(ExposedDropdownMenuAnchorType.PrimaryNotEditable)
                        .fillMaxWidth(),
                    readOnly = true,
                    label = { Text("Matches") },
                    trailingIcon = {
                        ExposedDropdownMenuDefaults.TrailingIcon(expanded = eventExpanded)
                    },
                )
                DropdownMenu(
                    expanded = eventExpanded,
                    onDismissRequest = { eventExpanded = false },
                    modifier = Modifier.heightIn(max = 360.dp),
                ) {
                    DropdownMenuItem(
                        text = {
                            Row(
                                verticalAlignment = Alignment.CenterVertically,
                                horizontalArrangement = Arrangement.spacedBy(10.dp),
                            ) {
                                Checkbox(
                                    checked = selectedEventIds.isEmpty(),
                                    onCheckedChange = null,
                                )
                                Text("All matches")
                            }
                        },
                        onClick = { onClearEventSelection() },
                    )
                    events.forEach { event ->
                        DropdownMenuItem(
                            text = {
                                Row(
                                    verticalAlignment = Alignment.CenterVertically,
                                    horizontalArrangement = Arrangement.spacedBy(10.dp),
                                ) {
                                    Checkbox(
                                        checked = event.id in selectedEventIds,
                                        onCheckedChange = null,
                                    )
                                    Text(event.matchName)
                                }
                            },
                            onClick = { onToggleEvent(event.id) },
                        )
                    }
                }
            }

            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.SpaceBetween,
                verticalAlignment = Alignment.CenterVertically,
            ) {
                Column(modifier = Modifier.weight(1f)) {
                    Text("Best market price", fontWeight = FontWeight.Medium)
                    BuilderSupportText("Only show source-agency rows where it currently has the best market price.")
                }
                Switch(
                    checked = bestOnly,
                    onCheckedChange = onBestOnlyChanged,
                )
            }
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun CgmControlsSheet(
    displayMode: BuilderDisplayMode,
    onDisplayModeChanged: (BuilderDisplayMode) -> Unit,
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
            verticalArrangement = Arrangement.spacedBy(16.dp),
        ) {
            Text(
                "Options",
                modifier = Modifier.semantics { heading() },
                style = MaterialTheme.typography.headlineSmall,
                fontWeight = FontWeight.SemiBold,
            )
            Column(verticalArrangement = Arrangement.spacedBy(8.dp)) {
                Text("Display mode", fontWeight = FontWeight.Medium)
                BuilderDisplayModeSegmented(
                    displayMode = displayMode,
                    onDisplayModeChanged = onDisplayModeChanged,
                )
            }
            Spacer(modifier = Modifier.height(8.dp))
        }
    }
}

@Composable
private fun CgmDraftSheet(
    selectedBookmaker: String?,
    selectedLegs: List<DraftLeg>,
    comparisonResults: List<CgmAgencyComparison>,
    isComparing: Boolean,
    onExpand: () -> Unit,
    onCompare: () -> Unit,
    onRemoveLeg: (Int) -> Unit,
    onClearDraft: () -> Unit,
) {
    val bestPrice = comparisonResults.maxOfOrNull { it.quotedPrice }
    val distinctGames = selectedLegs.map { it.eventId }.distinct().size
    LazyColumn(
        modifier = Modifier
            .fillMaxWidth()
            .heightIn(max = 560.dp),
        contentPadding = PaddingValues(start = 16.dp, top = 0.dp, end = 16.dp, bottom = 12.dp),
        verticalArrangement = Arrangement.spacedBy(12.dp),
    ) {
        item {
            DraftPeekBar(
                count = selectedLegs.size,
                primaryLabel = "${selectedLegs.size} leg${if (selectedLegs.size == 1) "" else "s"} selected",
                secondaryLabel = "$distinctGames game${if (distinctGames == 1) "" else "s"} • ${selectedBookmaker?.let(::bookmakerLabel) ?: "Tap to review draft"}",
                bestPriceLabel = bestPrice?.let(::formatDecimalPrice),
                onExpand = onExpand,
            )
        }

        item {
            HorizontalDivider()
        }

        item {
            Column(
                modifier = Modifier.fillMaxWidth(),
                verticalArrangement = Arrangement.spacedBy(10.dp),
            ) {
                Row(
                    modifier = Modifier.fillMaxWidth(),
                    horizontalArrangement = Arrangement.SpaceBetween,
                    verticalAlignment = Alignment.CenterVertically,
                ) {
                    Column(
                        modifier = Modifier.weight(1f),
                        verticalArrangement = Arrangement.spacedBy(4.dp),
                    ) {
                        Text(
                            "Current selections",
                            modifier = Modifier.semantics { heading() },
                            style = MaterialTheme.typography.titleMedium,
                            color = MaterialTheme.colorScheme.primary,
                            fontWeight = FontWeight.SemiBold,
                        )
                        Text(
                            "${selectedLegs.size} leg${if (selectedLegs.size == 1) "" else "s"} across $distinctGames game${if (distinctGames == 1) "" else "s"} • ${selectedBookmaker?.let(::bookmakerLabel) ?: "No agency"}",
                            style = MaterialTheme.typography.bodySmall,
                            color = MaterialTheme.colorScheme.onSurfaceVariant,
                        )
                    }
                    TextButton(onClick = onClearDraft) {
                        Text("Clear")
                    }
                }
                Button(
                    onClick = onCompare,
                    modifier = Modifier.fillMaxWidth(),
                    enabled = selectedLegs.size >= 2 && !isComparing,
                    colors = ButtonDefaults.buttonColors(
                        containerColor = MaterialTheme.colorScheme.tertiary,
                        contentColor = MaterialTheme.colorScheme.onTertiary,
                    ),
                ) {
                    Icon(Icons.Outlined.Refresh, contentDescription = null)
                    Text("Compare agencies", modifier = Modifier.padding(start = 8.dp))
                }
            }
        }

        if (isComparing) {
            item { LoadingCard("Comparing agencies") }
        }

        if (comparisonResults.isNotEmpty()) {
            item {
                Text(
                    "Agency comparison",
                    modifier = Modifier.semantics { heading() },
                    style = MaterialTheme.typography.titleMedium,
                    fontWeight = FontWeight.SemiBold,
                    color = MaterialTheme.colorScheme.primary,
                )
            }
            items(comparisonResults, key = { it.bookmaker }) { result ->
                CgmComparisonCard(
                    result = result,
                    rank = comparisonResults.indexOfFirst { it.bookmaker == result.bookmaker } + 1,
                )
            }
        }

        if (selectedLegs.isNotEmpty()) {
            item {
                Text(
                    "Legs",
                    modifier = Modifier.semantics { heading() },
                    style = MaterialTheme.typography.titleMedium,
                    fontWeight = FontWeight.SemiBold,
                    color = MaterialTheme.colorScheme.primary,
                )
            }
            items(selectedLegs, key = { it.selectionId }) { leg ->
                DraftLegCard(
                    leg = leg,
                    onRemove = onRemoveLeg,
                    showMatchLabel = true,
                )
            }
        }
    }
}

@Composable
private fun CgmComparisonCard(
    result: CgmAgencyComparison,
    rank: Int,
) {
    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = CardDefaults.cardColors(containerColor = MaterialTheme.colorScheme.surface),
        border = BorderStroke(1.dp, MaterialTheme.colorScheme.outlineVariant),
    ) {
        Column(
            modifier = Modifier.padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(10.dp),
        ) {
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.SpaceBetween,
                verticalAlignment = Alignment.CenterVertically,
            ) {
                Column(verticalArrangement = Arrangement.spacedBy(4.dp)) {
                    Text(
                        "#$rank ${bookmakerLabel(result.bookmaker)}",
                        style = MaterialTheme.typography.titleMedium,
                        color = if (rank == 1) MaterialTheme.colorScheme.tertiary else MaterialTheme.colorScheme.primary,
                        fontWeight = FontWeight.SemiBold,
                    )
                    Text(
                        "${result.selectionCount} legs priced",
                        style = MaterialTheme.typography.bodySmall,
                        color = MaterialTheme.colorScheme.onSurfaceVariant,
                    )
                }
                Text(
                    formatDecimalPrice(result.quotedPrice),
                    style = MaterialTheme.typography.headlineSmall.tabular,
                    fontWeight = FontWeight.Bold,
                )
            }
            result.legs.forEach { leg ->
                Row(
                    modifier = Modifier.fillMaxWidth(),
                    horizontalArrangement = Arrangement.SpaceBetween,
                    verticalAlignment = Alignment.CenterVertically,
                ) {
                    Column(
                        modifier = Modifier.weight(1f),
                        verticalArrangement = Arrangement.spacedBy(2.dp),
                    ) {
                        Text(leg.label, style = MaterialTheme.typography.bodyMedium, fontWeight = FontWeight.Medium)
                        Text(
                            leg.matchName,
                            style = MaterialTheme.typography.bodySmall,
                            color = MaterialTheme.colorScheme.onSurfaceVariant,
                        )
                    }
                    Text(
                        formatDecimalPrice(leg.basePrice),
                        style = MaterialTheme.typography.bodyMedium.tabular,
                        fontWeight = FontWeight.SemiBold,
                    )
                }
            }
        }
    }
}

@Composable
private fun CgmMarketSelectorRow(
    marketCodes: List<String>,
    selectedMarketCode: String?,
    onSelected: (String) -> Unit,
) {
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .horizontalScroll(rememberScrollState()),
        horizontalArrangement = Arrangement.spacedBy(10.dp),
    ) {
        marketCodes.forEach { marketCode ->
            FilterChip(
                selected = marketCode == selectedMarketCode,
                onClick = { onSelected(marketCode) },
                label = { Text(marketDisplayLabel(marketCode)) },
                colors = FilterChipDefaults.filterChipColors(
                    containerColor = MaterialTheme.colorScheme.secondaryContainer,
                    labelColor = MaterialTheme.colorScheme.primary,
                    selectedContainerColor = MaterialTheme.colorScheme.tertiary,
                    selectedLabelColor = MaterialTheme.colorScheme.onTertiary,
                ),
                border = FilterChipDefaults.filterChipBorder(
                    enabled = true,
                    selected = marketCode == selectedMarketCode,
                    borderColor = MaterialTheme.colorScheme.outlineVariant,
                    selectedBorderColor = MaterialTheme.colorScheme.tertiary,
                ),
            )
        }
    }
}

private fun selectedMatchesLabel(events: List<EventSummary>, selectedEventIds: Set<Int>): String =
    when {
        selectedEventIds.isEmpty() -> "All matches"
        selectedEventIds.size == 1 ->
            events.firstOrNull { it.id == selectedEventIds.first() }?.matchName ?: "1 match"
        else -> "${selectedEventIds.size} matches"
    }
