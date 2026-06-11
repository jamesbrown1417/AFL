package com.jamesbrown.aflmobile.ui.screens.sgm

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
import androidx.compose.material3.Surface
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
import com.jamesbrown.aflmobile.data.repository.SgmDraftStore
import com.jamesbrown.aflmobile.model.BookmakerSummary
import com.jamesbrown.aflmobile.model.BuilderDisplayMode
import com.jamesbrown.aflmobile.model.BuilderSortField
import com.jamesbrown.aflmobile.model.DraftLeg
import com.jamesbrown.aflmobile.model.EventSummary
import com.jamesbrown.aflmobile.model.OddsQuery
import com.jamesbrown.aflmobile.model.OddsSearchResult
import com.jamesbrown.aflmobile.model.SelectionMetricFilters
import com.jamesbrown.aflmobile.model.SgmAgencyComparison
import com.jamesbrown.aflmobile.model.SgmDraftState
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
import com.jamesbrown.aflmobile.ui.common.builder.SummaryMetricCard
import com.jamesbrown.aflmobile.ui.common.builder.buildCandidateBoard
import com.jamesbrown.aflmobile.ui.common.builder.defaultSortDirectionForField
import com.jamesbrown.aflmobile.ui.common.builder.marketDisplayLabel
import com.jamesbrown.aflmobile.ui.common.builder.marketSectionTitle
import com.jamesbrown.aflmobile.ui.common.builder.orderedMarketCodes
import com.jamesbrown.aflmobile.ui.common.builder.sortCandidateRows
import com.jamesbrown.aflmobile.ui.common.formatDateTime
import com.jamesbrown.aflmobile.ui.common.formatDecimalPrice
import com.jamesbrown.aflmobile.ui.common.simpleViewModelFactory
import com.jamesbrown.aflmobile.ui.navigation.PlayerLaunchRequest
import com.jamesbrown.aflmobile.ui.theme.appTopBarColors
import com.jamesbrown.aflmobile.ui.theme.tabular
import kotlinx.coroutines.Job
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.flow.update
import kotlinx.coroutines.launch


data class SgmBuilderUiState(
    val draft: SgmDraftState = SgmDraftState(),
    val bookmakers: List<BookmakerSummary> = emptyList(),
    val events: List<EventSummary> = emptyList(),
    val selectedBookmaker: String? = null,
    val selectedEventId: Int? = null,
    val bestOnly: Boolean = false,
    val metricFilters: SelectionMetricFilters = SelectionMetricFilters(),
    val candidateLegs: List<OddsSearchResult> = emptyList(),
    val isLoadingOptions: Boolean = true,
    val isLoadingQuote: Boolean = false,
    val errorMessage: String? = null,
    val infoMessage: String? = null,
)

class SgmBuilderViewModel(
    private val repository: AflRepository,
    private val draftStore: SgmDraftStore,
) : ViewModel() {
    private val _uiState = MutableStateFlow(SgmBuilderUiState())
    val uiState: StateFlow<SgmBuilderUiState> = _uiState.asStateFlow()
    private var loadJob: Job? = null

    init {
        viewModelScope.launch {
            draftStore.state.collect { draft ->
                _uiState.update { it.copy(draft = draft) }
            }
        }
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
                val preferredBookmaker = uiState.value.selectedBookmaker
                    ?: uiState.value.draft.bookmaker
                    ?: bookmakers.firstOrNull { it.code == defaultBookmaker && it.enabled }?.code
                    ?: bookmakers.firstOrNull { it.livePricingEnabled && it.enabled }?.code
                    ?: bookmakers.firstOrNull { it.enabled }?.code

                _uiState.update {
                    it.copy(
                        bookmakers = bookmakers,
                        selectedBookmaker = preferredBookmaker,
                    )
                }

                if (preferredBookmaker == null) {
                    _uiState.update { it.copy(isLoadingOptions = false, candidateLegs = emptyList(), events = emptyList()) }
                } else {
                    loadEventsForBookmaker(preferredBookmaker)
                }
            }.onFailure { error ->
                _uiState.update {
                    it.copy(
                        isLoadingOptions = false,
                        errorMessage = error.toUserMessage("Failed to load bookmakers."),
                    )
                }
            }
        }
    }

    fun selectBookmaker(bookmakerCode: String) {
        if (uiState.value.draft.legs.isNotEmpty() && uiState.value.draft.bookmaker != bookmakerCode) {
            draftStore.clear()
        }
        _uiState.update {
            it.copy(
                selectedBookmaker = bookmakerCode,
                selectedEventId = null,
                events = emptyList(),
                candidateLegs = emptyList(),
                isLoadingOptions = true,
                errorMessage = null,
            )
        }
        loadJob?.cancel()
        loadJob = viewModelScope.launch {
            loadEventsForBookmaker(bookmakerCode)
        }
    }

    fun selectEvent(eventId: Int) {
        val selectedBookmaker = uiState.value.selectedBookmaker ?: return
        if (uiState.value.draft.legs.isNotEmpty() && uiState.value.draft.eventId != eventId) {
            draftStore.clear()
        }
        _uiState.update {
            it.copy(
                selectedEventId = eventId,
                candidateLegs = emptyList(),
                isLoadingOptions = true,
                errorMessage = null,
            )
        }
        loadJob?.cancel()
        loadJob = viewModelScope.launch {
            loadCandidateLegs(bookmakerCode = selectedBookmaker, eventId = eventId)
        }
    }

    fun applyMetricFilters(metricFilters: SelectionMetricFilters) {
        val current = uiState.value
        _uiState.update {
            it.copy(
                metricFilters = metricFilters,
                isLoadingOptions = current.selectedBookmaker != null && current.selectedEventId != null,
            )
        }
        val selectedBookmaker = current.selectedBookmaker ?: return
        val selectedEventId = current.selectedEventId ?: return
        loadJob?.cancel()
        loadJob = viewModelScope.launch {
            loadCandidateLegs(bookmakerCode = selectedBookmaker, eventId = selectedEventId)
        }
    }

    fun toggleLeg(leg: OddsSearchResult) {
        val draft = uiState.value.draft
        if (draft.legs.any { it.selectionId == leg.selectionId }) {
            draftStore.removeLeg(leg.selectionId)
            _uiState.update { it.copy(infoMessage = "Leg removed.", errorMessage = null) }
            return
        }
        val basePrice = leg.decimalPrice
        if (!leg.sgmEligible || basePrice == null) {
            _uiState.update { it.copy(errorMessage = "That leg is not ready for SGM pricing.") }
            return
        }
        val result = draftStore.addLeg(
            DraftLeg(
                selectionId = leg.selectionId,
                eventId = leg.eventId,
                eventLabel = leg.matchName,
                bookmaker = leg.bookmaker,
                label = leg.label,
                marketTypeCode = leg.marketTypeCode,
                selectionType = leg.selectionType,
                basePrice = basePrice,
                diff2025 = leg.diff2025,
                diffLast10 = leg.diffLast10,
                nextBestProbDiff = leg.nextBestProbDiff,
                isBestPrice = leg.isBestPrice,
            ),
        )
        _uiState.update {
            it.copy(
                infoMessage = if (result.applied) result.message else null,
                errorMessage = if (result.applied) null else result.message,
            )
        }
    }

    fun removeLeg(selectionId: Int) {
        draftStore.removeLeg(selectionId)
        _uiState.update { it.copy(infoMessage = "Leg removed.", errorMessage = null) }
    }

    fun clearDraft() {
        draftStore.clear()
        _uiState.update { it.copy(errorMessage = null, infoMessage = "Draft cleared.") }
    }

    fun setForceRefresh(forceRefresh: Boolean) {
        draftStore.setForceRefresh(forceRefresh)
    }

    fun setBestOnly(bestOnly: Boolean) {
        val current = uiState.value
        _uiState.update {
            it.copy(
                bestOnly = bestOnly,
                isLoadingOptions = current.selectedBookmaker != null && current.selectedEventId != null,
                errorMessage = null,
            )
        }
        val selectedBookmaker = current.selectedBookmaker ?: return
        val selectedEventId = current.selectedEventId ?: return
        loadJob?.cancel()
        loadJob = viewModelScope.launch {
            loadCandidateLegs(bookmakerCode = selectedBookmaker, eventId = selectedEventId)
        }
    }

    fun quote() {
        val draft = uiState.value.draft
        val eventId = uiState.value.selectedEventId ?: draft.eventId
        if (eventId == null || draft.legs.size < 2) {
            _uiState.update {
                it.copy(errorMessage = "Choose one match and at least two legs before comparing.")
            }
            return
        }
        viewModelScope.launch {
            _uiState.update { it.copy(isLoadingQuote = true, errorMessage = null) }
            runCatchingCancellable {
                repository.compareSgm(
                    eventId = eventId,
                    selectionIds = draft.legs.map { it.selectionId },
                    forceRefresh = draft.forceRefresh,
                )
            }.onSuccess { comparison ->
                draftStore.setComparisons(comparison.results)
                _uiState.update {
                    it.copy(
                        isLoadingQuote = false,
                        infoMessage = if (comparison.results.isEmpty()) {
                            "No agency currently offers the full combination."
                        } else {
                            "Comparison updated."
                        },
                    )
                }
            }.onFailure { error ->
                val message = error.toUserMessage("Quote failed.")
                draftStore.setError(message)
                _uiState.update { it.copy(isLoadingQuote = false, errorMessage = message) }
            }
        }
    }

    fun consumeInfoMessage() {
        _uiState.update { it.copy(infoMessage = null) }
    }

    fun consumeErrorMessage() {
        _uiState.update { it.copy(errorMessage = null) }
    }

    private suspend fun loadEventsForBookmaker(bookmakerCode: String) {
        runCatchingCancellable {
            repository.events(bookmaker = bookmakerCode, query = null)
        }.onSuccess { events ->
            val preferredEventId = uiState.value.selectedEventId
                ?.takeIf { selectedId -> events.any { it.id == selectedId } }
                ?: uiState.value.draft.eventId
                    ?.takeIf { draftId -> uiState.value.draft.bookmaker == bookmakerCode && events.any { it.id == draftId } }
                ?: events.firstOrNull()?.id

            _uiState.update {
                it.copy(
                    events = events,
                    selectedEventId = preferredEventId,
                )
            }

            if (preferredEventId == null) {
                _uiState.update { it.copy(isLoadingOptions = false, candidateLegs = emptyList()) }
            } else {
                loadCandidateLegs(bookmakerCode = bookmakerCode, eventId = preferredEventId)
            }
        }.onFailure { error ->
            _uiState.update {
                it.copy(
                    isLoadingOptions = false,
                    errorMessage = error.toUserMessage("Failed to load matches."),
                )
            }
        }
    }

    private suspend fun loadCandidateLegs(bookmakerCode: String, eventId: Int) {
        val metricFilters = uiState.value.metricFilters
        runCatchingCancellable {
            repository.odds(
                OddsQuery(
                    bookmakers = listOf(bookmakerCode),
                    scope = "player",
                    eventIds = listOf(eventId),
                    sortBy = "market",
                    sortDirection = "asc",
                    matchupDifficulties = metricFilters.matchupDifficulties,
                    minPrice = metricFilters.minPriceText.toDoubleOrNull(),
                    maxPrice = metricFilters.maxPriceText.toDoubleOrNull(),
                    minDiff2025 = metricFilters.minDiff2025.toDouble(),
                    maxDiff2025 = metricFilters.maxDiff2025.toDouble(),
                    minDiffLast10 = metricFilters.minDiffLast10.toDouble(),
                    maxDiffLast10 = metricFilters.maxDiffLast10.toDouble(),
                    minNextBestProbDiff = metricFilters.minNextBestProbDiff.toDouble(),
                    maxNextBestProbDiff = metricFilters.maxNextBestProbDiff.toDouble(),
                    bestOnly = uiState.value.bestOnly,
                    limit = 5000,
                ),
            )
        }.onSuccess { odds ->
            _uiState.update {
                it.copy(
                    candidateLegs = odds.filter { leg -> leg.marketTypeCode.startsWith("player_") },
                    isLoadingOptions = false,
                    errorMessage = null,
                )
            }
        }.onFailure { error ->
            _uiState.update {
                it.copy(
                    candidateLegs = emptyList(),
                    isLoadingOptions = false,
                    errorMessage = error.toUserMessage("Failed to load SGM legs."),
                )
            }
        }
    }
}

@Composable
fun SgmBuilderRoute(
    repository: AflRepository,
    draftStore: SgmDraftStore,
    onOpenPlayerRequest: (PlayerLaunchRequest) -> Unit,
) {
    val viewModel: SgmBuilderViewModel = viewModel(
        factory = simpleViewModelFactory { SgmBuilderViewModel(repository, draftStore) },
    )
    val uiState by viewModel.uiState.collectAsStateWithLifecycle()
    SgmBuilderScreen(
        uiState = uiState,
        onSelectBookmaker = viewModel::selectBookmaker,
        onSelectEvent = viewModel::selectEvent,
        onToggleLeg = viewModel::toggleLeg,
        onRemoveLeg = viewModel::removeLeg,
        onClearDraft = viewModel::clearDraft,
        onForceRefreshChanged = viewModel::setForceRefresh,
        onBestOnlyChanged = viewModel::setBestOnly,
        onApplyMetricFilters = viewModel::applyMetricFilters,
        onQuote = viewModel::quote,
        onRefresh = viewModel::refresh,
        onInfoMessageShown = viewModel::consumeInfoMessage,
        onErrorMessageShown = viewModel::consumeErrorMessage,
        onOpenPlayerRequest = onOpenPlayerRequest,
    )
}

@OptIn(ExperimentalMaterial3Api::class, ExperimentalFoundationApi::class)
@Composable
private fun SgmBuilderScreen(
    uiState: SgmBuilderUiState,
    onSelectBookmaker: (String) -> Unit,
    onSelectEvent: (Int) -> Unit,
    onToggleLeg: (OddsSearchResult) -> Unit,
    onRemoveLeg: (Int) -> Unit,
    onClearDraft: () -> Unit,
    onForceRefreshChanged: (Boolean) -> Unit,
    onBestOnlyChanged: (Boolean) -> Unit,
    onApplyMetricFilters: (SelectionMetricFilters) -> Unit,
    onQuote: () -> Unit,
    onRefresh: () -> Unit,
    onInfoMessageShown: () -> Unit,
    onErrorMessageShown: () -> Unit,
    onOpenPlayerRequest: (PlayerLaunchRequest) -> Unit,
) {
    val draft = uiState.draft
    val selectedBookmaker = uiState.selectedBookmaker
    val selectedEvent = uiState.events.firstOrNull { it.id == uiState.selectedEventId }
    val selectedSelectionIds = remember(draft.legs) { draft.legs.map { it.selectionId }.toSet() }
    var showFilters by remember { mutableStateOf(false) }
    var showControls by remember { mutableStateOf(false) }
    var showGlossary by remember { mutableStateOf(false) }
    var pendingBookmaker by remember { mutableStateOf<String?>(null) }
    var pendingEventId by remember { mutableStateOf<Int?>(null) }
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
    val marketCodes = remember(uiState.candidateLegs) { orderedMarketCodes(uiState.candidateLegs) }
    var selectedMarketCode by rememberSaveable(uiState.selectedBookmaker, uiState.selectedEventId) {
        mutableStateOf(if (marketCodes.isNotEmpty()) AllMarketCode else null)
    }
    LaunchedEffect(marketCodes) {
        val allowedCodes = listOf(AllMarketCode) + marketCodes
        if (selectedMarketCode !in allowedCodes) {
            selectedMarketCode = if (marketCodes.isNotEmpty()) AllMarketCode else null
        }
    }
    val visibleLegs = remember(uiState.candidateLegs, selectedMarketCode) {
        uiState.candidateLegs.filter { leg ->
            selectedMarketCode == null || selectedMarketCode == AllMarketCode || leg.marketTypeCode == selectedMarketCode
        }
    }
    val groupedLegs = remember(visibleLegs) {
        buildCandidateBoard(legs = visibleLegs)
    }
    val rowLegs = remember(visibleLegs, rowSortField, rowSortDescending) {
        sortCandidateRows(legs = visibleLegs, sortField = rowSortField, descending = rowSortDescending)
    }
    LaunchedEffect(uiState.isLoadingQuote, draft.latestComparisons.size) {
        if (uiState.isLoadingQuote || draft.latestComparisons.isNotEmpty()) {
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
        sheetPeekHeight = if (draft.legs.isNotEmpty()) 76.dp else 0.dp,
        sheetContainerColor = MaterialTheme.colorScheme.surfaceContainerLow,
        sheetContentColor = MaterialTheme.colorScheme.onSurface,
        sheetShadowElevation = if (draft.legs.isNotEmpty()) 10.dp else 0.dp,
        sheetShape = RoundedCornerShape(topStart = 28.dp, topEnd = 28.dp),
        sheetDragHandle = null,
        sheetContent = {
            if (draft.legs.isNotEmpty()) {
                SgmDraftSheet(
                    draft = draft,
                    selectedEventName = selectedEvent?.matchName ?: draft.eventLabel,
                    selectedBookmaker = selectedBookmaker,
                    isLoadingQuote = uiState.isLoadingQuote,
                    onExpand = {
                        coroutineScope.launch {
                            scaffoldState.bottomSheetState.expand()
                        }
                    },
                    onCompare = {
                        coroutineScope.launch {
                            scaffoldState.bottomSheetState.expand()
                        }
                        onQuote()
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
                title = { Text("SGM builder") },
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
                    SgmControlCard(
                        bookmakers = uiState.bookmakers,
                        events = uiState.events,
                        selectedBookmaker = selectedBookmaker,
                        selectedEventId = uiState.selectedEventId,
                        bestOnly = uiState.bestOnly,
                        onSelectBookmaker = { code ->
                            if (draft.legs.isNotEmpty() && draft.bookmaker != code) {
                                pendingBookmaker = code
                            } else {
                                onSelectBookmaker(code)
                            }
                        },
                        onSelectEvent = { eventId ->
                            if (draft.legs.isNotEmpty() && draft.eventId != eventId) {
                                pendingEventId = eventId
                            } else {
                                onSelectEvent(eventId)
                            }
                        },
                        onBestOnlyChanged = onBestOnlyChanged,
                    )
                }

                if (marketCodes.isNotEmpty()) {
                    item {
                        SgmMarketSelectorRow(
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
                            title = "No eligible legs",
                            body = "No SGM-ready selections were found for this agency and match.",
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
                                enabled = isSelectionPriceable(leg),
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
                                isSelectionEnabled = ::isSelectionPriceable,
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
            SgmControlsSheet(
                forceRefresh = draft.forceRefresh,
                displayMode = displayMode,
                onForceRefreshChanged = onForceRefreshChanged,
                onDisplayModeChanged = { displayMode = it },
                onDismiss = { showControls = false },
            )
        }
        if (showGlossary) {
            MetricGlossarySheet(onDismiss = { showGlossary = false })
        }
        pendingBookmaker?.let { candidate ->
            DraftClearConfirmDialog(
                legCount = draft.legs.size,
                message = "Switching the agency clears your current draft.",
                onConfirm = {
                    onSelectBookmaker(candidate)
                    pendingBookmaker = null
                },
                onDismiss = { pendingBookmaker = null },
            )
        }
        pendingEventId?.let { candidate ->
            DraftClearConfirmDialog(
                legCount = draft.legs.size,
                message = "Switching the match clears your current draft.",
                onConfirm = {
                    onSelectEvent(candidate)
                    pendingEventId = null
                },
                onDismiss = { pendingEventId = null },
            )
        }
    }
}

@Composable
private fun DraftClearConfirmDialog(
    legCount: Int,
    message: String,
    onConfirm: () -> Unit,
    onDismiss: () -> Unit,
) {
    AlertDialog(
        onDismissRequest = onDismiss,
        title = { Text("Clear draft?") },
        text = {
            Text("$message You currently have $legCount leg${if (legCount == 1) "" else "s"} selected.")
        },
        confirmButton = {
            TextButton(onClick = onConfirm) {
                Text("Clear and switch")
            }
        },
        dismissButton = {
            TextButton(onClick = onDismiss) {
                Text("Keep draft")
            }
        },
    )
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun SgmControlCard(
    bookmakers: List<BookmakerSummary>,
    events: List<EventSummary>,
    selectedBookmaker: String?,
    selectedEventId: Int?,
    bestOnly: Boolean,
    onSelectBookmaker: (String) -> Unit,
    onSelectEvent: (Int) -> Unit,
    onBestOnlyChanged: (Boolean) -> Unit,
) {
    var bookmakerExpanded by remember { mutableStateOf(false) }
    var eventExpanded by remember { mutableStateOf(false) }
    Card(
        colors = androidx.compose.material3.CardDefaults.cardColors(
            containerColor = MaterialTheme.colorScheme.surface,
        ),
        border = androidx.compose.foundation.BorderStroke(1.dp, MaterialTheme.colorScheme.outlineVariant),
    ) {
        Column(
            modifier = Modifier.padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            Text(
                "Agency and match",
                modifier = Modifier.semantics { heading() },
                style = MaterialTheme.typography.titleMedium,
                fontWeight = FontWeight.SemiBold,
            )
            BuilderSupportText("Choose the source agency and the game you want to price.")

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
                    label = { Text("Agency") },
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
                            text = {
                                Text(
                                    if (bookmaker.livePricingEnabled) {
                                        "${bookmaker.displayName} • live"
                                    } else {
                                        bookmaker.displayName
                                    },
                                )
                            },
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
                    value = events.firstOrNull { it.id == selectedEventId }?.matchName ?: "Choose match",
                    onValueChange = {},
                    modifier = Modifier
                        .menuAnchor(ExposedDropdownMenuAnchorType.PrimaryNotEditable)
                        .fillMaxWidth(),
                    readOnly = true,
                    label = { Text("Match") },
                    trailingIcon = {
                        ExposedDropdownMenuDefaults.TrailingIcon(expanded = eventExpanded)
                    },
                )
                DropdownMenu(
                    expanded = eventExpanded,
                    onDismissRequest = { eventExpanded = false },
                    modifier = Modifier.heightIn(max = 360.dp),
                ) {
                    events.forEach { event ->
                        DropdownMenuItem(
                            text = { Text(event.matchName) },
                            onClick = {
                                onSelectEvent(event.id)
                                eventExpanded = false
                            },
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
                    BuilderSupportText("Only show props where this agency is currently the best price.")
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
private fun SgmControlsSheet(
    forceRefresh: Boolean,
    displayMode: BuilderDisplayMode,
    onForceRefreshChanged: (Boolean) -> Unit,
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
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.SpaceBetween,
                verticalAlignment = Alignment.CenterVertically,
            ) {
                Column(modifier = Modifier.weight(1f)) {
                    Text("Force refresh", fontWeight = FontWeight.Medium)
                    BuilderSupportText("Bypass the short quote cache for live agency quotes.")
                }
                Switch(
                    checked = forceRefresh,
                    onCheckedChange = onForceRefreshChanged,
                )
            }
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
private fun SgmDraftSheet(
    draft: SgmDraftState,
    selectedEventName: String?,
    selectedBookmaker: String?,
    isLoadingQuote: Boolean,
    onExpand: () -> Unit,
    onCompare: () -> Unit,
    onRemoveLeg: (Int) -> Unit,
    onClearDraft: () -> Unit,
) {
    val bestPrice = draft.latestComparisons.maxOfOrNull { it.quotedPrice }
    LazyColumn(
        modifier = Modifier
            .fillMaxWidth()
            .heightIn(max = 560.dp),
        contentPadding = PaddingValues(start = 16.dp, top = 0.dp, end = 16.dp, bottom = 12.dp),
        verticalArrangement = Arrangement.spacedBy(12.dp),
    ) {
        item {
            DraftPeekBar(
                count = draft.legs.size,
                primaryLabel = "${draft.legs.size} leg${if (draft.legs.size == 1) "" else "s"} selected",
                secondaryLabel = selectedEventName ?: selectedBookmaker?.let(::bookmakerLabel) ?: "Tap to review draft",
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
                            "${draft.legs.size} leg${if (draft.legs.size == 1) "" else "s"} • ${selectedBookmaker?.let(::bookmakerLabel) ?: "No agency"}",
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
                    enabled = draft.legs.size >= 2 && !isLoadingQuote,
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

        if (isLoadingQuote) {
            item { LoadingCard("Pricing selected legs") }
        }

        if (draft.latestComparisons.isNotEmpty()) {
            item {
                Text(
                    "Agency comparison",
                    modifier = Modifier.semantics { heading() },
                    style = MaterialTheme.typography.titleMedium,
                    fontWeight = FontWeight.SemiBold,
                    color = MaterialTheme.colorScheme.primary,
                )
            }
            items(draft.latestComparisons, key = { it.bookmaker }) { result ->
                SgmComparisonCard(
                    result = result,
                    rank = draft.latestComparisons.indexOfFirst { it.bookmaker == result.bookmaker } + 1,
                )
            }
        }

        if (draft.legs.isNotEmpty()) {
            item {
                Text(
                    "Legs",
                    modifier = Modifier.semantics { heading() },
                    style = MaterialTheme.typography.titleMedium,
                    fontWeight = FontWeight.SemiBold,
                    color = MaterialTheme.colorScheme.primary,
                )
            }
            items(draft.legs, key = { it.selectionId }) { leg ->
                DraftLegCard(leg = leg, onRemove = onRemoveLeg)
            }
        }
    }
}

@Composable
private fun SgmComparisonCard(
    result: SgmAgencyComparison,
    rank: Int,
) {
    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = androidx.compose.material3.CardDefaults.cardColors(
            containerColor = MaterialTheme.colorScheme.surface,
        ),
        border = androidx.compose.foundation.BorderStroke(1.dp, MaterialTheme.colorScheme.outlineVariant),
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
                        "${result.legs.size} legs priced",
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
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.spacedBy(8.dp),
            ) {
                SummaryMetricCard(label = "LOCAL", value = formatDecimalPrice(result.unadjustedPrice), modifier = Modifier.weight(1f))
                SummaryMetricCard(label = "FACTOR", value = formatDecimalPrice(result.adjustmentFactor), modifier = Modifier.weight(1f))
                SummaryMetricCard(label = "CACHE", value = if (result.fromCache) "Yes" else "No", modifier = Modifier.weight(1f))
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
                    }
                    Text(
                        formatDecimalPrice(leg.basePrice),
                        style = MaterialTheme.typography.bodyMedium.tabular,
                        fontWeight = FontWeight.SemiBold,
                    )
                }
            }
            Text(
                "Quoted ${formatDateTime(result.quotedAt)}",
                style = MaterialTheme.typography.bodySmall,
                color = MaterialTheme.colorScheme.onSurfaceVariant,
            )
        }
    }
}

@Composable
private fun SgmMarketSelectorRow(
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

private fun isSelectionPriceable(selection: OddsSearchResult): Boolean =
    selection.sgmEligible && selection.decimalPrice != null
