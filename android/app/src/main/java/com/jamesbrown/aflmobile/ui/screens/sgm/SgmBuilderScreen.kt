package com.jamesbrown.aflmobile.ui.screens.sgm

import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.ExperimentalFoundationApi
import androidx.compose.foundation.clickable
import androidx.compose.foundation.horizontalScroll
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.WindowInsets
import androidx.compose.foundation.layout.asPaddingValues
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.heightIn
import androidx.compose.foundation.layout.navigationBars
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.LazyRow
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Delete
import androidx.compose.material.icons.outlined.FilterList
import androidx.compose.material.icons.outlined.Menu
import androidx.compose.material.icons.outlined.MoreVert
import androidx.compose.material.icons.outlined.Refresh
import androidx.compose.material3.Button
import androidx.compose.material3.ButtonDefaults
import androidx.compose.material3.BottomSheetScaffold
import androidx.compose.material3.Card
import androidx.compose.material3.CardDefaults
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
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Scaffold
import androidx.compose.material3.SheetValue
import androidx.compose.material3.Surface
import androidx.compose.material3.Switch
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.material3.TopAppBar
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
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.lifecycle.ViewModel
import androidx.lifecycle.compose.collectAsStateWithLifecycle
import androidx.lifecycle.viewModelScope
import androidx.lifecycle.viewmodel.compose.viewModel
import com.jamesbrown.aflmobile.data.repository.AflRepository
import com.jamesbrown.aflmobile.data.repository.SgmDraftStore
import com.jamesbrown.aflmobile.model.BookmakerSummary
import com.jamesbrown.aflmobile.model.BuilderDisplayMode
import com.jamesbrown.aflmobile.model.BuilderSortField
import com.jamesbrown.aflmobile.model.DraftLeg
import com.jamesbrown.aflmobile.model.EventSummary
import com.jamesbrown.aflmobile.model.OddsDiffSliderMax
import com.jamesbrown.aflmobile.model.OddsDiffSliderMin
import com.jamesbrown.aflmobile.model.OddsSearchResult
import com.jamesbrown.aflmobile.model.SelectionMetricFilters
import com.jamesbrown.aflmobile.model.SgmAgencyComparison
import com.jamesbrown.aflmobile.model.SgmDraftState
import com.jamesbrown.aflmobile.ui.common.EmptyCard
import com.jamesbrown.aflmobile.ui.common.ErrorCard
import com.jamesbrown.aflmobile.ui.common.InlineChip
import com.jamesbrown.aflmobile.ui.common.LoadingCard
import com.jamesbrown.aflmobile.ui.common.BuilderDisplayModeSegmented
import com.jamesbrown.aflmobile.ui.common.BuilderSupportText
import com.jamesbrown.aflmobile.ui.common.ScreenPadding
import com.jamesbrown.aflmobile.ui.common.SelectionMetricFilterSheet
import com.jamesbrown.aflmobile.ui.common.formatDateTime
import com.jamesbrown.aflmobile.ui.common.formatDecimalPrice
import com.jamesbrown.aflmobile.ui.common.simpleViewModelFactory
import com.jamesbrown.aflmobile.ui.theme.Blue100
import com.jamesbrown.aflmobile.ui.theme.Blue200
import com.jamesbrown.aflmobile.ui.theme.Blue50
import com.jamesbrown.aflmobile.ui.theme.Blue700
import com.jamesbrown.aflmobile.ui.theme.IceWhite
import com.jamesbrown.aflmobile.ui.theme.NegativeSurface
import com.jamesbrown.aflmobile.ui.theme.NeutralSurface
import com.jamesbrown.aflmobile.ui.theme.Orange100
import com.jamesbrown.aflmobile.ui.theme.Orange300
import com.jamesbrown.aflmobile.ui.theme.Orange700
import com.jamesbrown.aflmobile.ui.theme.PositiveSurface
import com.jamesbrown.aflmobile.ui.theme.appCardColors
import com.jamesbrown.aflmobile.ui.theme.appGlassBorder
import com.jamesbrown.aflmobile.ui.theme.appTopBarColors
import java.util.Locale
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.flow.update
import kotlinx.coroutines.launch


private data class CandidateBoardGroup(
    val key: String,
    val title: String,
    val subtitle: String,
    val columns: List<CandidateLineColumn>,
)

private data class CandidateLineColumn(
    val key: String,
    val label: String,
    val slots: List<CandidateSelectionSlot>,
)

private data class CandidateSelectionSlot(
    val badge: String,
    val selection: OddsSearchResult,
)

private const val AllMarketCode = "__all__"

private val SgmAccent = Orange700
private val SgmAccentSoft = Orange100
private val SgmAccentBorder = Orange300
private val SgmMutedSurface = Blue50
private val SgmTitle = Blue700
private val DraftSheetPeekHeight = 76.dp

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

    init {
        viewModelScope.launch {
            draftStore.state.collect { draft ->
                _uiState.update { it.copy(draft = draft) }
            }
        }
        refresh()
    }

    fun refresh() {
        viewModelScope.launch {
            _uiState.update { it.copy(isLoadingOptions = true, errorMessage = null, infoMessage = null) }
            runCatching {
                repository.bookmakers()
            }.onSuccess { bookmakers ->
                val preferredBookmaker = uiState.value.selectedBookmaker
                    ?: uiState.value.draft.bookmaker
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
                        errorMessage = error.message ?: "Failed to load bookmakers.",
                    )
                }
            }
        }
    }

    fun selectBookmaker(bookmakerCode: String) {
        viewModelScope.launch {
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
                    infoMessage = null,
                )
            }
            loadEventsForBookmaker(bookmakerCode)
        }
    }

    fun selectEvent(eventId: Int) {
        viewModelScope.launch {
            val selectedBookmaker = uiState.value.selectedBookmaker ?: return@launch
            if (uiState.value.draft.legs.isNotEmpty() && uiState.value.draft.eventId != eventId) {
                draftStore.clear()
            }
            _uiState.update {
                it.copy(
                    selectedEventId = eventId,
                    candidateLegs = emptyList(),
                    isLoadingOptions = true,
                    errorMessage = null,
                    infoMessage = null,
                )
            }
            loadCandidateLegs(bookmakerCode = selectedBookmaker, eventId = eventId)
        }
    }

    fun applyMetricFilters(metricFilters: SelectionMetricFilters) {
        val current = uiState.value
        _uiState.update { it.copy(metricFilters = metricFilters, isLoadingOptions = current.selectedBookmaker != null && current.selectedEventId != null) }
        val selectedBookmaker = current.selectedBookmaker ?: return
        val selectedEventId = current.selectedEventId ?: return
        viewModelScope.launch {
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
            _uiState.update { it.copy(errorMessage = "That leg is not ready for SGM pricing.", infoMessage = null) }
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
                infoMessage = result.message,
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
                infoMessage = null,
            )
        }
        val selectedBookmaker = current.selectedBookmaker ?: return
        val selectedEventId = current.selectedEventId ?: return
        viewModelScope.launch {
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
            _uiState.update { it.copy(isLoadingQuote = true, errorMessage = null, infoMessage = null) }
            runCatching {
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
                draftStore.setError(error.message)
                _uiState.update { it.copy(isLoadingQuote = false, errorMessage = error.message ?: "Quote failed.") }
            }
        }
    }

    private suspend fun loadEventsForBookmaker(bookmakerCode: String) {
        runCatching {
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
                    errorMessage = error.message ?: "Failed to load matches.",
                )
            }
        }
    }

    private suspend fun loadCandidateLegs(bookmakerCode: String, eventId: Int) {
        val metricFilters = uiState.value.metricFilters
        runCatching {
            repository.odds(
                bookmakers = listOf(bookmakerCode),
                scope = "player",
                query = null,
                marketType = null,
                eventId = eventId,
                includePlayerIds = emptyList(),
                excludePlayerIds = emptyList(),
                sortBy = "market",
                sortDirection = "asc",
                selectionType = null,
                minEdge = null,
                minPrice = metricFilters.minPriceText.toDoubleOrNull(),
                maxPrice = metricFilters.maxPriceText.toDoubleOrNull(),
                minDiff2025 = metricFilters.minDiff2025.toDouble(),
                maxDiff2025 = metricFilters.maxDiff2025.toDouble(),
                minDiffLast10 = metricFilters.minDiffLast10.toDouble(),
                maxDiffLast10 = metricFilters.maxDiffLast10.toDouble(),
                minNextBestProbDiff = metricFilters.minNextBestProbDiff.toDouble(),
                maxNextBestProbDiff = metricFilters.maxNextBestProbDiff.toDouble(),
                sgmOnly = false,
                bestOnly = uiState.value.bestOnly,
                limit = 5000,
            )
        }.onSuccess { odds ->
            _uiState.update {
                it.copy(
                    candidateLegs = odds.filter { it.marketTypeCode.startsWith("player_") },
                    isLoadingOptions = false,
                    errorMessage = null,
                )
            }
        }.onFailure { error ->
            _uiState.update {
                it.copy(
                    candidateLegs = emptyList(),
                    isLoadingOptions = false,
                    errorMessage = error.message ?: "Failed to load SGM legs.",
                )
            }
        }
    }
}

@Composable
fun SgmBuilderRoute(
    repository: AflRepository,
    draftStore: SgmDraftStore,
    onOpenNavigation: () -> Unit,
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
        onOpenNavigation = onOpenNavigation,
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
    onOpenNavigation: () -> Unit,
) {
    val draft = uiState.draft
    val selectedBookmaker = uiState.selectedBookmaker
    val selectedEvent = uiState.events.firstOrNull { it.id == uiState.selectedEventId }
    val selectedSelectionIds = remember(draft.legs) { draft.legs.map { it.selectionId }.toSet() }
    val navigationBottomInset = WindowInsets.navigationBars.asPaddingValues().calculateBottomPadding()
    var showFilters by remember { mutableStateOf(false) }
    var showControls by remember { mutableStateOf(false) }
    var draftMetricFilters by remember(uiState.metricFilters) { mutableStateOf(uiState.metricFilters) }
    var displayMode by rememberSaveable { mutableStateOf(BuilderDisplayMode.ROW) }
    var rowSortField by rememberSaveable { mutableStateOf(BuilderSortField.NEXT_BEST) }
    var rowSortDescending by rememberSaveable { mutableStateOf(true) }
    val coroutineScope = rememberCoroutineScope()
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
    val content: @Composable (PaddingValues) -> Unit = { innerPadding ->
        LazyColumn(
            modifier = Modifier
                .fillMaxSize()
                .padding(innerPadding),
            contentPadding = ScreenPadding,
            verticalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            item {
                SgmControlCard(
                    bookmakers = uiState.bookmakers,
                    events = uiState.events,
                    selectedBookmaker = selectedBookmaker,
                    selectedEventId = uiState.selectedEventId,
                    onSelectBookmaker = onSelectBookmaker,
                    onSelectEvent = onSelectEvent,
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

            if (uiState.isLoadingOptions) {
                item { LoadingCard("Loading SGM legs") }
            }

            uiState.errorMessage?.let { message ->
                item { ErrorCard(message) }
            }

            uiState.infoMessage?.let { message ->
                item { EmptyCard("SGM status", message) }
            }

            if (!uiState.isLoadingOptions && visibleLegs.isEmpty()) {
                item {
                    EmptyCard(
                        title = "No eligible legs",
                        body = "No SGM-ready selections were found for this agency and match.",
                    )
                }
            } else {
                item {
                    Text(
                        marketSectionTitle(selectedMarketCode),
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
                            accent = SgmAccent,
                            accentBorder = SgmAccentBorder,
                            titleColor = SgmTitle,
                            onToggleLeg = onToggleLeg,
                        )
                    }
                } else {
                    items(groupedLegs, key = { it.key }) { group ->
                        CandidateBoardCard(
                            group = group,
                            selectedSelectionIds = selectedSelectionIds,
                            onToggleLeg = onToggleLeg,
                        )
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
                onClear = { draftMetricFilters = SelectionMetricFilters() },
                onDismiss = { showFilters = false },
            )
        }
        if (showControls) {
            SgmControlsSheet(
                forceRefresh = draft.forceRefresh,
                bestOnly = uiState.bestOnly,
                displayMode = displayMode,
                onForceRefreshChanged = onForceRefreshChanged,
                onBestOnlyChanged = onBestOnlyChanged,
                onDisplayModeChanged = { displayMode = it },
                onDismiss = { showControls = false },
            )
        }
    }

    BottomSheetScaffold(
        scaffoldState = scaffoldState,
        sheetPeekHeight = if (draft.legs.isNotEmpty()) DraftSheetPeekHeight + navigationBottomInset else 0.dp,
        sheetContainerColor = MaterialTheme.colorScheme.surfaceContainerLow.copy(alpha = 0.98f),
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
                    navigationBottomInset = navigationBottomInset,
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
        containerColor = Color.Transparent,
        topBar = {
            SgmTopBar(
                hasDraft = draft.legs.isNotEmpty(),
                onRefresh = onRefresh,
                onClearDraft = onClearDraft,
                onOpenNavigation = onOpenNavigation,
                onOpenFilters = { showFilters = true },
                onOpenOptions = { showControls = true },
            )
        },
    ) { innerPadding ->
        content(innerPadding)
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun SgmTopBar(
    hasDraft: Boolean,
    onRefresh: () -> Unit,
    onClearDraft: () -> Unit,
    onOpenNavigation: () -> Unit,
    onOpenFilters: () -> Unit,
    onOpenOptions: () -> Unit,
) {
    TopAppBar(
        title = { Text("SGM") },
        colors = appTopBarColors(),
        navigationIcon = {
            IconButton(onClick = onOpenNavigation) {
                Icon(Icons.Outlined.Menu, contentDescription = "Open navigation")
            }
        },
        actions = {
            IconButton(onClick = onOpenFilters) {
                Icon(Icons.Outlined.FilterList, contentDescription = "Open filters")
            }
            IconButton(onClick = onOpenOptions) {
                Icon(Icons.Outlined.MoreVert, contentDescription = "Open options")
            }
            IconButton(onClick = onRefresh) {
                Icon(Icons.Outlined.Refresh, contentDescription = "Refresh")
            }
            if (hasDraft) {
                IconButton(onClick = onClearDraft) {
                    Icon(Icons.Outlined.Delete, contentDescription = "Clear")
                }
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
    onSelectBookmaker: (String) -> Unit,
    onSelectEvent: (Int) -> Unit,
) {
    var bookmakerExpanded by remember { mutableStateOf(false) }
    var eventExpanded by remember { mutableStateOf(false) }
    Card(
        colors = appCardColors(),
        border = appGlassBorder(),
    ) {
        Column(
            modifier = Modifier.padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            Text("Agency and match", style = MaterialTheme.typography.titleMedium, fontWeight = FontWeight.SemiBold)
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
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun SgmControlsSheet(
    forceRefresh: Boolean,
    bestOnly: Boolean,
    displayMode: BuilderDisplayMode,
    onForceRefreshChanged: (Boolean) -> Unit,
    onBestOnlyChanged: (Boolean) -> Unit,
    onDisplayModeChanged: (BuilderDisplayMode) -> Unit,
    onDismiss: () -> Unit,
) {
    androidx.compose.material3.ModalBottomSheet(
        onDismissRequest = onDismiss,
        containerColor = MaterialTheme.colorScheme.surface,
        scrimColor = MaterialTheme.colorScheme.scrim.copy(alpha = 0.22f),
    ) {
        Column(
            modifier = Modifier
                .fillMaxWidth()
                .padding(horizontal = 20.dp, vertical = 8.dp),
            verticalArrangement = Arrangement.spacedBy(16.dp),
        ) {
            Text("Options", style = MaterialTheme.typography.headlineSmall, fontWeight = FontWeight.SemiBold)
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
            Column(verticalArrangement = Arrangement.spacedBy(8.dp)) {
                Text("Display mode", fontWeight = FontWeight.Medium)
                BuilderDisplayModeSegmented(
                    displayMode = displayMode,
                    onDisplayModeChanged = onDisplayModeChanged,
                    selectedContainerColor = SgmAccent,
                    selectedContentColor = IceWhite,
                    inactiveContainerColor = Blue100,
                    inactiveContentColor = Blue700,
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
    navigationBottomInset: androidx.compose.ui.unit.Dp,
    onExpand: () -> Unit,
    onCompare: () -> Unit,
    onRemoveLeg: (Int) -> Unit,
    onClearDraft: () -> Unit,
) {
    LazyColumn(
        modifier = Modifier
            .fillMaxWidth()
            .heightIn(max = 560.dp),
        contentPadding = PaddingValues(start = 16.dp, top = 0.dp, end = 16.dp, bottom = 12.dp + navigationBottomInset),
        verticalArrangement = Arrangement.spacedBy(12.dp),
    ) {
        item {
            DraftPeekBar(
                count = draft.legs.size,
                primaryLabel = "${draft.legs.size} leg${if (draft.legs.size == 1) "" else "s"} selected",
                secondaryLabel = selectedEventName ?: selectedBookmaker?.let(::bookmakerLabel) ?: "Tap to review draft",
                accent = SgmAccent,
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
                        Text("Current selections", style = MaterialTheme.typography.titleMedium, color = SgmTitle, fontWeight = FontWeight.SemiBold)
                        Text(
                            selectedEventName ?: "Choose a match to start building",
                            style = MaterialTheme.typography.bodyMedium,
                            color = MaterialTheme.colorScheme.onSurface,
                        )
                        Text(
                            "${draft.legs.size} leg${if (draft.legs.size == 1) "" else "s"} • ${selectedBookmaker?.let(::bookmakerLabel) ?: "No agency"}",
                            style = MaterialTheme.typography.bodySmall,
                            color = MaterialTheme.colorScheme.onSurfaceVariant,
                        )
                    }
                    if (draft.legs.isNotEmpty()) {
                        TextButton(onClick = onClearDraft) {
                            Text("Clear")
                        }
                    }
                }
                Button(
                    onClick = onCompare,
                    modifier = Modifier.fillMaxWidth(),
                    enabled = draft.legs.size >= 2 && !isLoadingQuote,
                    colors = ButtonDefaults.buttonColors(
                        containerColor = SgmAccent,
                        contentColor = Color.White,
                        disabledContainerColor = SgmAccent.copy(alpha = 0.35f),
                        disabledContentColor = Color.White.copy(alpha = 0.7f),
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

        items(draft.legs, key = { it.selectionId }) { leg ->
            DraftLegCard(leg = leg, onRemove = onRemoveLeg)
        }

        if (draft.latestComparisons.isNotEmpty()) {
            item {
                Text(
                    "Agency comparison",
                    style = MaterialTheme.typography.titleMedium,
                    fontWeight = FontWeight.SemiBold,
                    color = SgmTitle,
                )
            }
            items(draft.latestComparisons, key = { it.bookmaker }) { result ->
                SgmComparisonCard(
                    result = result,
                    rank = draft.latestComparisons.indexOfFirst { it.bookmaker == result.bookmaker } + 1,
                )
            }
        }
    }
}

@Composable
private fun DraftPeekBar(
    count: Int,
    primaryLabel: String,
    secondaryLabel: String,
    accent: Color,
    onExpand: () -> Unit,
) {
    Surface(
        modifier = Modifier
            .fillMaxWidth()
            .clickable(onClick = onExpand),
        color = Color.Transparent,
    ) {
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .height(DraftSheetPeekHeight)
                .padding(horizontal = 18.dp),
            horizontalArrangement = Arrangement.SpaceBetween,
            verticalAlignment = Alignment.CenterVertically,
        ) {
            Row(
                modifier = Modifier.weight(1f),
                horizontalArrangement = Arrangement.spacedBy(12.dp),
                verticalAlignment = Alignment.CenterVertically,
            ) {
                Surface(
                    shape = RoundedCornerShape(999.dp),
                    color = accent,
                ) {
                    Text(
                        text = count.toString(),
                        modifier = Modifier.padding(horizontal = 12.dp, vertical = 6.dp),
                        style = MaterialTheme.typography.labelLarge,
                        color = Color.White,
                        fontWeight = FontWeight.Bold,
                    )
                }
                Column(verticalArrangement = Arrangement.spacedBy(2.dp)) {
                    Text(
                        primaryLabel,
                        style = MaterialTheme.typography.titleSmall,
                        fontWeight = FontWeight.SemiBold,
                        color = SgmTitle,
                    )
                    Text(
                        secondaryLabel,
                        style = MaterialTheme.typography.bodySmall,
                        color = MaterialTheme.colorScheme.onSurface.copy(alpha = 0.72f),
                        maxLines = 1,
                        overflow = TextOverflow.Ellipsis,
                    )
                }
            }
            Text(
                "Open",
                style = MaterialTheme.typography.labelLarge,
                color = accent,
                fontWeight = FontWeight.SemiBold,
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
                    containerColor = Blue100,
                    labelColor = Blue700,
                    selectedContainerColor = SgmAccent,
                    selectedLabelColor = IceWhite,
                ),
                border = FilterChipDefaults.filterChipBorder(
                    enabled = true,
                    selected = marketCode == selectedMarketCode,
                    borderColor = Blue200,
                    selectedBorderColor = SgmAccent,
                ),
            )
        }
    }
}

@Composable
private fun CandidateBoardCard(
    group: CandidateBoardGroup,
    selectedSelectionIds: Set<Int>,
    onToggleLeg: (OddsSearchResult) -> Unit,
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
            Text(group.title, style = MaterialTheme.typography.titleMedium, color = SgmTitle, fontWeight = FontWeight.SemiBold)
            Text(
                group.subtitle,
                style = MaterialTheme.typography.bodySmall,
                color = MaterialTheme.colorScheme.onSurfaceVariant,
            )
            HorizontalDivider()
            val scrollState = rememberScrollState()
            CandidateSelectionBand(
                title = "Overs",
                scrollState = scrollState,
                columns = group.columns,
                selectedSelectionIds = selectedSelectionIds,
                targetSelectionType = "over",
                onToggleLeg = onToggleLeg,
            )
            CandidateSelectionBand(
                title = "Unders",
                scrollState = scrollState,
                columns = group.columns,
                selectedSelectionIds = selectedSelectionIds,
                targetSelectionType = "under",
                onToggleLeg = onToggleLeg,
            )
        }
    }
}

@Composable
private fun CandidateRowHeader(
    sortField: BuilderSortField,
    descending: Boolean,
    onSortSelected: (BuilderSortField) -> Unit,
) {
    Surface(
        modifier = Modifier.fillMaxWidth(),
        shape = RoundedCornerShape(16.dp),
        color = MaterialTheme.colorScheme.surface.copy(alpha = 0.98f),
        border = BorderStroke(1.dp, Blue200.copy(alpha = 0.8f)),
    ) {
        Row(
            modifier = Modifier.padding(horizontal = 16.dp, vertical = 10.dp),
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.spacedBy(10.dp),
        ) {
            SortableHeaderCell("Player", BuilderSortField.PLAYER, sortField, descending, Modifier.weight(4.1f), Alignment.Start, onSortSelected)
            SortableHeaderCell("Line", BuilderSortField.LINE, sortField, descending, Modifier.weight(0.9f), Alignment.End, onSortSelected)
            SortableHeaderCell("Type", BuilderSortField.TYPE, sortField, descending, Modifier.weight(1.1f), Alignment.Start, onSortSelected)
            SortableHeaderCell("Price", BuilderSortField.PRICE, sortField, descending, Modifier.weight(0.95f), Alignment.End, onSortSelected)
            SortableHeaderCell("L10", BuilderSortField.DIFF_LAST_10, sortField, descending, Modifier.weight(0.85f), Alignment.End, onSortSelected)
            SortableHeaderCell("25", BuilderSortField.DIFF_2025, sortField, descending, Modifier.weight(0.85f), Alignment.End, onSortSelected)
            SortableHeaderCell("NB", BuilderSortField.NEXT_BEST, sortField, descending, Modifier.weight(0.9f), Alignment.End, onSortSelected)
        }
    }
}

@Composable
private fun CandidateSelectionRow(
    selection: OddsSearchResult,
    selected: Boolean,
    enabled: Boolean,
    accent: Color,
    accentBorder: Color,
    titleColor: Color,
    onToggleLeg: (OddsSearchResult) -> Unit,
) {
    Surface(
        modifier = Modifier
            .fillMaxWidth()
            .clickable(enabled = enabled, onClick = { onToggleLeg(selection) }),
        shape = RoundedCornerShape(18.dp),
        color = when {
            selected -> accent
            enabled -> IceWhite
            else -> Blue50.copy(alpha = 0.95f)
        },
        border = BorderStroke(
            1.dp,
            when {
                selected -> accentBorder
                enabled -> Blue200.copy(alpha = 0.9f)
                else -> Blue200.copy(alpha = 0.6f)
            },
        ),
    ) {
        Row(
            modifier = Modifier.padding(horizontal = 16.dp, vertical = 10.dp),
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.spacedBy(10.dp),
        ) {
            Column(
                modifier = Modifier.weight(4.1f),
                verticalArrangement = Arrangement.spacedBy(2.dp),
            ) {
                Text(
                    selection.player?.fullName ?: selection.label,
                    style = MaterialTheme.typography.bodySmall,
                    fontWeight = FontWeight.SemiBold,
                    color = if (selected) IceWhite else titleColor,
                    maxLines = 1,
                )
                Text(
                    buildRowSubtitle(selection),
                    style = MaterialTheme.typography.labelSmall,
                    color = if (selected) IceWhite.copy(alpha = 0.84f) else MaterialTheme.colorScheme.onSurfaceVariant,
                    maxLines = 2,
                    overflow = TextOverflow.Ellipsis,
                )
            }
            MetricCell(formatLineValue(selection.lineValue), Modifier.weight(0.9f), selected)
            TextMetricCell(selectionTypeLabel(selection.selectionType), Modifier.weight(1.1f), selected)
            MetricCell(formatDecimalPrice(selection.decimalPrice), Modifier.weight(0.95f), selected, emphasize = true)
            MetricCell(selection.diffLast10?.let(::formatSignedDelta) ?: "--", Modifier.weight(0.85f), selected, value = selection.diffLast10)
            MetricCell(selection.diff2025?.let(::formatSignedDelta) ?: "--", Modifier.weight(0.85f), selected, value = selection.diff2025)
            MetricCell(selection.nextBestProbDiff?.let(::formatSignedDelta) ?: "--", Modifier.weight(0.9f), selected, value = selection.nextBestProbDiff)
        }
    }
}

@Composable
private fun HeaderCell(
    label: String,
    modifier: Modifier,
    alignment: Alignment.Horizontal,
) {
    Column(
        modifier = modifier,
        horizontalAlignment = alignment,
    ) {
        Text(
            text = label,
            style = MaterialTheme.typography.labelSmall,
            color = MaterialTheme.colorScheme.onSurfaceVariant,
            fontWeight = FontWeight.SemiBold,
        )
    }
}

@Composable
private fun SortableHeaderCell(
    label: String,
    field: BuilderSortField,
    sortField: BuilderSortField,
    descending: Boolean,
    modifier: Modifier,
    alignment: Alignment.Horizontal,
    onSortSelected: (BuilderSortField) -> Unit,
) {
    HeaderCell(
        label = if (sortField == field) {
            "$label ${if (descending) "▼" else "▲"}"
        } else {
            label
        },
        modifier = modifier.clickable { onSortSelected(field) },
        alignment = alignment,
    )
}

@Composable
private fun MetricCell(
    text: String,
    modifier: Modifier,
    selected: Boolean,
    emphasize: Boolean = false,
    value: Double? = null,
) {
    val color = when {
        selected -> IceWhite
        value == null -> MaterialTheme.colorScheme.onSurface
        value > 0 -> Color(0xFF1B7F46)
        value < 0 -> Color(0xFFB34A35)
        else -> Color(0xFF8E6B10)
    }
    Column(
        modifier = modifier,
        horizontalAlignment = Alignment.End,
    ) {
        Text(
            text = text,
            style = if (emphasize) MaterialTheme.typography.bodySmall else MaterialTheme.typography.labelSmall,
            color = color,
            fontWeight = if (emphasize) FontWeight.Bold else FontWeight.SemiBold,
            maxLines = 1,
        )
    }
}

@Composable
private fun TextMetricCell(
    text: String,
    modifier: Modifier,
    selected: Boolean,
) {
    Column(
        modifier = modifier,
        horizontalAlignment = Alignment.Start,
    ) {
        Text(
            text = text,
            style = MaterialTheme.typography.labelSmall,
            color = if (selected) IceWhite else MaterialTheme.colorScheme.onSurface,
            fontWeight = FontWeight.SemiBold,
            maxLines = 1,
        )
    }
}

@Composable
private fun CandidateSelectionBand(
    title: String,
    scrollState: androidx.compose.foundation.ScrollState,
    columns: List<CandidateLineColumn>,
    selectedSelectionIds: Set<Int>,
    targetSelectionType: String,
    onToggleLeg: (OddsSearchResult) -> Unit,
) {
    Column(verticalArrangement = Arrangement.spacedBy(8.dp)) {
        Text(
            title,
            style = MaterialTheme.typography.labelLarge,
            color = SgmAccent,
            fontWeight = FontWeight.SemiBold,
        )
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .horizontalScroll(scrollState),
            horizontalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            columns.forEach { column ->
                val slot = column.slots.firstOrNull { it.selection.selectionType == targetSelectionType }
                SelectionPriceTile(
                    label = compactTileLabel(column.label, targetSelectionType),
                    slot = slot,
                    selected = slot?.selection?.selectionId in selectedSelectionIds,
                    enabled = slot?.selection?.let(::isSelectionPriceable) == true,
                    onClick = { slot?.selection?.let(onToggleLeg) },
                )
            }
        }
    }
}

@Composable
private fun SelectionPriceTile(
    label: String,
    slot: CandidateSelectionSlot?,
    selected: Boolean,
    enabled: Boolean,
    onClick: () -> Unit,
) {
    if (slot == null) {
        BlankSelectionTile(label = label)
        return
    }
    Surface(
        modifier = Modifier
            .width(74.dp)
            .clickable(enabled = enabled, onClick = onClick),
        shape = RoundedCornerShape(18.dp),
        color = when {
            selected -> SgmAccent
            enabled -> IceWhite
            else -> SgmMutedSurface
        },
        tonalElevation = if (selected) 3.dp else 0.dp,
        border = BorderStroke(
            width = 1.dp,
            color = when {
                selected -> SgmAccent
                enabled -> Blue200
                else -> Blue200.copy(alpha = 0.7f)
            },
        ),
    ) {
        Column(
            modifier = Modifier.padding(horizontal = 6.dp, vertical = 7.dp),
            verticalArrangement = Arrangement.spacedBy(5.dp),
        ) {
            Text(
                label,
                style = MaterialTheme.typography.labelSmall,
                color = when {
                    selected -> IceWhite.copy(alpha = 0.94f)
                    enabled -> SgmTitle
                    else -> MaterialTheme.colorScheme.onSurfaceVariant
                },
                fontWeight = FontWeight.SemiBold,
            )
            Text(
                formatDecimalPrice(slot.selection.decimalPrice),
                style = MaterialTheme.typography.titleMedium,
                color = when {
                    selected -> IceWhite
                    enabled -> MaterialTheme.colorScheme.onSurface
                    else -> MaterialTheme.colorScheme.onSurfaceVariant
                },
                fontWeight = FontWeight.Bold,
            )
            Column(verticalArrangement = Arrangement.spacedBy(4.dp)) {
                EmbeddedMetricPill(
                    title = "L10",
                    value = slot.selection.diffLast10,
                    selected = selected,
                )
                EmbeddedMetricPill(
                    title = "25",
                    value = slot.selection.diff2025,
                    selected = selected,
                )
                EmbeddedMetricPill(
                    title = if (slot.selection.isBestPrice) "NB" else "GAP",
                    value = slot.selection.nextBestProbDiff,
                    selected = selected,
                )
                if (!enabled) {
                    EmbeddedStatusPill(
                        label = "View",
                        selected = selected,
                    )
                }
            }
        }
    }
}

@Composable
private fun BlankSelectionTile(
    label: String,
) {
    Surface(
        modifier = Modifier.width(74.dp),
        shape = RoundedCornerShape(18.dp),
        color = MaterialTheme.colorScheme.surfaceContainer.copy(alpha = 0.9f),
        border = BorderStroke(1.dp, Blue200.copy(alpha = 0.7f)),
    ) {
        Column(
            modifier = Modifier.padding(horizontal = 6.dp, vertical = 7.dp),
            verticalArrangement = Arrangement.spacedBy(5.dp),
        ) {
            Text(
                label,
                style = MaterialTheme.typography.labelSmall,
                color = MaterialTheme.colorScheme.onSurfaceVariant.copy(alpha = 0.65f),
                fontWeight = FontWeight.SemiBold,
            )
            Spacer(modifier = Modifier.height(52.dp))
        }
    }
}

@Composable
private fun EmbeddedMetricPill(
    title: String,
    value: Double?,
    selected: Boolean,
) {
    val background = when {
        value == null && selected -> IceWhite.copy(alpha = 0.18f)
        value == null -> Blue100
        value > 0 -> PositiveSurface
        value < 0 -> NegativeSurface
        else -> NeutralSurface
    }
    val textColor = when {
        value == null && selected -> IceWhite
        value == null -> MaterialTheme.colorScheme.onSurfaceVariant
        value > 0 -> Color(0xFF1B7F46)
        value < 0 -> Color(0xFFB34A35)
        else -> Color(0xFF8E6B10)
    }
    Surface(
        shape = RoundedCornerShape(999.dp),
        color = background,
    ) {
        Row(
            modifier = Modifier.padding(horizontal = 6.dp, vertical = 3.dp),
            horizontalArrangement = Arrangement.spacedBy(4.dp),
            verticalAlignment = Alignment.CenterVertically,
        ) {
            Text(
                title,
                style = MaterialTheme.typography.labelSmall,
                color = textColor,
                fontWeight = FontWeight.SemiBold,
            )
            Text(
                value?.let(::formatSignedDelta) ?: "--",
                style = MaterialTheme.typography.labelSmall,
                color = textColor,
                fontWeight = FontWeight.Bold,
            )
        }
    }
}

@Composable
private fun EmbeddedStatusPill(
    label: String,
    selected: Boolean,
) {
    Surface(
        shape = RoundedCornerShape(999.dp),
        color = if (selected) IceWhite.copy(alpha = 0.18f) else Blue100.copy(alpha = 0.98f),
    ) {
        Text(
            text = label,
            modifier = Modifier.padding(horizontal = 6.dp, vertical = 3.dp),
            style = MaterialTheme.typography.labelSmall,
            color = if (selected) IceWhite else MaterialTheme.colorScheme.onSurfaceVariant,
            fontWeight = FontWeight.SemiBold,
        )
    }
}

@Composable
private fun DraftLegCard(
    leg: DraftLeg,
    onRemove: (Int) -> Unit,
) {
    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appGlassBorder(),
    ) {
        Column(
            modifier = Modifier.padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            Text(leg.label, style = MaterialTheme.typography.titleMedium, color = SgmTitle)
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.SpaceBetween,
            ) {
                Text(leg.selectionType.uppercase(Locale.getDefault()))
                Text(formatDecimalPrice(leg.basePrice))
            }
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.spacedBy(8.dp),
            ) {
                DiffMetricCard(
                    label = "DIFF 2025",
                    value = leg.diff2025,
                    modifier = Modifier.weight(1f),
                )
                DiffMetricCard(
                    label = "DIFF L10",
                    value = leg.diffLast10,
                    modifier = Modifier.weight(1f),
                )
                DiffMetricCard(
                    label = if (leg.isBestPrice) "NEXT BEST" else "BEST GAP",
                    value = leg.nextBestProbDiff,
                    modifier = Modifier.weight(1f),
                )
            }
            Button(
                onClick = { onRemove(leg.selectionId) },
                modifier = Modifier.fillMaxWidth(),
                colors = ButtonDefaults.buttonColors(
                    containerColor = SgmAccentSoft,
                    contentColor = SgmAccent,
                ),
            ) {
                Icon(Icons.Outlined.Delete, contentDescription = null)
                Text("Remove leg", modifier = Modifier.padding(start = 8.dp))
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
        colors = appCardColors(),
        border = appGlassBorder(),
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
                        color = if (rank == 1) SgmAccent else SgmTitle,
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
                    style = MaterialTheme.typography.headlineSmall,
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
                    Text(formatDecimalPrice(leg.basePrice), fontWeight = FontWeight.SemiBold)
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
private fun DiffMetricCard(
    label: String,
    value: Double?,
    modifier: Modifier = Modifier,
) {
    val tone = when {
        value == null -> MaterialTheme.colorScheme.onSurface
        value > 0 -> androidx.compose.ui.graphics.Color(0xFF1B7F46)
        value < 0 -> MaterialTheme.colorScheme.error
        else -> MaterialTheme.colorScheme.onSurface
    }
    Column(
        modifier = modifier.padding(top = 2.dp),
        verticalArrangement = Arrangement.spacedBy(2.dp),
    ) {
        Text(
            label,
            style = MaterialTheme.typography.labelSmall,
            color = MaterialTheme.colorScheme.onSurfaceVariant,
        )
        Text(
            value?.let(::formatSignedDelta) ?: "-",
            style = MaterialTheme.typography.bodyMedium,
            fontWeight = FontWeight.SemiBold,
            color = tone,
        )
    }
}

@Composable
private fun SummaryMetricCard(
    label: String,
    value: String,
    modifier: Modifier = Modifier,
) {
    Column(
        modifier = modifier.padding(top = 2.dp),
        verticalArrangement = Arrangement.spacedBy(2.dp),
    ) {
        Text(
            label,
            style = MaterialTheme.typography.labelSmall,
            color = MaterialTheme.colorScheme.onSurfaceVariant,
        )
        Text(
            value,
            style = MaterialTheme.typography.bodyMedium,
            fontWeight = FontWeight.SemiBold,
            color = MaterialTheme.colorScheme.onSurface,
        )
    }
}

private fun SelectionMetricFilters.isDefault(): Boolean =
    minPriceText.isBlank() &&
        maxPriceText.isBlank() &&
        minDiff2025 == OddsDiffSliderMin &&
        maxDiff2025 == OddsDiffSliderMax &&
        minDiffLast10 == OddsDiffSliderMin &&
        maxDiffLast10 == OddsDiffSliderMax &&
        minNextBestProbDiff == OddsDiffSliderMin &&
        maxNextBestProbDiff == OddsDiffSliderMax

private fun formatMetricRange(min: Float, max: Float): String =
    String.format(Locale.getDefault(), "%+.2f to %+.2f", min, max)

private fun formatPriceRange(minText: String, maxText: String): String =
    when {
        minText.isBlank() && maxText.isBlank() -> "Any"
        minText.isBlank() -> "<= $maxText"
        maxText.isBlank() -> ">= $minText"
        else -> "$minText-$maxText"
    }

private fun orderedMarketCodes(legs: List<OddsSearchResult>): List<String> {
    val preferredOrder = listOf(
        "player_disposals",
        "player_fantasy_points",
        "player_goals",
        "player_marks",
        "player_tackles",
        "player_kicks",
        "player_handballs",
        "player_hitouts",
        "player_clearances",
        "total_points",
        "line",
        "h2h",
    )
    val orderIndex = preferredOrder.withIndex().associate { it.value to it.index }
    return legs.map { it.marketTypeCode }
        .distinct()
        .sortedWith(compareBy({ orderIndex[it] ?: Int.MAX_VALUE }, { marketDisplayLabel(it) }))
}

private fun marketSectionTitle(marketCode: String?): String =
    marketCode?.let {
        if (it == AllMarketCode) {
            "All player props"
        } else {
            "${marketDisplayLabel(it)} options"
        }
    } ?: "Available legs"

private fun marketDisplayLabel(marketTypeCode: String): String =
    when (marketTypeCode) {
        AllMarketCode -> "All"
        "player_disposals" -> "Disposals"
        "player_fantasy_points" -> "Fantasy"
        "player_goals" -> "Goals"
        "player_marks" -> "Marks"
        "player_tackles" -> "Tackles"
        "player_kicks" -> "Kicks"
        "player_handballs" -> "Handballs"
        "player_hitouts" -> "Hitouts"
        "player_clearances" -> "Clearances"
        "total_points" -> "Totals"
        "line" -> "Line"
        "h2h" -> "H2H"
        else -> marketTypeCode.replace("_", " ").replaceFirstChar {
            if (it.isLowerCase()) it.titlecase(Locale.getDefault()) else it.toString()
        }
    }

private fun buildCandidateBoard(legs: List<OddsSearchResult>): List<CandidateBoardGroup> =
    legs.groupBy { boardGroupKey(it) }
        .values
        .map { selections ->
            val first = selections.first()
            CandidateBoardGroup(
                key = boardGroupKey(first),
                title = first.player?.fullName ?: marketDisplayLabel(first.marketTypeCode),
                subtitle = buildBoardSubtitle(first),
                columns = buildLineColumns(selections, first.marketTypeCode),
            )
        }
        .sortedWith(
            compareBy<CandidateBoardGroup> { boardGroupSortBucket(it.key) }
                .thenByDescending { group ->
                    group.columns.maxOfOrNull { column ->
                        column.slots.maxOfOrNull { slot -> slot.selection.nextBestProbDiff ?: Double.NEGATIVE_INFINITY }
                            ?: Double.NEGATIVE_INFINITY
                    } ?: Double.NEGATIVE_INFINITY
                }
                .thenBy { it.title },
        )

private fun boardGroupKey(selection: OddsSearchResult): String =
    if (selection.player != null) {
        "${selection.marketTypeCode}|player|${selection.player.id}"
    } else {
        "${selection.marketTypeCode}|match"
    }

private fun boardGroupSortBucket(key: String): Int =
    if ("|player|" in key) 0 else 1

private fun buildBoardSubtitle(selection: OddsSearchResult): String =
    if (selection.player != null) {
        "${marketDisplayLabel(selection.marketTypeCode)} • ${selection.matchName}"
    } else {
        selection.matchName
    }

private fun buildRowSubtitle(selection: OddsSearchResult): String =
    if (selection.marketTypeCode == AllMarketCode) {
        selection.matchName
    } else {
        "${marketDisplayLabel(selection.marketTypeCode)}\n${selection.matchName}"
    }

private fun buildLineColumns(
    selections: List<OddsSearchResult>,
    marketTypeCode: String,
): List<CandidateLineColumn> =
    selections.groupBy { lineColumnKey(it) }
        .values
        .sortedWith(compareBy({ lineColumnSortValue(it.first()) }, { lineColumnLabel(it.first(), marketTypeCode) }))
        .map { columnSelections ->
            val first = columnSelections.first()
            CandidateLineColumn(
                key = lineColumnKey(first),
                label = lineColumnLabel(first, marketTypeCode),
                slots = columnSelections
                    .sortedBy { selectionSlotSortOrder(it.selectionType) }
                    .map { selection ->
                        CandidateSelectionSlot(
                            badge = selectionBadge(selection),
                            selection = selection,
                        )
                    },
            )
        }

private fun lineColumnKey(selection: OddsSearchResult): String =
    when (selection.marketTypeCode) {
        "h2h" -> "win"
        "line" -> selection.marketId.toString()
        else -> selection.lineValue?.toString() ?: selection.marketId.toString()
    }

private fun lineColumnLabel(selection: OddsSearchResult, marketTypeCode: String): String =
    when (marketTypeCode) {
        "h2h" -> "Win"
        "line" -> selection.lineValue?.let(::formatLineValue)?.let { "$it line" } ?: "Line"
        "total_points" -> selection.lineValue?.let(::formatLineValue)?.let { "$it pts" } ?: "Points"
        else -> selection.lineValue?.let(::formatLineValue) ?: "Line"
    }

private fun lineColumnSortValue(selection: OddsSearchResult): Double =
    selection.lineValue ?: Double.MAX_VALUE

private fun selectionSlotSortOrder(selectionType: String): Int =
    when (selectionType) {
        "over", "home" -> 0
        "under", "away" -> 1
        else -> 99
    }

private fun sortCandidateRows(
    legs: List<OddsSearchResult>,
    sortField: BuilderSortField,
    descending: Boolean,
): List<OddsSearchResult> =
    legs.sortedWith { left, right ->
        val primaryResult = when (sortField) {
            BuilderSortField.PLAYER -> compareValues(left.player?.fullName ?: left.label, right.player?.fullName ?: right.label)
            BuilderSortField.LINE -> compareValues(left.lineValue ?: Double.MAX_VALUE, right.lineValue ?: Double.MAX_VALUE)
            BuilderSortField.TYPE -> compareValues(selectionTypeLabel(left.selectionType), selectionTypeLabel(right.selectionType))
            BuilderSortField.NEXT_BEST -> compareValues(left.nextBestProbDiff ?: Double.NEGATIVE_INFINITY, right.nextBestProbDiff ?: Double.NEGATIVE_INFINITY)
            BuilderSortField.PRICE -> compareValues(left.decimalPrice ?: Double.NEGATIVE_INFINITY, right.decimalPrice ?: Double.NEGATIVE_INFINITY)
            BuilderSortField.DIFF_LAST_10 -> compareValues(left.diffLast10 ?: Double.NEGATIVE_INFINITY, right.diffLast10 ?: Double.NEGATIVE_INFINITY)
            BuilderSortField.DIFF_2025 -> compareValues(left.diff2025 ?: Double.NEGATIVE_INFINITY, right.diff2025 ?: Double.NEGATIVE_INFINITY)
        }
        val signedResult = if (descending) -primaryResult else primaryResult
        if (signedResult != 0) {
            signedResult
        } else {
            val byPlayer = compareValues(left.player?.fullName ?: left.label, right.player?.fullName ?: right.label)
            if (byPlayer != 0) {
                byPlayer
            } else {
                val byLine = compareValues(left.lineValue ?: Double.MAX_VALUE, right.lineValue ?: Double.MAX_VALUE)
                if (byLine != 0) {
                    byLine
                } else {
                    compareValues(selectionTypeLabel(left.selectionType), selectionTypeLabel(right.selectionType))
                }
            }
        }
    }

private fun defaultSortDirectionForField(field: BuilderSortField): Boolean =
    when (field) {
        BuilderSortField.PLAYER,
        BuilderSortField.LINE,
        BuilderSortField.TYPE -> false
        BuilderSortField.NEXT_BEST,
        BuilderSortField.PRICE,
        BuilderSortField.DIFF_LAST_10,
        BuilderSortField.DIFF_2025 -> true
    }

private fun isSelectionPriceable(selection: OddsSearchResult): Boolean =
    selection.sgmEligible && selection.decimalPrice != null

private fun compactTileLabel(columnLabel: String, selectionType: String): String =
    when (selectionType) {
        "over" -> "$columnLabel+"
        "under" -> "$columnLabel-"
        else -> columnLabel
    }

private fun selectionBadge(selection: OddsSearchResult): String =
    when (selection.selectionType) {
        "over" -> "Over"
        "under" -> "Under"
        "home" -> selection.label.substringBeforeLast(" ").ifBlank { "Home" }
        "away" -> selection.label.substringBeforeLast(" ").ifBlank { "Away" }
        else -> selection.selectionType.replaceFirstChar {
            if (it.isLowerCase()) it.titlecase(Locale.getDefault()) else it.toString()
        }
    }

private fun bookmakerLabel(bookmakerCode: String): String =
    bookmakerCode.replaceFirstChar { if (it.isLowerCase()) it.titlecase(Locale.getDefault()) else it.toString() }

private fun BuilderSortField.label(): String =
    when (this) {
        BuilderSortField.PLAYER -> "Player"
        BuilderSortField.LINE -> "Line"
        BuilderSortField.TYPE -> "Type"
        BuilderSortField.NEXT_BEST -> "NB"
        BuilderSortField.PRICE -> "Price"
        BuilderSortField.DIFF_LAST_10 -> "L10"
        BuilderSortField.DIFF_2025 -> "25"
    }

private fun selectionTypeLabel(selectionType: String): String =
    when (selectionType) {
        "over" -> "Over"
        "under" -> "Under"
        "home" -> "Home"
        "away" -> "Away"
        else -> selectionType.replaceFirstChar {
            if (it.isLowerCase()) it.titlecase(Locale.getDefault()) else it.toString()
        }
    }

private fun formatLineValue(value: Double?): String =
    if (value == null) {
        "-"
    } else if (value % 1.0 == 0.0) {
        String.format(Locale.getDefault(), "%.0f", value)
    } else {
        String.format(Locale.getDefault(), "%.1f", value)
    }

private fun formatSignedDelta(value: Double): String =
    String.format(Locale.getDefault(), "%+.2f", value)
