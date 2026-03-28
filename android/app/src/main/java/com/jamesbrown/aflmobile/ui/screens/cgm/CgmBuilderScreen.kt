package com.jamesbrown.aflmobile.ui.screens.cgm

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
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Delete
import androidx.compose.material.icons.outlined.FilterList
import androidx.compose.material.icons.outlined.MoreVert
import androidx.compose.material.icons.outlined.Refresh
import androidx.compose.material3.Button
import androidx.compose.material3.ButtonDefaults
import androidx.compose.material3.BottomSheetScaffold
import androidx.compose.material3.Card
import androidx.compose.material3.DropdownMenu
import androidx.compose.material3.DropdownMenuItem
import androidx.compose.material3.Checkbox
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
import com.jamesbrown.aflmobile.model.BookmakerSummary
import com.jamesbrown.aflmobile.model.BuilderDisplayMode
import com.jamesbrown.aflmobile.model.BuilderSortField
import com.jamesbrown.aflmobile.model.CgmAgencyComparison
import com.jamesbrown.aflmobile.model.DraftLeg
import com.jamesbrown.aflmobile.model.EventSummary
import com.jamesbrown.aflmobile.model.OddsDiffSliderMax
import com.jamesbrown.aflmobile.model.OddsDiffSliderMin
import com.jamesbrown.aflmobile.model.OddsSearchResult
import com.jamesbrown.aflmobile.model.SelectionMetricFilters
import com.jamesbrown.aflmobile.ui.common.EmptyCard
import com.jamesbrown.aflmobile.ui.common.ErrorCard
import com.jamesbrown.aflmobile.ui.common.InlineChip
import com.jamesbrown.aflmobile.ui.common.LoadingCard
import com.jamesbrown.aflmobile.ui.common.BuilderDisplayModeSegmented
import com.jamesbrown.aflmobile.ui.common.BuilderSupportText
import com.jamesbrown.aflmobile.ui.common.DataStatusNavigationIcons
import com.jamesbrown.aflmobile.ui.common.PlayerContextTags
import com.jamesbrown.aflmobile.ui.common.ScreenPadding
import com.jamesbrown.aflmobile.ui.common.SelectionMetricFilterSheet
import com.jamesbrown.aflmobile.ui.common.WeatherContextTags
import com.jamesbrown.aflmobile.ui.common.shortAflMatchLabel
import com.jamesbrown.aflmobile.ui.common.formatDecimalPrice
import com.jamesbrown.aflmobile.ui.common.simpleViewModelFactory
import com.jamesbrown.aflmobile.ui.navigation.PlayerLaunchRequest
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
    val playerPosition: String?,
    val matchupDifficulty: String?,
    val weather: com.jamesbrown.aflmobile.model.WeatherSummary?,
    val columns: List<CandidateLineColumn>,
)

private data class CandidateLineColumn(
    val key: String,
    val label: String,
    val slots: List<CandidateSelectionSlot>,
)

private data class CandidateSelectionSlot(
    val selection: OddsSearchResult,
)

private const val AllMarketCode = "__all__"

private val CgmAccent = Orange700
private val CgmAccentSoft = Orange100
private val CgmAccentBorder = Orange300
private val CgmMutedSurface = Blue50
private val CgmTitle = Blue700
private val DraftSheetPeekHeight = 76.dp

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

    init {
        refresh()
    }

    fun refresh() {
        viewModelScope.launch {
            _uiState.update { it.copy(isLoadingOptions = true, errorMessage = null, infoMessage = null) }
            runCatching {
                repository.bookmakers()
            }.onSuccess { bookmakers ->
                val selectedBookmaker = uiState.value.selectedBookmaker
                    ?.takeIf { selected -> bookmakers.any { it.code == selected && it.enabled } }
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
                        errorMessage = error.message ?: "Failed to load agencies.",
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
                infoMessage = null,
            )
        }
        viewModelScope.launch {
            loadBookmakerData(bookmakerCode)
        }
    }

    fun toggleEventSelection(eventId: Int) {
        _uiState.update { current ->
            val updatedEventIds = current.selectedEventIds.toMutableSet().also { selected ->
                if (selected.isEmpty()) {
                    selected += eventId
                } else if (!selected.add(eventId)) {
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
    }

    fun clearEventSelection() {
        _uiState.update {
            it.copy(
                selectedEventIds = emptySet(),
                comparisonResults = emptyList(),
                infoMessage = "Showing all matches.",
            )
        }
    }

    fun setBestOnly(bestOnly: Boolean) {
        _uiState.update {
            it.copy(
                bestOnly = bestOnly,
                isLoadingOptions = it.selectedBookmaker != null,
                comparisonResults = emptyList(),
                errorMessage = null,
                infoMessage = null,
            )
        }
        val selectedBookmaker = uiState.value.selectedBookmaker ?: return
        viewModelScope.launch {
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
        viewModelScope.launch {
            loadBookmakerData(selectedBookmaker)
        }
    }

    fun toggleLeg(leg: OddsSearchResult) {
        val decimalPrice = leg.decimalPrice
        if (decimalPrice == null) {
            _uiState.update { it.copy(errorMessage = "That leg does not have a current price.", infoMessage = null) }
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
                    infoMessage = null,
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
            _uiState.update { it.copy(errorMessage = "Choose at least two legs before comparing.", infoMessage = null) }
            return
        }
        viewModelScope.launch {
            _uiState.update { it.copy(isComparing = true, errorMessage = null, infoMessage = null) }
            runCatching {
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
                        errorMessage = error.message ?: "Failed to compare CGM prices.",
                    )
                }
            }
        }
    }

    private suspend fun loadBookmakerData(bookmakerCode: String) {
        val currentBestOnly = uiState.value.bestOnly
        val metricFilters = uiState.value.metricFilters
        val previousEventIds = uiState.value.selectedEventIds
        runCatching {
            val events = repository.events(bookmaker = bookmakerCode, query = null)
            val odds = repository.odds(
                bookmakers = listOf(bookmakerCode),
                scope = "player",
                query = null,
                marketType = null,
                eventId = null,
                includePlayerIds = emptyList(),
                excludePlayerIds = emptyList(),
                sortBy = "next_best_prob_diff",
                sortDirection = "desc",
                selectionType = null,
                matchupDifficulties = metricFilters.matchupDifficulties,
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
                bestOnly = currentBestOnly,
                limit = 5000,
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
                    errorMessage = error.message ?: "Failed to load CGM legs.",
                )
            }
        }
    }
}

@Composable
fun CgmBuilderRoute(
    repository: AflRepository,
    onOpenPlayerRequest: (PlayerLaunchRequest) -> Unit,
    onOpenNavigation: () -> Unit,
) {
    val viewModel: CgmBuilderViewModel = viewModel(
        factory = simpleViewModelFactory { CgmBuilderViewModel(repository) },
    )
    val uiState by viewModel.uiState.collectAsStateWithLifecycle()
    CgmBuilderScreen(
        repository = repository,
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
        onOpenPlayerRequest = onOpenPlayerRequest,
        onOpenNavigation = onOpenNavigation,
    )
}

@OptIn(ExperimentalMaterial3Api::class, ExperimentalFoundationApi::class)
@Composable
private fun CgmBuilderScreen(
    repository: AflRepository,
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
    onOpenPlayerRequest: (PlayerLaunchRequest) -> Unit,
    onOpenNavigation: () -> Unit,
) {
    val selectedEventIds = uiState.selectedEventIds
    val selectedSelectionIds = remember(uiState.selectedLegs) { uiState.selectedLegs.map { it.selectionId }.toSet() }
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
    val content: @Composable (PaddingValues) -> Unit = { innerPadding ->
        LazyColumn(
            modifier = Modifier
                .fillMaxSize()
                .padding(innerPadding),
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
                    onSelectBookmaker = onSelectBookmaker,
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

            if (uiState.isLoadingOptions) {
                item { LoadingCard("Loading CGM legs") }
            }

            uiState.errorMessage?.let { message ->
                item { ErrorCard(message) }
            }

            uiState.infoMessage?.let { message ->
                item { EmptyCard("CGM status", message) }
            }

            if (!uiState.isLoadingOptions && visibleLegs.isEmpty()) {
                item {
                    EmptyCard(
                        title = "No player props",
                        body = "No player props match the current source agency, match selection, best-price filter, and one-leg-per-game rule.",
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
                            enabled = leg.decimalPrice != null,
                            accent = CgmAccent,
                            accentBorder = CgmAccentBorder,
                            titleColor = CgmTitle,
                            onOpenPlayerRequest = onOpenPlayerRequest,
                            onToggleLeg = onToggleLeg,
                        )
                    }
                } else {
                    items(groupedLegs, key = { it.key }) { group ->
                        CandidateBoardCard(
                            group = group,
                            selectedSelectionIds = selectedSelectionIds,
                            onOpenPlayerRequest = onOpenPlayerRequest,
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
    }

    BottomSheetScaffold(
        scaffoldState = scaffoldState,
        sheetPeekHeight = if (uiState.selectedLegs.isNotEmpty()) DraftSheetPeekHeight + navigationBottomInset else 0.dp,
        sheetContainerColor = MaterialTheme.colorScheme.surfaceContainerLow.copy(alpha = 0.98f),
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
                        onCompare()
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
            CgmTopBar(
                hasDraft = uiState.selectedLegs.isNotEmpty(),
                onRefresh = onRefresh,
                onClearDraft = onClearDraft,
                repository = repository,
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
private fun CgmTopBar(
    repository: AflRepository,
    hasDraft: Boolean,
    onRefresh: () -> Unit,
    onClearDraft: () -> Unit,
    onOpenNavigation: () -> Unit,
    onOpenFilters: () -> Unit,
    onOpenOptions: () -> Unit,
) {
    TopAppBar(
        title = { Text("CGM") },
        colors = appTopBarColors(),
        navigationIcon = {
            DataStatusNavigationIcons(repository = repository, onOpenNavigation = onOpenNavigation)
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
        colors = appCardColors(),
        border = appGlassBorder(),
    ) {
        Column(
            modifier = Modifier.padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            Text("Agency and matches", style = MaterialTheme.typography.titleMedium, fontWeight = FontWeight.SemiBold)
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
            Column(verticalArrangement = Arrangement.spacedBy(8.dp)) {
                Text("Display mode", fontWeight = FontWeight.Medium)
                BuilderDisplayModeSegmented(
                    displayMode = displayMode,
                    onDisplayModeChanged = onDisplayModeChanged,
                    selectedContainerColor = CgmAccent,
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
private fun CgmDraftSheet(
    selectedBookmaker: String?,
    selectedLegs: List<DraftLeg>,
    comparisonResults: List<CgmAgencyComparison>,
    isComparing: Boolean,
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
            val distinctGames = selectedLegs.map { it.eventId }.distinct().size
            DraftPeekBar(
                count = selectedLegs.size,
                primaryLabel = "${selectedLegs.size} leg${if (selectedLegs.size == 1) "" else "s"} selected",
                secondaryLabel = "$distinctGames game${if (distinctGames == 1) "" else "s"} • ${selectedBookmaker?.let(::bookmakerLabel) ?: "Tap to review draft"}",
                accent = CgmAccent,
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
                        Text("Current selections", style = MaterialTheme.typography.titleMedium, color = CgmTitle, fontWeight = FontWeight.SemiBold)
                        Text(
                            "${selectedLegs.size} leg${if (selectedLegs.size == 1) "" else "s"} across ${selectedLegs.map { it.eventId }.distinct().size} game${if (selectedLegs.map { it.eventId }.distinct().size == 1) "" else "s"}",
                            style = MaterialTheme.typography.bodyMedium,
                            color = MaterialTheme.colorScheme.onSurface,
                        )
                        Text(
                            "Source agency: ${selectedBookmaker?.let(::bookmakerLabel) ?: "Not selected"}",
                            style = MaterialTheme.typography.bodySmall,
                            color = MaterialTheme.colorScheme.onSurfaceVariant,
                        )
                    }
                    if (selectedLegs.isNotEmpty()) {
                        TextButton(onClick = onClearDraft) {
                            Text("Clear")
                        }
                    }
                }
                Button(
                    onClick = onCompare,
                    modifier = Modifier.fillMaxWidth(),
                    enabled = selectedLegs.size >= 2 && !isComparing,
                    colors = ButtonDefaults.buttonColors(
                        containerColor = CgmAccent,
                        contentColor = Color.White,
                        disabledContainerColor = CgmAccent.copy(alpha = 0.35f),
                        disabledContentColor = Color.White.copy(alpha = 0.7f),
                    ),
                ) {
                    Text("Compare agencies")
                }
            }
        }

        if (isComparing) {
            item { LoadingCard("Comparing agency prices") }
        }

        items(selectedLegs, key = { it.selectionId }) { leg ->
            CgmDraftLegCard(leg = leg, onRemove = onRemoveLeg)
        }

        if (comparisonResults.isNotEmpty()) {
            item {
                Text(
                    "Agency comparison",
                    style = MaterialTheme.typography.titleMedium,
                    fontWeight = FontWeight.SemiBold,
                    color = CgmTitle,
                )
            }
            items(comparisonResults, key = { it.bookmaker }) { result ->
                CgmComparisonCard(
                    result = result,
                    rank = comparisonResults.indexOfFirst { it.bookmaker == result.bookmaker } + 1,
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
                        color = CgmTitle,
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
                    containerColor = Blue100,
                    labelColor = Blue700,
                    selectedContainerColor = CgmAccent,
                    selectedLabelColor = IceWhite,
                ),
                border = FilterChipDefaults.filterChipBorder(
                    enabled = true,
                    selected = marketCode == selectedMarketCode,
                    borderColor = Blue200,
                    selectedBorderColor = CgmAccent,
                ),
            )
        }
    }
}

@Composable
private fun CandidateBoardCard(
    group: CandidateBoardGroup,
    selectedSelectionIds: Set<Int>,
    onOpenPlayerRequest: (PlayerLaunchRequest) -> Unit,
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
            Text(
                text = group.title,
                modifier = Modifier.clickable(enabled = group.toPlayerLaunchRequest() != null) {
                    group.toPlayerLaunchRequest()?.let(onOpenPlayerRequest)
                },
                style = MaterialTheme.typography.titleMedium,
                color = CgmTitle,
                fontWeight = FontWeight.SemiBold,
            )
            PlayerContextTags(
                position = group.playerPosition,
                matchupDifficulty = group.matchupDifficulty,
            )
            WeatherContextTags(weather = group.weather)
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
            modifier = Modifier.padding(horizontal = 14.dp, vertical = 10.dp),
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            SortableHeaderCell("Player", BuilderSortField.PLAYER, sortField, descending, Modifier.weight(3.8f), Alignment.Start, onSortSelected)
            SortableHeaderCell("Line", BuilderSortField.LINE, sortField, descending, Modifier.weight(0.9f), Alignment.End, onSortSelected)
            SortableHeaderCell("Type", BuilderSortField.TYPE, sortField, descending, Modifier.weight(1.1f), Alignment.Start, onSortSelected)
            SortableHeaderCell("Price", BuilderSortField.PRICE, sortField, descending, Modifier.weight(0.95f), Alignment.End, onSortSelected)
            SortableHeaderCell("L10", BuilderSortField.DIFF_LAST_10, sortField, descending, Modifier.weight(1.0f), Alignment.End, onSortSelected)
            SortableHeaderCell("25", BuilderSortField.DIFF_2025, sortField, descending, Modifier.weight(1.0f), Alignment.End, onSortSelected)
            SortableHeaderCell("NB", BuilderSortField.NEXT_BEST, sortField, descending, Modifier.weight(1.0f), Alignment.End, onSortSelected)
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
    onOpenPlayerRequest: (PlayerLaunchRequest) -> Unit,
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
            modifier = Modifier.padding(horizontal = 14.dp, vertical = 10.dp),
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            Column(
                modifier = Modifier.weight(3.8f),
                verticalArrangement = Arrangement.spacedBy(2.dp),
            ) {
                Text(
                    text = selection.player?.fullName ?: selection.label,
                    modifier = Modifier.clickable(enabled = selection.player != null) {
                        selection.toPlayerLaunchRequest()?.let(onOpenPlayerRequest)
                    },
                    style = MaterialTheme.typography.bodySmall,
                    fontWeight = FontWeight.SemiBold,
                    color = if (selected) IceWhite else titleColor,
                    maxLines = 1,
                )
                PlayerContextTags(
                    position = selection.playerPosition,
                    matchupDifficulty = selection.matchupDifficulty,
                )
                WeatherContextTags(weather = selection.weather)
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
            MetricCell(selection.diffLast10?.let(::formatSignedDelta) ?: "--", Modifier.weight(1.0f), selected, value = selection.diffLast10)
            MetricCell(selection.diff2025?.let(::formatSignedDelta) ?: "--", Modifier.weight(1.0f), selected, value = selection.diff2025)
            MetricCell(selection.nextBestProbDiff?.let(::formatSignedDelta) ?: "--", Modifier.weight(1.0f), selected, value = selection.nextBestProbDiff)
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
            color = CgmAccent,
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
                    enabled = slot?.selection?.decimalPrice != null,
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
            .width(82.dp)
            .clickable(enabled = enabled, onClick = onClick),
        shape = RoundedCornerShape(18.dp),
        color = when {
            selected -> CgmAccent
            enabled -> IceWhite
            else -> CgmMutedSurface
        },
        tonalElevation = if (selected) 3.dp else 0.dp,
        border = BorderStroke(
            width = 1.dp,
            color = when {
                selected -> CgmAccent
                enabled -> Blue200
                else -> Blue200.copy(alpha = 0.7f)
            },
        ),
    ) {
        Column(
            modifier = Modifier.padding(horizontal = 5.dp, vertical = 7.dp),
            verticalArrangement = Arrangement.spacedBy(5.dp),
        ) {
            Text(
                label,
                style = MaterialTheme.typography.labelSmall,
                color = when {
                    selected -> IceWhite.copy(alpha = 0.94f)
                    enabled -> CgmTitle
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
                EmbeddedMetricPill(title = "L10", value = slot.selection.diffLast10, selected = selected)
                EmbeddedMetricPill(title = "25", value = slot.selection.diff2025, selected = selected)
                EmbeddedMetricPill(
                    title = if (slot.selection.isBestPrice) "NB" else "GAP",
                    value = slot.selection.nextBestProbDiff,
                    selected = selected,
                )
            }
        }
    }
}

@Composable
private fun BlankSelectionTile(
    label: String,
) {
    Surface(
        modifier = Modifier.width(82.dp),
        shape = RoundedCornerShape(18.dp),
        color = MaterialTheme.colorScheme.surfaceContainer.copy(alpha = 0.9f),
        border = BorderStroke(1.dp, Blue200.copy(alpha = 0.7f)),
    ) {
        Column(
            modifier = Modifier.padding(horizontal = 5.dp, vertical = 7.dp),
            verticalArrangement = Arrangement.spacedBy(5.dp),
        ) {
            Text(
                label,
                style = MaterialTheme.typography.labelSmall,
                color = MaterialTheme.colorScheme.onSurfaceVariant.copy(alpha = 0.65f),
                fontWeight = FontWeight.SemiBold,
            )
            Spacer(modifier = Modifier.height(48.dp))
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
            modifier = Modifier.padding(horizontal = 5.dp, vertical = 3.dp),
            horizontalArrangement = Arrangement.spacedBy(3.dp),
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
private fun CgmDraftLegCard(
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
            Text(leg.label, style = MaterialTheme.typography.titleMedium, color = CgmTitle)
            Text(leg.eventLabel, style = MaterialTheme.typography.bodySmall)
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
                DiffMetricCard(label = "DIFF 2025", value = leg.diff2025, modifier = Modifier.weight(1f))
                DiffMetricCard(label = "DIFF L10", value = leg.diffLast10, modifier = Modifier.weight(1f))
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
                    containerColor = CgmAccentSoft,
                    contentColor = CgmAccent,
                ),
            ) {
                Icon(Icons.Outlined.Delete, contentDescription = null)
                Text("Remove leg", modifier = Modifier.padding(start = 8.dp))
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
                        color = if (rank == 1) CgmAccent else CgmTitle,
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
                    style = MaterialTheme.typography.headlineSmall,
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
                    Text(formatDecimalPrice(leg.basePrice), fontWeight = FontWeight.SemiBold)
                }
            }
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
        value > 0 -> Color(0xFF1B7F46)
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

private fun SelectionMetricFilters.isDefault(): Boolean =
    matchupDifficulties.isEmpty() &&
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

private fun selectedMatchesLabel(events: List<EventSummary>, selectedEventIds: Set<Int>): String =
    when {
        events.isEmpty() -> "No matches"
        selectedEventIds.isEmpty() || selectedEventIds.size == events.size -> "All remaining matches"
        selectedEventIds.size == 1 -> events.firstOrNull { it.id in selectedEventIds }?.matchName ?: "1 match"
        else -> "${selectedEventIds.size} matches"
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
    )
    val orderIndex = preferredOrder.withIndex().associate { it.value to it.index }
    return legs.map { it.marketTypeCode }
        .distinct()
        .sortedWith(compareBy({ orderIndex[it] ?: Int.MAX_VALUE }, { marketDisplayLabel(it) }))
}

private fun marketSectionTitle(marketCode: String?): String =
    marketCode?.let {
        if (it == AllMarketCode) "All player props" else "${marketDisplayLabel(it)} options"
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
        else -> marketTypeCode.replace("_", " ").replaceFirstChar {
            if (it.isLowerCase()) it.titlecase(Locale.getDefault()) else it.toString()
        }
    }

private fun buildRowSubtitle(selection: OddsSearchResult): String =
    "${marketDisplayLabel(selection.marketTypeCode)}\n${shortMatchLabel(selection.matchName)}"

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

private fun buildCandidateBoard(legs: List<OddsSearchResult>): List<CandidateBoardGroup> =
    legs.groupBy { boardGroupKey(it) }
        .values
        .map { selections ->
            val first = selections.first()
            CandidateBoardGroup(
                key = boardGroupKey(first),
                title = first.player?.fullName ?: marketDisplayLabel(first.marketTypeCode),
                subtitle = "${marketDisplayLabel(first.marketTypeCode)} • ${shortMatchLabel(first.matchName)}",
                playerPosition = first.playerPosition,
                matchupDifficulty = first.matchupDifficulty,
                weather = first.weather,
                columns = buildLineColumns(selections),
            )
        }
        .sortedWith(
            compareByDescending<CandidateBoardGroup> { group ->
                group.columns.maxOfOrNull { column ->
                    column.slots.maxOfOrNull { slot -> slot.selection.nextBestProbDiff ?: Double.NEGATIVE_INFINITY }
                        ?: Double.NEGATIVE_INFINITY
                } ?: Double.NEGATIVE_INFINITY
            }.thenBy { it.title },
        )

private fun boardGroupKey(selection: OddsSearchResult): String =
    "${selection.marketTypeCode}|${selection.player?.id ?: selection.selectionId}"

private fun buildLineColumns(
    selections: List<OddsSearchResult>,
): List<CandidateLineColumn> =
    selections.groupBy { selection -> selection.lineValue?.toString() ?: selection.marketId.toString() }
        .values
        .sortedWith(compareBy({ it.first().lineValue ?: Double.MAX_VALUE }, { it.first().label }))
        .map { columnSelections ->
            val first = columnSelections.first()
            CandidateLineColumn(
                key = first.lineValue?.toString() ?: first.marketId.toString(),
                label = first.lineValue?.let(::formatLineValue) ?: "Line",
                slots = columnSelections
                    .sortedBy { selectionSlotSortOrder(it.selectionType) }
                    .map { selection -> CandidateSelectionSlot(selection = selection) },
            )
        }

private fun selectionSlotSortOrder(selectionType: String): Int =
    when (selectionType) {
        "over" -> 0
        "under" -> 1
        else -> 99
    }

private fun compactTileLabel(columnLabel: String, selectionType: String): String =
    when (selectionType) {
        "over" -> columnLabel
        "under" -> columnLabel
        else -> columnLabel
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

private fun shortMatchLabel(matchName: String): String {
    return shortAflMatchLabel(matchName)
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

private fun CandidateBoardGroup.toPlayerLaunchRequest(): PlayerLaunchRequest? =
    columns.asSequence()
        .flatMap { column -> column.slots.asSequence() }
        .map { slot -> slot.selection }
        .mapNotNull { selection -> selection.toPlayerLaunchRequest() }
        .firstOrNull()

private fun OddsSearchResult.toPlayerLaunchRequest(): PlayerLaunchRequest? {
    val playerSummary = player ?: return null
    return PlayerLaunchRequest(
        requestId = System.nanoTime(),
        playerId = playerSummary.id,
        playerName = playerSummary.fullName,
        marketTypeCode = marketTypeCode,
        lineValue = lineValue,
    )
}
