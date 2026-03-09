package com.jamesbrown.aflmobile.ui.screens.sgm

import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.clickable
import androidx.compose.foundation.horizontalScroll
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.heightIn
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.LazyRow
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Delete
import androidx.compose.material.icons.outlined.Refresh
import androidx.compose.material3.Button
import androidx.compose.material3.ButtonDefaults
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
import androidx.compose.material3.Surface
import androidx.compose.material3.Switch
import androidx.compose.material3.Text
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
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.lifecycle.ViewModel
import androidx.lifecycle.compose.collectAsStateWithLifecycle
import androidx.lifecycle.viewModelScope
import androidx.lifecycle.viewmodel.compose.viewModel
import com.jamesbrown.aflmobile.data.repository.AflRepository
import com.jamesbrown.aflmobile.data.repository.SgmDraftStore
import com.jamesbrown.aflmobile.model.BookmakerSummary
import com.jamesbrown.aflmobile.model.DraftLeg
import com.jamesbrown.aflmobile.model.EventSummary
import com.jamesbrown.aflmobile.model.OddsSearchResult
import com.jamesbrown.aflmobile.model.SgmDraftState
import com.jamesbrown.aflmobile.ui.common.EmptyCard
import com.jamesbrown.aflmobile.ui.common.ErrorCard
import com.jamesbrown.aflmobile.ui.common.LoadingCard
import com.jamesbrown.aflmobile.ui.common.ScreenPadding
import com.jamesbrown.aflmobile.ui.common.formatDateTime
import com.jamesbrown.aflmobile.ui.common.formatDecimalPrice
import com.jamesbrown.aflmobile.ui.common.simpleViewModelFactory
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

private val SgmAccent = Color(0xFFD9791D)
private val SgmAccentSoft = Color(0xFFFFE8D2)
private val SgmAccentSurface = Color(0xFFFFF6ED)
private val SgmAccentBorder = Color(0xFFE7B98A)
private val SgmMutedSurface = Color(0xFFF4F6F7)

data class SgmBuilderUiState(
    val draft: SgmDraftState = SgmDraftState(),
    val bookmakers: List<BookmakerSummary> = emptyList(),
    val events: List<EventSummary> = emptyList(),
    val selectedBookmaker: String? = null,
    val selectedEventId: Int? = null,
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

    fun quote() {
        val draft = uiState.value.draft
        val eventId = uiState.value.selectedEventId ?: draft.eventId
        val bookmaker = uiState.value.selectedBookmaker ?: draft.bookmaker
        if (bookmaker == null || eventId == null || draft.legs.size < 2) {
            _uiState.update {
                it.copy(errorMessage = "Choose one agency, one match, and at least two legs before quoting.")
            }
            return
        }
        if (!isLivePricingEnabled(bookmaker)) {
            _uiState.update {
                it.copy(errorMessage = "${displayBookmaker(bookmaker)} is not enabled for live SGM pricing yet.")
            }
            return
        }
        viewModelScope.launch {
            _uiState.update { it.copy(isLoadingQuote = true, errorMessage = null, infoMessage = null) }
            runCatching {
                repository.quoteSgm(
                    bookmaker = bookmaker,
                    eventId = eventId,
                    selectionIds = draft.legs.map { it.selectionId },
                    forceRefresh = draft.forceRefresh,
                )
            }.onSuccess { quote ->
                draftStore.setQuote(quote)
                _uiState.update { it.copy(isLoadingQuote = false, infoMessage = "Quote updated.") }
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
        runCatching {
            repository.odds(
                bookmakers = listOf(bookmakerCode),
                scope = "all",
                query = null,
                marketType = null,
                eventId = eventId,
                sortBy = "market",
                sortDirection = "asc",
                selectionType = null,
                minEdge = null,
                minPrice = null,
                maxPrice = null,
                sgmOnly = false,
                bestOnly = false,
            )
        }.onSuccess { odds ->
            val playerPropOdds = odds.filter { it.marketTypeCode.startsWith("player_") }
            _uiState.update {
                it.copy(
                    candidateLegs = playerPropOdds,
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

    private fun isLivePricingEnabled(bookmakerCode: String): Boolean =
        uiState.value.bookmakers.firstOrNull { it.code == bookmakerCode }?.livePricingEnabled == true

    private fun displayBookmaker(bookmakerCode: String): String =
        uiState.value.bookmakers.firstOrNull { it.code == bookmakerCode }?.displayName
            ?: bookmakerCode.replaceFirstChar { if (it.isLowerCase()) it.titlecase(Locale.getDefault()) else it.toString() }
}

@Composable
fun SgmBuilderRoute(
    repository: AflRepository,
    draftStore: SgmDraftStore,
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
        onQuote = viewModel::quote,
        onRefresh = viewModel::refresh,
    )
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun SgmBuilderScreen(
    uiState: SgmBuilderUiState,
    onSelectBookmaker: (String) -> Unit,
    onSelectEvent: (Int) -> Unit,
    onToggleLeg: (OddsSearchResult) -> Unit,
    onRemoveLeg: (Int) -> Unit,
    onClearDraft: () -> Unit,
    onForceRefreshChanged: (Boolean) -> Unit,
    onQuote: () -> Unit,
    onRefresh: () -> Unit,
) {
    val draft = uiState.draft
    val selectedBookmaker = uiState.selectedBookmaker
    val selectedEvent = uiState.events.firstOrNull { it.id == uiState.selectedEventId }
    val livePricingEnabled = uiState.bookmakers.firstOrNull { it.code == selectedBookmaker }?.livePricingEnabled == true
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
    val groupedLegs = remember(uiState.candidateLegs, selectedMarketCode) {
        buildCandidateBoard(
            legs = uiState.candidateLegs.filter { leg ->
                selectedMarketCode == null || selectedMarketCode == AllMarketCode || leg.marketTypeCode == selectedMarketCode
            },
        )
    }

    Scaffold(
        topBar = {
            TopAppBar(
                title = { Text("SGM") },
                actions = {
                    IconButton(onClick = onRefresh) {
                        Icon(Icons.Outlined.Refresh, contentDescription = "Refresh")
                    }
                    if (draft.legs.isNotEmpty()) {
                        IconButton(onClick = onClearDraft) {
                            Icon(Icons.Outlined.Delete, contentDescription = "Clear")
                        }
                    }
                },
            )
        },
    ) { innerPadding ->
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
                    livePricingEnabled = livePricingEnabled,
                    forceRefresh = draft.forceRefresh,
                    onSelectBookmaker = onSelectBookmaker,
                    onSelectEvent = onSelectEvent,
                    onForceRefreshChanged = onForceRefreshChanged,
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

            item {
                Card {
                    Column(
                        modifier = Modifier.padding(16.dp),
                        verticalArrangement = Arrangement.spacedBy(10.dp),
                    ) {
                        Text("Current draft", style = MaterialTheme.typography.titleMedium)
                        Text(selectedEvent?.matchName ?: draft.eventLabel ?: "No match selected")
                        Text("Agency: ${selectedBookmaker?.let(::bookmakerLabel) ?: "Not selected"}")
                        Text("${draft.legs.size} leg${if (draft.legs.size == 1) "" else "s"} selected")
                        Button(
                            onClick = onQuote,
                            modifier = Modifier.fillMaxWidth(),
                            enabled = draft.legs.size >= 2 && !uiState.isLoadingQuote && livePricingEnabled,
                            colors = ButtonDefaults.buttonColors(
                                containerColor = SgmAccent,
                                contentColor = Color.White,
                                disabledContainerColor = SgmAccent.copy(alpha = 0.35f),
                                disabledContentColor = Color.White.copy(alpha = 0.7f),
                            ),
                        ) {
                            Icon(Icons.Outlined.Refresh, contentDescription = null)
                            Text("Compare quote", modifier = Modifier.padding(start = 8.dp))
                        }
                        if (!livePricingEnabled && selectedBookmaker != null) {
                            Text(
                                "Live SGM pricing is only enabled for Sportsbet right now.",
                                style = MaterialTheme.typography.bodySmall,
                                color = MaterialTheme.colorScheme.onSurfaceVariant,
                            )
                        }
                    }
                }
            }

            if (uiState.isLoadingQuote) {
                item { LoadingCard("Pricing selected legs") }
            }

            if (draft.legs.isEmpty()) {
                item {
                    EmptyCard(
                        title = "No legs selected",
                        body = "Choose one agency and one match, then tap live-priceable legs below. Read-only tiles are still shown from processed odds.",
                    )
                }
            } else {
                items(draft.legs, key = { it.selectionId }) { leg ->
                    DraftLegCard(leg = leg, onRemove = onRemoveLeg)
                }
            }

            draft.latestQuote?.let { quote ->
                item {
                    Card {
                        Column(
                            modifier = Modifier.padding(16.dp),
                            verticalArrangement = Arrangement.spacedBy(8.dp),
                        ) {
                            Text("Comparison", style = MaterialTheme.typography.titleMedium)
                            Text("Quoted SGM: ${formatDecimalPrice(quote.quotedPrice)}")
                            Text("Local multi: ${formatDecimalPrice(quote.unadjustedPrice)}")
                            Text("Adjustment factor: ${formatDecimalPrice(quote.adjustmentFactor)}")
                            Text("Cached: ${if (quote.fromCache) "yes" else "no"}")
                            Text("Quoted at: ${formatDateTime(quote.quotedAt)}")
                            Text("Expires: ${formatDateTime(quote.expiresAt)}")
                        }
                    }
                }
            }

            if (!uiState.isLoadingOptions && groupedLegs.isEmpty()) {
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
                items(groupedLegs, key = { it.key }) { group ->
                    CandidateBoardCard(
                        group = group,
                        selectedSelectionIds = draft.legs.map { it.selectionId }.toSet(),
                        onToggleLeg = onToggleLeg,
                    )
                }
            }
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun SgmControlCard(
    bookmakers: List<BookmakerSummary>,
    events: List<EventSummary>,
    selectedBookmaker: String?,
    selectedEventId: Int?,
    livePricingEnabled: Boolean,
    forceRefresh: Boolean,
    onSelectBookmaker: (String) -> Unit,
    onSelectEvent: (Int) -> Unit,
    onForceRefreshChanged: (Boolean) -> Unit,
) {
    var bookmakerExpanded by remember { mutableStateOf(false) }
    var eventExpanded by remember { mutableStateOf(false) }

    Card {
        Column(
            modifier = Modifier.padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            Text("Select agency and match", style = MaterialTheme.typography.titleMedium)

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
                    Text("Force refresh")
                    Text(
                        if (livePricingEnabled) {
                            "Bypass the short quote cache."
                        } else {
                            "Only applies when live pricing is available."
                        },
                        style = MaterialTheme.typography.bodySmall,
                        color = MaterialTheme.colorScheme.onSurfaceVariant,
                    )
                }
                Switch(
                    checked = forceRefresh,
                    onCheckedChange = onForceRefreshChanged,
                )
            }
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
                    containerColor = SgmAccentSoft,
                    labelColor = SgmAccent,
                    selectedContainerColor = SgmAccent,
                    selectedLabelColor = Color.White,
                ),
                border = FilterChipDefaults.filterChipBorder(
                    enabled = true,
                    selected = marketCode == selectedMarketCode,
                    borderColor = SgmAccentBorder,
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
        colors = CardDefaults.cardColors(containerColor = Color.White),
        border = BorderStroke(1.dp, SgmAccentBorder.copy(alpha = 0.45f)),
    ) {
        Column(
            modifier = Modifier.padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(10.dp),
        ) {
            Text(group.title, style = MaterialTheme.typography.titleMedium, color = SgmAccent)
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
            .width(82.dp)
            .clickable(enabled = enabled, onClick = onClick),
        shape = RoundedCornerShape(20.dp),
        color = when {
            selected -> SgmAccent
            enabled -> SgmAccentSurface
            else -> SgmMutedSurface
        },
        tonalElevation = if (selected) 3.dp else 0.dp,
        border = BorderStroke(
            width = 1.dp,
            color = when {
                selected -> SgmAccent
                enabled -> SgmAccentBorder
                else -> SgmAccentBorder.copy(alpha = 0.55f)
            },
        ),
    ) {
        Column(
            modifier = Modifier.padding(horizontal = 8.dp, vertical = 8.dp),
            verticalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            Text(
                label,
                style = MaterialTheme.typography.labelMedium,
                color = when {
                    selected -> Color.White.copy(alpha = 0.92f)
                    enabled -> SgmAccent
                    else -> MaterialTheme.colorScheme.onSurfaceVariant
                },
                fontWeight = FontWeight.SemiBold,
            )
            Text(
                formatDecimalPrice(slot.selection.decimalPrice),
                style = MaterialTheme.typography.headlineSmall,
                color = when {
                    selected -> Color.White
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
                    title = "2025",
                    value = slot.selection.diff2025,
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
        modifier = Modifier.width(82.dp),
        shape = RoundedCornerShape(20.dp),
        color = Color(0xFFF7F2ED),
        border = BorderStroke(1.dp, SgmAccentBorder.copy(alpha = 0.35f)),
    ) {
        Column(
            modifier = Modifier.padding(horizontal = 8.dp, vertical = 8.dp),
            verticalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            Text(
                label,
                style = MaterialTheme.typography.labelMedium,
                color = MaterialTheme.colorScheme.onSurfaceVariant.copy(alpha = 0.65f),
                fontWeight = FontWeight.SemiBold,
            )
            Spacer(modifier = Modifier.height(58.dp))
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
        value == null && selected -> Color.White.copy(alpha = 0.18f)
        value == null -> SgmMutedSurface
        value > 0 -> Color(0xFFD6F0DE)
        value < 0 -> Color(0xFFF7DBD7)
        else -> Color(0xFFF2E7CB)
    }
    val textColor = when {
        value == null && selected -> Color.White
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
            modifier = Modifier.padding(horizontal = 8.dp, vertical = 4.dp),
            horizontalArrangement = Arrangement.spacedBy(6.dp),
            verticalAlignment = Alignment.CenterVertically,
        ) {
            Text(
                title,
                style = MaterialTheme.typography.labelSmall,
                color = textColor,
                fontWeight = FontWeight.SemiBold,
            )
            Text(
                value?.let(::formatSignedDelta) ?: "-",
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
        color = if (selected) Color.White.copy(alpha = 0.18f) else Color(0xFFE7EBEE),
    ) {
        Text(
            text = label,
            modifier = Modifier.padding(horizontal = 8.dp, vertical = 4.dp),
            style = MaterialTheme.typography.labelSmall,
            color = if (selected) Color.White else MaterialTheme.colorScheme.onSurfaceVariant,
            fontWeight = FontWeight.SemiBold,
        )
    }
}

@Composable
private fun DraftLegCard(
    leg: DraftLeg,
    onRemove: (Int) -> Unit,
) {
    Card(modifier = Modifier.fillMaxWidth()) {
        Column(
            modifier = Modifier.padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            Text(leg.label, style = MaterialTheme.typography.titleMedium, color = SgmAccent)
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
        .sortedWith(compareBy({ boardGroupSortBucket(it.key) }, { it.title }))

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

private fun formatLineValue(value: Double): String =
    if (value % 1.0 == 0.0) {
        String.format(Locale.getDefault(), "%.0f", value)
    } else {
        String.format(Locale.getDefault(), "%.1f", value)
    }

private fun formatSignedDelta(value: Double): String =
    String.format(Locale.getDefault(), "%+.2f", value)
