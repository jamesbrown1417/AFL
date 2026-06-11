package com.jamesbrown.aflmobile.ui.common.builder

import androidx.compose.animation.animateColorAsState
import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.ExperimentalFoundationApi
import androidx.compose.foundation.ScrollState
import androidx.compose.foundation.clickable
import androidx.compose.foundation.combinedClickable
import androidx.compose.foundation.horizontalScroll
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.heightIn
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.rememberScrollState
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.automirrored.outlined.HelpOutline
import androidx.compose.material.icons.filled.ArrowDropDown
import androidx.compose.material.icons.outlined.Delete
import androidx.compose.material.icons.outlined.KeyboardArrowUp
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.HorizontalDivider
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.ModalBottomSheet
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.rotate
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.hapticfeedback.HapticFeedbackType
import androidx.compose.ui.platform.LocalHapticFeedback
import androidx.compose.ui.semantics.heading
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import com.jamesbrown.aflmobile.model.BuilderSortField
import com.jamesbrown.aflmobile.model.DraftLeg
import com.jamesbrown.aflmobile.model.OddsSearchResult
import com.jamesbrown.aflmobile.ui.common.PlayerContextTags
import com.jamesbrown.aflmobile.ui.common.WeatherContextTags
import com.jamesbrown.aflmobile.ui.common.formatDecimalPrice
import com.jamesbrown.aflmobile.ui.common.formatSignedDelta
import com.jamesbrown.aflmobile.ui.navigation.PlayerLaunchRequest
import com.jamesbrown.aflmobile.ui.theme.AppTheme
import com.jamesbrown.aflmobile.ui.theme.tabular


/**
 * Shared building blocks for the SGM and CGM builders. Selection accent is
 * always the theme tertiary color; metric tones come from [AppTheme.colors].
 */

@Composable
fun CandidateRowHeader(
    sortField: BuilderSortField,
    descending: Boolean,
    onSortSelected: (BuilderSortField) -> Unit,
) {
    Surface(
        modifier = Modifier.fillMaxWidth(),
        shape = MaterialTheme.shapes.medium,
        color = MaterialTheme.colorScheme.surfaceContainerHigh,
        border = BorderStroke(1.dp, MaterialTheme.colorScheme.outlineVariant),
    ) {
        Row(
            modifier = Modifier.padding(horizontal = 14.dp, vertical = 6.dp),
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            SortableHeaderCell("Player", BuilderSortField.PLAYER, sortField, descending, Modifier.weight(3.4f), Alignment.Start, onSortSelected)
            SortableHeaderCell("Line", BuilderSortField.LINE, sortField, descending, Modifier.weight(1.2f), Alignment.End, onSortSelected)
            SortableHeaderCell("Price", BuilderSortField.PRICE, sortField, descending, Modifier.weight(1.0f), Alignment.End, onSortSelected)
            SortableHeaderCell("L10", BuilderSortField.DIFF_LAST_10, sortField, descending, Modifier.weight(1.0f), Alignment.End, onSortSelected)
            SortableHeaderCell("Szn", BuilderSortField.DIFF_2025, sortField, descending, Modifier.weight(1.0f), Alignment.End, onSortSelected)
            SortableHeaderCell("NB", BuilderSortField.NEXT_BEST, sortField, descending, Modifier.weight(1.0f), Alignment.End, onSortSelected)
        }
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
    val selected = sortField == field
    Row(
        modifier = modifier
            .clickable { onSortSelected(field) }
            .heightIn(min = 36.dp),
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = if (alignment == Alignment.End) Arrangement.End else Arrangement.Start,
    ) {
        Text(
            text = label,
            style = MaterialTheme.typography.labelSmall,
            color = if (selected) MaterialTheme.colorScheme.tertiary else MaterialTheme.colorScheme.onSurfaceVariant,
            fontWeight = FontWeight.SemiBold,
        )
        if (selected) {
            Icon(
                imageVector = Icons.Filled.ArrowDropDown,
                contentDescription = if (descending) "Sorted descending" else "Sorted ascending",
                modifier = Modifier.rotate(if (descending) 0f else 180f),
                tint = MaterialTheme.colorScheme.tertiary,
            )
        }
    }
}

@OptIn(ExperimentalFoundationApi::class)
@Composable
fun CandidateSelectionRow(
    selection: OddsSearchResult,
    selected: Boolean,
    enabled: Boolean,
    onOpenPlayerRequest: (PlayerLaunchRequest) -> Unit,
    onToggleLeg: (OddsSearchResult) -> Unit,
    modifier: Modifier = Modifier,
) {
    val haptics = LocalHapticFeedback.current
    val containerColor by animateColorAsState(
        targetValue = when {
            selected -> MaterialTheme.colorScheme.tertiary
            enabled -> MaterialTheme.colorScheme.surface
            else -> MaterialTheme.colorScheme.surfaceContainerLow
        },
        label = "candidateRowColor",
    )
    val borderColor = when {
        selected -> MaterialTheme.colorScheme.tertiary
        else -> MaterialTheme.colorScheme.outlineVariant
    }
    val primaryTextColor = when {
        selected -> MaterialTheme.colorScheme.onTertiary
        enabled -> MaterialTheme.colorScheme.primary
        else -> MaterialTheme.colorScheme.onSurfaceVariant
    }

    Surface(
        modifier = modifier
            .fillMaxWidth()
            .combinedClickable(
                enabled = enabled,
                onClick = {
                    haptics.performHapticFeedback(
                        if (selected) HapticFeedbackType.ToggleOff else HapticFeedbackType.ToggleOn,
                    )
                    onToggleLeg(selection)
                },
                onLongClick = {
                    selection.toPlayerLaunchRequest()?.let {
                        haptics.performHapticFeedback(HapticFeedbackType.LongPress)
                        onOpenPlayerRequest(it)
                    }
                },
                onLongClickLabel = "Open player stats",
            ),
        shape = MaterialTheme.shapes.medium,
        color = containerColor,
        border = BorderStroke(1.dp, borderColor),
    ) {
        Row(
            modifier = Modifier.padding(horizontal = 14.dp, vertical = 10.dp),
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            Column(
                modifier = Modifier.weight(3.4f),
                verticalArrangement = Arrangement.spacedBy(2.dp),
            ) {
                Text(
                    text = selection.player?.fullName ?: selection.label,
                    style = MaterialTheme.typography.bodySmall,
                    fontWeight = FontWeight.SemiBold,
                    color = primaryTextColor,
                    maxLines = 1,
                    overflow = TextOverflow.Ellipsis,
                )
                PlayerContextTags(
                    position = selection.playerPosition,
                    matchupDifficulty = selection.matchupDifficulty,
                )
                WeatherContextTags(weather = selection.weather)
                Text(
                    buildRowSubtitle(selection),
                    style = MaterialTheme.typography.labelSmall,
                    color = if (selected) {
                        MaterialTheme.colorScheme.onTertiary.copy(alpha = 0.84f)
                    } else {
                        MaterialTheme.colorScheme.onSurfaceVariant
                    },
                    maxLines = 2,
                    overflow = TextOverflow.Ellipsis,
                )
            }
            MetricCell(lineWithSideLabel(selection), Modifier.weight(1.2f), selected)
            MetricCell(formatDecimalPrice(selection.decimalPrice), Modifier.weight(1.0f), selected, emphasize = true)
            MetricCell(selection.diffLast10?.let(::formatSignedDelta) ?: "--", Modifier.weight(1.0f), selected, value = selection.diffLast10)
            MetricCell(selection.diff2025?.let(::formatSignedDelta) ?: "--", Modifier.weight(1.0f), selected, value = selection.diff2025)
            MetricCell(selection.nextBestProbDiff?.let(::formatSignedDelta) ?: "--", Modifier.weight(1.0f), selected, value = selection.nextBestProbDiff)
        }
    }
}

@Composable
private fun MetricCell(
    text: String,
    modifier: Modifier,
    selected: Boolean,
    emphasize: Boolean = false,
    value: Double? = null,
) {
    val colors = AppTheme.colors
    val color = when {
        selected -> MaterialTheme.colorScheme.onTertiary
        value == null -> MaterialTheme.colorScheme.onSurface
        value > 0 -> colors.positive
        value < 0 -> colors.negative
        else -> colors.neutral
    }
    Column(
        modifier = modifier,
        horizontalAlignment = Alignment.End,
    ) {
        Text(
            text = text,
            style = MaterialTheme.typography.bodySmall.tabular,
            color = color,
            fontWeight = if (emphasize) FontWeight.Bold else FontWeight.SemiBold,
            maxLines = 1,
        )
    }
}

@Composable
fun CandidateBoardCard(
    group: CandidateBoardGroup,
    selectedSelectionIds: Set<Int>,
    isSelectionEnabled: (OddsSearchResult) -> Boolean,
    onOpenPlayerRequest: (PlayerLaunchRequest) -> Unit,
    onToggleLeg: (OddsSearchResult) -> Unit,
    modifier: Modifier = Modifier,
) {
    androidx.compose.material3.Card(
        modifier = modifier.fillMaxWidth(),
        colors = androidx.compose.material3.CardDefaults.cardColors(
            containerColor = MaterialTheme.colorScheme.surface,
        ),
        border = BorderStroke(1.dp, MaterialTheme.colorScheme.outlineVariant),
    ) {
        Column(
            modifier = Modifier.padding(14.dp),
            verticalArrangement = Arrangement.spacedBy(10.dp),
        ) {
            Text(
                text = group.title,
                modifier = Modifier
                    .clickable(enabled = group.toPlayerLaunchRequest() != null) {
                        group.toPlayerLaunchRequest()?.let(onOpenPlayerRequest)
                    }
                    .semantics { heading() },
                style = MaterialTheme.typography.titleMedium,
                color = MaterialTheme.colorScheme.primary,
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
                isSelectionEnabled = isSelectionEnabled,
                onToggleLeg = onToggleLeg,
            )
            CandidateSelectionBand(
                title = "Unders",
                scrollState = scrollState,
                columns = group.columns,
                selectedSelectionIds = selectedSelectionIds,
                targetSelectionType = "under",
                isSelectionEnabled = isSelectionEnabled,
                onToggleLeg = onToggleLeg,
            )
        }
    }
}

@Composable
private fun CandidateSelectionBand(
    title: String,
    scrollState: ScrollState,
    columns: List<CandidateLineColumn>,
    selectedSelectionIds: Set<Int>,
    targetSelectionType: String,
    isSelectionEnabled: (OddsSearchResult) -> Boolean,
    onToggleLeg: (OddsSearchResult) -> Unit,
) {
    Column(verticalArrangement = Arrangement.spacedBy(8.dp)) {
        Text(
            title,
            style = MaterialTheme.typography.labelLarge,
            color = MaterialTheme.colorScheme.tertiary,
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
                    enabled = slot?.selection?.let(isSelectionEnabled) == true,
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
    val haptics = LocalHapticFeedback.current
    val containerColor by animateColorAsState(
        targetValue = when {
            selected -> MaterialTheme.colorScheme.tertiary
            enabled -> MaterialTheme.colorScheme.surface
            else -> MaterialTheme.colorScheme.surfaceContainerLow
        },
        label = "tileColor",
    )
    Surface(
        modifier = Modifier
            .width(82.dp)
            .clickable(enabled = enabled) {
                haptics.performHapticFeedback(
                    if (selected) HapticFeedbackType.ToggleOff else HapticFeedbackType.ToggleOn,
                )
                onClick()
            },
        shape = MaterialTheme.shapes.medium,
        color = containerColor,
        tonalElevation = if (selected) 3.dp else 0.dp,
        border = BorderStroke(
            width = 1.dp,
            color = if (selected) MaterialTheme.colorScheme.tertiary else MaterialTheme.colorScheme.outlineVariant,
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
                    selected -> MaterialTheme.colorScheme.onTertiary.copy(alpha = 0.94f)
                    enabled -> MaterialTheme.colorScheme.primary
                    else -> MaterialTheme.colorScheme.onSurfaceVariant
                },
                fontWeight = FontWeight.SemiBold,
            )
            Text(
                formatDecimalPrice(slot.selection.decimalPrice),
                style = MaterialTheme.typography.titleMedium.tabular,
                color = when {
                    selected -> MaterialTheme.colorScheme.onTertiary
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
                    title = "Szn",
                    value = slot.selection.diff2025,
                    selected = selected,
                )
                EmbeddedMetricPill(
                    title = "NB",
                    value = slot.selection.nextBestProbDiff,
                    selected = selected,
                )
                if (!enabled) {
                    EmbeddedStatusPill(label = "No SGM", selected = selected)
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
        shape = MaterialTheme.shapes.medium,
        color = MaterialTheme.colorScheme.surfaceContainer,
        border = BorderStroke(1.dp, MaterialTheme.colorScheme.outlineVariant),
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
    val colors = AppTheme.colors
    val background = when {
        selected -> MaterialTheme.colorScheme.onTertiary.copy(alpha = 0.18f)
        value == null -> MaterialTheme.colorScheme.secondaryContainer
        value > 0 -> colors.positiveContainer
        value < 0 -> colors.negativeContainer
        else -> colors.neutralContainer
    }
    val textColor = when {
        selected -> MaterialTheme.colorScheme.onTertiary
        value == null -> MaterialTheme.colorScheme.onSurfaceVariant
        value > 0 -> colors.positive
        value < 0 -> colors.negative
        else -> colors.neutral
    }
    Surface(
        shape = MaterialTheme.shapes.extraSmall,
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
                style = MaterialTheme.typography.labelSmall.tabular,
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
        shape = MaterialTheme.shapes.extraSmall,
        color = if (selected) {
            MaterialTheme.colorScheme.onTertiary.copy(alpha = 0.18f)
        } else {
            MaterialTheme.colorScheme.secondaryContainer
        },
    ) {
        Text(
            text = label,
            modifier = Modifier.padding(horizontal = 6.dp, vertical = 3.dp),
            style = MaterialTheme.typography.labelSmall,
            color = if (selected) MaterialTheme.colorScheme.onTertiary else MaterialTheme.colorScheme.onSurfaceVariant,
            fontWeight = FontWeight.SemiBold,
        )
    }
}

/**
 * Collapsed draft bar. Shows the count, context, and — once a comparison has
 * run — the best quoted price, which is the number the user actually wants at
 * a glance.
 */
@Composable
fun DraftPeekBar(
    count: Int,
    primaryLabel: String,
    secondaryLabel: String,
    bestPriceLabel: String?,
    onExpand: () -> Unit,
    modifier: Modifier = Modifier,
) {
    Surface(
        modifier = modifier
            .fillMaxWidth()
            .clickable(onClick = onExpand),
        color = Color.Transparent,
    ) {
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .height(76.dp)
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
                    shape = MaterialTheme.shapes.extraLarge,
                    color = MaterialTheme.colorScheme.tertiary,
                ) {
                    Text(
                        text = count.toString(),
                        modifier = Modifier.padding(horizontal = 12.dp, vertical = 6.dp),
                        style = MaterialTheme.typography.labelLarge.tabular,
                        color = MaterialTheme.colorScheme.onTertiary,
                        fontWeight = FontWeight.Bold,
                    )
                }
                Column(verticalArrangement = Arrangement.spacedBy(2.dp)) {
                    Text(
                        primaryLabel,
                        style = MaterialTheme.typography.titleSmall,
                        fontWeight = FontWeight.SemiBold,
                        color = MaterialTheme.colorScheme.primary,
                    )
                    Text(
                        secondaryLabel,
                        style = MaterialTheme.typography.bodySmall,
                        color = MaterialTheme.colorScheme.onSurfaceVariant,
                        maxLines = 1,
                        overflow = TextOverflow.Ellipsis,
                    )
                }
            }
            Row(
                verticalAlignment = Alignment.CenterVertically,
                horizontalArrangement = Arrangement.spacedBy(4.dp),
            ) {
                if (bestPriceLabel != null) {
                    Text(
                        bestPriceLabel,
                        style = MaterialTheme.typography.titleMedium.tabular,
                        color = MaterialTheme.colorScheme.tertiary,
                        fontWeight = FontWeight.Bold,
                    )
                }
                Icon(
                    imageVector = Icons.Outlined.KeyboardArrowUp,
                    contentDescription = "Expand draft",
                    tint = MaterialTheme.colorScheme.tertiary,
                )
            }
        }
    }
}

/** A draft leg row with a compact trailing remove button. */
@Composable
fun DraftLegCard(
    leg: DraftLeg,
    onRemove: (Int) -> Unit,
    modifier: Modifier = Modifier,
    showMatchLabel: Boolean = false,
) {
    androidx.compose.material3.Card(
        modifier = modifier.fillMaxWidth(),
        colors = androidx.compose.material3.CardDefaults.cardColors(
            containerColor = MaterialTheme.colorScheme.surface,
        ),
        border = BorderStroke(1.dp, MaterialTheme.colorScheme.outlineVariant),
    ) {
        Column(
            modifier = Modifier.padding(start = 16.dp, top = 12.dp, end = 8.dp, bottom = 12.dp),
            verticalArrangement = Arrangement.spacedBy(6.dp),
        ) {
            Row(
                modifier = Modifier.fillMaxWidth(),
                verticalAlignment = Alignment.CenterVertically,
                horizontalArrangement = Arrangement.spacedBy(8.dp),
            ) {
                Column(
                    modifier = Modifier.weight(1f),
                    verticalArrangement = Arrangement.spacedBy(2.dp),
                ) {
                    Text(
                        leg.label,
                        style = MaterialTheme.typography.titleSmall,
                        color = MaterialTheme.colorScheme.primary,
                        fontWeight = FontWeight.SemiBold,
                    )
                    if (showMatchLabel) {
                        Text(
                            leg.eventLabel,
                            style = MaterialTheme.typography.bodySmall,
                            color = MaterialTheme.colorScheme.onSurfaceVariant,
                        )
                    }
                }
                Text(
                    formatDecimalPrice(leg.basePrice),
                    style = MaterialTheme.typography.titleMedium.tabular,
                    fontWeight = FontWeight.Bold,
                )
                IconButton(onClick = { onRemove(leg.selectionId) }) {
                    Icon(
                        imageVector = Icons.Outlined.Delete,
                        contentDescription = "Remove ${leg.label}",
                        tint = MaterialTheme.colorScheme.onSurfaceVariant,
                    )
                }
            }
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.spacedBy(8.dp),
            ) {
                DiffMetricCard(
                    label = "L10",
                    value = leg.diffLast10,
                    modifier = Modifier.weight(1f),
                )
                DiffMetricCard(
                    label = "SEASON",
                    value = leg.diff2025,
                    modifier = Modifier.weight(1f),
                )
                DiffMetricCard(
                    label = "NEXT BEST",
                    value = leg.nextBestProbDiff,
                    modifier = Modifier.weight(1f),
                )
            }
        }
    }
}

@Composable
fun DiffMetricCard(
    label: String,
    value: Double?,
    modifier: Modifier = Modifier,
) {
    val colors = AppTheme.colors
    val tone = when {
        value == null -> MaterialTheme.colorScheme.onSurface
        value > 0 -> colors.positive
        value < 0 -> colors.negative
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
            style = MaterialTheme.typography.bodyMedium.tabular,
            fontWeight = FontWeight.SemiBold,
            color = tone,
        )
    }
}

@Composable
fun SummaryMetricCard(
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
            style = MaterialTheme.typography.bodyMedium.tabular,
            fontWeight = FontWeight.SemiBold,
            color = MaterialTheme.colorScheme.onSurface,
        )
    }
}

private data class MetricGlossaryEntry(
    val term: String,
    val definition: String,
)

private val metricGlossary = listOf(
    MetricGlossaryEntry(
        term = "Price",
        definition = "The agency's current decimal price for the selection.",
    ),
    MetricGlossaryEntry(
        term = "Line",
        definition = "The stat threshold for the prop. \"O 19.5\" means over 19.5; \"U 19.5\" means under.",
    ),
    MetricGlossaryEntry(
        term = "L10 (last-10 diff)",
        definition = "The player's hit rate over their last 10 games minus the probability implied by the price. Positive means the recent form beats the price.",
    ),
    MetricGlossaryEntry(
        term = "Szn (season diff)",
        definition = "The player's season-long hit rate minus the implied probability. Positive means the season sample beats the price.",
    ),
    MetricGlossaryEntry(
        term = "NB (next best)",
        definition = "Implied-probability gap to the nearest other agency. Positive: this agency has the best price and NB is its margin over second best. Negative: how far it trails the best price.",
    ),
    MetricGlossaryEntry(
        term = "Matchup",
        definition = "How friendly the opposition is to this player's position historically, from Terrible to Excellent.",
    ),
    MetricGlossaryEntry(
        term = "No SGM",
        definition = "The agency lists the selection but won't include it in a same-game multi, so it can't be priced here.",
    ),
)

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun MetricGlossarySheet(
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
            verticalArrangement = Arrangement.spacedBy(14.dp),
        ) {
            Row(
                verticalAlignment = Alignment.CenterVertically,
                horizontalArrangement = Arrangement.spacedBy(10.dp),
            ) {
                Icon(
                    imageVector = Icons.AutoMirrored.Outlined.HelpOutline,
                    contentDescription = null,
                    tint = MaterialTheme.colorScheme.tertiary,
                )
                Text(
                    "What the metrics mean",
                    modifier = Modifier.semantics { heading() },
                    style = MaterialTheme.typography.headlineSmall,
                    fontWeight = FontWeight.SemiBold,
                )
            }
            metricGlossary.forEach { entry ->
                Column(verticalArrangement = Arrangement.spacedBy(2.dp)) {
                    Text(
                        entry.term,
                        style = MaterialTheme.typography.titleSmall,
                        color = MaterialTheme.colorScheme.primary,
                        fontWeight = FontWeight.SemiBold,
                    )
                    Text(
                        entry.definition,
                        style = MaterialTheme.typography.bodyMedium,
                        color = MaterialTheme.colorScheme.onSurfaceVariant,
                    )
                }
            }
            Spacer(modifier = Modifier.height(16.dp))
        }
    }
}
