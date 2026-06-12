package com.jamesbrown.aflmobile.ui.common

import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.ExperimentalLayoutApi
import androidx.compose.foundation.layout.FlowRow
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.BoxWithConstraints
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.offset
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.FilterChip
import androidx.compose.material3.FilterChipDefaults
import androidx.compose.material3.FilledTonalButton
import androidx.compose.material3.Icon
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.ModalBottomSheet
import androidx.compose.material3.RangeSlider
import androidx.compose.material3.SegmentedButton
import androidx.compose.material3.SegmentedButtonDefaults
import androidx.compose.material3.SingleChoiceSegmentedButtonRow
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import com.jamesbrown.aflmobile.model.BuilderDisplayMode
import com.jamesbrown.aflmobile.model.BuilderSortField
import com.jamesbrown.aflmobile.model.MatchupDifficultyOptions
import com.jamesbrown.aflmobile.model.OddsDiffSliderMax
import com.jamesbrown.aflmobile.model.OddsDiffSliderMin
import com.jamesbrown.aflmobile.model.QuickFilterPreset
import com.jamesbrown.aflmobile.model.SelectionMetricFilters
import com.jamesbrown.aflmobile.model.applyQuickFilterPreset
import java.util.Locale

@Composable
fun builderSupportTextColor(): Color = MaterialTheme.colorScheme.onSurface.copy(alpha = 0.76f)

@Composable
fun BuilderSupportText(
    text: String,
    modifier: Modifier = Modifier,
) {
    Text(
        text = text,
        modifier = modifier,
        style = MaterialTheme.typography.bodySmall,
        color = builderSupportTextColor(),
    )
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun BuilderDisplayModeSegmented(
    displayMode: BuilderDisplayMode,
    onDisplayModeChanged: (BuilderDisplayMode) -> Unit,
    modifier: Modifier = Modifier,
    selectedContainerColor: Color = MaterialTheme.colorScheme.tertiary,
    selectedContentColor: Color = MaterialTheme.colorScheme.onTertiary,
    inactiveContainerColor: Color = MaterialTheme.colorScheme.surfaceContainerHigh,
    inactiveContentColor: Color = MaterialTheme.colorScheme.onSurface,
) {
    SingleChoiceSegmentedButtonRow(modifier = modifier.fillMaxWidth()) {
        BuilderDisplayMode.entries.forEachIndexed { index, option ->
            SegmentedButton(
                selected = displayMode == option,
                onClick = { onDisplayModeChanged(option) },
                shape = SegmentedButtonDefaults.itemShape(index = index, count = BuilderDisplayMode.entries.size),
                colors = SegmentedButtonDefaults.colors(
                    activeContainerColor = selectedContainerColor,
                    activeContentColor = selectedContentColor,
                    inactiveContainerColor = inactiveContainerColor,
                    inactiveContentColor = inactiveContentColor,
                ),
                label = {
                    Text(
                        when (option) {
                            BuilderDisplayMode.ROW -> "Row mode"
                            BuilderDisplayMode.TILE -> "Tile mode"
                        },
                        maxLines = 1,
                    )
                },
            )
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun BuilderSortSheet(
    selectedSortField: BuilderSortField,
    onSelect: (BuilderSortField) -> Unit,
    onDismiss: () -> Unit,
) {
    ModalBottomSheet(
        onDismissRequest = onDismiss,
        containerColor = MaterialTheme.colorScheme.surface,
        scrimColor = MaterialTheme.colorScheme.scrim.copy(alpha = 0.22f),
    ) {
        Column(
            modifier = Modifier
                .fillMaxWidth()
                .padding(horizontal = 20.dp, vertical = 8.dp),
            verticalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            Text(
                "Sort rows by",
                style = MaterialTheme.typography.headlineSmall,
                fontWeight = FontWeight.SemiBold,
            )
            BuilderSupportText("Applies to dense row mode only.")
            Spacer(modifier = Modifier.height(4.dp))
            BuilderSortField.entries.forEach { option ->
                Surface(
                    modifier = Modifier
                        .fillMaxWidth()
                        .clickable {
                            onSelect(option)
                            onDismiss()
                        },
                    shape = RoundedCornerShape(18.dp),
                    color = if (selectedSortField == option) {
                        MaterialTheme.colorScheme.tertiaryContainer
                    } else {
                        MaterialTheme.colorScheme.surfaceContainerLow
                    },
                ) {
                    Row(
                        modifier = Modifier.padding(horizontal = 16.dp, vertical = 16.dp),
                        horizontalArrangement = Arrangement.SpaceBetween,
                        verticalAlignment = Alignment.CenterVertically,
                    ) {
                        Column(
                            modifier = Modifier.weight(1f),
                            verticalArrangement = Arrangement.spacedBy(2.dp),
                        ) {
                            Text(
                                option.shortLabel(),
                                style = MaterialTheme.typography.titleMedium,
                                fontWeight = FontWeight.SemiBold,
                            )
                            BuilderSupportText(option.description())
                        }
                        if (selectedSortField == option) {
                            Text(
                                "Selected",
                                style = MaterialTheme.typography.labelLarge,
                                color = MaterialTheme.colorScheme.tertiary,
                                fontWeight = FontWeight.SemiBold,
                            )
                        }
                    }
                }
            }
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class, ExperimentalLayoutApi::class)
@Composable
fun SelectionMetricFilterSheet(
    filters: SelectionMetricFilters,
    onFiltersChanged: (SelectionMetricFilters) -> Unit,
    onApply: () -> Unit,
    onApplyQuickFilter: (SelectionMetricFilters) -> Unit,
    onClear: () -> Unit,
    onDismiss: () -> Unit,
) {
    ModalBottomSheet(
        onDismissRequest = onDismiss,
        containerColor = MaterialTheme.colorScheme.surface,
        scrimColor = MaterialTheme.colorScheme.scrim.copy(alpha = 0.22f),
    ) {
        Column(
            modifier = Modifier
                .fillMaxWidth()
                .padding(horizontal = 20.dp, vertical = 8.dp),
            verticalArrangement = Arrangement.spacedBy(0.dp),
        ) {
            Text(
                "Selection filters",
                style = MaterialTheme.typography.headlineSmall,
                fontWeight = FontWeight.SemiBold,
            )
            Spacer(modifier = Modifier.height(16.dp))
            QuickFilterActionSection(
                onSelectPreset = { preset ->
                    onApplyQuickFilter(filters.applyQuickFilterPreset(preset))
                },
            )
            Spacer(modifier = Modifier.height(20.dp))
            Text(
                "Matchup difficulty",
                style = MaterialTheme.typography.titleMedium,
                fontWeight = FontWeight.SemiBold,
            )
            Spacer(modifier = Modifier.height(8.dp))
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
                        colors = FilterChipDefaults.filterChipColors(
                            selectedContainerColor = MaterialTheme.colorScheme.tertiaryContainer,
                            selectedLabelColor = MaterialTheme.colorScheme.tertiary,
                        ),
                    )
                }
            }
            Spacer(modifier = Modifier.height(20.dp))
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.spacedBy(12.dp),
            ) {
                StepperField(
                    value = filters.minPriceText,
                    onValueChange = { onFiltersChanged(filters.copy(minPriceText = it)) },
                    label = "Min price",
                    modifier = Modifier.weight(1f),
                    step = 0.25,
                    minValue = 1.0,
                    prefix = "$",
                )
                StepperField(
                    value = filters.maxPriceText,
                    onValueChange = { onFiltersChanged(filters.copy(maxPriceText = it)) },
                    label = "Max price",
                    modifier = Modifier.weight(1f),
                    step = 0.25,
                    minValue = 1.0,
                    prefix = "$",
                )
            }
            Spacer(modifier = Modifier.height(20.dp))
            SelectionMetricRangeSection(
                title = "Diff last 10",
                range = filters.minDiffLast10..filters.maxDiffLast10,
                onRangeChange = { range ->
                    onFiltersChanged(filters.copy(minDiffLast10 = range.start, maxDiffLast10 = range.endInclusive))
                },
            )
            SelectionMetricRangeSection(
                title = "Season diff",
                range = filters.minDiff2025..filters.maxDiff2025,
                onRangeChange = { range ->
                    onFiltersChanged(filters.copy(minDiff2025 = range.start, maxDiff2025 = range.endInclusive))
                },
            )
            SelectionMetricRangeSection(
                title = "Next best diff",
                range = filters.minNextBestProbDiff..filters.maxNextBestProbDiff,
                onRangeChange = { range ->
                    onFiltersChanged(filters.copy(minNextBestProbDiff = range.start, maxNextBestProbDiff = range.endInclusive))
                },
            )
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.spacedBy(12.dp),
            ) {
                TextButton(onClick = onClear, modifier = Modifier.weight(1f)) {
                    Text("Clear")
                }
                FilledTonalButton(onClick = onApply, modifier = Modifier.weight(1f)) {
                    Text("Apply")
                }
            }
            Spacer(modifier = Modifier.height(12.dp))
        }
    }
}

@OptIn(ExperimentalLayoutApi::class)
@Composable
fun QuickFilterActionSection(
    onSelectPreset: (QuickFilterPreset) -> Unit,
) {
    var expanded by remember { mutableStateOf(false) }

    Column(verticalArrangement = Arrangement.spacedBy(10.dp)) {
        FilledTonalButton(
            onClick = { expanded = !expanded },
            modifier = Modifier.fillMaxWidth(),
        ) {
            Text(if (expanded) "Hide quick filters" else "Quick filters")
        }

        if (expanded) {
            FlowRow(
                horizontalArrangement = Arrangement.spacedBy(8.dp),
                verticalArrangement = Arrangement.spacedBy(8.dp),
            ) {
                QuickFilterPreset.entries.forEach { preset ->
                    FilterChip(
                        selected = false,
                        onClick = { onSelectPreset(preset) },
                        label = { Text(preset.label()) },
                        colors = FilterChipDefaults.filterChipColors(
                            containerColor = MaterialTheme.colorScheme.surfaceContainerLow,
                            labelColor = MaterialTheme.colorScheme.onSurface,
                        ),
                    )
                }
            }
            BuilderSupportText("Tap once to apply immediately.")
        }
    }
}

/** Labelled +/- range slider shared by the odds and builder filter sheets. */
@Composable
fun DiffRangeSection(
    title: String,
    range: ClosedFloatingPointRange<Float>,
    onRangeChange: (ClosedFloatingPointRange<Float>) -> Unit,
) {
    Column(verticalArrangement = Arrangement.spacedBy(8.dp)) {
        Text(title, style = MaterialTheme.typography.titleMedium)
        BuilderSupportText(formatMetricRange(range.start, range.endInclusive))
        RangeSlider(
            value = range.start..range.endInclusive,
            onValueChange = { onRangeChange(it.start..it.endInclusive) },
            valueRange = OddsDiffSliderMin..OddsDiffSliderMax,
            steps = 39,
        )
    }
}

@Composable
private fun SelectionMetricRangeSection(
    title: String,
    range: ClosedFloatingPointRange<Float>,
    onRangeChange: (ClosedFloatingPointRange<Float>) -> Unit,
) {
    Column(verticalArrangement = Arrangement.spacedBy(0.dp)) {
        Text(
            title,
            style = MaterialTheme.typography.titleMedium,
            fontWeight = FontWeight.SemiBold,
        )
        Spacer(modifier = Modifier.height(2.dp))
        BuilderSupportText(formatMetricRange(range.start, range.endInclusive))
        Spacer(modifier = Modifier.height(10.dp))
        SliderValueBadges(range = range)
        RangeSlider(
            value = range.start..range.endInclusive,
            onValueChange = { onRangeChange(it.start..it.endInclusive) },
            valueRange = OddsDiffSliderMin..OddsDiffSliderMax,
            steps = 39,
        )
        Spacer(modifier = Modifier.height(22.dp))
    }
}

@Composable
private fun SliderValueBadges(
    range: ClosedFloatingPointRange<Float>,
) {
    BoxWithConstraints(
        modifier = Modifier
            .fillMaxWidth()
            .height(28.dp),
    ) {
        val badgeWidth = 64.dp
        val maxOffset = ((maxWidth - badgeWidth).value).coerceAtLeast(0f).dp
        val fractionSpan = OddsDiffSliderMax - OddsDiffSliderMin
        val startFraction = ((range.start - OddsDiffSliderMin) / fractionSpan).coerceIn(0f, 1f)
        val endFraction = ((range.endInclusive - OddsDiffSliderMin) / fractionSpan).coerceIn(0f, 1f)
        var startOffset = maxOffset * startFraction
        var endOffset = maxOffset * endFraction
        val minimumGap = 52.dp
        if ((endOffset - startOffset).value < minimumGap.value) {
            val center = (startOffset + endOffset) / 2f
            val adjustedStart = center - minimumGap / 2f
            val adjustedEnd = center + minimumGap / 2f
            startOffset = if (adjustedStart.value < 0f) 0.dp else adjustedStart
            endOffset = if (adjustedEnd.value > maxOffset.value) maxOffset else adjustedEnd
        }

        ValueBadge(
            text = formatMetricValue(range.start),
            modifier = Modifier.offset(x = startOffset),
        )
        ValueBadge(
            text = formatMetricValue(range.endInclusive),
            modifier = Modifier.offset(x = endOffset),
        )
    }
}

@Composable
private fun ValueBadge(
    text: String,
    modifier: Modifier = Modifier,
) {
    Surface(
        modifier = modifier.width(64.dp),
        shape = RoundedCornerShape(999.dp),
        color = MaterialTheme.colorScheme.surfaceContainerHighest,
        tonalElevation = 1.dp,
    ) {
        Text(
            text = text,
            modifier = Modifier.padding(horizontal = 8.dp, vertical = 4.dp),
            style = MaterialTheme.typography.labelMedium,
            color = MaterialTheme.colorScheme.onSurface,
            fontWeight = FontWeight.SemiBold,
            textAlign = TextAlign.Center,
        )
    }
}

private fun BuilderSortField.shortLabel(): String =
    when (this) {
        BuilderSortField.PLAYER -> "Player"
        BuilderSortField.LINE -> "Line"
        BuilderSortField.NEXT_BEST -> "Next best"
        BuilderSortField.PRICE -> "Price"
        BuilderSortField.DIFF_LAST_10 -> "Last 10"
        BuilderSortField.DIFF_2025 -> "Season"
    }

private fun BuilderSortField.description(): String =
    when (this) {
        BuilderSortField.PLAYER -> "Alphabetical by player name."
        BuilderSortField.LINE -> "Sort by line value."
        BuilderSortField.NEXT_BEST -> "Largest next-best probability gap first."
        BuilderSortField.PRICE -> "Highest current price first."
        BuilderSortField.DIFF_LAST_10 -> "Strongest last-10 edge first."
        BuilderSortField.DIFF_2025 -> "Strongest season edge first."
    }

private fun formatMetricRange(min: Float, max: Float): String =
    String.format(Locale.getDefault(), "%+.2f to %+.2f", min, max)

private fun formatMetricValue(value: Float): String =
    String.format(Locale.getDefault(), "%+.2f", value)

private fun QuickFilterPreset.label(): String =
    when (this) {
        QuickFilterPreset.LAST10_POSITIVE -> "L10 positive"
        QuickFilterPreset.LAST10_AND_NB_POSITIVE -> "L10+NB positive"
        QuickFilterPreset.LAST10_NB_AND_FAVORABLE_MATCHUP -> "L10+NB+matchup edge"
    }
