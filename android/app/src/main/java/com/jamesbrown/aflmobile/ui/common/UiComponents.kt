package com.jamesbrown.aflmobile.ui.common

import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.ExperimentalLayoutApi
import androidx.compose.foundation.layout.FlowRow
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.WindowInsets
import androidx.compose.foundation.layout.WindowInsetsSides
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.only
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.safeDrawing
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Air
import androidx.compose.material.icons.outlined.BlurOn
import androidx.compose.material.icons.outlined.Cloud
import androidx.compose.material.icons.outlined.Grain
import androidx.compose.material.icons.outlined.SearchOff
import androidx.compose.material.icons.outlined.Thunderstorm
import androidx.compose.material.icons.outlined.WaterDrop
import androidx.compose.material.icons.outlined.WbCloudy
import androidx.compose.material.icons.outlined.WbSunny
import androidx.compose.material3.Card
import androidx.compose.material3.CardDefaults
import androidx.compose.material3.CircularProgressIndicator
import androidx.compose.material3.Icon
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import com.jamesbrown.aflmobile.model.WeatherSummary
import com.jamesbrown.aflmobile.ui.theme.AppTheme
import com.jamesbrown.aflmobile.ui.theme.appCardBorder
import com.jamesbrown.aflmobile.ui.theme.appCardColors


/**
 * Window insets for screens hosted inside the app shell: the shell's bottom
 * navigation bar already consumes the bottom inset, so inner scaffolds only
 * handle the top and sides.
 */
@Composable
fun appScreenInsets(): WindowInsets =
    WindowInsets.safeDrawing.only(WindowInsetsSides.Top + WindowInsetsSides.Horizontal)

@Composable
fun LoadingCard(message: String) {
    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appCardBorder(),
    ) {
        Row(
            modifier = Modifier
                .fillMaxWidth()
                .padding(20.dp),
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.spacedBy(16.dp),
        ) {
            CircularProgressIndicator()
            Text(text = message, style = MaterialTheme.typography.bodyLarge)
        }
    }
}

@Composable
fun ErrorCard(
    message: String,
    onRetry: (() -> Unit)? = null,
) {
    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = CardDefaults.cardColors(containerColor = MaterialTheme.colorScheme.errorContainer),
        border = BorderStroke(1.dp, MaterialTheme.colorScheme.error.copy(alpha = 0.16f)),
    ) {
        Column(
            modifier = Modifier.padding(start = 20.dp, top = 20.dp, end = 20.dp, bottom = 8.dp),
            verticalArrangement = Arrangement.spacedBy(4.dp),
        ) {
            Text(
                text = message,
                color = MaterialTheme.colorScheme.onErrorContainer,
                style = MaterialTheme.typography.bodyMedium,
            )
            if (onRetry != null) {
                TextButton(
                    onClick = onRetry,
                    modifier = Modifier.align(Alignment.End),
                ) {
                    Text("Retry", color = MaterialTheme.colorScheme.onErrorContainer, fontWeight = FontWeight.SemiBold)
                }
            } else {
                androidx.compose.foundation.layout.Spacer(modifier = Modifier.padding(bottom = 12.dp))
            }
        }
    }
}

@Composable
fun EmptyCard(
    title: String,
    body: String,
    icon: ImageVector = Icons.Outlined.SearchOff,
    actionLabel: String? = null,
    onAction: (() -> Unit)? = null,
) {
    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appCardBorder(),
    ) {
        Column(
            modifier = Modifier.padding(20.dp),
            verticalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            Row(
                verticalAlignment = Alignment.CenterVertically,
                horizontalArrangement = Arrangement.spacedBy(10.dp),
            ) {
                Icon(
                    imageVector = icon,
                    contentDescription = null,
                    tint = MaterialTheme.colorScheme.onSurfaceVariant,
                )
                Text(text = title, style = MaterialTheme.typography.titleMedium)
            }
            Text(
                text = body,
                style = MaterialTheme.typography.bodyMedium,
                color = MaterialTheme.colorScheme.onSurfaceVariant,
            )
            if (actionLabel != null && onAction != null) {
                TextButton(
                    onClick = onAction,
                    modifier = Modifier.align(Alignment.End),
                ) {
                    Text(actionLabel)
                }
            }
        }
    }
}

/** A small static tag. Purely informational — never interactive. */
@Composable
fun InlineChip(label: String) {
    Surface(
        shape = MaterialTheme.shapes.small,
        color = MaterialTheme.colorScheme.secondaryContainer,
        contentColor = MaterialTheme.colorScheme.onSecondaryContainer,
    ) {
        Text(
            text = label,
            modifier = Modifier.padding(horizontal = 10.dp, vertical = 6.dp),
            style = MaterialTheme.typography.labelMedium,
        )
    }
}

@OptIn(ExperimentalLayoutApi::class)
@Composable
fun PlayerContextTags(
    position: String?,
    matchupDifficulty: String?,
    modifier: Modifier = Modifier,
) {
    val positionLabel = formatPlayerPositionTag(position)
    val matchupLabel = formatMatchupDifficultyTag(matchupDifficulty)
    if (positionLabel == null && matchupLabel == null) {
        return
    }

    FlowRow(
        modifier = modifier,
        horizontalArrangement = Arrangement.spacedBy(4.dp),
        verticalArrangement = Arrangement.spacedBy(4.dp),
    ) {
        if (positionLabel != null) {
            ContextTag(
                label = positionLabel,
                containerColor = MaterialTheme.colorScheme.secondaryContainer,
                contentColor = MaterialTheme.colorScheme.primary,
            )
        }
        if (matchupLabel != null) {
            val style = matchupTagStyle(matchupDifficulty ?: matchupLabel)
            ContextTag(
                label = matchupLabel,
                containerColor = style.containerColor,
                contentColor = style.contentColor,
            )
        }
    }
}

@OptIn(ExperimentalLayoutApi::class)
@Composable
fun WeatherContextTags(
    weather: WeatherSummary?,
    modifier: Modifier = Modifier,
) {
    val icon = weatherIcon(weather?.iconCode)
    val tempLabel = formatWeatherTemperatureTag(weather?.temperatureC)
    val rainLabel = formatWeatherRainTag(weather?.precipMm)
    if (icon == null && tempLabel == null && rainLabel == null) {
        return
    }

    FlowRow(
        modifier = modifier,
        horizontalArrangement = Arrangement.spacedBy(4.dp),
        verticalArrangement = Arrangement.spacedBy(4.dp),
    ) {
        icon?.let {
            WeatherIconTag(icon = it, contentDescription = weather?.label ?: "Weather")
        }
        tempLabel?.let {
            ContextTag(
                label = it,
                containerColor = MaterialTheme.colorScheme.secondaryContainer,
                contentColor = MaterialTheme.colorScheme.primary,
            )
        }
        rainLabel?.let {
            ContextTag(
                label = it,
                containerColor = MaterialTheme.colorScheme.secondaryContainer,
                contentColor = MaterialTheme.colorScheme.primary,
            )
        }
    }
}

@Composable
private fun ContextTag(
    label: String,
    containerColor: Color,
    contentColor: Color,
) {
    Surface(
        shape = MaterialTheme.shapes.extraSmall,
        color = containerColor,
    ) {
        Text(
            text = label,
            modifier = Modifier.padding(horizontal = 6.dp, vertical = 2.dp),
            style = MaterialTheme.typography.labelSmall.copy(fontSize = 10.sp),
            color = contentColor,
            fontWeight = FontWeight.SemiBold,
            maxLines = 1,
            overflow = TextOverflow.Clip,
            softWrap = false,
        )
    }
}

@Composable
private fun WeatherIconTag(
    icon: ImageVector,
    contentDescription: String,
) {
    Surface(
        shape = MaterialTheme.shapes.extraSmall,
        color = MaterialTheme.colorScheme.secondaryContainer,
    ) {
        Icon(
            imageVector = icon,
            contentDescription = contentDescription,
            modifier = Modifier.padding(horizontal = 6.dp, vertical = 4.dp),
            tint = MaterialTheme.colorScheme.primary,
        )
    }
}

private data class MatchupTagStyle(
    val containerColor: Color,
    val contentColor: Color,
)

/** Accent colour for a matchup difficulty when rendered as inline text rather than a tag. */
@Composable
fun matchupAccentColor(matchupLabel: String): Color = matchupTagStyle(matchupLabel).contentColor

/**
 * One-line match context: weather icon plus a dot-separated venue / round /
 * conditions summary. Used where the match is fixed (e.g. the SGM header) so
 * the per-selection rows don't have to repeat it.
 */
@Composable
fun MatchContextLine(
    venue: String?,
    roundLabel: String?,
    weather: WeatherSummary?,
    modifier: Modifier = Modifier,
) {
    val icon = weatherIcon(weather?.iconCode)
    val parts = listOfNotNull(
        venue,
        roundLabel,
        weather?.label,
        formatWeatherTemperatureTag(weather?.temperatureC),
        formatWeatherRainTag(weather?.precipMm),
    )
    if (icon == null && parts.isEmpty()) {
        return
    }
    Row(
        modifier = modifier,
        verticalAlignment = Alignment.CenterVertically,
        horizontalArrangement = Arrangement.spacedBy(6.dp),
    ) {
        icon?.let {
            Icon(
                imageVector = it,
                contentDescription = weather?.label ?: "Weather",
                modifier = Modifier.size(16.dp),
                tint = MaterialTheme.colorScheme.primary,
            )
        }
        Text(
            text = parts.joinToString(" · "),
            style = MaterialTheme.typography.labelMedium,
            color = MaterialTheme.colorScheme.onSurfaceVariant,
            maxLines = 1,
            overflow = TextOverflow.Ellipsis,
        )
    }
}

@Composable
private fun matchupTagStyle(matchupLabel: String): MatchupTagStyle {
    val colors = AppTheme.colors
    return when (matchupLabel.lowercase()) {
        "terrible", "terr" -> MatchupTagStyle(colors.negativeStrongContainer, colors.negativeStrong)
        "bad" -> MatchupTagStyle(colors.warningContainer, colors.warning)
        "good" -> MatchupTagStyle(colors.positiveContainer, colors.positive)
        "excellent", "excl" -> MatchupTagStyle(colors.positiveStrongContainer, colors.positiveStrong)
        else -> MatchupTagStyle(colors.neutralContainer, colors.neutral)
    }
}

private fun weatherIcon(iconCode: String?): ImageVector? =
    when (iconCode) {
        "clear" -> Icons.Outlined.WbSunny
        "partly_cloudy" -> Icons.Outlined.WbCloudy
        "cloudy" -> Icons.Outlined.Cloud
        "fog" -> Icons.Outlined.BlurOn
        "drizzle" -> Icons.Outlined.Grain
        "rain" -> Icons.Outlined.WaterDrop
        "storm" -> Icons.Outlined.Thunderstorm
        "snow" -> Icons.Outlined.Air
        else -> null
    }

val ScreenPadding = PaddingValues(horizontal = 16.dp, vertical = 12.dp)
