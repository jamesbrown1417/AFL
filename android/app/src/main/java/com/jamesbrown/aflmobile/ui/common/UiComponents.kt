package com.jamesbrown.aflmobile.ui.common

import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.ExperimentalLayoutApi
import androidx.compose.foundation.layout.FlowRow
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Air
import androidx.compose.material.icons.outlined.BlurOn
import androidx.compose.material.icons.outlined.Cloud
import androidx.compose.material.icons.outlined.Grain
import androidx.compose.material.icons.outlined.Thunderstorm
import androidx.compose.material.icons.outlined.WaterDrop
import androidx.compose.material.icons.outlined.WbCloudy
import androidx.compose.material.icons.outlined.WbSunny
import androidx.compose.material3.AssistChip
import androidx.compose.material3.AssistChipDefaults
import androidx.compose.material3.Card
import androidx.compose.material3.CardDefaults
import androidx.compose.material3.CircularProgressIndicator
import androidx.compose.material3.Icon
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.sp
import androidx.compose.ui.unit.dp
import com.jamesbrown.aflmobile.model.WeatherSummary
import com.jamesbrown.aflmobile.ui.theme.Blue100
import com.jamesbrown.aflmobile.ui.theme.Blue200
import com.jamesbrown.aflmobile.ui.theme.Blue50
import com.jamesbrown.aflmobile.ui.theme.Blue700
import com.jamesbrown.aflmobile.ui.theme.IceWhite
import com.jamesbrown.aflmobile.ui.theme.Mint500
import com.jamesbrown.aflmobile.ui.theme.NegativeSurface
import com.jamesbrown.aflmobile.ui.theme.NeutralSurface
import com.jamesbrown.aflmobile.ui.theme.Orange200
import com.jamesbrown.aflmobile.ui.theme.Orange700
import com.jamesbrown.aflmobile.ui.theme.PositiveSurface
import com.jamesbrown.aflmobile.ui.theme.Rose500
import com.jamesbrown.aflmobile.ui.theme.appCardColors
import com.jamesbrown.aflmobile.ui.theme.appGlassBorder


@Composable
fun LoadingCard(message: String) {
    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appGlassBorder(),
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
fun ErrorCard(message: String) {
    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = CardDefaults.cardColors(containerColor = MaterialTheme.colorScheme.errorContainer),
        border = BorderStroke(1.dp, MaterialTheme.colorScheme.error.copy(alpha = 0.16f)),
    ) {
        Text(
            text = message,
            modifier = Modifier.padding(16.dp),
            color = MaterialTheme.colorScheme.onErrorContainer,
            style = MaterialTheme.typography.bodyMedium,
        )
    }
}

@Composable
fun EmptyCard(title: String, body: String) {
    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appGlassBorder(),
    ) {
        Column(
            modifier = Modifier.padding(20.dp),
            verticalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            Text(text = title, style = MaterialTheme.typography.titleMedium)
            Text(text = body, style = MaterialTheme.typography.bodyMedium)
        }
    }
}

@Composable
fun InlineChip(label: String) {
    AssistChip(
        onClick = {},
        enabled = false,
        border = null,
        colors = AssistChipDefaults.assistChipColors(
            containerColor = MaterialTheme.colorScheme.secondaryContainer.copy(alpha = 0.98f),
            labelColor = MaterialTheme.colorScheme.primary,
            disabledContainerColor = MaterialTheme.colorScheme.secondaryContainer.copy(alpha = 0.98f),
            disabledLabelColor = MaterialTheme.colorScheme.primary,
        ),
        label = { Text(label) },
    )
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
                containerColor = Blue100.copy(alpha = 0.95f),
                contentColor = Blue700,
                borderColor = Blue200.copy(alpha = 0.95f),
            )
        }
        if (matchupLabel != null) {
            val style = matchupTagStyle(matchupDifficulty ?: matchupLabel)
            ContextTag(
                label = matchupLabel,
                containerColor = style.containerColor,
                contentColor = style.contentColor,
                borderColor = style.borderColor,
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
                containerColor = Blue50.copy(alpha = 0.98f),
                contentColor = Blue700,
                borderColor = Blue200.copy(alpha = 0.9f),
            )
        }
        rainLabel?.let {
            ContextTag(
                label = it,
                containerColor = Blue50.copy(alpha = 0.98f),
                contentColor = Blue700,
                borderColor = Blue200.copy(alpha = 0.9f),
            )
        }
    }
}

@Composable
private fun ContextTag(
    label: String,
    containerColor: Color,
    contentColor: Color,
    borderColor: Color,
) {
    Surface(
        shape = MaterialTheme.shapes.small,
        color = containerColor,
        border = BorderStroke(1.dp, borderColor),
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
        shape = MaterialTheme.shapes.small,
        color = Blue50.copy(alpha = 0.98f),
        border = BorderStroke(1.dp, Blue200.copy(alpha = 0.9f)),
    ) {
        Icon(
            imageVector = icon,
            contentDescription = contentDescription,
            modifier = Modifier.padding(horizontal = 6.dp, vertical = 4.dp),
            tint = Blue700,
        )
    }
}

private data class MatchupTagStyle(
    val containerColor: Color,
    val contentColor: Color,
    val borderColor: Color,
)

private fun matchupTagStyle(matchupLabel: String): MatchupTagStyle =
    when (matchupLabel.lowercase()) {
        "terrible" -> MatchupTagStyle(
            containerColor = Color(0xFFF7D2CC),
            contentColor = Color(0xFFA3362A),
            borderColor = Color(0xFFA3362A).copy(alpha = 0.22f),
        )
        "bad" -> MatchupTagStyle(
            containerColor = Orange200.copy(alpha = 0.92f),
            contentColor = Orange700,
            borderColor = Orange700.copy(alpha = 0.18f),
        )
        "good" -> MatchupTagStyle(
            containerColor = Color(0xFFE5F7EA),
            contentColor = Color(0xFF2F8A57),
            borderColor = Color(0xFF2F8A57).copy(alpha = 0.16f),
        )
        "excellent" -> MatchupTagStyle(
            containerColor = Color(0xFFBEE8CB),
            contentColor = Color(0xFF0C512F),
            borderColor = Color(0xFF0C512F).copy(alpha = 0.24f),
        )
        else -> MatchupTagStyle(
            containerColor = NeutralSurface.copy(alpha = 0.96f),
            contentColor = Blue700,
            borderColor = Blue200.copy(alpha = 0.95f),
        )
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
