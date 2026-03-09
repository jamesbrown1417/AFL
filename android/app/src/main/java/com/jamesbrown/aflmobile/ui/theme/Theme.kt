package com.jamesbrown.aflmobile.ui.theme

import androidx.compose.foundation.isSystemInDarkTheme
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.darkColorScheme
import androidx.compose.material3.lightColorScheme
import androidx.compose.runtime.Composable


private val LightColors = lightColorScheme(
    primary = Grass700,
    onPrimary = Gold100,
    secondary = Gold700,
    onSecondary = Slate900,
    tertiary = Coral500,
    background = Slate100,
    surface = androidx.compose.ui.graphics.Color.White,
    onSurface = Slate900,
    onSurfaceVariant = Slate700,
    outline = Grass500,
)

private val DarkColors = darkColorScheme(
    primary = Gold500,
    onPrimary = Slate900,
    secondary = Grass500,
    onSecondary = Slate900,
    tertiary = Coral500,
    background = Slate900,
    surface = androidx.compose.ui.graphics.Color(0xFF223036),
    onSurface = androidx.compose.ui.graphics.Color.White,
    onSurfaceVariant = Grass100,
    outline = Grass500,
)

@Composable
fun AflEdgeTheme(
    darkTheme: Boolean = isSystemInDarkTheme(),
    content: @Composable () -> Unit,
) {
    MaterialTheme(
        colorScheme = if (darkTheme) DarkColors else LightColors,
        content = content,
    )
}
