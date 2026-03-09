package com.jamesbrown.aflmobile.ui.theme

import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.darkColorScheme
import androidx.compose.material3.lightColorScheme
import androidx.compose.runtime.Composable
import androidx.compose.ui.graphics.Color


private val LightColors = lightColorScheme(
    primary = Blue700,
    onPrimary = IceWhite,
    primaryContainer = Blue200,
    onPrimaryContainer = Navy950,
    secondary = Blue600,
    onSecondary = IceWhite,
    secondaryContainer = Blue100,
    onSecondaryContainer = Navy900,
    tertiary = Orange700,
    onTertiary = IceWhite,
    tertiaryContainer = Orange100,
    onTertiaryContainer = Navy950,
    background = Blue25,
    onBackground = Navy950,
    surface = IceWhite,
    onSurface = Navy950,
    surfaceVariant = Blue100,
    onSurfaceVariant = Navy700,
    surfaceTint = Blue600,
    outline = Blue300,
    outlineVariant = Color(0xFFD7E6F8),
    error = Rose500,
    onError = IceWhite,
    errorContainer = Color(0xFFFFE6E7),
    onErrorContainer = Color(0xFF6A1B1B),
    inverseSurface = Navy900,
    inverseOnSurface = Blue50,
    inversePrimary = Blue300,
    surfaceBright = IceWhite,
    surfaceDim = Blue100,
    surfaceContainerLowest = Color(0xFFFEFEFF),
    surfaceContainerLow = Color(0xFFF7FAFF),
    surfaceContainer = Color(0xFFF0F6FF),
    surfaceContainerHigh = Color(0xFFE8F2FF),
    surfaceContainerHighest = Color(0xFFE0EDFF),
)

private val DarkColors = darkColorScheme(
    primary = Blue300,
    onPrimary = Navy950,
    primaryContainer = Blue800,
    onPrimaryContainer = IceWhite,
    secondary = Blue400,
    onSecondary = Navy950,
    secondaryContainer = Color(0xFF1C3551),
    onSecondaryContainer = Blue50,
    tertiary = Orange600,
    onTertiary = Navy950,
    tertiaryContainer = Color(0xFF5A3411),
    onTertiaryContainer = Color(0xFFFFEFD9),
    background = Navy950,
    onBackground = Blue50,
    surface = Color(0xEE13253A),
    onSurface = Blue50,
    surfaceVariant = Color(0xFF223B56),
    onSurfaceVariant = Color(0xFFC4D8EE),
    surfaceTint = Blue400,
    outline = Color(0xFF5F7C99),
    outlineVariant = Color(0xFF33516E),
    error = Color(0xFFFF8A86),
    onError = Navy950,
    errorContainer = Color(0xFF5B2223),
    onErrorContainer = Color(0xFFFFDAD8),
    inverseSurface = Blue50,
    inverseOnSurface = Navy950,
    inversePrimary = Blue700,
    surfaceBright = Color(0xFF19324A),
    surfaceDim = Color(0xFF0E1D2E),
    surfaceContainerLowest = Color(0xFF102131),
    surfaceContainerLow = Color(0xFF13283E),
    surfaceContainer = Color(0xFF17304A),
    surfaceContainerHigh = Color(0xFF1B3855),
    surfaceContainerHighest = Color(0xFF20405F),
)

@Composable
fun AflEdgeTheme(
    darkTheme: Boolean = false,
    content: @Composable () -> Unit,
) {
    MaterialTheme(
        colorScheme = if (darkTheme) DarkColors else LightColors,
        typography = AflTypography,
        content = content,
    )
}
