package com.jamesbrown.aflmobile.ui.theme

import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Shapes
import androidx.compose.material3.darkColorScheme
import androidx.compose.material3.lightColorScheme
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.ReadOnlyComposable
import androidx.compose.runtime.staticCompositionLocalOf
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.dp


private val LightColors = lightColorScheme(
    primary = Teal700,
    onPrimary = IceWhite,
    primaryContainer = Teal100,
    onPrimaryContainer = Teal950,
    secondary = Indigo600,
    onSecondary = IceWhite,
    secondaryContainer = Indigo100,
    onSecondaryContainer = Navy900,
    tertiary = Amber700,
    onTertiary = IceWhite,
    tertiaryContainer = Amber100,
    onTertiaryContainer = Amber950,
    background = Paper50,
    onBackground = Navy950,
    surface = Color.White,
    onSurface = Navy950,
    surfaceVariant = Mist100,
    onSurfaceVariant = Slate600,
    surfaceTint = Teal700,
    outline = Mist400,
    outlineVariant = Mist200,
    error = Rose500,
    onError = IceWhite,
    errorContainer = Color(0xFFFFE6E7),
    onErrorContainer = Color(0xFF6A1B1B),
    inverseSurface = Navy900,
    inverseOnSurface = Paper50,
    inversePrimary = Teal300,
    surfaceBright = IceWhite,
    surfaceDim = Mist100,
    surfaceContainerLowest = Color.White,
    surfaceContainerLow = Color(0xFFFBFCFE),
    surfaceContainer = Color(0xFFF5F8FB),
    surfaceContainerHigh = Color(0xFFEFF4F8),
    surfaceContainerHighest = Color(0xFFE8EEF5),
)

private val DarkColors = darkColorScheme(
    primary = Teal300,
    onPrimary = Teal950,
    primaryContainer = Teal800,
    onPrimaryContainer = Color(0xFFE4FFFA),
    secondary = Blue300,
    onSecondary = Navy950,
    secondaryContainer = Color(0xFF1D3B64),
    onSecondaryContainer = Color(0xFFEAF3FF),
    tertiary = Amber300,
    onTertiary = Amber950,
    tertiaryContainer = Color(0xFF5B3B0D),
    onTertiaryContainer = Color(0xFFFFF1CF),
    background = Ink950,
    onBackground = Color(0xFFEAF0F7),
    surface = Ink900,
    onSurface = Color(0xFFEAF0F7),
    surfaceVariant = Ink700,
    onSurfaceVariant = Color(0xFFC9D4E2),
    surfaceTint = Teal300,
    outline = Color(0xFF66778B),
    outlineVariant = Color(0xFF314154),
    error = Color(0xFFFF8A86),
    onError = Ink950,
    errorContainer = Color(0xFF5B2223),
    onErrorContainer = Color(0xFFFFDAD8),
    inverseSurface = Paper50,
    inverseOnSurface = Ink950,
    inversePrimary = Teal700,
    surfaceBright = Color(0xFF202C3C),
    surfaceDim = Color(0xFF0A1422),
    surfaceContainerLowest = Color(0xFF08111D),
    surfaceContainerLow = Color(0xFF0E1928),
    surfaceContainer = Color(0xFF132033),
    surfaceContainerHigh = Color(0xFF19283C),
    surfaceContainerHighest = Color(0xFF213249),
)

private val AflShapes = Shapes(
    extraSmall = RoundedCornerShape(6.dp),
    small = RoundedCornerShape(10.dp),
    medium = RoundedCornerShape(16.dp),
    large = RoundedCornerShape(24.dp),
    extraLarge = RoundedCornerShape(28.dp),
)

val LocalAppColors = staticCompositionLocalOf { LightAppColors }

/** Access point for the app's semantic (non-Material) colors. */
object AppTheme {
    val colors: AppColors
        @Composable
        @ReadOnlyComposable
        get() = LocalAppColors.current
}

@Composable
fun AflEdgeTheme(
    darkTheme: Boolean = false,
    content: @Composable () -> Unit,
) {
    val appColors = if (darkTheme) DarkAppColors else LightAppColors
    CompositionLocalProvider(LocalAppColors provides appColors) {
        MaterialTheme(
            colorScheme = if (darkTheme) DarkColors else LightColors,
            typography = AflTypography,
            shapes = AflShapes,
            content = content,
        )
    }
}
