package com.jamesbrown.aflmobile.ui.theme

import androidx.compose.runtime.Immutable
import androidx.compose.ui.graphics.Color


val Navy950 = Color(0xFF0D1A2B)
val Navy900 = Color(0xFF12263D)
val Navy700 = Color(0xFF35516D)
val Blue800 = Color(0xFF1F5C9E)
val Blue700 = Color(0xFF2A72BC)
val Blue600 = Color(0xFF4B90D6)
val Blue400 = Color(0xFF8EC1F5)
val Blue300 = Color(0xFFB7DAFA)
val Blue200 = Color(0xFFD8EBFF)
val Blue100 = Color(0xFFEAF5FF)
val Blue50 = Color(0xFFF5FAFF)
val IceWhite = Color(0xFFFDFEFF)
val Orange700 = Color(0xFFC56612)
val Orange600 = Color(0xFFD9791D)
val Orange300 = Color(0xFFFFC897)
val Orange100 = Color(0xFFFFEEDB)
val Rose500 = Color(0xFFD65A5A)

val Paper50 = Color(0xFFF7FAFC)
val Mist100 = Color(0xFFEFF6F8)
val Mist200 = Color(0xFFDCE7EC)
val Mist400 = Color(0xFFA8B8C4)
val Slate600 = Color(0xFF526274)
val Ink950 = Color(0xFF07111F)
val Ink900 = Color(0xFF101A2A)
val Ink700 = Color(0xFF273548)
val Teal950 = Color(0xFF062E2B)
val Teal800 = Color(0xFF115E59)
val Teal700 = Color(0xFF0F766E)
val Teal300 = Color(0xFF5EEAD4)
val Teal100 = Color(0xFFD7F5EF)
val Indigo600 = Color(0xFF4F46E5)
val Indigo100 = Color(0xFFE6E8FF)
val Amber950 = Color(0xFF3A2504)
val Amber700 = Color(0xFFB45309)
val Amber300 = Color(0xFFFCD34D)
val Amber100 = Color(0xFFFFE8B6)

/**
 * Semantic colors that Material's scheme has no slot for: metric deltas,
 * matchup grades, and hit/miss states. Every screen reads these through
 * [com.jamesbrown.aflmobile.ui.theme.AppTheme.colors] instead of hardcoding hex
 * values, which is what makes dark theme possible.
 */
@Immutable
data class AppColors(
    val positive: Color,
    val positiveContainer: Color,
    val positiveStrong: Color,
    val positiveStrongContainer: Color,
    val negative: Color,
    val negativeContainer: Color,
    val negativeStrong: Color,
    val negativeStrongContainer: Color,
    val warning: Color,
    val warningContainer: Color,
    val neutral: Color,
    val neutralContainer: Color,
)

val LightAppColors = AppColors(
    positive = Color(0xFF157A4B),
    positiveContainer = Color(0xFFDFF6E8),
    positiveStrong = Color(0xFF075E3A),
    positiveStrongContainer = Color(0xFFBDEDCF),
    negative = Color(0xFFB84A3D),
    negativeContainer = Color(0xFFFFE4DE),
    negativeStrong = Color(0xFF8F3028),
    negativeStrongContainer = Color(0xFFF8CFC8),
    warning = Amber700,
    warningContainer = Amber100,
    neutral = Color(0xFF6F5A14),
    neutralContainer = Color(0xFFEFE7C8),
)

val DarkAppColors = AppColors(
    positive = Color(0xFF7EE2A8),
    positiveContainer = Color(0xFF123E2A),
    positiveStrong = Color(0xFFA7F3C3),
    positiveStrongContainer = Color(0xFF0D5334),
    negative = Color(0xFFFFA79B),
    negativeContainer = Color(0xFF4E241F),
    negativeStrong = Color(0xFFFFC0B7),
    negativeStrongContainer = Color(0xFF642D25),
    warning = Amber300,
    warningContainer = Color(0xFF4C3510),
    neutral = Color(0xFFE5D28A),
    neutralContainer = Color(0xFF40391D),
)
