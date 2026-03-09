package com.jamesbrown.aflmobile.ui.theme

import androidx.compose.foundation.Canvas
import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.material3.CardDefaults
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.NavigationBarItemColors
import androidx.compose.material3.NavigationBarItemDefaults
import androidx.compose.material3.TopAppBarColors
import androidx.compose.material3.TopAppBarDefaults
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Brush
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.dp


@Composable
fun AppBackdrop(modifier: Modifier = Modifier) {
    Canvas(modifier = modifier.fillMaxSize()) {
        drawRect(
            brush = Brush.verticalGradient(
                colors = listOf(
                    Blue25,
                    Blue50,
                    Color(0xFFFDFEFF),
                ),
            ),
        )
        drawCircle(
            color = Blue300.copy(alpha = 0.26f),
            radius = size.minDimension * 0.58f,
            center = Offset(size.width * 0.2f, size.height * 0.1f),
        )
        drawCircle(
            color = Blue200.copy(alpha = 0.24f),
            radius = size.minDimension * 0.54f,
            center = Offset(size.width * 0.94f, size.height * 0.2f),
        )
        drawCircle(
            color = Orange100.copy(alpha = 0.34f),
            radius = size.minDimension * 0.36f,
            center = Offset(size.width * 0.84f, size.height * 0.68f),
        )
        drawCircle(
            color = Color.White.copy(alpha = 0.82f),
            radius = size.minDimension * 0.62f,
            center = Offset(size.width * 0.46f, size.height * 0.82f),
        )
    }
}

@Composable
fun appGlassSurfaceColor(): Color = MaterialTheme.colorScheme.surfaceContainerLow.copy(alpha = 0.96f)

@Composable
fun appGlassBorder(): BorderStroke =
    BorderStroke(1.dp, MaterialTheme.colorScheme.outlineVariant.copy(alpha = 0.9f))

@Composable
fun appCardColors() = CardDefaults.cardColors(containerColor = appGlassSurfaceColor())

@Composable
fun appTopBarColors(): TopAppBarColors = TopAppBarDefaults.topAppBarColors(
    containerColor = Blue100.copy(alpha = 0.98f),
    scrolledContainerColor = Blue200.copy(alpha = 0.98f),
    titleContentColor = Blue800,
    navigationIconContentColor = Blue800,
    actionIconContentColor = MaterialTheme.colorScheme.tertiary,
)

@Composable
fun appNavigationBarItemColors(): NavigationBarItemColors = NavigationBarItemDefaults.colors(
    selectedIconColor = MaterialTheme.colorScheme.tertiary,
    selectedTextColor = MaterialTheme.colorScheme.tertiary,
    indicatorColor = MaterialTheme.colorScheme.tertiaryContainer.copy(alpha = 0.98f),
    unselectedIconColor = MaterialTheme.colorScheme.onSurfaceVariant,
    unselectedTextColor = MaterialTheme.colorScheme.onSurfaceVariant,
)
