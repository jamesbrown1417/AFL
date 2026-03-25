package com.jamesbrown.aflmobile.ui.theme

import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.material3.CardDefaults
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.NavigationBarItemColors
import androidx.compose.material3.NavigationBarItemDefaults
import androidx.compose.material3.TopAppBarColors
import androidx.compose.material3.TopAppBarDefaults
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.dp


@Composable
fun AppBackdrop(modifier: Modifier = Modifier) {
    Box(
        modifier = modifier
            .fillMaxSize()
            .background(MaterialTheme.colorScheme.background),
    )
}

@Composable
fun appGlassSurfaceColor(): Color = MaterialTheme.colorScheme.surface.copy(alpha = 0.98f)

@Composable
fun appGlassBorder(): BorderStroke =
    BorderStroke(1.dp, MaterialTheme.colorScheme.outlineVariant.copy(alpha = 0.9f))

@Composable
fun appCardColors() = CardDefaults.cardColors(containerColor = appGlassSurfaceColor())

@Composable
fun appTopBarColors(): TopAppBarColors = TopAppBarDefaults.topAppBarColors(
    containerColor = Color(0xFFF2F7FF),
    scrolledContainerColor = Color(0xFFE9F1FF),
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
