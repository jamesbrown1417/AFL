package com.jamesbrown.aflmobile.ui

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.padding
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Person
import androidx.compose.material.icons.outlined.Settings
import androidx.compose.material.icons.outlined.Search
import androidx.compose.material.icons.outlined.SportsFootball
import androidx.compose.material.icons.outlined.ViewWeek
import androidx.compose.material3.Icon
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.NavigationBar
import androidx.compose.material3.NavigationBarItem
import androidx.compose.material3.Scaffold
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.unit.dp
import androidx.navigation.NavDestination.Companion.hierarchy
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.composable
import androidx.navigation.compose.currentBackStackEntryAsState
import androidx.navigation.compose.rememberNavController
import com.jamesbrown.aflmobile.AflApplication
import com.jamesbrown.aflmobile.ui.navigation.TopLevelDestination
import com.jamesbrown.aflmobile.ui.screens.cgm.CgmBuilderRoute
import com.jamesbrown.aflmobile.ui.screens.props.OddsRoute
import com.jamesbrown.aflmobile.ui.screens.props.PlayerStatsRoute
import com.jamesbrown.aflmobile.ui.screens.settings.SettingsRoute
import com.jamesbrown.aflmobile.ui.screens.sgm.SgmBuilderRoute
import com.jamesbrown.aflmobile.ui.theme.AppBackdrop
import com.jamesbrown.aflmobile.ui.theme.appNavigationBarItemColors


@Composable
fun AflApp() {
    val context = LocalContext.current.applicationContext as AflApplication
    val container = context.container
    val navController = rememberNavController()
    val backStackEntry by navController.currentBackStackEntryAsState()
    val currentDestination = backStackEntry?.destination
    val topLevelDestinations = listOf(
        TopLevelDestination.Player,
        TopLevelDestination.Odds,
        TopLevelDestination.Sgm,
        TopLevelDestination.Cgm,
        TopLevelDestination.Settings,
    )
    val showBottomBar = topLevelDestinations.any { destination ->
        currentDestination?.hierarchy?.any { it.route == destination.route } == true
    }

    Box(modifier = Modifier.fillMaxSize()) {
        AppBackdrop()
        Scaffold(
            containerColor = Color.Transparent,
            bottomBar = {
                if (showBottomBar) {
                    NavigationBar(
                        containerColor = MaterialTheme.colorScheme.surfaceContainerLow.copy(alpha = 0.97f),
                        tonalElevation = 0.dp,
                    ) {
                        topLevelDestinations.forEach { destination ->
                            val selected = currentDestination?.hierarchy?.any { it.route == destination.route } == true
                            NavigationBarItem(
                                selected = selected,
                                colors = appNavigationBarItemColors(),
                                onClick = {
                                    navController.navigate(destination.route) {
                                        popUpTo(navController.graph.startDestinationId) {
                                            saveState = true
                                        }
                                        launchSingleTop = true
                                        restoreState = true
                                    }
                                },
                                label = { Text(destination.label) },
                                icon = {
                                    Icon(
                                        imageVector = when (destination) {
                                            TopLevelDestination.Player -> Icons.Outlined.Person
                                            TopLevelDestination.Odds -> Icons.Outlined.Search
                                            TopLevelDestination.Sgm -> Icons.Outlined.SportsFootball
                                            TopLevelDestination.Cgm -> Icons.Outlined.ViewWeek
                                            TopLevelDestination.Settings -> Icons.Outlined.Settings
                                        },
                                        contentDescription = destination.label,
                                    )
                                },
                            )
                        }
                    }
                }
            },
        ) { innerPadding ->
            NavHost(
                navController = navController,
                startDestination = TopLevelDestination.Player.route,
                modifier = Modifier.padding(innerPadding),
            ) {
                composable(TopLevelDestination.Player.route) {
                    PlayerStatsRoute(repository = container.repository)
                }
                composable(TopLevelDestination.Odds.route) {
                    OddsRoute(repository = container.repository)
                }
                composable(TopLevelDestination.Sgm.route) {
                    SgmBuilderRoute(
                        repository = container.repository,
                        draftStore = container.sgmDraftStore,
                    )
                }
                composable(TopLevelDestination.Cgm.route) {
                    CgmBuilderRoute(repository = container.repository)
                }
                composable(TopLevelDestination.Settings.route) {
                    SettingsRoute(repository = container.repository)
                }
            }
        }
    }
}
