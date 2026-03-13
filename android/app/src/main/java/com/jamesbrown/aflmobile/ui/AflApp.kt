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
import androidx.compose.material3.ModalDrawerSheet
import androidx.compose.material3.ModalNavigationDrawer
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.NavigationDrawerItem
import androidx.compose.material3.NavigationDrawerItemDefaults
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.ui.Modifier
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
import kotlinx.coroutines.launch
import androidx.compose.material3.DrawerValue
import androidx.compose.material3.HorizontalDivider
import androidx.compose.material3.rememberDrawerState


@Composable
fun AflApp() {
    val context = LocalContext.current.applicationContext as AflApplication
    val container = context.container
    val navController = rememberNavController()
    val drawerState = rememberDrawerState(initialValue = DrawerValue.Closed)
    val coroutineScope = rememberCoroutineScope()
    val backStackEntry by navController.currentBackStackEntryAsState()
    val currentDestination = backStackEntry?.destination
    val topLevelDestinations = listOf(
        TopLevelDestination.Player,
        TopLevelDestination.Odds,
        TopLevelDestination.Sgm,
        TopLevelDestination.Cgm,
        TopLevelDestination.Settings,
    )
    fun navigateToTopLevel(route: String) {
        navController.navigate(route) {
            popUpTo(navController.graph.startDestinationId) {
                saveState = true
            }
            launchSingleTop = true
            restoreState = true
        }
    }

    Box(modifier = Modifier.fillMaxSize()) {
        AppBackdrop()
        ModalNavigationDrawer(
            drawerState = drawerState,
            drawerContent = {
                ModalDrawerSheet(
                    drawerContainerColor = MaterialTheme.colorScheme.surface,
                    drawerContentColor = MaterialTheme.colorScheme.onSurface,
                ) {
                    Text(
                        text = "AFL Edge",
                        modifier = Modifier.padding(horizontal = 20.dp, vertical = 24.dp),
                        style = MaterialTheme.typography.headlineSmall,
                        color = MaterialTheme.colorScheme.primary,
                    )
                    HorizontalDivider()
                    topLevelDestinations.forEach { destination ->
                        val selected = currentDestination?.hierarchy?.any { it.route == destination.route } == true
                        NavigationDrawerItem(
                            modifier = Modifier.padding(horizontal = 12.dp, vertical = 4.dp),
                            label = { Text(destination.label) },
                            selected = selected,
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
                            colors = NavigationDrawerItemDefaults.colors(
                                selectedContainerColor = MaterialTheme.colorScheme.tertiaryContainer,
                                selectedTextColor = MaterialTheme.colorScheme.tertiary,
                                selectedIconColor = MaterialTheme.colorScheme.tertiary,
                            ),
                            onClick = {
                                navigateToTopLevel(destination.route)
                                coroutineScope.launch { drawerState.close() }
                            },
                        )
                    }
                }
            },
        ) {
            NavHost(
                navController = navController,
                startDestination = TopLevelDestination.Player.route,
            ) {
                composable(TopLevelDestination.Player.route) {
                    PlayerStatsRoute(
                        repository = container.repository,
                        onOpenNavigation = { coroutineScope.launch { drawerState.open() } },
                    )
                }
                composable(TopLevelDestination.Odds.route) {
                    OddsRoute(
                        repository = container.repository,
                        onOpenNavigation = { coroutineScope.launch { drawerState.open() } },
                    )
                }
                composable(TopLevelDestination.Sgm.route) {
                    SgmBuilderRoute(
                        repository = container.repository,
                        draftStore = container.sgmDraftStore,
                        onOpenNavigation = { coroutineScope.launch { drawerState.open() } },
                    )
                }
                composable(TopLevelDestination.Cgm.route) {
                    CgmBuilderRoute(
                        repository = container.repository,
                        onOpenNavigation = { coroutineScope.launch { drawerState.open() } },
                    )
                }
                composable(TopLevelDestination.Settings.route) {
                    SettingsRoute(
                        repository = container.repository,
                        onOpenNavigation = { coroutineScope.launch { drawerState.open() } },
                    )
                }
            }
        }
    }
}
