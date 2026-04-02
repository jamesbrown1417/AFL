package com.jamesbrown.aflmobile.ui

import androidx.compose.foundation.Image
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.pager.HorizontalPager
import androidx.compose.foundation.pager.rememberPagerState
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Person
import androidx.compose.material.icons.outlined.Search
import androidx.compose.material.icons.outlined.Settings
import androidx.compose.material.icons.outlined.SportsFootball
import androidx.compose.material.icons.outlined.ViewWeek
import androidx.compose.material3.DrawerValue
import androidx.compose.material3.HorizontalDivider
import androidx.compose.material3.Icon
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.ModalDrawerSheet
import androidx.compose.material3.ModalNavigationDrawer
import androidx.compose.material3.NavigationDrawerItem
import androidx.compose.material3.NavigationDrawerItemDefaults
import androidx.compose.material3.Text
import androidx.compose.material3.rememberDrawerState
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.unit.dp
import com.jamesbrown.aflmobile.AflApplication
import com.jamesbrown.aflmobile.R
import com.jamesbrown.aflmobile.ui.navigation.PlayerLaunchRequest
import com.jamesbrown.aflmobile.ui.navigation.TopLevelDestination
import com.jamesbrown.aflmobile.ui.screens.cgm.CgmBuilderRoute
import com.jamesbrown.aflmobile.ui.screens.props.OddsRoute
import com.jamesbrown.aflmobile.ui.screens.props.PlayerStatsRoute
import com.jamesbrown.aflmobile.ui.screens.settings.SettingsRoute
import com.jamesbrown.aflmobile.ui.screens.sgm.SgmBuilderRoute
import com.jamesbrown.aflmobile.ui.theme.AppBackdrop
import kotlinx.coroutines.launch


@Composable
fun AflApp() {
    val context = LocalContext.current.applicationContext as AflApplication
    val container = context.container
    val drawerState = rememberDrawerState(initialValue = DrawerValue.Closed)
    val coroutineScope = rememberCoroutineScope()
    val topLevelDestinations = listOf(
        TopLevelDestination.Player,
        TopLevelDestination.Odds,
        TopLevelDestination.Sgm,
        TopLevelDestination.Cgm,
        TopLevelDestination.Settings,
    )
    val pagerState = rememberPagerState(
        initialPage = 0,
        pageCount = { topLevelDestinations.size },
    )
    var playerLaunchRequest by remember { mutableStateOf<PlayerLaunchRequest?>(null) }
    val currentDestination = topLevelDestinations[pagerState.currentPage]

    fun openDrawer() {
        coroutineScope.launch { drawerState.open() }
    }

    fun openPlayerRequest(request: PlayerLaunchRequest) {
        playerLaunchRequest = request
        coroutineScope.launch {
            pagerState.animateScrollToPage(topLevelDestinations.indexOf(TopLevelDestination.Player))
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
                    Image(
                        painter = painterResource(id = R.drawable.app_logo_wordmark),
                        contentDescription = "AFL Edge logo",
                        modifier = Modifier
                            .fillMaxWidth()
                            .height(112.dp)
                            .padding(horizontal = 18.dp, vertical = 20.dp),
                        contentScale = ContentScale.Fit,
                    )
                    HorizontalDivider()
                    topLevelDestinations.forEachIndexed { index, destination ->
                        val selected = currentDestination.route == destination.route
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
                                coroutineScope.launch {
                                    pagerState.animateScrollToPage(index)
                                    drawerState.close()
                                }
                            },
                        )
                    }
                }
            },
        ) {
            HorizontalPager(
                state = pagerState,
                modifier = Modifier.fillMaxSize(),
                key = { topLevelDestinations[it].route },
            ) { page ->
                when (topLevelDestinations[page]) {
                    TopLevelDestination.Player -> {
                        PlayerStatsRoute(
                            repository = container.repository,
                            launchRequest = playerLaunchRequest,
                            onOpenNavigation = ::openDrawer,
                        )
                    }

                    TopLevelDestination.Odds -> {
                        OddsRoute(
                            repository = container.repository,
                            onOpenPlayerRequest = ::openPlayerRequest,
                            onOpenNavigation = ::openDrawer,
                        )
                    }

                    TopLevelDestination.Sgm -> {
                        SgmBuilderRoute(
                            repository = container.repository,
                            draftStore = container.sgmDraftStore,
                            onOpenPlayerRequest = ::openPlayerRequest,
                            onOpenNavigation = ::openDrawer,
                        )
                    }

                    TopLevelDestination.Cgm -> {
                        CgmBuilderRoute(
                            repository = container.repository,
                            onOpenPlayerRequest = ::openPlayerRequest,
                            onOpenNavigation = ::openDrawer,
                        )
                    }

                    TopLevelDestination.Settings -> {
                        SettingsRoute(
                            repository = container.repository,
                            onOpenNavigation = ::openDrawer,
                        )
                    }
                }
            }
        }
    }
}
