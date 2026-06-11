package com.jamesbrown.aflmobile.ui

import androidx.compose.foundation.layout.WindowInsets
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.pager.HorizontalPager
import androidx.compose.foundation.pager.rememberPagerState
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.Hub
import androidx.compose.material.icons.filled.Person
import androidx.compose.material.icons.filled.Settings
import androidx.compose.material.icons.filled.SportsFootball
import androidx.compose.material.icons.filled.TrendingUp
import androidx.compose.material.icons.outlined.Hub
import androidx.compose.material.icons.outlined.Person
import androidx.compose.material.icons.outlined.Settings
import androidx.compose.material.icons.outlined.SportsFootball
import androidx.compose.material.icons.outlined.TrendingUp
import androidx.compose.material3.Icon
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.NavigationBar
import androidx.compose.material3.NavigationBarItem
import androidx.compose.material3.Scaffold
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.platform.LocalContext
import com.jamesbrown.aflmobile.AflApplication
import com.jamesbrown.aflmobile.ui.navigation.PlayerLaunchRequest
import com.jamesbrown.aflmobile.ui.navigation.TopLevelDestination
import com.jamesbrown.aflmobile.ui.screens.cgm.CgmBuilderRoute
import com.jamesbrown.aflmobile.ui.screens.props.OddsRoute
import com.jamesbrown.aflmobile.ui.screens.props.PlayerStatsRoute
import com.jamesbrown.aflmobile.ui.screens.settings.SettingsRoute
import com.jamesbrown.aflmobile.ui.screens.sgm.SgmBuilderRoute
import com.jamesbrown.aflmobile.ui.theme.appNavigationBarItemColors
import kotlinx.coroutines.launch


private data class DestinationIcons(
    val selected: ImageVector,
    val unselected: ImageVector,
)

private fun iconsFor(destination: TopLevelDestination): DestinationIcons = when (destination) {
    TopLevelDestination.Player -> DestinationIcons(Icons.Filled.Person, Icons.Outlined.Person)
    TopLevelDestination.Odds -> DestinationIcons(Icons.Filled.TrendingUp, Icons.Outlined.TrendingUp)
    TopLevelDestination.Sgm -> DestinationIcons(Icons.Filled.SportsFootball, Icons.Outlined.SportsFootball)
    TopLevelDestination.Cgm -> DestinationIcons(Icons.Filled.Hub, Icons.Outlined.Hub)
    TopLevelDestination.Settings -> DestinationIcons(Icons.Filled.Settings, Icons.Outlined.Settings)
}

@Composable
fun AflApp() {
    val context = LocalContext.current.applicationContext as AflApplication
    val container = context.container
    val coroutineScope = rememberCoroutineScope()
    val topLevelDestinations = listOf(
        TopLevelDestination.Player,
        TopLevelDestination.Odds,
        TopLevelDestination.Sgm,
        TopLevelDestination.Cgm,
        TopLevelDestination.Settings,
    )
    val pagerState = rememberPagerState(
        initialPage = topLevelDestinations.indexOf(TopLevelDestination.Odds),
        pageCount = { topLevelDestinations.size },
    )
    var playerLaunchRequest by remember { mutableStateOf<PlayerLaunchRequest?>(null) }

    fun openPlayerRequest(request: PlayerLaunchRequest) {
        playerLaunchRequest = request
        coroutineScope.launch {
            pagerState.animateScrollToPage(topLevelDestinations.indexOf(TopLevelDestination.Player))
        }
    }

    Scaffold(
        containerColor = MaterialTheme.colorScheme.background,
        contentWindowInsets = WindowInsets(0, 0, 0, 0),
        bottomBar = {
            NavigationBar(
                containerColor = MaterialTheme.colorScheme.surfaceContainer,
            ) {
                topLevelDestinations.forEachIndexed { index, destination ->
                    val selected = pagerState.currentPage == index
                    val icons = iconsFor(destination)
                    NavigationBarItem(
                        selected = selected,
                        onClick = {
                            coroutineScope.launch { pagerState.animateScrollToPage(index) }
                        },
                        icon = {
                            Icon(
                                imageVector = if (selected) icons.selected else icons.unselected,
                                contentDescription = null,
                            )
                        },
                        label = { Text(destination.label) },
                        colors = appNavigationBarItemColors(),
                    )
                }
            }
        },
    ) { innerPadding ->
        HorizontalPager(
            state = pagerState,
            modifier = Modifier
                .fillMaxSize()
                .padding(innerPadding),
            // Page changes go through the navigation bar; free-swiping is
            // disabled because every screen contains horizontally scrolling
            // content that would fight the pager gesture.
            userScrollEnabled = false,
            key = { topLevelDestinations[it].route },
        ) { page ->
            when (topLevelDestinations[page]) {
                TopLevelDestination.Player -> {
                    PlayerStatsRoute(
                        repository = container.repository,
                        launchRequest = playerLaunchRequest,
                    )
                }

                TopLevelDestination.Odds -> {
                    OddsRoute(
                        repository = container.repository,
                        onOpenPlayerRequest = ::openPlayerRequest,
                    )
                }

                TopLevelDestination.Sgm -> {
                    SgmBuilderRoute(
                        repository = container.repository,
                        draftStore = container.sgmDraftStore,
                        onOpenPlayerRequest = ::openPlayerRequest,
                    )
                }

                TopLevelDestination.Cgm -> {
                    CgmBuilderRoute(
                        repository = container.repository,
                        onOpenPlayerRequest = ::openPlayerRequest,
                    )
                }

                TopLevelDestination.Settings -> {
                    SettingsRoute(
                        repository = container.repository,
                    )
                }
            }
        }
    }
}
