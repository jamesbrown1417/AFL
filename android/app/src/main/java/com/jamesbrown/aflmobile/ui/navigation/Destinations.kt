package com.jamesbrown.aflmobile.ui.navigation

import android.net.Uri


sealed class TopLevelDestination(
    val route: String,
    val label: String,
) {
    data object Player : TopLevelDestination("player", "Player")
    data object Odds : TopLevelDestination("odds", "Odds")
    data object Sgm : TopLevelDestination("sgm", "SGM")
    data object Settings : TopLevelDestination("settings", "Settings")
}

object Destinations {
    const val EventDetail = "event/{eventId}/{bookmaker}"
    const val MarketSelections = "market/{marketId}/{eventId}/{bookmaker}/{eventLabel}"

    fun eventDetail(eventId: Int, bookmaker: String): String = "event/$eventId/$bookmaker"

    fun marketSelections(
        marketId: Int,
        eventId: Int,
        bookmaker: String,
        eventLabel: String,
    ): String = "market/$marketId/$eventId/$bookmaker/${Uri.encode(eventLabel)}"
}
