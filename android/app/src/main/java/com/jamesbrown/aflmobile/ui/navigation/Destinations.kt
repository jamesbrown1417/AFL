package com.jamesbrown.aflmobile.ui.navigation


sealed class TopLevelDestination(
    val route: String,
    val label: String,
) {
    data object Player : TopLevelDestination("player", "Player")
    data object Odds : TopLevelDestination("odds", "Odds")
    data object Sgm : TopLevelDestination("sgm", "SGM")
    data object Cgm : TopLevelDestination("cgm", "CGM")
    data object Settings : TopLevelDestination("settings", "Settings")
}

data class PlayerLaunchRequest(
    val requestId: Long,
    val playerId: Int,
    val playerName: String,
    val marketTypeCode: String,
    val lineValue: Double?,
)
