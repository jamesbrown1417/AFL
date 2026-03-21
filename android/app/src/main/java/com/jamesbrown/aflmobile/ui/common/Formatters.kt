package com.jamesbrown.aflmobile.ui.common

import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import java.time.ZoneId
import java.util.Locale


private val inputFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
private val outputFormatter = DateTimeFormatter.ofPattern("EEE d MMM, h:mm a", Locale.getDefault())
private val adelaideZoneId = ZoneId.of("Australia/Adelaide")
private val adelaideOutputFormatter = DateTimeFormatter.ofPattern("EEE d MMM, h:mm a z", Locale.getDefault())

fun formatDateTime(value: String?): String =
    value?.let {
        runCatching {
            OffsetDateTime.parse(it, inputFormatter).format(outputFormatter)
        }.getOrElse { value }
    } ?: "TBA"

fun formatDateTimeInAdelaide(value: String?): String =
    value?.let {
        runCatching {
            OffsetDateTime.parse(it, inputFormatter)
                .atZoneSameInstant(adelaideZoneId)
                .format(adelaideOutputFormatter)
        }.getOrElse { value }
    } ?: "TBA"

fun formatDecimalPrice(value: Double?): String = value?.let { String.format(Locale.getDefault(), "%.2f", it) } ?: "--"

fun formatPercentage(value: Double?): String =
    value?.let { String.format(Locale.getDefault(), "%.1f%%", it * 100.0) } ?: "--"

fun formatPlayerPositionTag(value: String?): String? {
    val normalized = value
        ?.trim()
        ?.uppercase(Locale.getDefault())
        ?.replace('-', '_')
        ?: return null
    return when (normalized) {
        "KEY_DEFENDER" -> "KDEF"
        "MEDIUM_DEFENDER" -> "MDEF"
        "KEY_FORWARD" -> "KFWD"
        "MEDIUM_FORWARD" -> "MFWD"
        "MIDFIELDER" -> "MID"
        "MIDFIELDER_FORWARD" -> "MID/F"
        "RUCK" -> "RUC"
        else -> normalized.replace('_', ' ')
    }
}

fun formatMatchupDifficultyTag(value: String?): String? {
    val normalized = value?.trim()?.lowercase(Locale.getDefault()) ?: return null
    if (normalized.isBlank()) {
        return null
    }
    return when (normalized) {
        "terrible" -> "TERR"
        "bad" -> "BAD"
        "neutral" -> "NEUT"
        "good" -> "GOOD"
        "excellent" -> "EXCL"
        else -> normalized.uppercase(Locale.getDefault())
    }
}

fun formatWeatherTemperatureTag(value: Double?): String? =
    value?.let { "${it.toInt()}°" }

fun formatWeatherWindTag(value: Double?): String? =
    value?.let { "${it.toInt()}k" }

fun shortAflMatchLabel(matchName: String): String {
    val normalized = matchName.replace(" vs ", " v ", ignoreCase = true)
    val parts = normalized.split(" v ")
    if (parts.size != 2) {
        return matchName
    }
    val home = aflTeamCode(parts[0]) ?: return matchName
    val away = aflTeamCode(parts[1]) ?: return matchName
    return "$home v $away"
}

fun aflTeamCode(teamName: String): String? {
    val normalized = teamName
        .trim()
        .lowercase(Locale.getDefault())
        .replace(".", "")
    return when {
        normalized.contains("port adelaide") || normalized.startsWith("port ") || normalized.contains(" power") -> "PTA"
        normalized.contains("north melbourne") || normalized.contains("kangaroos") -> "NTH"
        normalized == "adelaide" || normalized.contains("adelaide crows") || normalized.endsWith(" crows") -> "ADE"
        normalized.contains("brisbane") -> "BRL"
        normalized.contains("carlton") -> "CAR"
        normalized.contains("collingwood") -> "COL"
        normalized.contains("essendon") -> "ESS"
        normalized.contains("fremantle") -> "FRE"
        normalized.contains("geelong") -> "GEE"
        normalized.contains("gold coast") -> "GCS"
        normalized.contains("greater western sydney") || normalized.contains("gws") -> "GWS"
        normalized.contains("hawthorn") -> "HAW"
        normalized.contains("melbourne") -> "MEL"
        normalized.contains("richmond") -> "RIC"
        normalized.contains("st kilda") -> "STK"
        normalized.contains("sydney") -> "SYD"
        normalized.contains("west coast") -> "WCE"
        normalized.contains("western bulldogs") || normalized.contains("bulldogs") || normalized.contains("footscray") -> "WBD"
        else -> null
    }
}
