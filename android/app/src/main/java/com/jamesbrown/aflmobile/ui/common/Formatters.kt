package com.jamesbrown.aflmobile.ui.common

import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import java.util.Locale


private val inputFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
private val outputFormatter = DateTimeFormatter.ofPattern("EEE d MMM, h:mm a", Locale.getDefault())

fun formatDateTime(value: String?): String =
    value?.let {
        runCatching {
            OffsetDateTime.parse(it, inputFormatter).format(outputFormatter)
        }.getOrElse { value }
    } ?: "TBA"

fun formatDecimalPrice(value: Double?): String = value?.let { String.format(Locale.getDefault(), "%.2f", it) } ?: "--"

fun formatPercentage(value: Double?): String =
    value?.let { String.format(Locale.getDefault(), "%.1f%%", it * 100.0) } ?: "--"
