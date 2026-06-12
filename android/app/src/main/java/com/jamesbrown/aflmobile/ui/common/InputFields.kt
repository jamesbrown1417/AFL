package com.jamesbrown.aflmobile.ui.common

import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Add
import androidx.compose.material.icons.outlined.Remove
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.input.KeyboardType
import kotlin.math.roundToLong

/**
 * Numeric entry tuned for values that move in fixed increments (lines, odds,
 * margins, game counts). The -/+ buttons step by [step] and clamp to
 * [minValue]/[maxValue]; typed input is sanitised to a valid number as you
 * type, and the keyboard is numeric-only.
 */
@Composable
fun StepperField(
    value: String,
    onValueChange: (String) -> Unit,
    label: String,
    modifier: Modifier = Modifier,
    step: Double = 1.0,
    minValue: Double? = null,
    maxValue: Double? = null,
    allowDecimal: Boolean = true,
    allowNegative: Boolean = false,
    prefix: String? = null,
) {
    OutlinedTextField(
        value = value,
        onValueChange = { raw -> onValueChange(sanitizeNumericInput(raw, allowDecimal, allowNegative)) },
        modifier = modifier,
        singleLine = true,
        label = { Text(label) },
        prefix = prefix?.let { { Text(it) } },
        leadingIcon = {
            IconButton(onClick = { onValueChange(stepNumericValue(value, -step, minValue, maxValue, allowDecimal)) }) {
                Icon(Icons.Outlined.Remove, contentDescription = "Decrease $label")
            }
        },
        trailingIcon = {
            IconButton(onClick = { onValueChange(stepNumericValue(value, step, minValue, maxValue, allowDecimal)) }) {
                Icon(Icons.Outlined.Add, contentDescription = "Increase $label")
            }
        },
        keyboardOptions = KeyboardOptions(
            keyboardType = if (allowDecimal) KeyboardType.Decimal else KeyboardType.Number,
        ),
    )
}

private fun sanitizeNumericInput(raw: String, allowDecimal: Boolean, allowNegative: Boolean): String {
    val builder = StringBuilder()
    var seenDecimal = false
    raw.forEachIndexed { index, char ->
        when {
            char.isDigit() -> builder.append(char)
            char == '-' && allowNegative && index == 0 -> builder.append(char)
            (char == '.' || char == ',') && allowDecimal && !seenDecimal -> {
                builder.append('.')
                seenDecimal = true
            }
        }
    }
    return builder.toString()
}

private fun stepNumericValue(
    current: String,
    delta: Double,
    minValue: Double?,
    maxValue: Double?,
    allowDecimal: Boolean,
): String {
    var next = (current.toDoubleOrNull() ?: 0.0) + delta
    minValue?.let { next = next.coerceAtLeast(it) }
    maxValue?.let { next = next.coerceAtMost(it) }
    if (!allowDecimal) {
        return next.roundToLong().toString()
    }
    // Steps are halves/quarters, so two decimals always round-trips cleanly.
    val rounded = (next * 100).roundToLong() / 100.0
    return if (rounded % 1.0 == 0.0) {
        rounded.toLong().toString()
    } else {
        rounded.toString()
    }
}
