package com.jamesbrown.aflmobile.ui.common

import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Refresh
import androidx.compose.material3.Card
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.ModalBottomSheet
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.semantics.heading
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import com.jamesbrown.aflmobile.core.runCatchingCancellable
import com.jamesbrown.aflmobile.core.toUserMessage
import com.jamesbrown.aflmobile.data.repository.AflRepository
import com.jamesbrown.aflmobile.model.DataFileSection
import com.jamesbrown.aflmobile.model.DataStatusResponse
import com.jamesbrown.aflmobile.ui.theme.appCardBorder
import com.jamesbrown.aflmobile.ui.theme.appCardColors
import kotlinx.coroutines.launch

/**
 * Self-contained data freshness viewer. Hosts call [DataStatusHost] with a
 * visibility flag; the host loads on open and exposes a refresh action.
 */
@Composable
fun DataStatusHost(
    repository: AflRepository,
    visible: Boolean,
    onDismiss: () -> Unit,
) {
    var isLoading by remember { mutableStateOf(false) }
    var response by remember { mutableStateOf<DataStatusResponse?>(null) }
    var errorMessage by remember { mutableStateOf<String?>(null) }
    val scope = rememberCoroutineScope()

    fun loadStatus() {
        isLoading = true
        errorMessage = null
        scope.launch {
            runCatchingCancellable { repository.dataStatus() }
                .onSuccess { response = it }
                .onFailure { errorMessage = it.toUserMessage("Failed to load data status.") }
            isLoading = false
        }
    }

    if (visible) {
        androidx.compose.runtime.LaunchedEffect(Unit) {
            if (response == null) loadStatus()
        }
        DataStatusSheet(
            response = response,
            isLoading = isLoading,
            errorMessage = errorMessage,
            onRefresh = ::loadStatus,
            onDismiss = onDismiss,
        )
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun DataStatusSheet(
    response: DataStatusResponse?,
    isLoading: Boolean,
    errorMessage: String?,
    onRefresh: () -> Unit,
    onDismiss: () -> Unit,
) {
    ModalBottomSheet(
        onDismissRequest = onDismiss,
        containerColor = MaterialTheme.colorScheme.surface,
    ) {
        Column(
            modifier = Modifier
                .fillMaxWidth()
                .verticalScroll(rememberScrollState())
                .padding(horizontal = 20.dp, vertical = 8.dp),
            verticalArrangement = Arrangement.spacedBy(14.dp),
        ) {
            Row(
                modifier = Modifier.fillMaxWidth(),
                horizontalArrangement = Arrangement.SpaceBetween,
                verticalAlignment = Alignment.CenterVertically,
            ) {
                Column(verticalArrangement = Arrangement.spacedBy(4.dp)) {
                    Text(
                        "Data status",
                        modifier = Modifier.semantics { heading() },
                        style = MaterialTheme.typography.headlineSmall,
                    )
                    response?.let {
                        Text(
                            "Updated ${formatDateTimeInAdelaide(it.generatedAt)}",
                            style = MaterialTheme.typography.bodySmall,
                            color = MaterialTheme.colorScheme.onSurfaceVariant,
                        )
                    }
                }
                IconButton(onClick = onRefresh) {
                    Icon(Icons.Outlined.Refresh, contentDescription = "Refresh status")
                }
            }

            when {
                isLoading -> LoadingCard("Loading file status")
                errorMessage != null -> ErrorCard(errorMessage, onRetry = onRefresh)
                response == null || response.sections.isEmpty() ->
                    EmptyCard("No file status", "No processed or scraped files were found.")
                else -> response.sections.forEach { section ->
                    DataStatusSectionCard(section = section)
                }
            }
        }
    }
}

@Composable
private fun DataStatusSectionCard(
    section: DataFileSection,
) {
    Card(
        modifier = Modifier.fillMaxWidth(),
        colors = appCardColors(),
        border = appCardBorder(),
    ) {
        Column(
            modifier = Modifier.padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(10.dp),
        ) {
            Text(
                section.title,
                modifier = Modifier.semantics { heading() },
                style = MaterialTheme.typography.titleMedium,
                fontWeight = FontWeight.SemiBold,
            )
            section.files.forEach { file ->
                Row(
                    modifier = Modifier.fillMaxWidth(),
                    horizontalArrangement = Arrangement.spacedBy(12.dp),
                    verticalAlignment = Alignment.Top,
                ) {
                    Column(
                        modifier = Modifier.weight(1f),
                        verticalArrangement = Arrangement.spacedBy(2.dp),
                    ) {
                        Text(
                            file.fileName,
                            style = MaterialTheme.typography.bodyMedium,
                            fontWeight = FontWeight.Medium,
                        )
                        Text(
                            file.relativePath,
                            style = MaterialTheme.typography.bodySmall,
                            color = MaterialTheme.colorScheme.onSurfaceVariant,
                        )
                    }
                    Text(
                        formatDateTimeInAdelaide(file.modifiedAt),
                        modifier = Modifier.width(118.dp),
                        style = MaterialTheme.typography.bodySmall,
                        color = MaterialTheme.colorScheme.onSurfaceVariant,
                    )
                }
            }
        }
    }
}
