package com.jamesbrown.aflmobile.ui.common

import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.Info
import androidx.compose.material.icons.outlined.Menu
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
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import com.jamesbrown.aflmobile.data.repository.AflRepository
import com.jamesbrown.aflmobile.model.DataFileSection
import com.jamesbrown.aflmobile.model.DataStatusResponse
import com.jamesbrown.aflmobile.ui.theme.appCardColors
import com.jamesbrown.aflmobile.ui.theme.appGlassBorder
import kotlinx.coroutines.launch

@Composable
fun DataStatusNavigationIcons(
    repository: AflRepository,
    onOpenNavigation: () -> Unit,
) {
    var showSheet by remember { mutableStateOf(false) }
    var isLoading by remember { mutableStateOf(false) }
    var response by remember { mutableStateOf<DataStatusResponse?>(null) }
    var errorMessage by remember { mutableStateOf<String?>(null) }
    val scope = rememberCoroutineScope()

    fun loadStatus() {
        isLoading = true
        errorMessage = null
        scope.launch {
            runCatching { repository.dataStatus() }
                .onSuccess { response = it }
                .onFailure { errorMessage = it.message ?: "Failed to load data status." }
            isLoading = false
        }
    }

    Row(verticalAlignment = Alignment.CenterVertically) {
        IconButton(onClick = onOpenNavigation) {
            Icon(Icons.Outlined.Menu, contentDescription = "Open navigation")
        }
        IconButton(
            onClick = {
                showSheet = true
                loadStatus()
            },
        ) {
            Icon(Icons.Outlined.Info, contentDescription = "Data status")
        }
    }

    if (showSheet) {
        DataStatusSheet(
            response = response,
            isLoading = isLoading,
            errorMessage = errorMessage,
            onRefresh = ::loadStatus,
            onDismiss = { showSheet = false },
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
        containerColor = MaterialTheme.colorScheme.surface.copy(alpha = 0.98f),
        scrimColor = MaterialTheme.colorScheme.scrim.copy(alpha = 0.26f),
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
                    Text("Data status", style = MaterialTheme.typography.headlineSmall)
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
                errorMessage != null -> ErrorCard(errorMessage)
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
        border = appGlassBorder(),
    ) {
        Column(
            modifier = Modifier.padding(16.dp),
            verticalArrangement = Arrangement.spacedBy(10.dp),
        ) {
            Text(
                section.title,
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
