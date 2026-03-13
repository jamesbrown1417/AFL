package com.jamesbrown.aflmobile.ui.screens.settings

import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.CloudDone
import androidx.compose.material.icons.outlined.Menu
import androidx.compose.material.icons.outlined.Save
import androidx.compose.material3.Button
import androidx.compose.material3.Card
import androidx.compose.material3.DropdownMenu
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.ExposedDropdownMenuAnchorType
import androidx.compose.material3.ExposedDropdownMenuBox
import androidx.compose.material3.ExposedDropdownMenuDefaults
import androidx.compose.material3.Icon
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Scaffold
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.material3.TopAppBar
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.remember
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import androidx.lifecycle.ViewModel
import androidx.lifecycle.compose.collectAsStateWithLifecycle
import androidx.lifecycle.viewModelScope
import androidx.lifecycle.viewmodel.compose.viewModel
import com.jamesbrown.aflmobile.data.repository.AflRepository
import com.jamesbrown.aflmobile.model.AppSettings
import com.jamesbrown.aflmobile.model.BookmakerSummary
import com.jamesbrown.aflmobile.model.HealthResponse
import com.jamesbrown.aflmobile.ui.common.EmptyCard
import com.jamesbrown.aflmobile.ui.common.ErrorCard
import com.jamesbrown.aflmobile.ui.common.LoadingCard
import com.jamesbrown.aflmobile.ui.common.ScreenPadding
import com.jamesbrown.aflmobile.ui.common.formatDateTime
import com.jamesbrown.aflmobile.ui.common.simpleViewModelFactory
import com.jamesbrown.aflmobile.ui.theme.appCardColors
import com.jamesbrown.aflmobile.ui.theme.appGlassBorder
import com.jamesbrown.aflmobile.ui.theme.appTopBarColors
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.flow.update
import kotlinx.coroutines.launch


data class SettingsUiState(
    val apiBaseUrl: String = "",
    val authToken: String = "",
    val defaultBookmaker: String = "sportsbet",
    val bookmakers: List<BookmakerSummary> = emptyList(),
    val isLoading: Boolean = true,
    val isSaving: Boolean = false,
    val healthResponse: HealthResponse? = null,
    val infoMessage: String? = null,
    val errorMessage: String? = null,
)

class SettingsViewModel(
    private val repository: AflRepository,
) : ViewModel() {
    private val _uiState = MutableStateFlow(SettingsUiState())
    val uiState: StateFlow<SettingsUiState> = _uiState.asStateFlow()

    init {
        viewModelScope.launch {
            val settings = repository.currentSettings()
            val bookmakers = runCatching { repository.bookmakers() }.getOrDefault(emptyList())
            _uiState.update {
                it.copy(
                    apiBaseUrl = settings.apiBaseUrl,
                    authToken = settings.authToken,
                    defaultBookmaker = settings.defaultBookmaker,
                    bookmakers = bookmakers,
                    isLoading = false,
                )
            }
        }
    }

    fun onBaseUrlChanged(value: String) {
        _uiState.update { it.copy(apiBaseUrl = value, infoMessage = null, errorMessage = null) }
    }

    fun onAuthTokenChanged(value: String) {
        _uiState.update { it.copy(authToken = value, infoMessage = null, errorMessage = null) }
    }

    fun onDefaultBookmakerChanged(value: String) {
        _uiState.update { it.copy(defaultBookmaker = value, infoMessage = null, errorMessage = null) }
    }

    fun save() {
        viewModelScope.launch {
            _uiState.update { it.copy(isSaving = true, infoMessage = null, errorMessage = null) }
            runCatching {
                repository.saveSettings(
                    AppSettings(
                        apiBaseUrl = uiState.value.apiBaseUrl,
                        authToken = uiState.value.authToken,
                        defaultBookmaker = uiState.value.defaultBookmaker,
                    ),
                )
            }.onSuccess {
                _uiState.update { it.copy(isSaving = false, infoMessage = "Settings saved.") }
            }.onFailure { error ->
                _uiState.update { it.copy(isSaving = false, errorMessage = error.message ?: "Failed to save settings.") }
            }
        }
    }

    fun testConnection() {
        viewModelScope.launch {
            _uiState.update { it.copy(isSaving = true, infoMessage = null, errorMessage = null) }
            runCatching { repository.health() }
                .onSuccess { response ->
                    _uiState.update {
                        it.copy(
                            isSaving = false,
                            healthResponse = response,
                            infoMessage = "Backend check succeeded.",
                        )
                    }
                }
                .onFailure { error ->
                    _uiState.update {
                        it.copy(isSaving = false, errorMessage = error.message ?: "Health check failed.")
                    }
                }
        }
    }
}

@Composable
fun SettingsRoute(
    repository: AflRepository,
    onOpenNavigation: () -> Unit,
) {
    val viewModel: SettingsViewModel = viewModel(
        factory = simpleViewModelFactory { SettingsViewModel(repository) },
    )
    val uiState by viewModel.uiState.collectAsStateWithLifecycle()
    SettingsScreen(
        uiState = uiState,
        onBaseUrlChanged = viewModel::onBaseUrlChanged,
        onAuthTokenChanged = viewModel::onAuthTokenChanged,
        onDefaultBookmakerChanged = viewModel::onDefaultBookmakerChanged,
        onSave = viewModel::save,
        onTestConnection = viewModel::testConnection,
        onOpenNavigation = onOpenNavigation,
    )
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun SettingsScreen(
    uiState: SettingsUiState,
    onBaseUrlChanged: (String) -> Unit,
    onAuthTokenChanged: (String) -> Unit,
    onDefaultBookmakerChanged: (String) -> Unit,
    onSave: () -> Unit,
    onTestConnection: () -> Unit,
    onOpenNavigation: () -> Unit,
) {
    var bookmakerExpanded by remember { mutableStateOf(false) }

    Scaffold(
        containerColor = androidx.compose.ui.graphics.Color.Transparent,
        topBar = {
            TopAppBar(
                title = { Text("Connection & defaults") },
                colors = appTopBarColors(),
                navigationIcon = {
                    androidx.compose.material3.IconButton(onClick = onOpenNavigation) {
                        Icon(Icons.Outlined.Menu, contentDescription = "Open navigation")
                    }
                },
            )
        },
    ) { innerPadding ->
        LazyColumn(
            modifier = Modifier
                .fillMaxSize()
                .padding(innerPadding),
            contentPadding = ScreenPadding,
            verticalArrangement = Arrangement.spacedBy(12.dp),
        ) {
            item {
                Card(
                    colors = appCardColors(),
                    border = appGlassBorder(),
                ) {
                    Column(
                        modifier = Modifier.padding(16.dp),
                        verticalArrangement = Arrangement.spacedBy(14.dp),
                    ) {
                        Text(
                            text = "Point the app at the backend’s full API base URL, including /api/v1/.",
                            style = MaterialTheme.typography.bodyMedium,
                        )
                        OutlinedTextField(
                            value = uiState.apiBaseUrl,
                            onValueChange = onBaseUrlChanged,
                            modifier = Modifier.fillMaxWidth(),
                            label = { Text("API base URL") },
                            singleLine = true,
                        )
                        OutlinedTextField(
                            value = uiState.authToken,
                            onValueChange = onAuthTokenChanged,
                            modifier = Modifier.fillMaxWidth(),
                            label = { Text("Bearer token") },
                            singleLine = true,
                        )
                        ExposedDropdownMenuBox(
                            expanded = bookmakerExpanded,
                            onExpandedChange = { bookmakerExpanded = !bookmakerExpanded },
                        ) {
                            OutlinedTextField(
                                value = uiState.defaultBookmaker,
                                onValueChange = {},
                                modifier = Modifier
                                    .menuAnchor(ExposedDropdownMenuAnchorType.PrimaryNotEditable)
                                    .fillMaxWidth(),
                                readOnly = true,
                                label = { Text("Default bookmaker") },
                                trailingIcon = {
                                    ExposedDropdownMenuDefaults.TrailingIcon(expanded = bookmakerExpanded)
                                },
                            )
                            DropdownMenu(
                                expanded = bookmakerExpanded,
                                onDismissRequest = { bookmakerExpanded = false },
                            ) {
                                uiState.bookmakers.forEach { bookmaker ->
                                    androidx.compose.material3.DropdownMenuItem(
                                        text = { Text(bookmaker.displayName) },
                                        onClick = {
                                            onDefaultBookmakerChanged(bookmaker.code)
                                            bookmakerExpanded = false
                                        },
                                    )
                                }
                            }
                        }
                        Column(verticalArrangement = Arrangement.spacedBy(8.dp)) {
                            Button(
                                onClick = onSave,
                                modifier = Modifier.fillMaxWidth(),
                                enabled = !uiState.isSaving,
                            ) {
                                Icon(Icons.Outlined.Save, contentDescription = null)
                                Text("Save settings", modifier = Modifier.padding(start = 8.dp))
                            }
                            TextButton(
                                onClick = onTestConnection,
                                modifier = Modifier.fillMaxWidth(),
                                enabled = !uiState.isSaving,
                            ) {
                                Icon(Icons.Outlined.CloudDone, contentDescription = null)
                                Text("Test backend connection", modifier = Modifier.padding(start = 8.dp))
                            }
                        }
                    }
                }
            }

            if (uiState.isLoading) {
                item { LoadingCard("Loading saved settings") }
            }

            uiState.errorMessage?.let { message ->
                item { ErrorCard(message) }
            }

            uiState.infoMessage?.let { message ->
                item {
                    EmptyCard(
                        title = "Status",
                        body = message,
                    )
                }
            }

            uiState.healthResponse?.let { health ->
                item {
                    Card(
                        colors = appCardColors(),
                        border = appGlassBorder(),
                    ) {
                        Column(
                            modifier = Modifier.padding(16.dp),
                            verticalArrangement = Arrangement.spacedBy(8.dp),
                        ) {
                            Text("Backend health", style = MaterialTheme.typography.titleMedium)
                            Text("Status: ${health.status}")
                            Text("Database: ${if (health.databaseOk) "reachable" else "unavailable"}")
                            Text("Last import: ${formatDateTime(health.lastSuccessfulImportAt)}")
                        }
                    }
                }
            }
        }
    }
}
