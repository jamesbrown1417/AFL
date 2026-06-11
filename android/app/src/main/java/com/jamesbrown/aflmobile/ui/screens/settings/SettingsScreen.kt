package com.jamesbrown.aflmobile.ui.screens.settings

import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.CloudDone
import androidx.compose.material.icons.outlined.DarkMode
import androidx.compose.material.icons.outlined.LightMode
import androidx.compose.material.icons.outlined.Save
import androidx.compose.material.icons.outlined.Storage
import androidx.compose.material.icons.outlined.Visibility
import androidx.compose.material.icons.outlined.VisibilityOff
import androidx.compose.material3.Button
import androidx.compose.material3.Card
import androidx.compose.material3.DropdownMenu
import androidx.compose.material3.DropdownMenuItem
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.ExposedDropdownMenuAnchorType
import androidx.compose.material3.ExposedDropdownMenuBox
import androidx.compose.material3.ExposedDropdownMenuDefaults
import androidx.compose.material3.Icon
import androidx.compose.material3.IconButton
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.OutlinedButton
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Scaffold
import androidx.compose.material3.SegmentedButton
import androidx.compose.material3.SegmentedButtonDefaults
import androidx.compose.material3.SingleChoiceSegmentedButtonRow
import androidx.compose.material3.SnackbarHost
import androidx.compose.material3.SnackbarHostState
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.material3.TopAppBar
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.semantics.heading
import androidx.compose.ui.semantics.semantics
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.KeyboardType
import androidx.compose.ui.text.input.PasswordVisualTransformation
import androidx.compose.ui.text.input.VisualTransformation
import androidx.compose.ui.unit.dp
import androidx.lifecycle.ViewModel
import androidx.lifecycle.compose.collectAsStateWithLifecycle
import androidx.lifecycle.viewModelScope
import androidx.lifecycle.viewmodel.compose.viewModel
import com.jamesbrown.aflmobile.core.runCatchingCancellable
import com.jamesbrown.aflmobile.core.toUserMessage
import com.jamesbrown.aflmobile.data.repository.AflRepository
import com.jamesbrown.aflmobile.model.AppSettings
import com.jamesbrown.aflmobile.model.AppThemeMode
import com.jamesbrown.aflmobile.model.BookmakerSummary
import com.jamesbrown.aflmobile.model.HealthResponse
import com.jamesbrown.aflmobile.ui.common.DataStatusHost
import com.jamesbrown.aflmobile.ui.common.ErrorCard
import com.jamesbrown.aflmobile.ui.common.LoadingCard
import com.jamesbrown.aflmobile.ui.common.ScreenPadding
import com.jamesbrown.aflmobile.ui.common.appScreenInsets
import com.jamesbrown.aflmobile.ui.common.formatDateTime
import com.jamesbrown.aflmobile.ui.common.simpleViewModelFactory
import com.jamesbrown.aflmobile.ui.theme.appCardBorder
import com.jamesbrown.aflmobile.ui.theme.appCardColors
import com.jamesbrown.aflmobile.ui.theme.appTopBarColors
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.flow.update
import kotlinx.coroutines.launch
import okhttp3.HttpUrl.Companion.toHttpUrlOrNull


data class SettingsUiState(
    val apiBaseUrl: String = "",
    val authToken: String = "",
    val defaultBookmaker: String = "sportsbet",
    val themeMode: AppThemeMode = AppThemeMode.LIGHT,
    val bookmakers: List<BookmakerSummary> = emptyList(),
    val isLoading: Boolean = true,
    val isSaving: Boolean = false,
    val isTesting: Boolean = false,
    val urlError: String? = null,
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
            val bookmakers = runCatchingCancellable { repository.bookmakers() }.getOrDefault(emptyList())
            _uiState.update {
                it.copy(
                    apiBaseUrl = settings.apiBaseUrl,
                    authToken = settings.authToken,
                    defaultBookmaker = settings.defaultBookmaker,
                    themeMode = settings.themeMode,
                    bookmakers = bookmakers,
                    isLoading = false,
                )
            }
        }
    }

    fun onBaseUrlChanged(value: String) {
        _uiState.update {
            it.copy(apiBaseUrl = value, urlError = null, infoMessage = null, errorMessage = null)
        }
    }

    fun onAuthTokenChanged(value: String) {
        _uiState.update { it.copy(authToken = value, infoMessage = null, errorMessage = null) }
    }

    fun onDefaultBookmakerChanged(value: String) {
        _uiState.update { it.copy(defaultBookmaker = value, infoMessage = null, errorMessage = null) }
    }

    fun onThemeModeChanged(value: AppThemeMode) {
        if (uiState.value.themeMode == value) return
        _uiState.update { it.copy(themeMode = value, errorMessage = null) }
        viewModelScope.launch {
            runCatchingCancellable {
                repository.saveThemeMode(value)
            }.onFailure { error ->
                _uiState.update {
                    it.copy(errorMessage = error.toUserMessage("Failed to save appearance."))
                }
            }
        }
    }

    private fun validateUrl(): Boolean {
        val candidate = uiState.value.apiBaseUrl.trim()
        val parsed = candidate.toHttpUrlOrNull()
        return when {
            candidate.isBlank() -> {
                _uiState.update { it.copy(urlError = "Enter the backend's API base URL.") }
                false
            }
            parsed == null -> {
                _uiState.update {
                    it.copy(urlError = "Not a valid URL. Expected something like http://192.168.1.10:8000/api/v1/")
                }
                false
            }
            else -> true
        }
    }

    fun save() {
        if (!validateUrl()) return
        viewModelScope.launch {
            _uiState.update { it.copy(isSaving = true, infoMessage = null, errorMessage = null) }
            runCatchingCancellable {
                repository.saveSettings(
                    AppSettings(
                        apiBaseUrl = uiState.value.apiBaseUrl,
                        authToken = uiState.value.authToken,
                        defaultBookmaker = uiState.value.defaultBookmaker,
                        themeMode = uiState.value.themeMode,
                    ),
                )
            }.onSuccess {
                _uiState.update { it.copy(isSaving = false, infoMessage = "Settings saved.") }
            }.onFailure { error ->
                _uiState.update {
                    it.copy(isSaving = false, errorMessage = error.toUserMessage("Failed to save settings."))
                }
            }
        }
    }

    /** Saves first so the health check hits the URL currently in the field. */
    fun testConnection() {
        if (!validateUrl()) return
        viewModelScope.launch {
            _uiState.update { it.copy(isTesting = true, infoMessage = null, errorMessage = null) }
            runCatchingCancellable {
                repository.saveSettings(
                    AppSettings(
                        apiBaseUrl = uiState.value.apiBaseUrl,
                        authToken = uiState.value.authToken,
                        defaultBookmaker = uiState.value.defaultBookmaker,
                        themeMode = uiState.value.themeMode,
                    ),
                )
                repository.health()
            }
                .onSuccess { response ->
                    _uiState.update {
                        it.copy(
                            isTesting = false,
                            healthResponse = response,
                            infoMessage = "Backend check succeeded.",
                        )
                    }
                }
                .onFailure { error ->
                    _uiState.update {
                        it.copy(
                            isTesting = false,
                            healthResponse = null,
                            errorMessage = error.toUserMessage("Health check failed."),
                        )
                    }
                }
        }
    }

    fun consumeInfoMessage() {
        _uiState.update { it.copy(infoMessage = null) }
    }
}

@Composable
fun SettingsRoute(
    repository: AflRepository,
) {
    val viewModel: SettingsViewModel = viewModel(
        factory = simpleViewModelFactory { SettingsViewModel(repository) },
    )
    val uiState by viewModel.uiState.collectAsStateWithLifecycle()
    SettingsScreen(
        repository = repository,
        uiState = uiState,
        onBaseUrlChanged = viewModel::onBaseUrlChanged,
        onAuthTokenChanged = viewModel::onAuthTokenChanged,
        onDefaultBookmakerChanged = viewModel::onDefaultBookmakerChanged,
        onThemeModeChanged = viewModel::onThemeModeChanged,
        onSave = viewModel::save,
        onTestConnection = viewModel::testConnection,
        onInfoMessageShown = viewModel::consumeInfoMessage,
    )
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun ThemeModeSegmentedControl(
    selected: AppThemeMode,
    onSelected: (AppThemeMode) -> Unit,
    modifier: Modifier = Modifier,
) {
    val options = AppThemeMode.entries
    SingleChoiceSegmentedButtonRow(modifier = modifier.fillMaxWidth()) {
        options.forEachIndexed { index, option ->
            SegmentedButton(
                selected = selected == option,
                onClick = { onSelected(option) },
                shape = SegmentedButtonDefaults.itemShape(index = index, count = options.size),
                colors = SegmentedButtonDefaults.colors(
                    activeContainerColor = MaterialTheme.colorScheme.primary,
                    activeContentColor = MaterialTheme.colorScheme.onPrimary,
                    inactiveContainerColor = MaterialTheme.colorScheme.surfaceContainerHigh,
                    inactiveContentColor = MaterialTheme.colorScheme.onSurface,
                ),
                label = {
                    Row(
                        horizontalArrangement = Arrangement.spacedBy(8.dp),
                        verticalAlignment = Alignment.CenterVertically,
                    ) {
                        Icon(
                            imageVector = when (option) {
                                AppThemeMode.LIGHT -> Icons.Outlined.LightMode
                                AppThemeMode.DARK -> Icons.Outlined.DarkMode
                            },
                            contentDescription = null,
                        )
                        Text(
                            text = when (option) {
                                AppThemeMode.LIGHT -> "Light"
                                AppThemeMode.DARK -> "Dark"
                            },
                            maxLines = 1,
                        )
                    }
                },
            )
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
private fun SettingsScreen(
    repository: AflRepository,
    uiState: SettingsUiState,
    onBaseUrlChanged: (String) -> Unit,
    onAuthTokenChanged: (String) -> Unit,
    onDefaultBookmakerChanged: (String) -> Unit,
    onThemeModeChanged: (AppThemeMode) -> Unit,
    onSave: () -> Unit,
    onTestConnection: () -> Unit,
    onInfoMessageShown: () -> Unit,
) {
    var bookmakerExpanded by remember { mutableStateOf(false) }
    var tokenVisible by remember { mutableStateOf(false) }
    var showDataStatus by remember { mutableStateOf(false) }
    val snackbarHostState = remember { SnackbarHostState() }
    val busy = uiState.isSaving || uiState.isTesting

    LaunchedEffect(uiState.infoMessage) {
        uiState.infoMessage?.let { message ->
            snackbarHostState.showSnackbar(message)
            onInfoMessageShown()
        }
    }

    Scaffold(
        containerColor = MaterialTheme.colorScheme.background,
        contentWindowInsets = appScreenInsets(),
        snackbarHost = { SnackbarHost(snackbarHostState) },
        topBar = {
            TopAppBar(
                title = { Text("Settings") },
                colors = appTopBarColors(),
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
                    border = appCardBorder(),
                ) {
                    Column(
                        modifier = Modifier.padding(16.dp),
                        verticalArrangement = Arrangement.spacedBy(14.dp),
                    ) {
                        Text(
                            "Appearance",
                            modifier = Modifier.semantics { heading() },
                            style = MaterialTheme.typography.titleMedium,
                            fontWeight = FontWeight.SemiBold,
                        )
                        ThemeModeSegmentedControl(
                            selected = uiState.themeMode,
                            onSelected = onThemeModeChanged,
                        )
                    }
                }
            }

            item {
                Card(
                    colors = appCardColors(),
                    border = appCardBorder(),
                ) {
                    Column(
                        modifier = Modifier.padding(16.dp),
                        verticalArrangement = Arrangement.spacedBy(14.dp),
                    ) {
                        Text(
                            "Backend connection",
                            modifier = Modifier.semantics { heading() },
                            style = MaterialTheme.typography.titleMedium,
                            fontWeight = FontWeight.SemiBold,
                        )
                        Text(
                            text = "Point the app at the backend's full API base URL, including /api/v1/.",
                            style = MaterialTheme.typography.bodyMedium,
                            color = MaterialTheme.colorScheme.onSurfaceVariant,
                        )
                        OutlinedTextField(
                            value = uiState.apiBaseUrl,
                            onValueChange = onBaseUrlChanged,
                            modifier = Modifier.fillMaxWidth(),
                            label = { Text("API base URL") },
                            singleLine = true,
                            isError = uiState.urlError != null,
                            supportingText = uiState.urlError?.let { { Text(it) } },
                            keyboardOptions = KeyboardOptions(keyboardType = KeyboardType.Uri),
                        )
                        OutlinedTextField(
                            value = uiState.authToken,
                            onValueChange = onAuthTokenChanged,
                            modifier = Modifier.fillMaxWidth(),
                            label = { Text("Bearer token") },
                            singleLine = true,
                            visualTransformation = if (tokenVisible) {
                                VisualTransformation.None
                            } else {
                                PasswordVisualTransformation()
                            },
                            keyboardOptions = KeyboardOptions(keyboardType = KeyboardType.Password),
                            trailingIcon = {
                                IconButton(onClick = { tokenVisible = !tokenVisible }) {
                                    Icon(
                                        imageVector = if (tokenVisible) {
                                            Icons.Outlined.VisibilityOff
                                        } else {
                                            Icons.Outlined.Visibility
                                        },
                                        contentDescription = if (tokenVisible) "Hide token" else "Show token",
                                    )
                                }
                            },
                        )
                        ExposedDropdownMenuBox(
                            expanded = bookmakerExpanded,
                            onExpandedChange = { bookmakerExpanded = !bookmakerExpanded },
                        ) {
                            OutlinedTextField(
                                value = uiState.bookmakers
                                    .firstOrNull { it.code == uiState.defaultBookmaker }
                                    ?.displayName
                                    ?: uiState.defaultBookmaker,
                                onValueChange = {},
                                modifier = Modifier
                                    .menuAnchor(ExposedDropdownMenuAnchorType.PrimaryNotEditable)
                                    .fillMaxWidth(),
                                readOnly = true,
                                label = { Text("Default agency for builders") },
                                trailingIcon = {
                                    ExposedDropdownMenuDefaults.TrailingIcon(expanded = bookmakerExpanded)
                                },
                            )
                            DropdownMenu(
                                expanded = bookmakerExpanded,
                                onDismissRequest = { bookmakerExpanded = false },
                            ) {
                                uiState.bookmakers.forEach { bookmaker ->
                                    DropdownMenuItem(
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
                                enabled = !busy,
                            ) {
                                Icon(Icons.Outlined.Save, contentDescription = null)
                                Text(
                                    if (uiState.isSaving) "Saving..." else "Save settings",
                                    modifier = Modifier.padding(start = 8.dp),
                                )
                            }
                            TextButton(
                                onClick = onTestConnection,
                                modifier = Modifier.fillMaxWidth(),
                                enabled = !busy,
                            ) {
                                Icon(Icons.Outlined.CloudDone, contentDescription = null)
                                Text(
                                    if (uiState.isTesting) "Testing..." else "Save & test connection",
                                    modifier = Modifier.padding(start = 8.dp),
                                )
                            }
                        }
                    }
                }
            }

            item {
                Card(
                    colors = appCardColors(),
                    border = appCardBorder(),
                ) {
                    Column(
                        modifier = Modifier.padding(16.dp),
                        verticalArrangement = Arrangement.spacedBy(10.dp),
                    ) {
                        Text(
                            "Data freshness",
                            modifier = Modifier.semantics { heading() },
                            style = MaterialTheme.typography.titleMedium,
                            fontWeight = FontWeight.SemiBold,
                        )
                        Text(
                            "Check when each scraped and processed file was last updated.",
                            style = MaterialTheme.typography.bodyMedium,
                            color = MaterialTheme.colorScheme.onSurfaceVariant,
                        )
                        OutlinedButton(
                            onClick = { showDataStatus = true },
                            modifier = Modifier.fillMaxWidth(),
                        ) {
                            Icon(Icons.Outlined.Storage, contentDescription = null)
                            Text("View data status", modifier = Modifier.padding(start = 8.dp))
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

            uiState.healthResponse?.let { health ->
                item {
                    Card(
                        colors = appCardColors(),
                        border = appCardBorder(),
                    ) {
                        Column(
                            modifier = Modifier.padding(16.dp),
                            verticalArrangement = Arrangement.spacedBy(8.dp),
                        ) {
                            Text(
                                "Backend health",
                                modifier = Modifier.semantics { heading() },
                                style = MaterialTheme.typography.titleMedium,
                            )
                            Row {
                                Text("Status: ", color = MaterialTheme.colorScheme.onSurfaceVariant)
                                Text(health.status, fontWeight = FontWeight.SemiBold)
                            }
                            Row {
                                Text("Database: ", color = MaterialTheme.colorScheme.onSurfaceVariant)
                                Text(
                                    if (health.databaseOk) "reachable" else "unavailable",
                                    fontWeight = FontWeight.SemiBold,
                                )
                            }
                            Row {
                                Text("Last import: ", color = MaterialTheme.colorScheme.onSurfaceVariant)
                                Text(formatDateTime(health.lastSuccessfulImportAt), fontWeight = FontWeight.SemiBold)
                            }
                        }
                    }
                }
            }
        }

        DataStatusHost(
            repository = repository,
            visible = showDataStatus,
            onDismiss = { showDataStatus = false },
        )
    }
}
