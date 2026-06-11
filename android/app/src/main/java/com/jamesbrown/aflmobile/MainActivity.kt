package com.jamesbrown.aflmobile

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.activity.enableEdgeToEdge
import androidx.core.splashscreen.SplashScreen.Companion.installSplashScreen
import androidx.lifecycle.compose.collectAsStateWithLifecycle
import com.jamesbrown.aflmobile.model.AppSettings
import com.jamesbrown.aflmobile.model.AppThemeMode
import com.jamesbrown.aflmobile.ui.AflApp
import com.jamesbrown.aflmobile.ui.theme.AflEdgeTheme


class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        installSplashScreen()
        super.onCreate(savedInstanceState)
        enableEdgeToEdge()
        val repository = (application as AflApplication).container.repository
        setContent {
            val settings = repository.settingsFlow.collectAsStateWithLifecycle(
                initialValue = AppSettings(),
            ).value
            AflEdgeTheme(darkTheme = settings.themeMode == AppThemeMode.DARK) {
                AflApp()
            }
        }
    }
}
