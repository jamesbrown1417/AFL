package com.jamesbrown.aflmobile

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import com.jamesbrown.aflmobile.ui.AflApp
import com.jamesbrown.aflmobile.ui.theme.AflEdgeTheme


class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContent {
            AflEdgeTheme {
                AflApp()
            }
        }
    }
}
