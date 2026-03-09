package com.jamesbrown.aflmobile

import android.app.Application
import com.jamesbrown.aflmobile.data.repository.AppContainer


class AflApplication : Application() {
    val container: AppContainer by lazy { AppContainer(this) }
}
