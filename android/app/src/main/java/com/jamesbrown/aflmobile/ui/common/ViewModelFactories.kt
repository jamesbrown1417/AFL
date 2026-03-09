package com.jamesbrown.aflmobile.ui.common

import androidx.lifecycle.ViewModel
import androidx.lifecycle.ViewModelProvider


fun <T : ViewModel> simpleViewModelFactory(creator: () -> T): ViewModelProvider.Factory =
    object : ViewModelProvider.Factory {
        @Suppress("UNCHECKED_CAST")
        override fun <VM : ViewModel> create(modelClass: Class<VM>): VM = creator() as VM
    }
