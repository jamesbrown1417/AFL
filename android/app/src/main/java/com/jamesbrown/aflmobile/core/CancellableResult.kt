package com.jamesbrown.aflmobile.core

import kotlin.coroutines.cancellation.CancellationException


/**
 * Like [runCatching], but rethrows [CancellationException] so structured
 * cancellation keeps working inside coroutines. Always prefer this over
 * runCatching in ViewModel scopes.
 */
suspend inline fun <T> runCatchingCancellable(block: () -> T): Result<T> =
    try {
        Result.success(block())
    } catch (cancellation: CancellationException) {
        throw cancellation
    } catch (throwable: Throwable) {
        Result.failure(throwable)
    }
