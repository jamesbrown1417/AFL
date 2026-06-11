package com.jamesbrown.aflmobile.core

import com.jamesbrown.aflmobile.data.network.BackendApiException
import java.net.ConnectException
import java.net.SocketTimeoutException
import java.net.UnknownHostException
import java.io.InterruptedIOException


/**
 * Maps low-level failures to copy a person can act on. Raw exception text
 * (host names, serializer traces) never reaches the UI.
 */
fun Throwable.toUserMessage(fallback: String = "Something went wrong."): String =
    when (this) {
        is BackendApiException -> when {
            statusCode == 401 || statusCode == 403 ->
                "The backend rejected the request. Check the bearer token in Settings."
            code == "decode_error" ->
                "The backend sent an unexpected response. It may be running an older version."
            statusCode >= 500 ->
                "The backend hit an internal error. Try again shortly."
            else -> message
        }
        is UnknownHostException, is ConnectException ->
            "Can't reach the backend. Check the API base URL in Settings and that the server is running."
        is SocketTimeoutException, is InterruptedIOException ->
            "The backend took too long to respond. Try again."
        else -> message?.takeIf { it.isNotBlank() } ?: fallback
    }
