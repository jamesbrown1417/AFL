# AFL Edge Android

Personal Android client for the AFL backend in `/Users/jamesbrown/Projects/AFL/backend`.

## Stack

- Kotlin
- Jetpack Compose + Material 3
- Lifecycle ViewModel + Coroutines + StateFlow
- OkHttp + kotlinx.serialization
- DataStore Preferences

## Notes

- Configure the app with a full API base URL that already includes `/api/v1/`.
- Example for emulator testing against the local backend:
  - `http://10.0.2.2:8000/api/v1/`
- Example for a physical device:
  - Use a Tailscale Serve or other private tunnel URL that reaches the backend, since the backend is intentionally bound to `127.0.0.1`.
- If backend auth is enabled, enter the bearer token in the app settings screen.
