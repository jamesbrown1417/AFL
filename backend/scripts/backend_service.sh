#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
BACKEND_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$(cd "$BACKEND_DIR/.." && pwd)"
LABEL="com.jamesbrown.afl-backend"
DOMAIN="gui/$(id -u)"
SERVICE_TARGET="$DOMAIN/$LABEL"
PLIST_SOURCE="$BACKEND_DIR/deployment/launchd/$LABEL.plist"
PLIST_DEST="$HOME/Library/LaunchAgents/$LABEL.plist"
STDOUT_LOG="$REPO_ROOT/runtime/logs/backend.stdout.log"
STDERR_LOG="$REPO_ROOT/runtime/logs/backend.stderr.log"

usage() {
    cat <<EOF
Usage: $(basename "$0") <install|restart|status|logs>

  install  Copy the LaunchAgent plist into ~/Library/LaunchAgents and start it
  restart  Restart the installed backend LaunchAgent
  status   Show launchd status for the backend service
  logs     Tail backend stdout/stderr logs
EOF
}

ensure_paths() {
    mkdir -p "$HOME/Library/LaunchAgents" "$REPO_ROOT/runtime/logs"
    chmod +x "$BACKEND_DIR/scripts/run_prod.sh"
}

install_service() {
    ensure_paths
    cp "$PLIST_SOURCE" "$PLIST_DEST"
    chmod 644 "$PLIST_DEST"

    pkill -f 'uvicorn app.main:app --host 127.0.0.1 --port 8000 --workers 1' 2>/dev/null || true
    launchctl bootout "$SERVICE_TARGET" 2>/dev/null || true
    launchctl bootstrap "$DOMAIN" "$PLIST_DEST"
    launchctl enable "$SERVICE_TARGET"
    launchctl kickstart -k "$SERVICE_TARGET"

    echo "Installed and started $LABEL"
    echo "Status: launchctl print $SERVICE_TARGET"
}

restart_service() {
    if [[ ! -f "$PLIST_DEST" ]]; then
        echo "LaunchAgent not installed. Run: $0 install" >&2
        exit 1
    fi
    ensure_paths
    if ! launchctl print "$SERVICE_TARGET" >/dev/null 2>&1; then
        launchctl bootstrap "$DOMAIN" "$PLIST_DEST"
        launchctl enable "$SERVICE_TARGET"
    fi
    launchctl kickstart -k "$SERVICE_TARGET"
    echo "Restarted $LABEL"
}

status_service() {
    launchctl print "$SERVICE_TARGET"
}

logs_service() {
    touch "$STDOUT_LOG" "$STDERR_LOG"
    tail -n 100 -f "$STDOUT_LOG" "$STDERR_LOG"
}

case "${1:-}" in
    install)
        install_service
        ;;
    restart)
        restart_service
        ;;
    status)
        status_service
        ;;
    logs)
        logs_service
        ;;
    *)
        usage
        exit 1
        ;;
esac
