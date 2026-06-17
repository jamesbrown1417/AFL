#!/usr/bin/env bash
set -euo pipefail

export PATH="/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH"

REPO_ROOT="/Users/jamesbrown/Projects/AFL"
BACKEND_DIR="$REPO_ROOT/backend"
WEB_DIR="$REPO_ROOT/web"
LOG_DIR="$REPO_ROOT/runtime/logs"
BACKEND_URL="http://127.0.0.1:8000/docs"
WEB_URL="http://127.0.0.1:5173/"

mkdir -p "$LOG_DIR"

wait_for_url() {
  local url="$1"
  local label="$2"
  local attempts="${3:-45}"

  for _ in $(seq 1 "$attempts"); do
    if curl -fsS "$url" >/dev/null 2>&1; then
      echo "$label is ready: $url"
      return 0
    fi
    sleep 1
  done

  echo "$label did not become ready: $url" >&2
  return 1
}

start_backend() {
  if lsof -nP -iTCP:8000 -sTCP:LISTEN >/dev/null 2>&1; then
    echo "Backend already running on 127.0.0.1:8000"
    return 0
  fi

  echo "Starting backend..."
  cd "$BACKEND_DIR"
  nohup ./scripts/run_dev.sh > "$LOG_DIR/backend.automator.log" 2>&1 < /dev/null &
}

start_web() {
  if lsof -nP -iTCP:5173 -sTCP:LISTEN >/dev/null 2>&1; then
    echo "Stopping existing web server on 127.0.0.1:5173"
    lsof -tiTCP:5173 -sTCP:LISTEN | xargs kill
    sleep 1
  fi

  echo "Starting web app..."
  cd "$WEB_DIR"
  nohup env CI=1 npm run dev -- --host 127.0.0.1 > "$LOG_DIR/web.automator.log" 2>&1 < /dev/null &
}

start_backend
start_web

wait_for_url "$BACKEND_URL" "Backend"
wait_for_url "$WEB_URL" "Web app"

open "$WEB_URL"
