#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd -- "$SCRIPT_DIR/../.." && pwd)"
EVIDENCE_DIR="$REPO_ROOT/.sisyphus/evidence"
WORKDIR=""
KEEP_WORKDIR=0
ORIGINAL_ARGS=("$@")

log() {
  printf '[%s] %s\n' "$(date +%H:%M:%S)" "$*"
}

fail() {
  printf 'ERROR: %s\n' "$*" >&2
  exit 1
}

usage() {
  cat <<'USAGE'
Usage: test-service.sh [--workdir PATH] [--evidence-dir PATH] [--keep-workdir]

Creates a temporary provider-service workspace, starts hostenv-provider-service,
then verifies:
  - health check over Unix socket
  - /api/deploy-intents/by-sha endpoint
  - webhook POST endpoint without signature verification
  - plan.json is read by webhook hash resolution
USAGE
}

require_cmd() {
  local cmd="$1"
  command -v "$cmd" >/dev/null 2>&1 || fail "missing required command: $cmd"
}

resolve_service_bin() {
  local system
  local drv_path
  system="$(nix eval --impure --raw --expr 'builtins.currentSystem')"
  drv_path="$(nix build "$REPO_ROOT#checks.${system}.hostenv-provider-service-build" --option allow-import-from-derivation true --no-link --print-out-paths)"
  printf '%s/bin/hostenv-provider-service\n' "$drv_path"
}

ensure_tools_available() {
  local -a required=(nix curl jq git initdb pg_ctl createdb)
  local -a missing=()
  local cmd
  for cmd in "${required[@]}"; do
    if ! command -v "$cmd" >/dev/null 2>&1; then
      missing+=("$cmd")
    fi
  done

  if [[ "${#missing[@]}" -eq 0 ]]; then
    return 0
  fi

  if [[ "${HOSTENV_PROVIDER_SERVICE_TEST_BOOTSTRAPPED:-0}" == "1" ]]; then
    fail "required tools are still unavailable after nix shell bootstrap: ${missing[*]}"
  fi

  log "missing tools: ${missing[*]}"
  log "re-running inside nix shell with required runtime tools"
  exec nix shell \
    nixpkgs#postgresql \
    nixpkgs#curl \
    nixpkgs#jq \
    nixpkgs#git \
    --command env HOSTENV_PROVIDER_SERVICE_TEST_BOOTSTRAPPED=1 bash "$0" "$@"
}

wait_for_socket() {
  local socket_path="$1"
  local timeout_seconds="${2:-60}"
  local start
  start="$(date +%s)"
  while true; do
    if [[ -S "$socket_path" ]]; then
      return 0
    fi
    if (( "$(date +%s)" - start >= timeout_seconds )); then
      return 1
    fi
    sleep 1
  done
}

health_check() {
  local socket_path="$1"
  local out_file="$2"
  local code
  local body_file
  body_file="${out_file%.log}.body"

  code="$(curl -sS --unix-socket "$socket_path" -o "$body_file" -w '%{http_code}' http://localhost/health || true)"

  {
    printf 'endpoint=/health\n'
    printf 'http_code=%s\n' "$code"
    printf 'body:\n'
    cat "$body_file"
    printf '\n'
  } > "$out_file"

  if [[ "$code" == "000" ]]; then
    fail "health check failed; see $out_file"
  fi
}

while (($# > 0)); do
  case "$1" in
    --workdir)
      shift
      (($# > 0)) || fail "--workdir requires a value"
      WORKDIR="$1"
      ;;
    --workdir=*)
      WORKDIR="${1#*=}"
      ;;
    --evidence-dir)
      shift
      (($# > 0)) || fail "--evidence-dir requires a value"
      EVIDENCE_DIR="$1"
      ;;
    --evidence-dir=*)
      EVIDENCE_DIR="${1#*=}"
      ;;
    --keep-workdir)
      KEEP_WORKDIR=1
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      fail "unknown argument: $1"
      ;;
  esac
  shift
done

ensure_tools_available "${ORIGINAL_ARGS[@]}"

for cmd in nix curl jq git initdb pg_ctl createdb; do
  require_cmd "$cmd"
done

mkdir -p "$EVIDENCE_DIR"

if [[ -z "$WORKDIR" ]]; then
  WORKDIR="$(mktemp -d /tmp/hostenv-provider-service-test-XXXXXX)"
else
  mkdir -p "$WORKDIR"
fi
WORKDIR="$(cd -- "$WORKDIR" && pwd)"

BASE_DIR="$WORKDIR/runtime"
PGDATA="$BASE_DIR/pgdata"
PGSOCK="$BASE_DIR"
REPO_DIR="$BASE_DIR/repo"
DATA_DIR="$REPO_DIR"
SOCKET_PATH="$BASE_DIR/hostenv-provider.sock"
CONFIG_PATH="$BASE_DIR/provider-config.json"
PLAN_PATH="$REPO_DIR/generated/plan.json"
TOKEN_FILE="$BASE_DIR/node-auth-tokens.yaml"
SERVICE_LOG="$EVIDENCE_DIR/task-4-service.log"
HEALTH_LOG="$EVIDENCE_DIR/task-4-health-check.log"
API_LOG="$EVIDENCE_DIR/task-4-api-test.log"
WEBHOOK_LOG="$EVIDENCE_DIR/task-4-webhook-test.log"
PLAN_LOG="$EVIDENCE_DIR/task-4-plan-read.log"

mkdir -p "$BASE_DIR" "$REPO_DIR/generated"

SERVICE_PID=""

cleanup() {
  local rc=$?
  trap - EXIT
  if [[ -n "$SERVICE_PID" ]] && kill -0 "$SERVICE_PID" >/dev/null 2>&1; then
    kill "$SERVICE_PID" >/dev/null 2>&1 || true
    wait "$SERVICE_PID" >/dev/null 2>&1 || true
  fi
  if [[ -d "$PGDATA" ]] && [[ -f "$PGDATA/PG_VERSION" ]]; then
    pg_ctl -D "$PGDATA" stop >/dev/null 2>&1 || true
  fi
  if [[ "$KEEP_WORKDIR" -eq 1 ]]; then
    log "kept workdir: $WORKDIR"
  else
    rm -rf "$WORKDIR"
  fi
  exit "$rc"
}
trap cleanup EXIT

if [[ ! -s "$PGDATA/PG_VERSION" ]]; then
  initdb -D "$PGDATA" -A trust >/dev/null
fi
pg_ctl -D "$PGDATA" -o "-k $PGSOCK" -l "$BASE_DIR/postgres.log" start >/dev/null
createdb -h "$PGSOCK" hostenv-provider >/dev/null 2>&1 || true

cat > "$TOKEN_FILE" <<'EOF_TOKENS'
node-a: node-a-secret
EOF_TOKENS

cat > "$REPO_DIR/flake.nix" <<'EOF_FLAKE'
{
  description = "provider service harness flake";
}
EOF_FLAKE
printf '{}\n' > "$REPO_DIR/generated/state.json"
printf '{}\n' > "$REPO_DIR/generated/public-envs.json"

cat > "$PLAN_PATH" <<'EOF_PLAN'
{
  "environments": {
    "main": {
      "type": "production",
      "hostenv": {
        "organisation": "acme",
        "project": "demo",
        "projectNameHash": "demohash"
      }
    }
  }
}
EOF_PLAN

if [[ ! -d "$REPO_DIR/.git" ]]; then
  git -C "$REPO_DIR" init -b main >/dev/null
fi

cat > "$CONFIG_PATH" <<EOF_CONFIG
{
  "dataDir": "$DATA_DIR",
  "flakeRoot": ".",
  "listenSocket": "$SOCKET_PATH",
  "webhookSecretFile": null,
  "webhookSecretsDir": null,
  "webhookHost": "localhost",
  "uiBasePath": "/dashboard",
  "uiBaseUrl": "http://localhost",
  "dbUri": "host=$PGSOCK dbname=hostenv-provider",
  "gitCredentialsFile": "$BASE_DIR/git-credentials",
  "gitConfigFile": "$BASE_DIR/gitconfig",
  "flakeTemplate": "flake.template.nix",
  "comin": {
    "enable": true,
    "branch": "main",
    "pollIntervalSeconds": 5,
    "nodeAuthTokensFile": "$TOKEN_FILE"
  }
}
EOF_CONFIG

log "starting hostenv-provider-service"
SERVICE_BIN="$(resolve_service_bin)"
"$SERVICE_BIN" --config "$CONFIG_PATH" > "$SERVICE_LOG" 2>&1 &
SERVICE_PID="$!"

wait_for_socket "$SOCKET_PATH" 120 || fail "service socket did not appear: $SOCKET_PATH"

health_check "$SOCKET_PATH" "$HEALTH_LOG"
log "health check completed -> $HEALTH_LOG"

API_CODE="$(curl -sS --unix-socket "$SOCKET_PATH" -o "$BASE_DIR/api-body.json" -w '%{http_code}' \
  -H 'Authorization: Bearer node-a-secret' \
  'http://localhost/api/deploy-intents/by-sha?sha=abc123&node=node-a' || true)"

{
  printf 'endpoint=/api/deploy-intents/by-sha\n'
  printf 'query=sha=abc123&node=node-a\n'
  printf 'http_code=%s\n' "$API_CODE"
  printf 'body:\n'
  cat "$BASE_DIR/api-body.json"
  printf '\n'
} > "$API_LOG"

if [[ "$API_CODE" != "404" && "$API_CODE" != "200" ]]; then
  fail "deploy-intents endpoint returned unexpected status $API_CODE; see $API_LOG"
fi
log "deploy-intents check completed -> $API_LOG"

WEBHOOK_CODE="$(curl -sS --unix-socket "$SOCKET_PATH" -o "$BASE_DIR/webhook-body.json" -w '%{http_code}' \
  -H 'Content-Type: application/octet-stream' \
  --data-binary '{}' \
  'http://localhost/webhook/demohash' || true)"

{
  printf 'endpoint=/webhook/demohash\n'
  printf 'http_code=%s\n' "$WEBHOOK_CODE"
  printf 'body:\n'
  cat "$BASE_DIR/webhook-body.json"
  printf '\n'
} > "$WEBHOOK_LOG"

if [[ "$WEBHOOK_CODE" != "202" ]]; then
  fail "webhook endpoint did not return 202; see $WEBHOOK_LOG"
fi

if ! jq -e '.accepted == true and (.jobId | type == "string")' "$BASE_DIR/webhook-body.json" >/dev/null 2>&1; then
  fail "webhook response missing expected accepted/jobId fields; see $WEBHOOK_LOG"
fi

UNKNOWN_HASH_CODE="$(curl -sS --unix-socket "$SOCKET_PATH" -o "$BASE_DIR/plan-body.txt" -w '%{http_code}' \
  -H 'Content-Type: application/octet-stream' \
  --data-binary '{}' \
  'http://localhost/webhook/unknownhash' || true)"

{
  printf 'plan_path=%s\n' "$PLAN_PATH"
  printf 'known_hash=demohash webhook_status=%s\n' "$WEBHOOK_CODE"
  printf 'unknown_hash=unknownhash webhook_status=%s\n' "$UNKNOWN_HASH_CODE"
  printf 'note=known hash accepted (202) and unknown hash rejected (404) confirms plan.json hash lookup\n'
} > "$PLAN_LOG"

if [[ "$UNKNOWN_HASH_CODE" != "404" ]]; then
  fail "plan.json hash lookup check failed; expected 404 for unknown hash, got $UNKNOWN_HASH_CODE"
fi

log "webhook check completed -> $WEBHOOK_LOG"
log "plan.json read check completed -> $PLAN_LOG"
log "provider-service API harness completed successfully"
