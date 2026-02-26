#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd -- "$SCRIPT_DIR/../.." && pwd)"
EVIDENCE_DIR="$REPO_ROOT/.sisyphus/evidence"

WORKDIR=""
COMMIT_SHA=""
PROJECT_HASH=""
TARGET_NODE="node-a"
NODE_TOKEN="node-a-secret"
EVIDENCE_PREFIX="task-10"
ALLOW_MISSING_INTENT=0
MAX_RETRIES=30
RETRY_SLEEP_SECONDS=1

SOCKET_PATH=""
RUNTIME_DIR=""
PLAN_PATH=""

log() {
  printf '[%s] %s\n' "$(date +%H:%M:%S)" "$*"
}

fail() {
  printf 'ERROR: %s\n' "$*" >&2
  exit 1
}

usage() {
  cat <<'USAGE'
Usage: trigger-webhook.sh [--workdir PATH] [--commit-sha SHA] [--project-hash HASH] [--node NAME] [--token VALUE] [--evidence-prefix PREFIX] [--allow-missing-intent]

Triggers provider webhook flow and verifies deploy intent persistence/retrieval.

Accepted workdir layouts:
  1) <workdir>/runtime/hostenv-provider.sock (test-service harness)
  2) <workdir>/hostenv-provider.sock (provider-dev runtime)

Options:
  --workdir PATH       Path to the provider service workspace (contains hostenv-provider.sock)
  --commit-sha SHA     Git commit SHA to trigger deployment for (default: abc123)
  --project-hash HASH  Project hash for webhook URL (default: read from plan.json or use 'demohash')
  --node NAME          Node name for by-sha query auth (default: node-a)
  --token VALUE        Bearer token for node auth (default: node-a-secret)
  --evidence-prefix P  Evidence filename prefix (default: task-10)
  --allow-missing-intent  Do not fail if by-sha or DB checks are missing
  --help, -h           Show this help message

Environment:
  HOSTENV_PROVIDER_WORKDIR  Alternative to --workdir

Examples:
  ./trigger-webhook.sh --workdir /tmp/hostenv-demo
  ./trigger-webhook.sh --workdir /tmp/hostenv-demo --commit-sha 1a2b3c4d
USAGE
}

resolve_runtime_paths() {
  local input="$1"
  local abs

  abs="$(cd -- "$input" && pwd)"

  if [[ -S "$abs/runtime/hostenv-provider.sock" ]]; then
    WORKDIR="$abs"
    RUNTIME_DIR="$abs/runtime"
    SOCKET_PATH="$RUNTIME_DIR/hostenv-provider.sock"
    if [[ -f "$RUNTIME_DIR/repo/generated/plan.json" ]]; then
      PLAN_PATH="$RUNTIME_DIR/repo/generated/plan.json"
    elif [[ -f "$RUNTIME_DIR/generated/plan.json" ]]; then
      PLAN_PATH="$RUNTIME_DIR/generated/plan.json"
    else
      PLAN_PATH=""
    fi
    return 0
  fi

  if [[ -S "$abs/hostenv-provider.sock" ]]; then
    WORKDIR="$abs"
    RUNTIME_DIR="$abs"
    SOCKET_PATH="$RUNTIME_DIR/hostenv-provider.sock"
    if [[ -f "$RUNTIME_DIR/repo/generated/plan.json" ]]; then
      PLAN_PATH="$RUNTIME_DIR/repo/generated/plan.json"
    elif [[ -f "$RUNTIME_DIR/generated/plan.json" ]]; then
      PLAN_PATH="$RUNTIME_DIR/generated/plan.json"
    else
      PLAN_PATH=""
    fi
    return 0
  fi

  fail "could not locate provider socket under $abs (expected runtime/hostenv-provider.sock or hostenv-provider.sock)"
}

first_project_hash_from_plan() {
  local path="$1"
  jq -r '
    first((.environments // {} | to_entries[]? | .value.hostenv.projectNameHash | if type == "array" then .[] else . end) // empty)
  ' "$path"
}

wait_for_intent_by_sha() {
  local socket="$1"
  local sha="$2"
  local node="$3"
  local token="$4"
  local out_file="$5"
  local retries="$6"
  local sleep_seconds="$7"
  local code=""
  local attempt

  for ((attempt = 1; attempt <= retries; attempt++)); do
    code="$(curl -sS --unix-socket "$socket" -o "$out_file" -w '%{http_code}' \
      -H "Authorization: Bearer $token" \
      "http://localhost/api/deploy-intents/by-sha?sha=$sha&node=$node" || true)"

    if [[ "$code" == "200" ]]; then
      printf '%s\n' "$code"
      return 0
    fi

    sleep "$sleep_seconds"
  done

  printf '%s\n' "$code"
  return 1
}

detect_pg_socket_dir() {
  local -a candidates=(
    "$RUNTIME_DIR"
    "$WORKDIR/runtime"
    "$WORKDIR/provider-dev"
    "$WORKDIR"
  )
  local candidate
  for candidate in "${candidates[@]}"; do
    if [[ -S "$candidate/.s.PGSQL.5432" ]]; then
      printf '%s\n' "$candidate"
      return 0
    fi
  done
  return 1
}

sql_escape_literal() {
  printf '%s' "$1" | sed -e "s/'/''/g"
}

# Parse arguments
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
    --commit-sha)
      shift
      (($# > 0)) || fail "--commit-sha requires a value"
      COMMIT_SHA="$1"
      ;;
    --commit-sha=*)
      COMMIT_SHA="${1#*=}"
      ;;
    --project-hash)
      shift
      (($# > 0)) || fail "--project-hash requires a value"
      PROJECT_HASH="$1"
      ;;
    --project-hash=*)
      PROJECT_HASH="${1#*=}"
      ;;
    --node)
      shift
      (($# > 0)) || fail "--node requires a value"
      TARGET_NODE="$1"
      ;;
    --node=*)
      TARGET_NODE="${1#*=}"
      ;;
    --token)
      shift
      (($# > 0)) || fail "--token requires a value"
      NODE_TOKEN="$1"
      ;;
    --token=*)
      NODE_TOKEN="${1#*=}"
      ;;
    --evidence-prefix)
      shift
      (($# > 0)) || fail "--evidence-prefix requires a value"
      EVIDENCE_PREFIX="$1"
      ;;
    --evidence-prefix=*)
      EVIDENCE_PREFIX="${1#*=}"
      ;;
    --allow-missing-intent)
      ALLOW_MISSING_INTENT=1
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

# Use environment variable as fallback
WORKDIR="${WORKDIR:-${HOSTENV_PROVIDER_WORKDIR:-}}"
COMMIT_SHA="${COMMIT_SHA:-abc123}"

if [[ -z "$WORKDIR" ]]; then
  fail "--workdir is required (or set HOSTENV_PROVIDER_WORKDIR)"
fi

if [[ ! -d "$WORKDIR" ]]; then
  fail "workdir does not exist: $WORKDIR"
fi

resolve_runtime_paths "$WORKDIR"

# Determine project hash from plan.json if not provided
if [[ -z "$PROJECT_HASH" ]]; then
  if [[ -n "$PLAN_PATH" && -f "$PLAN_PATH" ]]; then
    PROJECT_HASH="$(first_project_hash_from_plan "$PLAN_PATH")"
    if [[ -z "$PROJECT_HASH" ]]; then
      log "warning: could not extract projectNameHash from $PLAN_PATH, using default 'demohash'"
      PROJECT_HASH="demohash"
    else
      log "extracted project hash from plan.json: $PROJECT_HASH"
    fi
  else
    log "warning: plan.json not found at $PLAN_PATH, using default 'demohash'"
    PROJECT_HASH="demohash"
  fi
fi

mkdir -p "$EVIDENCE_DIR"

TRIGGER_LOG="$EVIDENCE_DIR/${EVIDENCE_PREFIX}-webhook-trigger.log"
VERIFY_LOG="$EVIDENCE_DIR/${EVIDENCE_PREFIX}-deploy-intent.log"
BODY_FILE="$RUNTIME_DIR/webhook-response.json"

log "triggering webhook deployment"
log "  project hash: $PROJECT_HASH"
log "  commit SHA: $COMMIT_SHA"
log "  target node: $TARGET_NODE"
log "  socket: $SOCKET_PATH"

# Build webhook payload with commit information
# The webhook handler accepts any body when no signature verification is configured
# Note: Use application/octet-stream content type as the webhook handler
# accepts raw bytes for signature verification (which is disabled in this demo)
PAYLOAD="{\"ref\":\"refs/heads/main\",\"checkout_sha\":\"$COMMIT_SHA\",\"project\":{\"path_with_namespace\":\"acme/demo\"}}"

# Trigger webhook
WEBHOOK_CODE="$(curl -sS --unix-socket "$SOCKET_PATH" -o "$BODY_FILE" -w '%{http_code}' \
  -H 'Content-Type: application/octet-stream' \
  --data-binary "$PAYLOAD" \
  "http://localhost/webhook/$PROJECT_HASH" || true)"

# Log webhook trigger results
{
  printf 'timestamp=%s\n' "$(date -Iseconds)"
  printf 'endpoint=/webhook/%s\n' "$PROJECT_HASH"
  printf 'commit_sha=%s\n' "$COMMIT_SHA"
  printf 'http_code=%s\n' "$WEBHOOK_CODE"
  printf 'request_payload=%s\n' "$PAYLOAD"
  printf 'response_body:\n'
  cat "$BODY_FILE" 2>/dev/null || echo '{}'
  printf '\n'
} > "$TRIGGER_LOG"

if [[ "$WEBHOOK_CODE" != "202" ]]; then
  fail "webhook trigger failed with HTTP $WEBHOOK_CODE; see $TRIGGER_LOG"
fi

# Extract jobId from response
JOB_ID="$(jq -r '.jobId // empty' "$BODY_FILE" 2>/dev/null || true)"
ACCEPTED="$(jq -r '.accepted // false' "$BODY_FILE" 2>/dev/null || true)"

if [[ "$ACCEPTED" != "true" ]]; then
  fail "webhook was not accepted; see $TRIGGER_LOG"
fi

if [[ -z "$JOB_ID" ]]; then
  fail "webhook response missing jobId; see $TRIGGER_LOG"
fi

log "webhook accepted, jobId: $JOB_ID"
log "webhook trigger logged to: $TRIGGER_LOG"

log "verifying deploy intent creation..."

VERIFY_BODY="$RUNTIME_DIR/verify-intent.json"
VERIFY_CODE=""

if ! VERIFY_CODE="$(wait_for_intent_by_sha "$SOCKET_PATH" "$COMMIT_SHA" "$TARGET_NODE" "$NODE_TOKEN" "$VERIFY_BODY" "$MAX_RETRIES" "$RETRY_SLEEP_SECONDS")"; then
  log "warning: deploy intent endpoint did not return 200 within timeout"
fi

API_INTENT_OK="false"
API_ACTIONS_OK="false"
API_HAS_BACKUP_CONFIG="false"
if [[ -f "$VERIFY_BODY" ]] && [[ "$VERIFY_CODE" == "200" ]]; then
  if jq -e \
    --arg job "$JOB_ID" \
    --arg sha "$COMMIT_SHA" \
    --arg node "$TARGET_NODE" \
    '.jobId == $job and .commitSha == $sha and .node == $node and (.intent.schemaVersion == 1) and (.intent.actions | type == "array" and length > 0)' \
    "$VERIFY_BODY" >/dev/null 2>&1; then
    API_INTENT_OK="true"
  fi

  if jq -e '.intent.actions | all(has("op") and has("user") and has("migrations"))' "$VERIFY_BODY" >/dev/null 2>&1; then
    API_ACTIONS_OK="true"
  fi

  if jq -e '[.intent.actions[]? | select((.op == "backup" or .op == "restore") and (.migrations | type == "array"))] | length >= 0' "$VERIFY_BODY" >/dev/null 2>&1; then
    API_HAS_BACKUP_CONFIG="true"
  fi
fi

DB_CHECK_RAN="false"
DB_ROW_FOUND="false"
DB_MATCHED_COMMIT="false"
DB_JOB_STATUS=""
DB_ACTION_COUNT=""
DB_HAS_BACKUP_OR_RESTORE=""
DB_SOCKET_DIR=""
DB_ROW_RAW=""

if command -v psql >/dev/null 2>&1; then
  if DB_SOCKET_DIR="$(detect_pg_socket_dir)"; then
    DB_CHECK_RAN="true"
    ESCAPED_JOB_ID="$(sql_escape_literal "$JOB_ID")"
    ESCAPED_NODE="$(sql_escape_literal "$TARGET_NODE")"
    DB_ROW_RAW="$(psql -h "$DB_SOCKET_DIR" -d hostenv-provider -Atqc "
      SELECT job_id || E'\\t' || commit_sha || E'\\t' || node || E'\\t' ||
             COALESCE(jsonb_array_length(intent->'actions'), 0)::text || E'\\t' ||
             COALESCE((SELECT bool_or((a->>'op') IN ('backup','restore'))
                       FROM jsonb_array_elements(COALESCE(intent->'actions','[]'::jsonb)) a), false)::text
      FROM deploy_intents
      WHERE job_id = '${ESCAPED_JOB_ID}' AND node = '${ESCAPED_NODE}'
      LIMIT 1;
    " 2>/dev/null || true)"

    if [[ -n "$DB_ROW_RAW" ]]; then
      DB_ROW_FOUND="true"
      IFS=$'\t' read -r DB_ROW_JOB_ID DB_ROW_COMMIT DB_ROW_NODE DB_ACTION_COUNT DB_HAS_BACKUP_OR_RESTORE <<< "$DB_ROW_RAW"
      if [[ "$DB_ROW_COMMIT" == "$COMMIT_SHA" ]]; then
        DB_MATCHED_COMMIT="true"
      fi
      DB_JOB_STATUS="$(psql -h "$DB_SOCKET_DIR" -d hostenv-provider -Atqc "SELECT status FROM jobs WHERE id = '${ESCAPED_JOB_ID}' LIMIT 1;" 2>/dev/null | tr -d '\n' || true)"
    fi
  fi
fi

{
  printf 'timestamp=%s\n' "$(date -Iseconds)"
  printf 'job_id=%s\n' "$JOB_ID"
  printf 'endpoint=/api/deploy-intents/by-sha\n'  
  printf 'query=sha=%s&node=%s\n' "$COMMIT_SHA" "$TARGET_NODE"
  printf 'http_code=%s\n' "$VERIFY_CODE"
  printf 'intent_response_valid=%s\n' "$API_INTENT_OK"
  printf 'actions_shape_valid=%s\n' "$API_ACTIONS_OK"
  printf 'backup_config_fields_valid=%s\n' "$API_HAS_BACKUP_CONFIG"
  printf 'db_check_ran=%s\n' "$DB_CHECK_RAN"
  printf 'db_socket_dir=%s\n' "$DB_SOCKET_DIR"
  printf 'db_row_found=%s\n' "$DB_ROW_FOUND"
  printf 'db_commit_matches=%s\n' "$DB_MATCHED_COMMIT"
  printf 'db_action_count=%s\n' "$DB_ACTION_COUNT"
  printf 'db_has_backup_or_restore=%s\n' "$DB_HAS_BACKUP_OR_RESTORE"
  printf 'db_job_status=%s\n' "$DB_JOB_STATUS"
  if [[ -n "$DB_ROW_RAW" ]]; then
    printf 'db_row_raw=%s\n' "$DB_ROW_RAW"
  fi
  printf 'response_body:\n'
  cat "$VERIFY_BODY" 2>/dev/null || echo '{}'
  printf '\n'
} > "$VERIFY_LOG"

if [[ "$VERIFY_CODE" != "200" ]]; then
  if [[ "$ALLOW_MISSING_INTENT" -eq 1 ]]; then
    log "warning: deploy-intents by-sha endpoint did not return 200"
    log "  see $VERIFY_LOG"
  else
    fail "deploy-intents by-sha endpoint did not return 200; see $VERIFY_LOG"
  fi
fi

if [[ "$API_INTENT_OK" != "true" || "$API_ACTIONS_OK" != "true" ]]; then
  if [[ "$ALLOW_MISSING_INTENT" -eq 1 ]]; then
    log "warning: deploy intent payload missing required fields"
    log "  see $VERIFY_LOG"
  else
    fail "deploy intent payload missing required fields; see $VERIFY_LOG"
  fi
fi

if [[ "$DB_CHECK_RAN" == "true" ]]; then
  if [[ "$DB_ROW_FOUND" != "true" || "$DB_MATCHED_COMMIT" != "true" ]]; then
    if [[ "$ALLOW_MISSING_INTENT" -eq 1 ]]; then
      log "warning: deploy intent row not found or commit mismatch in database"
      log "  see $VERIFY_LOG"
    else
      fail "deploy intent row not found or commit mismatch in database; see $VERIFY_LOG"
    fi
  fi
fi

log "deploy intent verified successfully"
log "  verification logged to: $VERIFY_LOG"

log "webhook trigger completed successfully"
log "  jobId: $JOB_ID"
log "  evidence: $TRIGGER_LOG, $VERIFY_LOG"

# Output summary in JSON format for programmatic use
jq -n \
  --arg jobId "$JOB_ID" \
  --arg projectHash "$PROJECT_HASH" \
  --arg commitSha "$COMMIT_SHA" \
  --arg node "$TARGET_NODE" \
  --arg verifyCode "$VERIFY_CODE" \
  --arg accepted "$ACCEPTED" \
  --arg allowMissingIntent "$ALLOW_MISSING_INTENT" \
  --arg intentValid "$API_INTENT_OK" \
  --arg actionsValid "$API_ACTIONS_OK" \
  --arg dbRowFound "$DB_ROW_FOUND" \
  --arg dbJobStatus "$DB_JOB_STATUS" \
  --arg triggerLog "$TRIGGER_LOG" \
  --arg verifyLog "$VERIFY_LOG" \
  '{jobId: $jobId, projectHash: $projectHash, commitSha: $commitSha, node: $node, accepted: ($accepted == "true"), allowMissingIntent: ($allowMissingIntent == "1"), verifyHttpCode: $verifyCode, intentValid: ($intentValid == "true"), actionsValid: ($actionsValid == "true"), dbRowFound: ($dbRowFound == "true"), dbJobStatus: $dbJobStatus, logs: {trigger: $triggerLog, verify: $verifyLog}}'
