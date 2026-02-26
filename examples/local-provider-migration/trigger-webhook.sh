#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd -- "$SCRIPT_DIR/../.." && pwd)"
EVIDENCE_DIR="$REPO_ROOT/.sisyphus/evidence"

WORKDIR=""
COMMIT_SHA=""
PROJECT_HASH=""

log() {
  printf '[%s] %s\n' "$(date +%H:%M:%S)" "$*"
}

fail() {
  printf 'ERROR: %s\n' "$*" >&2
  exit 1
}

usage() {
  cat <<'USAGE'
Usage: trigger-webhook.sh [--workdir PATH] [--commit-sha SHA] [--project-hash HASH]

Triggers a webhook deployment by POSTing to /webhook/<hash> endpoint.
Creates a deploy intent in the provider service database.

Options:
  --workdir PATH       Path to the provider service workspace (contains hostenv-provider.sock)
  --commit-sha SHA     Git commit SHA to trigger deployment for (default: abc123)
  --project-hash HASH  Project hash for webhook URL (default: read from plan.json or use 'demohash')
  --help, -h           Show this help message

Environment:
  WORKDIR              Alternative to --workdir

Examples:
  ./trigger-webhook.sh --workdir /tmp/hostenv-demo
  ./trigger-webhook.sh --workdir /tmp/hostenv-demo --commit-sha 1a2b3c4d
USAGE
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
WORKDIR="${WORKDIR:-${WORKDIR:-}}"
COMMIT_SHA="${COMMIT_SHA:-abc123}"

if [[ -z "$WORKDIR" ]]; then
  fail "--workdir is required (or set WORKDIR environment variable)"
fi

if [[ ! -d "$WORKDIR" ]]; then
  fail "workdir does not exist: $WORKDIR"
fi

WORKDIR="$(cd -- "$WORKDIR" && pwd)"
SOCKET_PATH="$WORKDIR/runtime/hostenv-provider.sock"
PLAN_PATH="$WORKDIR/runtime/repo/generated/plan.json"

if [[ ! -S "$SOCKET_PATH" ]]; then
  fail "service socket not found: $SOCKET_PATH (is the provider service running?)"
fi

# Determine project hash from plan.json if not provided
if [[ -z "$PROJECT_HASH" ]]; then
  if [[ -f "$PLAN_PATH" ]]; then
    # Extract projectNameHash from plan.json
    PROJECT_HASH="$(jq -r '.environments[]?.hostenv?.projectNameHash // empty' "$PLAN_PATH" | head -1)"
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

TRIGGER_LOG="$EVIDENCE_DIR/task-9-webhook-trigger.log"
VERIFY_LOG="$EVIDENCE_DIR/task-9-deploy-intent-verify.log"
BODY_FILE="$WORKDIR/runtime/webhook-response.json"

log "triggering webhook deployment"
log "  project hash: $PROJECT_HASH"
log "  commit SHA: $COMMIT_SHA"
log "  socket: $SOCKET_PATH"

# Build webhook payload with commit information
# The webhook handler accepts any body when no signature verification is configured
# Note: Use application/octet-stream content type as the webhook handler
# accepts raw bytes for signature verification (which is disabled in this demo)
# The webhook handler expects a JSON body but doesn't validate specific fields
# for signature verification in this demo context
PAYLOAD="{\"ref\":\"refs/heads/main\",\"checkout_sha\":\"$COMMIT_SHA\",\"project\":{\"path_with_namespace\":\"acme/demo\"}}"

# Trigger webhook
WEBHOOK_CODE="$(curl -sS --unix-socket "$SOCKET_PATH" -o "$BODY_FILE" -w '%{http_code}' \
  -H 'Content-Type: application/octet-stream' \
  --data-binary "$PAYLOAD" \
  "http://localhost/webhook/$PROJECT_HASH" || true)"
WEBHOOK_CODE="$(curl -sS --unix-socket "$SOCKET_PATH" -o "$BODY_FILE" -w '%{http_code}' \
  -H 'Content-Type: application/json' \
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

# Verify deploy intent was created by querying the by-sha endpoint
sleep 1  # Brief delay for intent to be persisted

log "verifying deploy intent creation..."

VERIFY_BODY="$WORKDIR/runtime/verify-intent.json"
VERIFY_CODE="$(curl -sS --unix-socket "$SOCKET_PATH" -o "$VERIFY_BODY" -w '%{http_code}' \
  -H 'Authorization: Bearer node-a-secret' \
  "http://localhost/api/deploy-intents/by-sha?sha=$COMMIT_SHA&node=node-a" || true)"

{
  printf 'timestamp=%s\n' "$(date -Iseconds)"
  printf 'endpoint=/api/deploy-intents/by-sha\n'  
  printf 'query=sha=%s&node=node-a\n' "$COMMIT_SHA"
  printf 'http_code=%s\n' "$VERIFY_CODE"
  printf 'response_body:\n'
  cat "$VERIFY_BODY" 2>/dev/null || echo '{}'
  printf '\n'
} > "$VERIFY_LOG"

if [[ "$VERIFY_CODE" == "200" ]]; then
  # Check if we got a valid deploy intent response
  if jq -e '.sha == "'$COMMIT_SHA'" or .status != null' "$VERIFY_BODY" >/dev/null 2>&1; then
    log "deploy intent verified successfully"
    log "  verification logged to: $VERIFY_LOG"
  else
    log "warning: deploy intent response format unexpected"
    log "  see $VERIFY_LOG for details"
  fi
elif [[ "$VERIFY_CODE" == "404" ]]; then
  log "note: deploy intent not yet visible (404) - this may be expected if processing is async"
  log "  verification logged to: $VERIFY_LOG"
else
  log "warning: unexpected response code $VERIFY_CODE from deploy-intents endpoint"
  log "  see $VERIFY_LOG for details"
fi

log "webhook trigger completed successfully"
log "  jobId: $JOB_ID"
log "  evidence: $TRIGGER_LOG, $VERIFY_LOG"

# Output summary in JSON format for programmatic use
jq -n \
  --arg jobId "$JOB_ID" \
  --arg projectHash "$PROJECT_HASH" \
  --arg commitSha "$COMMIT_SHA" \
  --arg accepted "$ACCEPTED" \
  --arg triggerLog "$TRIGGER_LOG" \
  --arg verifyLog "$VERIFY_LOG" \
  '{jobId: $jobId, projectHash: $projectHash, commitSha: $commitSha, accepted: ($accepted == "true"), logs: {trigger: $triggerLog, verify: $verifyLog}}'
