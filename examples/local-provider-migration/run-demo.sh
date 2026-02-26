#!/usr/bin/env bash
set -euo pipefail

ORIGINAL_ARGS=("$@")

usage() {
  cat <<'USAGE'
Usage: run-demo.sh [--workdir PATH] [--cleanup] [--teardown] [--automated] [--no-color]

Modes:
  default         Interactive wizard that guides you through the demo.
  --automated     Run the full demo end-to-end without prompts.
  --teardown      Stop old demo VMs and remove old demo workdirs, then exit.

Options:
  --workdir PATH  Reuse/create a specific work directory
  --cleanup       Remove the work directory at exit
  --no-color      Disable ANSI colors in script output
  --help          Show this help text
USAGE
}

COLOR_RESET=""
COLOR_INFO=""
COLOR_WARN=""
COLOR_GOOD=""
COLOR_HEAD=""
NO_COLOR=0
USE_GUM=0
HOSTCTL_IN_USE=0
HOSTCTL_PROFILE=""
HOSTCTL_SOURCE_FILE=""
HOSTCTL_CMD=()
DEMO_FAILED=0

setup_colors() {
  if [[ "$NO_COLOR" -eq 1 ]] || [[ ! -t 1 ]]; then
    return 0
  fi

  COLOR_RESET=$'\033[0m'
  COLOR_INFO=$'\033[36m'
  COLOR_WARN=$'\033[33m'
  COLOR_GOOD=$'\033[32m'
  COLOR_HEAD=$'\033[1;34m'
}

log() {
  printf '[%s] %s\n' "$(date +%H:%M:%S)" "$*"
}

info() {
  printf '%s%s%s\n' "$COLOR_INFO" "$*" "$COLOR_RESET"
}

warn() {
  printf '%s%s%s\n' "$COLOR_WARN" "$*" "$COLOR_RESET"
}

success() {
  printf '%s%s%s\n' "$COLOR_GOOD" "$*" "$COLOR_RESET"
}

stage() {
  if can_use_gum; then
    echo
    gum style --bold --foreground 63 --border rounded --border-foreground 240 --padding "0 1" "== $* =="
    return 0
  fi
  printf '\n%s== %s ==%s\n' "$COLOR_HEAD" "$*" "$COLOR_RESET"
}

ISSUE_URL="https://gitlab.com/woolwichweb/hostenv/-/issues/new"

show_issue_hint() {
  echo "If this looks like a hostenv bug, please open an issue: $ISSUE_URL" >&2
}

fail() {
  DEMO_FAILED=1
  echo "ERROR: $*" >&2
  exit 1
}

fail_stage() {
  DEMO_FAILED=1
  echo "ERROR: $*" >&2
  if [[ -n "${LOG_DIR:-}" ]]; then
    echo "VM logs: $LOG_DIR" >&2
  fi
  show_issue_hint
  exit 1
}

require_cmd() {
  local cmd="$1"
  command -v "$cmd" >/dev/null 2>&1 || fail "missing required command: $cmd"
}

ensure_demo_tools_available() {
  local -a args=("$@")
  local need_shell=0
  local -a shell_inputs=()

  if ! command -v hostctl >/dev/null 2>&1; then
    need_shell=1
    shell_inputs+=(nixpkgs#hostctl)
  fi

  if [[ "$MODE" == "wizard" && "$TEARDOWN_ONLY" -eq 0 ]] && ! command -v gum >/dev/null 2>&1; then
    need_shell=1
    shell_inputs+=(nixpkgs#gum)
  fi

  if [[ "$need_shell" -eq 0 ]]; then
    if [[ "$MODE" == "wizard" && "$TEARDOWN_ONLY" -eq 0 ]] && command -v gum >/dev/null 2>&1; then
      USE_GUM=1
    fi
    return 0
  fi

  command -v nix >/dev/null 2>&1 || fail "missing required command: nix (needed to bootstrap demo tools)"
  [[ "${HOSTENV_DEMO_TOOLS_BOOTSTRAPPED:-0}" != "1" ]] || fail "required demo tools are still unavailable after nix bootstrap"

  local script_path
  script_path="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)/$(basename -- "${BASH_SOURCE[0]}")"
  info "missing demo tools; re-running this script inside 'nix shell ${shell_inputs[*]}'"
  exec nix shell "${shell_inputs[@]}" --command env HOSTENV_DEMO_TOOLS_BOOTSTRAPPED=1 bash "$script_path" "${args[@]}"
}

can_use_gum() {
  [[ "${MODE:-wizard}" == "wizard" ]] || return 1
  [[ "${USE_GUM:-0}" -eq 1 ]] || return 1
  [[ -t 1 ]] || return 1
  return 0
}

render_wizard_text() {
  local content="$1"
  if can_use_gum; then
    gum style --border rounded --border-foreground 240 --padding "1 2" "$content"
  else
    printf '%s\n' "$content"
  fi
}

escape_sed() {
  printf '%s' "$1" | sed -e 's/[\/&]/\\&/g'
}

port_in_use() {
  local port="$1"
  if command -v ss >/dev/null 2>&1; then
    ss -H -ltn "sport = :${port}" 2>/dev/null | grep -q .
    return $?
  fi
  if command -v netstat >/dev/null 2>&1; then
    netstat -ltn 2>/dev/null | awk '{print $4}' | grep -Eq "[:.]${port}\$"
    return $?
  fi
  return 1
}

USED_PORTS=()
CHOSEN_PORT=""

choose_free_port() {
  local candidate="$1"
  while port_in_use "$candidate" || [[ " ${USED_PORTS[*]} " == *" ${candidate} "* ]]; do
    candidate=$((candidate + 1))
  done
  USED_PORTS+=("$candidate")
  CHOSEN_PORT="$candidate"
}

profile_name_for_workdir() {
  local dir="$1"
  local base

  base="$(basename "$dir" | sed -e 's/[^A-Za-z0-9_.-]/-/g')"
  printf 'hostenv-local-demo-%s\n' "$base"
}

configure_hostctl_command() {
  # Allow skipping hostctl for testing environments without passwordless sudo
  if [[ "${HOSTENV_DEMO_SKIP_HOSTCTL:-}" == "1" ]]; then
    log "Skipping hostctl (HOSTENV_DEMO_SKIP_HOSTCTL=1)"
    HOSTCTL_CMD=()
    return 0
  fi

  if [[ -w /etc/hosts ]]; then
    HOSTCTL_CMD=(hostctl)
    return 0
  fi

  if command -v sudo >/dev/null 2>&1; then
    HOSTCTL_CMD=(sudo hostctl)
    return 0
  fi

  fail "hostctl needs write access to /etc/hosts; run as root or install sudo"
}

run_hostctl() {
  [[ "${#HOSTCTL_CMD[@]}" -gt 0 ]] || {
    log "hostctl skipped: $@"
    return 0
  }
  "${HOSTCTL_CMD[@]}" "$@"
}

remove_hostctl_profile() {
  local profile="$1"

  [[ -n "$profile" ]] || return 0
  if ! run_hostctl remove "$profile" --quiet >/dev/null 2>&1; then
    run_hostctl remove "$profile" >/dev/null 2>&1 || true
  fi
}

sync_hostctl_profile() {
  local active_node="$1"
  local active_ip=""

  case "$active_node" in
    node-a) active_ip="$NODE_A_HOST_IP" ;;
    node-b) active_ip="$NODE_B_HOST_IP" ;;
    *) fail_stage "unknown active node for hostctl profile: $active_node" ;;
  esac

  [[ -n "$ENV_HOST" ]] || fail_stage "missing environment hostname while preparing hostctl profile"
  [[ -n "$VHOST" ]] || fail_stage "missing vhost while preparing hostctl profile"
  [[ -n "$HOSTCTL_PROFILE" ]] || fail_stage "missing hostctl profile name"
  [[ -n "$HOSTCTL_SOURCE_FILE" ]] || fail_stage "missing hostctl source path"

  cat > "$HOSTCTL_SOURCE_FILE" <<EOF_HOSTCTL
${NODE_A_HOST_IP} node-a.${HOSTENV_HOSTNAME}
${NODE_B_HOST_IP} node-b.${HOSTENV_HOSTNAME}
${active_ip} ${ENV_HOST}
${active_ip} ${VHOST}
EOF_HOSTCTL

  run_hostctl replace "$HOSTCTL_PROFILE" --from "$HOSTCTL_SOURCE_FILE" --quiet >/dev/null
  HOSTCTL_IN_USE=1
}

kill_pid_if_running() {
  local pid="$1"
  [[ "$pid" =~ ^[0-9]+$ ]] || return 0

  if kill -0 "$pid" >/dev/null 2>&1; then
    kill "$pid" >/dev/null 2>&1 || true
    # wait errors for non-child processes; suppress to keep teardown idempotent.
    wait "$pid" >/dev/null 2>&1 || true
  fi
}

is_demo_workdir() {
  local dir="$1"
  [[ -d "$dir" ]] || return 1

  if [[ -f "$dir/.hostenv-local-vm-demo" ]]; then
    return 0
  fi

  # Backward compatibility for earlier runs before the marker file existed.
  if [[ "$(basename "$dir")" == hostenv-local-vm-demo-* ]] && [[ -d "$dir/provider" ]]; then
    return 0
  fi

  return 1
}

stop_demo_vms_for_workdir() {
  local dir="$1"
  local pidfile
  local pid
  local cmdline

  if [[ -d "$dir/pids" ]]; then
    for pidfile in "$dir"/pids/*.pid; do
      [[ -e "$pidfile" ]] || continue
      pid="$(cat "$pidfile" 2>/dev/null || true)"
      [[ -n "$pid" ]] || continue
      kill_pid_if_running "$pid"
    done
  fi

  # Fallback for prior runs that do not have pid files.
  if command -v pgrep >/dev/null 2>&1; then
    while IFS= read -r pid; do
      [[ -n "$pid" ]] || continue
      cmdline="$(ps -o command= -p "$pid" 2>/dev/null || true)"
      [[ -n "$cmdline" ]] || continue
      if [[ "$cmdline" == *"$dir"* ]] && [[ "$cmdline" == *qemu* ]]; then
        kill_pid_if_running "$pid"
      fi
    done < <(pgrep -f -- "$dir" || true)
  fi
}

collect_teardown_targets() {
  local requested_workdir="$1"
  local candidate

  if [[ -n "$requested_workdir" ]]; then
    [[ -e "$requested_workdir" ]] || fail "workdir does not exist: $requested_workdir"
    cd -- "$requested_workdir" >/dev/null
    pwd
    return 0
  fi

  while IFS= read -r candidate; do
    [[ -n "$candidate" ]] || continue
    printf '%s\n' "$candidate"
  done < <(find /tmp -mindepth 1 -maxdepth 1 -type d -name 'hostenv-local-vm-demo-*' 2>/dev/null | sort)
}

run_teardown() {
  local requested_workdir="$1"
  local target
  local profile
  local removed=0
  local skipped=0
  local found=0

  while IFS= read -r target; do
    [[ -n "$target" ]] || continue
    found=1

    if ! is_demo_workdir "$target"; then
      if [[ -n "$requested_workdir" ]]; then
        fail "refusing to remove non-demo workdir: $target"
      fi
      log "Skipping non-demo path: $target"
      skipped=$((skipped + 1))
      continue
    fi

    log "Stopping demo VMs under $target"
    stop_demo_vms_for_workdir "$target"
    profile="$(profile_name_for_workdir "$target")"
    remove_hostctl_profile "$profile"
    rm -rf "$target"
    log "Removed $target"
    removed=$((removed + 1))
  done < <(collect_teardown_targets "$requested_workdir")

  if [[ "$found" -eq 0 ]]; then
    log "No demo workdirs found to tear down"
    return 0
  fi

  log "Teardown complete: removed=$removed skipped=$skipped"
}

abort_demo() {
  ABORTED=1
  echo "Aborting demo and cleaning up..." >&2
  exit 130
}

prompt_continue_or_abort() {
  [[ "$MODE" == "automated" ]] && return 0
  [[ -r /dev/tty ]] || fail "wizard mode requires a terminal (/dev/tty)"

  while true; do
    if can_use_gum; then
      gum style --foreground 245 "Press <Enter> to continue, or press 'a' to abort and clean up."
    fi
    printf "Press <Enter> to continue, or press 'a' to abort and clean up: "
    local choice
    if ! read -r -n 1 -s choice < /dev/tty; then
      abort_demo
    fi
    printf '\n'
    case "$choice" in
      "")
        return 0
        ;;
      a|A)
        abort_demo
        ;;
      *)
        warn "Unrecognised input '$choice'."
        ;;
    esac
  done
}

check_abort_nonblocking() {
  [[ "$MODE" == "automated" ]] && return 0
  [[ -r /dev/tty ]] || return 0

  local choice
  while read -r -t 0.05 -n 1 -s choice < /dev/tty; do
    case "$choice" in
      a|A)
        abort_demo
        ;;
      "")
        ;;
      *)
        warn "Ignoring input '$choice' while waiting. Press 'a' to abort and clean up."
        ;;
    esac
  done
}

poll_until_or_abort() {
  local timeout="$1"
  local description="$2"
  shift 2

  local start now
  start="$(date +%s)"

  log "$description"
  if [[ "$MODE" == "wizard" ]]; then
    log "While waiting, press 'a' at any time to abort and clean up."
  fi

  while true; do
    if "$@"; then
      return 0
    fi

    now="$(date +%s)"
    if (( now - start >= timeout )); then
      return 1
    fi

    check_abort_nonblocking || true
    sleep 2
  done
}

wait_for_ssh() {
  local host_alias="$1"
  local user="$2"
  local timeout="${3:-360}"
  local monitor_pid="${4:-}"
  local start
  start="$(date +%s)"

  while true; do
    if ssh -F "$SSH_CONFIG" -o ConnectTimeout=2 "${user}@${host_alias}" true >/dev/null 2>&1; then
      return 0
    fi
    if [[ "$monitor_pid" =~ ^[0-9]+$ ]] && ! kill -0 "$monitor_pid" >/dev/null 2>&1; then
      return 2
    fi
    if (( "$(date +%s)" - start > timeout )); then
      return 1
    fi
    check_abort_nonblocking || true
    sleep 2
  done
}

wait_for_mysql_socket() {
  local host_alias="$1"
  local user="$2"
  local runtime_dir="$3"
  local timeout="${4:-240}"
  local start
  start="$(date +%s)"

  while true; do
    if ssh -F "$SSH_CONFIG" "${user}@${host_alias}" \
      "test -S '${runtime_dir}/mysql.sock'" >/dev/null 2>&1; then
      return 0
    fi
    if (( "$(date +%s)" - start > timeout )); then
      return 1
    fi
    sleep 2
  done
}

http_status() {
  local host_header="$1"
  local url="$2"
  curl --connect-timeout 2 --max-time 4 -sS -o /dev/null -w '%{http_code}' \
    -H "Host: ${host_header}" "$url" 2>/dev/null || true
}

installer_page_present() {
  local host_header="$1"
  local url="$2"
  local body

  body="$(curl --connect-timeout 5 --max-time 20 -sS -H "Host: ${host_header}" "$url" 2>/dev/null || true)"
  printf '%s' "$body" | grep -qi "Install Drupal"
}

assert_not_installer_page() {
  local host_header="$1"
  local url="$2"

  if installer_page_present "$host_header" "$url"; then
    fail_stage "Drupal installer page is still shown at ${url} for host ${host_header}"
  fi
}

marker_present_on_node() {
  local node_alias="$1"

  ssh -F "$SSH_CONFIG" "${ENV_USER}@${node_alias}" \
    "mysql --batch --skip-column-names -e \"SELECT note FROM hostenv_demo_marker WHERE id = 1;\"" \
    2>/dev/null | grep -qx 'from-node-a'
}

find_vm_runner() {
  local vm_out="$1"
  local runner
  runner="$(find -L "$vm_out" -type f -name 'run-*-vm' | head -n1 || true)"
  [[ -n "$runner" ]] || fail "could not locate VM runner in $vm_out"
  printf '%s\n' "$runner"
}

sign_deploy_profile() {
  local node="$1"
  local profile="$2"
  local profile_path

  # For comin flow, use nixosConfigurations instead of deploy-rs
  if [[ "$profile" == "system" ]]; then
    profile_path="$(nix build --no-link --print-out-paths "$PROVIDER_DIR/generated/.#nixosConfigurations.${node}.config.system.build.toplevel")"
  else
    # For environment user packages, use the env-${profile} package
    profile_path="$(nix build --no-link --print-out-paths "$PROVIDER_DIR/generated/.#packages.x86_64-linux.env-${profile}")"
  fi
  nix store sign --key-file "$NIX_SIGNING_KEY_FILE" --recursive "$profile_path" >/dev/null
}

run_provider_plan() {
  (
    cd "$PROVIDER_DIR"
    nix run .#hostenv-provider -- plan
    nix run .#hostenv-provider -- dns-gate
  )
}

load_plan_metadata() {
  [[ -f "$PLAN_PATH" ]] || return 1

  local env runtime data vhost migrate env_host
  env="$(jq -r '.environments | keys[0] // empty' "$PLAN_PATH")"
  [[ -n "$env" ]] || return 1

  runtime="$(jq -r --arg env "$env" '.environments[$env].hostenv.runtimeDir // empty' "$PLAN_PATH")"
  data="$(jq -r --arg env "$env" '.environments[$env].hostenv.dataDir // empty' "$PLAN_PATH")"
  env_host="$(jq -r --arg env "$env" '.environments[$env].hostenv.hostname // empty' "$PLAN_PATH")"
  vhost="$(jq -r --arg env "$env" '.environments[$env].virtualHosts | keys[0] // empty' "$PLAN_PATH")"
  migrate="$(jq -r --arg env "$env" '.environments[$env].migrations[0] // empty' "$PLAN_PATH")"

  [[ -n "$runtime" && -n "$data" && -n "$vhost" && -n "$env_host" ]] || return 1

  ENV_USER="$env"
  RUNTIME_DIR="$runtime"
  DATA_DIR="$data"
  ENV_HOST="$env_host"
  VHOST="$vhost"
  MIGRATE_BACKUP="$migrate"
  return 0
}

wait_for_unix_socket() {
  local socket_path="$1"
  local timeout_seconds="${2:-60}"
  local monitor_pid="${3:-}"
  local start
  start="$(date +%s)"

  while true; do
    if [[ -S "$socket_path" ]]; then
      return 0
    fi
    if [[ "$monitor_pid" =~ ^[0-9]+$ ]] && ! kill -0 "$monitor_pid" >/dev/null 2>&1; then
      return 2
    fi
    if (( "$(date +%s)" - start >= timeout_seconds )); then
      return 1
    fi
    check_abort_nonblocking || true
    sleep 1
  done
}

load_project_hash_from_plan() {
  [[ -f "$PLAN_PATH" ]] || return 1
  PROJECT_HASH="$(jq -r '
    first((.environments // {} | to_entries[]? | .value.hostenv.projectNameHash | if type == "array" then .[] else . end) // empty)
  ' "$PLAN_PATH")"
  [[ -n "$PROJECT_HASH" ]]
}

load_comin_node_token() {
  local node="$1"
  local token

  token="$(sops -d --output-type json "$PROVIDER_DIR/secrets/provider.yaml" | jq -r --arg node "$node" '.comin_node_tokens[$node] // empty')"
  [[ -n "$token" ]] || return 1
  printf '%s\n' "$token"
}

ensure_provider_service_running() {
  local token_file="$PROVIDER_SERVICE_RUNTIME_DIR/comin-node-tokens.yaml"
  local health_code=""

  if [[ -S "$PROVIDER_SERVICE_SOCKET" ]]; then
    health_code="$(curl -sS --unix-socket "$PROVIDER_SERVICE_SOCKET" -o /dev/null -w '%{http_code}' "http://localhost/health" --connect-timeout 1 --max-time 2 2>/dev/null || true)"
    if [[ "$health_code" =~ ^2[0-9][0-9]$ ]]; then
      return 0
    fi
    rm -f "$PROVIDER_SERVICE_SOCKET"
  fi

  mkdir -p "$PROVIDER_SERVICE_RUNTIME_DIR"
  [[ -n "$NODE_A_AUTH_TOKEN" ]] || fail_stage "missing node-a auth token for provider-service"
  [[ -n "$NODE_B_AUTH_TOKEN" ]] || fail_stage "missing node-b auth token for provider-service"
  {
    printf 'node-a: %s\n' "$NODE_A_AUTH_TOKEN"
    printf 'node-b: %s\n' "$NODE_B_AUTH_TOKEN"
  } > "$token_file"
  chmod 600 "$token_file"

  log "Starting provider-service in demo runtime"
  (
    cd "$PROVIDER_DIR"
    HOSTENV_PROVIDER_DEV_DIR="$PROVIDER_SERVICE_RUNTIME_DIR" \
      HOSTENV_PROVIDER_DATA_DIR="$PROVIDER_DIR" \
      HOSTENV_PROVIDER_COMIN_TOKENS_FILE="$token_file" \
      HOSTENV_PROVIDER_WEBHOOK_HOST="$HOSTENV_HOSTNAME" \
      HOSTENV_PROVIDER_UI_BASE_URL="http://${VHOST}:${NODE_HTTP_PORT}" \
      hostenv-provider-service-dev > "$TASK11_PROVIDER_SERVICE_LOG" 2>&1
  ) &
  PROVIDER_SERVICE_PID="$!"

  wait_for_unix_socket "$PROVIDER_SERVICE_SOCKET" 120 "$PROVIDER_SERVICE_PID"
  case "$?" in
    0)
      ;;
    2)
      fail_stage "provider-service exited before creating socket; see $TASK11_PROVIDER_SERVICE_LOG"
      ;;
    *)
      fail_stage "provider-service socket did not appear at $PROVIDER_SERVICE_SOCKET; see $TASK11_PROVIDER_SERVICE_LOG"
      ;;
  esac
}

trigger_webhook_deploy() {
  local node="$1"
  local token="$2"
  local sha="$3"
  local evidence_prefix="$4"
  local runner_log="$EVIDENCE_DIR/${evidence_prefix}-runner.log"
  local intent_log="$EVIDENCE_DIR/${evidence_prefix}-deploy-intent.log"
  local job_id

  mkdir -p "$EVIDENCE_DIR"
  if ! "$SCRIPT_DIR/trigger-webhook.sh" \
    --workdir "$PROVIDER_SERVICE_RUNTIME_DIR" \
    --project-hash "$PROJECT_HASH" \
    --commit-sha "$sha" \
    --node "$node" \
    --token "$token" \
    --evidence-prefix "$evidence_prefix" > "$runner_log" 2>&1; then
    fail_stage "webhook trigger failed for ${node}; see $runner_log"
  fi

  job_id="$(awk -F= '/^job_id=/{print $2; exit}' "$intent_log" | tr -d '[:space:]')"
  [[ -n "$job_id" ]] || fail_stage "failed to resolve webhook job id for ${node}; see $intent_log"
  printf '%s\n' "$job_id"
}

wait_for_job_id_by_sha() {
  local commit_sha="$1"
  local node="$2"
  local token="$3"
  local timeout="$4"
  local start
  local code=""
  local body_file="$PROVIDER_SERVICE_RUNTIME_DIR/job-id-${node}.json"

  FOUND_JOB_ID=""
  start="$(date +%s)"
  while true; do
    code="$(curl -sS --unix-socket "$PROVIDER_SERVICE_SOCKET" -o "$body_file" -w '%{http_code}' \
      -H "Authorization: Bearer $token" \
      "http://localhost/api/deploy-intents/by-sha?sha=$commit_sha&node=$node" || true)"

    if [[ "$code" == "200" ]]; then
      FOUND_JOB_ID="$(jq -r '.jobId // empty' "$body_file")"
      [[ -n "$FOUND_JOB_ID" ]] && return 0
    fi

    if (( "$(date +%s)" - start >= timeout )); then
      return 1
    fi

    check_abort_nonblocking || true
    sleep 2
  done
}

wait_for_provider_job_completion() {
  local job_id="$1"
  local node="$2"
  local token="$3"
  local timeout="$4"
  local status_log="$5"
  local start now
  local code=""
  local body_file="$PROVIDER_SERVICE_RUNTIME_DIR/job-status-${node}.json"
  local final_state="timeout"

  log "Polling provider-service job status for ${node} (job ${job_id})"
  start="$(date +%s)"
  while true; do
    code="$(curl -sS --unix-socket "$PROVIDER_SERVICE_SOCKET" -o "$body_file" -w '%{http_code}' \
      -H "Authorization: Bearer $token" \
      "http://localhost/api/deploy-jobs/${job_id}/statuses?node=${node}" || true)"

    if [[ "$code" == "200" ]]; then
      if jq -e '.statuses[]? | select(.status == "failed" or .status == "timed_out")' "$body_file" >/dev/null 2>&1; then
        final_state="failed"
        break
      fi

      if jq -e --arg node "$node" '.statuses[]? | select(.node == $node and .phase == "intent" and .status == "success")' "$body_file" >/dev/null 2>&1; then
        final_state="success"
        break
      fi
    fi

    now="$(date +%s)"
    if (( now - start >= timeout )); then
      final_state="timeout"
      break
    fi

    check_abort_nonblocking || true
    sleep 2
  done

  {
    printf 'timestamp=%s\n' "$(date -Iseconds)"
    printf 'job_id=%s\n' "$job_id"
    printf 'node=%s\n' "$node"
    printf 'http_code=%s\n' "$code"
    printf 'final_state=%s\n' "$final_state"
    printf 'response_body:\n'
    if [[ -f "$body_file" ]]; then
      cat "$body_file"
    else
      printf '{}\n'
    fi
    printf '\n'
  } > "$status_log"

  [[ "$final_state" == "success" ]]
}

condition_node_a_deployed() {
  load_plan_metadata || return 1

  ssh -F "$SSH_CONFIG" -o ConnectTimeout=2 "${ENV_USER}@node-a.${HOSTENV_HOSTNAME}" true >/dev/null 2>&1 || return 1
  ssh -F "$SSH_CONFIG" "${ENV_USER}@node-a.${HOSTENV_HOSTNAME}" "test -S '${RUNTIME_DIR}/mysql.sock'" >/dev/null 2>&1 || return 1

  local code
  code="$(http_status "$VHOST" "http://${NODE_A_HOST_IP}:${NODE_HTTP_PORT}/")"
  [[ "$code" != "000" ]]
}

run_task8_post_deploy_verification() {
  stage "Task 8: Verify comin post-deployment activation"

  mkdir -p "$EVIDENCE_DIR"
  local node_alias="node-a.${HOSTENV_HOSTNAME}"

  log "Running activation script presence/env checks on ${node_alias}"
  ssh -F "$SSH_CONFIG" "deploy@${node_alias}" "bash -s" >"$TASK8_ACTIVATION_LOG" 2>&1 <<'EOF_TASK8_ACTIVATION'
set -euo pipefail

echo "[task8] checking activation script binary"
test -x /run/current-system/sw/bin/hostenv-comin-activate

echo "[task8] locating comin post-deployment wrapper"
unit_text="$(systemctl cat comin.service)"
wrapper_path="$(printf '%s\n' "$unit_text" | tr ' ' '\n' | grep 'hostenv-comin-activate-wrapper' | head -n1 || true)"
test -n "$wrapper_path"
test -r "$wrapper_path"

echo "[task8] validating wrapper exports"
grep -q '^export HOSTENV_COMIN_NODE_NAME=' "$wrapper_path"
grep -q '^export HOSTENV_COMIN_API_BASE_URL=' "$wrapper_path"
grep -q '^export HOSTENV_COMIN_TOKEN_FILE=' "$wrapper_path"
grep -q '^export HOSTENV_COMIN_ACTION_TIMEOUT=' "$wrapper_path"

echo "[task8] validating token file and curl availability"
token_file="$(grep '^export HOSTENV_COMIN_TOKEN_FILE=' "$wrapper_path" | cut -d= -f2-)"
test -n "$token_file"
test -r "$token_file"
command -v curl

echo "[task8] validating provider event callback endpoint exists in script"
grep -q '/api/deploy-jobs/\$job_id/events' /run/current-system/sw/bin/hostenv-comin-activate

echo "[task8] PASS"
EOF_TASK8_ACTIVATION

  log "Running provider API query check on ${node_alias}"
  ssh -F "$SSH_CONFIG" "deploy@${node_alias}" "bash -s" >"$TASK8_API_LOG" 2>&1 <<'EOF_TASK8_API'
set -euo pipefail

unit_text="$(systemctl cat comin.service)"
wrapper_path="$(printf '%s\n' "$unit_text" | tr ' ' '\n' | grep 'hostenv-comin-activate-wrapper' | head -n1 || true)"
test -n "$wrapper_path"

api_base="$(grep '^export HOSTENV_COMIN_API_BASE_URL=' "$wrapper_path" | cut -d= -f2-)"
node_name="$(grep '^export HOSTENV_COMIN_NODE_NAME=' "$wrapper_path" | cut -d= -f2-)"
token_file="$(grep '^export HOSTENV_COMIN_TOKEN_FILE=' "$wrapper_path" | cut -d= -f2-)"
token="$(tr -d '\n' < "$token_file")"

query_url="$api_base/api/deploy-intents/by-sha?sha=task8-manual-check&node=$node_name"
status="$(curl -sS -o /tmp/task8-api-query.json -w '%{http_code}' -H "Authorization: Bearer $token" "$query_url")"

echo "query_url=$query_url"
echo "http_status=$status"
cat /tmp/task8-api-query.json
rm -f /tmp/task8-api-query.json

case "$status" in
  200|404)
    echo "[task8] PASS"
    ;;
  *)
    echo "[task8] unexpected status: $status" >&2
    exit 1
    ;;
esac
EOF_TASK8_API

  log "Running manual comin trigger check on ${node_alias}"
  ssh -F "$SSH_CONFIG" "deploy@${node_alias}" "bash -s" >"$TASK8_MANUAL_TRIGGER_LOG" 2>&1 <<'EOF_TASK8_MANUAL'
set -euo pipefail

echo "[task8] restarting comin service"
sudo -n systemctl restart comin.service || systemctl restart comin.service
systemctl is-active comin.service

echo "[task8] running activation script manually with synthetic SHA"
unit_text="$(systemctl cat comin.service)"
wrapper_path="$(printf '%s\n' "$unit_text" | tr ' ' '\n' | grep 'hostenv-comin-activate-wrapper' | head -n1 || true)"
test -n "$wrapper_path"

export COMIN_GIT_SHA="task8-manual-trigger"
export HOSTENV_COMIN_NODE_NAME="$(grep '^export HOSTENV_COMIN_NODE_NAME=' "$wrapper_path" | cut -d= -f2-)"
export HOSTENV_COMIN_API_BASE_URL="$(grep '^export HOSTENV_COMIN_API_BASE_URL=' "$wrapper_path" | cut -d= -f2-)"
export HOSTENV_COMIN_TOKEN_FILE="$(grep '^export HOSTENV_COMIN_TOKEN_FILE=' "$wrapper_path" | cut -d= -f2-)"
export HOSTENV_COMIN_ACTION_TIMEOUT="10"
/run/current-system/sw/bin/hostenv-comin-activate

echo "[task8] recent comin journal"
journalctl -u comin.service -n 20 --no-pager
echo "[task8] PASS"
EOF_TASK8_MANUAL

  success "Task 8 verification logs written:"
  log "  - $TASK8_ACTIVATION_LOG"
  log "  - $TASK8_API_LOG"
  log "  - $TASK8_MANUAL_TRIGGER_LOG"
}

run_task10_webhook_deploy_intent_verification() {
  stage "Task 10: Verify webhook deploy intent creation"

  mkdir -p "$EVIDENCE_DIR"

  local runtime_hint="${HOSTENV_PROVIDER_DEV_DIR:-$WORKDIR/provider-dev}"
  local task10_sha="task10-$(date +%s)"
  local -a trigger_cmd=(
    "$SCRIPT_DIR/trigger-webhook.sh"
    --workdir "$runtime_hint"
    --commit-sha "$task10_sha"
    --evidence-prefix task-10
    --allow-missing-intent
  )

  if [[ ! -S "$runtime_hint/hostenv-provider.sock" && ! -S "$runtime_hint/runtime/hostenv-provider.sock" ]]; then
    warn "Task 10 runtime check skipped: provider service socket not found under $runtime_hint"
    warn "Start provider service in demo shell, then run:"
    warn "  ${trigger_cmd[*]}"
    {
      printf 'timestamp=%s\n' "$(date -Iseconds)"
      printf 'status=skipped\n'
      printf 'reason=provider service socket not found\n'
      printf 'runtime_hint=%s\n' "$runtime_hint"
      printf 'command=%s\n' "${trigger_cmd[*]}"
    } > "$TASK10_RUNNER_LOG"
    return 0
  fi

  log "Running webhook trigger for deploy-intent verification"
  if "${trigger_cmd[@]}" >"$TASK10_RUNNER_LOG" 2>&1; then
    success "Task 10 verification logs written:"
    log "  - $TASK10_RUNNER_LOG"
    log "  - $TASK10_WEBHOOK_LOG"
    log "  - $TASK10_DEPLOY_INTENT_LOG"
  else
    fail_stage "Task 10 deploy-intent verification failed; see $TASK10_RUNNER_LOG"
  fi
}

condition_seed_imported_node_a() {
  load_plan_metadata || return 1

  ssh -F "$SSH_CONFIG" "${ENV_USER}@node-a.${HOSTENV_HOSTNAME}" "test -S '${RUNTIME_DIR}/mysql.sock'" >/dev/null 2>&1 || return 1
  marker_present_on_node "node-a.${HOSTENV_HOSTNAME}" || return 1

  local url="http://${NODE_A_HOST_IP}:${NODE_HTTP_PORT}/"
  installer_page_present "$VHOST" "$url" && return 1

  local code
  code="$(http_status "$VHOST" "$url")"
  [[ "$code" =~ ^[23][0-9][0-9]$ ]]
}

capture_task12_migration_source_plan() {
  [[ -f "$PLAN_PATH" ]] || fail_stage "cannot capture migration source: missing $PLAN_PATH"
  cp "$PLAN_PATH" "$TASK12_MIGRATION_SOURCE_PLAN"
}

apply_task12_migration_source_plan() {
  [[ -f "$TASK12_MIGRATION_SOURCE_PLAN" ]] || fail_stage "missing migration source snapshot: $TASK12_MIGRATION_SOURCE_PLAN"
  cp "$TASK12_MIGRATION_SOURCE_PLAN" "$PLAN_PATH"
}

condition_node_b_migrated() {
  load_plan_metadata || return 1

  ssh -F "$SSH_CONFIG" "${ENV_USER}@node-b.${HOSTENV_HOSTNAME}" "test -S '${RUNTIME_DIR}/mysql.sock'" >/dev/null 2>&1 || return 1
  marker_present_on_node "node-b.${HOSTENV_HOSTNAME}" || return 1

  local url="http://${NODE_B_HOST_IP}:${NODE_HTTP_PORT}/"
  installer_page_present "$VHOST" "$url" && return 1

  local code
  code="$(http_status "$VHOST" "$url")"
  [[ "$code" =~ ^[23][0-9][0-9]$ ]]
}

run_task12_backup_restore_verification() {
  stage "Task 12: Verify migration backup/restore actions"

  mkdir -p "$EVIDENCE_DIR"
  local status_body_json="$PROVIDER_SERVICE_RUNTIME_DIR/task-12-statuses.json"
  local snapshot_body_json="$PROVIDER_SERVICE_RUNTIME_DIR/task-12-backup-snapshot.json"
  local snapshot_code=""
  local backup_status="missing"
  local restore_status="missing"
  local snapshot_present="false"
  local snapshot_count="0"

  awk 'found { print } /^response_body:$/ { found=1; next }' "$TASK11_NODE_B_STATUS_LOG" > "$status_body_json"

  if jq -e '.statuses[]? | select(.node == "node-a" and .phase == "backup" and .status == "success")' "$status_body_json" >/dev/null 2>&1; then
    backup_status="success"
  fi

  if jq -e '.statuses[]? | select(.node == "node-b" and .phase == "restore" and .status == "success")' "$status_body_json" >/dev/null 2>&1; then
    restore_status="success"
  fi

  snapshot_code="$(curl -sS --unix-socket "$PROVIDER_SERVICE_SOCKET" -o "$snapshot_body_json" -w '%{http_code}' \
    -H "Authorization: Bearer $NODE_B_AUTH_TOKEN" \
    "http://localhost/api/deploy-jobs/${NODE_B_JOB_ID}/backup-snapshot?node=node-b&source=node-a&user=${ENV_USER}" || true)"

  if [[ "$snapshot_code" == "200" ]] && jq -e '.payload.snapshots | type == "object"' "$snapshot_body_json" >/dev/null 2>&1; then
    snapshot_count="$(jq -r '(.payload.snapshots | keys | length)' "$snapshot_body_json")"
    if [[ "$snapshot_count" =~ ^[1-9][0-9]*$ ]]; then
      snapshot_present="true"
    fi
  fi

  {
    printf 'timestamp=%s\n' "$(date -Iseconds)"
    printf 'job_id=%s\n' "$NODE_B_JOB_ID"
    printf 'backup_node=node-a\n'
    printf 'restore_node=node-b\n'
    printf 'backup_phase_status=%s\n' "$backup_status"
    printf 'restore_phase_status=%s\n' "$restore_status"
    printf 'backup_snapshot_http_code=%s\n' "$snapshot_code"
    printf 'backup_snapshot_present=%s\n' "$snapshot_present"
    printf 'backup_snapshot_count=%s\n' "$snapshot_count"
    printf 'status_response_body:\n'
    cat "$status_body_json"
    printf '\nbackup_snapshot_body:\n'
    cat "$snapshot_body_json" 2>/dev/null || printf '{}\n'
    printf '\n'
  } > "$TASK12_BACKUP_LOG"

  [[ "$backup_status" == "success" ]] || fail_stage "Task 12 backup verification failed; see $TASK12_BACKUP_LOG"
  [[ "$restore_status" == "success" ]] || fail_stage "Task 12 restore verification failed; see $TASK12_BACKUP_LOG"
  [[ "$snapshot_present" == "true" ]] || fail_stage "Task 12 backup snapshot verification failed; see $TASK12_BACKUP_LOG"

  success "Task 12 backup/restore evidence written: $TASK12_BACKUP_LOG"
}

run_task12_migration_marker_verification() {
  stage "Task 12: Verify migrated database marker"

  mkdir -p "$EVIDENCE_DIR"
  local body_file="$PROVIDER_SERVICE_RUNTIME_DIR/task-12-node-b-http-body.html"
  local http_code=""
  local marker_present="false"

  http_code="$(curl -sS -o "$body_file" -w '%{http_code}' -H "Host: ${VHOST}" "http://${NODE_B_HOST_IP}:${NODE_HTTP_PORT}/" || true)"

  if grep -q 'from-node-a' "$body_file"; then
    marker_present="true"
  fi

  {
    printf 'timestamp=%s\n' "$(date -Iseconds)"
    printf 'url=http://%s:%s/\n' "$NODE_B_HOST_IP" "$NODE_HTTP_PORT"
    printf 'host_header=%s\n' "$VHOST"
    printf 'http_code=%s\n' "$http_code"
    printf 'marker_expected=from-node-a\n'
    printf 'marker_found=%s\n' "$marker_present"
    printf 'response_body:\n'
    cat "$body_file" 2>/dev/null || printf '\n'
    printf '\n'
  } > "$TASK12_MIGRATION_VERIFY_LOG"

  [[ "$http_code" =~ ^[23][0-9][0-9]$ ]] || fail_stage "Task 12 migration HTTP verification failed; see $TASK12_MIGRATION_VERIFY_LOG"
  [[ "$marker_present" == "true" ]] || fail_stage "Task 12 migration marker verification failed; see $TASK12_MIGRATION_VERIFY_LOG"

  success "Task 12 migration marker evidence written: $TASK12_MIGRATION_VERIFY_LOG"
}

write_provider_flake() {
  local production_node="$1"
  local shared_provider_repo="$SHARED_DIR/provider-repo"

  rm -f "$shared_provider_repo"
  ln -s "$PROVIDER_DIR" "$shared_provider_repo"

  cat > "$PROVIDER_DIR/flake.nix" <<EOF_FLAKE
{
  description = "Hostenv local VM migration demo provider";

  inputs = {
    hostenv = {
      url = "path:${REPO_ROOT}";
    };

    nixpkgs.follows = "hostenv/nixpkgs";
    flake-parts.follows = "hostenv/flake-parts";
    sops-nix.follows = "hostenv/sops-nix";
    phps.follows = "hostenv/phps";
    comin.follows = "hostenv/comin";

    demo__drupal = {
      url = "git+file://${PROJECT_DIR}?dir=.hostenv&ref=main";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.hostenv.follows = "hostenv";
      inputs.flake-parts.follows = "flake-parts";
    };
  };

  outputs = inputs@{ flake-parts, hostenv, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [ hostenv.flakeModules.provider ];

      provider = {
        hostenvHostname = "${HOSTENV_HOSTNAME}";
        nixSigning.trustedPublicKeys = [ "${SSH_PUBLIC_KEY}" ];
        nodeSystems = {
          default = "x86_64-linux";
          node-a = "x86_64-linux";
          node-b = "x86_64-linux";
        };
        nodeFor = {
          default = "${production_node}";
          production = "${production_node}";
          testing = "${production_node}";
          development = "${production_node}";
        };

        service = {
          organisation = "demo";
          project = "drupal";
          environmentName = "main";
        };

        comin = {
          enable = true;
          remoteUrl = "file:///mnt/hostenv-shared/provider-repo";
          providerApiBaseUrl = "http://${PROVIDER_API_VM_GATEWAY}:${PROVIDER_HTTP_PORT}";
          nodeAuthTokenFile = "/run/secrets/hostenv-provider/comin_node_token";
        };

        planSource = "eval";
        letsEncrypt = {
          adminEmail = "demo@example.test";
          acceptTerms = true;
        };
      };
    };
}
EOF_FLAKE
}

write_node_config() {
  local node="$1"
  local host_ip="$2"
  local ssh_port="$3"
  local http_port="$4"
  mkdir -p "$PROVIDER_DIR/nodes/$node"
  cat > "$PROVIDER_DIR/nodes/$node/configuration.nix" <<EOF_NODE
{ modulesPath, lib, ... }:
{
  imports = [
    (modulesPath + "/virtualisation/qemu-vm.nix")
  ];

  networking.hostName = "${node}";
  networking.firewall.allowedTCPPorts = [ 80 ];
  system.stateVersion = "24.11";

  # deploy-rs evaluates and activates system profiles; keep a VM-safe GRUB
  # configuration so activation does not try to write a real disk bootloader.
  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
  };
  boot.loader.grub.device = lib.mkForce "nodev";
  boot.loader.grub.devices = lib.mkForce [ "nodev" ];
  nix.settings.trusted-public-keys = lib.mkAfter [ "${NIX_SIGNING_PUBLIC_KEY}" ];

  virtualisation = {
    graphics = false;
    cores = 2;
    memorySize = 4096;
    diskSize = 16384;
    forwardPorts = [
      { from = "host"; host.address = "${host_ip}"; host.port = ${ssh_port}; guest.port = 22; }
      { from = "host"; host.address = "${host_ip}"; host.port = ${http_port}; guest.port = 80; }
    ];
    sharedDirectories.hostenv-shared = {
      source = "${SHARED_DIR}";
      target = "/mnt/hostenv-shared";
    };
    mountHostNixStore = true;
    writableStore = true;
    writableStoreUseTmpfs = false;
  };

  environment.etc."hostenv-demo-age-key.txt".text = ''
${AGE_PRIVATE_KEY}
'';
  sops.age.keyFile = "/etc/hostenv-demo-age-key.txt";
}
EOF_NODE
}

init_git_repo() {
  local dir="$1"
  git -C "$dir" init -b main >/dev/null
  git -C "$dir" config user.email "demo@example.test"
  git -C "$dir" config user.name "Hostenv Demo"
  git -C "$dir" add .
  git -C "$dir" commit -m "Initial commit for local demo" >/dev/null
}

start_vm() {
  local node="$1"
  local vm_out="$PROVIDER_DIR/vm-$node"
  local vm_runner

  log "Building VM for $node"
  (
    cd "$PROVIDER_DIR"
    nix build "./generated#nixosConfigurations.${node}.config.system.build.vm" -o "$vm_out"
  )

  vm_runner="$(find_vm_runner "$vm_out")"
  log "Starting VM for $node using $vm_runner"
  (
    # Keep VM disk images inside the demo workdir instead of the caller's cwd.
    cd "$PROVIDER_DIR"
    "$vm_runner" > "$LOG_DIR/${node}.log" 2>&1
  ) &
  local vm_pid="$!"
  VM_PIDS+=("$vm_pid")
  mkdir -p "$PIDS_DIR"
  printf '%s\n' "$vm_pid" > "$PIDS_DIR/${node}.pid"

  log "Waiting for SSH on $node"
  wait_for_ssh "${node}.${HOSTENV_HOSTNAME}" deploy 420 "$vm_pid"
  case "$?" in
    0)
      ;;
    2)
      fail_stage "${node} VM process exited during boot; see $LOG_DIR/${node}.log"
      ;;
    *)
      fail_stage "timed out waiting for ${node} SSH; see $LOG_DIR/${node}.log"
      ;;
  esac
}

write_ssh_wrapper() {
  local ssh_bin
  ssh_bin="$(command -v ssh)"

  mkdir -p "$WRAPPER_DIR"
  cat > "$WRAPPER_DIR/ssh" <<EOF_SSH_WRAP
#!/usr/bin/env bash
exec ${ssh_bin@Q} -F ${SSH_CONFIG@Q} "\$@"
EOF_SSH_WRAP
  chmod 700 "$WRAPPER_DIR/ssh"
}

write_demo_shell_flake() {
  # Create the provider service dev wrapper script
  mkdir -p "${WRAPPER_DIR}"
  cat > "${WRAPPER_DIR}/hostenv-provider-service-dev" <<'EOF_DEV_SCRIPT'
#!/usr/bin/env bash
set -euo pipefail

base="${HOSTENV_PROVIDER_DEV_DIR:-/tmp/hostenv-provider-dev}"
pgdata="$base/pgdata"
mkdir -p "$base"

if [ ! -s "$pgdata/PG_VERSION" ]; then
  initdb -D "$pgdata" -A trust >/dev/null
fi

pg_ctl -D "$pgdata" -o "-k $base" -l "$base/postgres.log" start >/dev/null
cleanup() {
  if [ -n "${socat_pid:-}" ]; then
    kill "$socat_pid" >/dev/null 2>&1 || true
  fi
  pg_ctl -D "$pgdata" stop >/dev/null || true
}
trap cleanup EXIT

createdb -h "$base" hostenv-provider >/dev/null 2>&1 || true

data_dir="${HOSTENV_PROVIDER_DATA_DIR:-$base/data}"
listen_socket="${HOSTENV_PROVIDER_LISTEN_SOCKET:-$base/hostenv-provider.sock}"
webhook_host="${HOSTENV_PROVIDER_WEBHOOK_HOST:-localhost}"
ui_base_url="${HOSTENV_PROVIDER_UI_BASE_URL:-http://localhost}"
db_uri="host=$base dbname=hostenv-provider"
git_credentials_file="${HOSTENV_PROVIDER_GIT_CREDENTIALS_FILE:-$data_dir/git-credentials}"
git_config_file="${HOSTENV_PROVIDER_GIT_CONFIG_FILE:-$data_dir/gitconfig}"
flake_template="${HOSTENV_PROVIDER_FLAKE_TEMPLATE:-flake.template.nix}"
deploy_token_ttl_minutes="${HOSTENV_PROVIDER_GITLAB_DEPLOY_TOKEN_TTL_MINUTES:-15}"
token_key_file="${HOSTENV_PROVIDER_GITLAB_TOKEN_KEY_FILE:-$base/gitlab_token_key}"
comin_tokens_file="${HOSTENV_PROVIDER_COMIN_TOKENS_FILE:-$base/comin-node-tokens.yaml}"

secrets="${HOSTENV_PROVIDER_GITLAB_SECRETS_FILE:-$base/gitlab_oauth}"
if [ ! -f "$secrets" ]; then
  printf "client_id=dev\nclient_secret=dev\n" > "$secrets"
fi
if [ ! -f "$token_key_file" ]; then
  printf "key=0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\n" > "$token_key_file"
  chmod 600 "$token_key_file"
fi

config_file="$base/provider-config.json"
cat > "$config_file" <<EOFCFG
{
  \"dataDir\": \"$data_dir\",
  \"flakeRoot\": \".\",
  \"listenSocket\": \"$listen_socket\",
  \"webhookSecretFile\": null,
  \"webhookSecretsDir\": null,
  \"webhookHost\": \"$webhook_host\",
  \"uiBasePath\": \"/dashboard\",
  \"uiBaseUrl\": \"$ui_base_url\",
  \"dbUri\": \"$db_uri\",
  \"gitlab\": {
    \"enable\": true,
    \"oAuthSecretsFile\": \"$secrets\",
    \"hosts\": [\"gitlab.com\"],
    \"tokenEncryptionKeyFile\": \"$token_key_file\",
    \"deployTokenTtlMinutes\": $deploy_token_ttl_minutes
  },
  \"comin\": {
    \"enable\": true,
    \"branch\": \"main\",
    \"pollIntervalSeconds\": 5,
    \"nodeAuthTokensFile\": \"$comin_tokens_file\"
  },
  \"gitCredentialsFile\": \"$git_credentials_file\",
  \"gitConfigFile\": \"$git_config_file\",
  \"flakeTemplate\": \"$flake_template\"
}
EOFCFG

if [ -n "${HOSTENV_PROVIDER_HTTP_PORT:-}" ]; then
  http_bind="${HOSTENV_PROVIDER_HTTP_BIND:-127.0.0.1}"
  socat TCP-LISTEN:"$HOSTENV_PROVIDER_HTTP_PORT",bind="$http_bind",fork,reuseaddr UNIX-CONNECT:"$listen_socket" &
  socat_pid=$!
  echo "hostenv-provider-service-dev: proxying http://$http_bind:$HOSTENV_PROVIDER_HTTP_PORT -> unix:$listen_socket" >&2
fi

exec hostenv-provider-service --config "$config_file"
EOF_DEV_SCRIPT
  chmod +x "${WRAPPER_DIR}/hostenv-provider-service-dev"

  cat > "$WORKDIR/flake.nix" <<EOF_DEMO_FLAKE
{
  description = "Hostenv local demo shell";

  inputs.hostenv.url = "path:${REPO_ROOT}";
  inputs.nixpkgs.follows = "hostenv/nixpkgs";
  inputs.flake-parts.follows = "hostenv/flake-parts";

  outputs = inputs@{ flake-parts, hostenv, nixpkgs, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [ hostenv.flakeModules.provider ];

      provider.enable = true;

      perSystem = { config, pkgs, ... }:
        {
          devShells.default = pkgs.mkShellNoCC {
            packages = with pkgs; [
              git
              jq
              openssh
              rsync
              curl
              pv
              gnused
              gzip
              nix
              postgresql
              socat
              config.packages.hostenv-provider-service
            ];

            shellHook = ''
              export PATH=${WRAPPER_DIR}:\$PATH
              export GIT_SSH_COMMAND='ssh -F ${SSH_CONFIG}'
              export HOSTENV_PROVIDER_DEV_DIR=${WORKDIR}/provider-dev
              export HOSTENV_PROVIDER_DATA_DIR=${WORKDIR}/provider-dev/data
              export HOSTENV_PROVIDER_HTTP_PORT=${PROVIDER_HTTP_PORT}
              export HOSTENV_PROVIDER_HTTP_BIND=127.0.0.1

              if [ -n "''\${NIX_CONFIG:-}" ]; then
                export NIX_CONFIG="''\${NIX_CONFIG}
secret-key-files = ${NIX_SIGNING_KEY_FILE}"
              else
                export NIX_CONFIG="secret-key-files = ${NIX_SIGNING_KEY_FILE}"
              fi

              echo "Working in hostenv demo shell (SSH wrapper + signing key configured)."
              echo "Provider dev mode: HOSTENV_PROVIDER_DEV_DIR=\$HOSTENV_PROVIDER_DEV_DIR"
            '';
          };
        };
    };
}
EOF_DEMO_FLAKE
}

create_demo_symlink() {
  local existing_target=""

  if [[ -L "$DEMO_LINK_PATH" ]]; then
    existing_target="$(readlink -f "$DEMO_LINK_PATH" || true)"
    if [[ "$existing_target" == "$WORKDIR" ]]; then
      return 0
    fi
    if [[ -n "$existing_target" ]] && is_demo_workdir "$existing_target"; then
      rm -f "$DEMO_LINK_PATH"
    else
      fail "refusing to replace existing symlink at $DEMO_LINK_PATH"
    fi
  elif [[ -e "$DEMO_LINK_PATH" ]]; then
    fail "path already exists and is not a symlink: $DEMO_LINK_PATH"
  fi

  ln -s "$WORKDIR" "$DEMO_LINK_PATH"
}

prepare_node_a_baseline() {
  stage "Preparing node-a"
  log "Generating provider plan for node-a"
  run_provider_plan

  log "Generating comin node tokens"
  (
    cd "$PROVIDER_DIR"
    nix run .#hostenv-provider -- comin-tokens
  )

  load_plan_metadata || fail_stage "failed to load environment metadata from $PLAN_PATH"
  load_project_hash_from_plan || fail_stage "failed to resolve project hash from $PLAN_PATH"
  NODE_A_AUTH_TOKEN="$(load_comin_node_token "node-a")" || fail_stage "failed to load node-a auth token from provider secrets"
  # Note: node-b token will be loaded in prepare_node_b_baseline() after it's generated
  sync_hostctl_profile "node-a"

  log "Signing deploy closures for node-a"
  sign_deploy_profile "node-a" "system"
  sign_deploy_profile "node-a" "$ENV_USER"

  start_vm "node-a"
  ensure_provider_service_running
}

prepare_node_b_baseline() {
  stage "Preparing node-b"
  log "Capturing node-a plan snapshot for migration source"
  capture_task12_migration_source_plan

  log "Switching provider placement to node-b"
  write_provider_flake "node-b"

  log "Generating provider plan for node-b"
  log "Generating provider plan for node-b"
  run_provider_plan

  log "Generating comin node token for node-b"
  (
    cd "$PROVIDER_DIR"
    nix run .#hostenv-provider -- comin-tokens
  )

  load_plan_metadata || fail_stage "failed to load environment metadata from $PLAN_PATH after switching to node-b"
  load_project_hash_from_plan || fail_stage "failed to resolve project hash from $PLAN_PATH after switching to node-b"
  NODE_B_AUTH_TOKEN="$(load_comin_node_token "node-b")" || fail_stage "failed to load node-b auth token after switching to node-b"

  log "Signing deploy closures for node-b"
  sign_deploy_profile "node-b" "system"
  sign_deploy_profile "node-b" "$ENV_USER"

  start_vm "node-b"
}

run_automated_seed_import() {
  stage "Automated DB seed import"
  local env_ref
  env_ref="$(git -C "$PROJECT_DIR" symbolic-ref -q --short HEAD || true)"
  if [[ -z "$env_ref" ]]; then
    env_ref="main"
  fi
  (
    cd "$PROJECT_DIR/.hostenv"
    pv ./seed.sql.gz | gunzip -c | nix develop ".#${env_ref}" --command mysql
  )
}

show_intro_wizard() {
  stage "Local hostenv demo workspace ready"

  local body
  body="$(cat <<EOF_WIZ
The demo has prepared a local provider workspace, started VM node-a, and started provider-service.
It also configured a temporary hostctl profile so local hostnames resolve.

Use this symlink to inspect the generated files:
  ${DEMO_LINK_PATH}

Key paths:
  Provider repo: ${PROVIDER_DIR}
  Project repo:  ${PROJECT_DIR}
  VM logs:       ${LOG_DIR}

In another terminal, enter the demo shell before running commands:
  cd ${DEMO_LINK_PATH}
  nix develop

From now on the script will pause between stages.
At each pause: press <Enter> to continue, or press 'a' to abort and clean up.
EOF_WIZ
)"
  render_wizard_text "$body"
}

show_node_a_deploy_instructions() {
  stage "Step 1: Deploy to node-a"

  local body
  body="$(cat <<EOF_STEP
Run these commands in another terminal:

  cd ${DEMO_LINK_PATH}
  nix develop
  ${SCRIPT_DIR}/trigger-webhook.sh --workdir ${PROVIDER_SERVICE_RUNTIME_DIR} --project-hash ${PROJECT_HASH} --commit-sha ${NODE_A_DEPLOY_SHA} --node node-a --token ${NODE_A_AUTH_TOKEN} --evidence-prefix task-11-node-a

This window is now polling for completion.
Press 'a' at any time to abort and clean up.
After success is detected, press <Enter> here to continue.
EOF_STEP
)"
  render_wizard_text "$body"
}

show_seed_instructions() {
  stage "Step 2: Import Drupal seed database"

  local body
  body="$(cat <<EOF_STEP
Run these commands in another terminal:

  cd ${DEMO_LINK_PATH}
  nix develop
  cd ${PROJECT_DIR}/.hostenv
  nix develop ".#\$(git symbolic-ref -q --short HEAD || echo main)"

Inside the dev shell run:

  pv ./seed.sql.gz | gunzip -c | mysql
  exit

This window is now polling for completion.
Press 'a' at any time to abort and clean up.
After success is detected, press <Enter> here to continue.
EOF_STEP
)"
  render_wizard_text "$body"
}

show_node_b_migration_instructions() {
  stage "Step 3: Migrate to node-b"

  local body
  body="$(cat <<EOF_STEP
Run these commands in another terminal:

  cd ${DEMO_LINK_PATH}
  nix develop
  ${SCRIPT_DIR}/trigger-webhook.sh --workdir ${PROVIDER_SERVICE_RUNTIME_DIR} --project-hash ${PROJECT_HASH} --commit-sha ${NODE_B_DEPLOY_SHA} --node node-b --token ${NODE_B_AUTH_TOKEN} --evidence-prefix task-11-node-b

This window is now polling for completion.
Press 'a' at any time to abort and clean up.
After success is detected, press <Enter> here to continue.
EOF_STEP
)"
  render_wizard_text "$body"
}

run_wizard_flow() {
  show_intro_wizard
  prompt_continue_or_abort

  NODE_A_DEPLOY_SHA="task11-node-a-$(date +%s)"
  show_node_a_deploy_instructions

  if ! wait_for_job_id_by_sha "$NODE_A_DEPLOY_SHA" "node-a" "$NODE_A_AUTH_TOKEN" 5400; then
    fail_stage "timed out waiting for node-a deploy intent creation"
  fi
  NODE_A_JOB_ID="$FOUND_JOB_ID"

  if ! wait_for_provider_job_completion "$NODE_A_JOB_ID" "node-a" "$NODE_A_AUTH_TOKEN" 5400 "$TASK11_NODE_A_STATUS_LOG"; then
    fail_stage "node-a deployment job did not complete successfully; see $TASK11_NODE_A_STATUS_LOG"
  fi

  if ! poll_until_or_abort 1200 "Waiting for node-a deployment to become reachable..." condition_node_a_deployed; then
    fail_stage "timed out waiting for node-a deployment to become reachable"
  fi

  success "node-a deployment detected."
  log "View node-a installer page with: curl http://${VHOST}:${NODE_HTTP_PORT}/"
  run_task8_post_deploy_verification
  run_task10_webhook_deploy_intent_verification
  prompt_continue_or_abort

  show_seed_instructions

  if ! poll_until_or_abort 5400 "Waiting for seed import to complete on node-a..." condition_seed_imported_node_a; then
    fail_stage "timed out waiting for Drupal seed import on node-a"
  fi

  success "Drupal seed import detected on node-a."
  log "View node-a site with: curl http://${VHOST}:${NODE_HTTP_PORT}/"

  prompt_continue_or_abort
  prepare_node_b_baseline

  apply_task12_migration_source_plan
  NODE_B_DEPLOY_SHA="task11-node-b-$(date +%s)"
  show_node_b_migration_instructions

  if ! wait_for_job_id_by_sha "$NODE_B_DEPLOY_SHA" "node-b" "$NODE_B_AUTH_TOKEN" 5400; then
    fail_stage "timed out waiting for node-b migration deploy intent creation"
  fi
  NODE_B_JOB_ID="$FOUND_JOB_ID"

  if ! wait_for_provider_job_completion "$NODE_B_JOB_ID" "node-b" "$NODE_B_AUTH_TOKEN" 5400 "$TASK11_NODE_B_STATUS_LOG"; then
    fail_stage "node-b migration job did not complete successfully; see $TASK11_NODE_B_STATUS_LOG"
  fi

  if ! poll_until_or_abort 1200 "Waiting for migration deploy to node-b to become reachable..." condition_node_b_migrated; then
    fail_stage "timed out waiting for migrated site on node-b"
  fi

  sync_hostctl_profile "node-b"
  success "Migration to node-b detected."
  assert_not_installer_page "$VHOST" "http://${NODE_B_HOST_IP}:${NODE_HTTP_PORT}/"
  run_task12_backup_restore_verification
  run_task12_migration_marker_verification
  prompt_continue_or_abort

  stage "Demo completed"
  log "View node-b site with: curl http://${VHOST}:${NODE_HTTP_PORT}/"
  log "Provider workspace: $PROVIDER_DIR"
  log "VM logs: $LOG_DIR"
}

run_automated_flow() {
  stage "Automated deploy: node-a"
  NODE_A_DEPLOY_SHA="task11-node-a-$(date +%s)"
  NODE_A_JOB_ID="$(trigger_webhook_deploy "node-a" "$NODE_A_AUTH_TOKEN" "$NODE_A_DEPLOY_SHA" "task-11-node-a")"

  if ! wait_for_provider_job_completion "$NODE_A_JOB_ID" "node-a" "$NODE_A_AUTH_TOKEN" 1800 "$TASK11_NODE_A_STATUS_LOG"; then
    fail_stage "node-a deployment job did not complete successfully; see $TASK11_NODE_A_STATUS_LOG"
  fi

  if ! poll_until_or_abort 1200 "Waiting for node-a deployment to become ready..." condition_node_a_deployed; then
    fail_stage "node-a deployment did not become ready"
  fi

  run_task8_post_deploy_verification
  run_task10_webhook_deploy_intent_verification

  run_automated_seed_import

  if ! poll_until_or_abort 1800 "Waiting for seed import to complete on node-a..." condition_seed_imported_node_a; then
    fail_stage "seed import did not complete on node-a"
  fi

  prepare_node_b_baseline

  stage "Automated deploy: node-b migration"
  apply_task12_migration_source_plan
  NODE_B_DEPLOY_SHA="task11-node-b-$(date +%s)"
  NODE_B_JOB_ID="$(trigger_webhook_deploy "node-b" "$NODE_B_AUTH_TOKEN" "$NODE_B_DEPLOY_SHA" "task-11-node-b")"

  if ! wait_for_provider_job_completion "$NODE_B_JOB_ID" "node-b" "$NODE_B_AUTH_TOKEN" 1800 "$TASK11_NODE_B_STATUS_LOG"; then
    fail_stage "node-b migration job did not complete successfully; see $TASK11_NODE_B_STATUS_LOG"
  fi

  if ! poll_until_or_abort 1200 "Waiting for migrated site on node-b..." condition_node_b_migrated; then
    fail_stage "node-b migration did not become ready"
  fi

  sync_hostctl_profile "node-b"
  assert_not_installer_page "$VHOST" "http://${NODE_B_HOST_IP}:${NODE_HTTP_PORT}/"
  run_task12_backup_restore_verification
  run_task12_migration_marker_verification

  stage "Demo completed"
  log "View site via local hostctl mapping: curl http://${VHOST}:${NODE_HTTP_PORT}/"
  log "Provider workspace: $PROVIDER_DIR"
  log "VM logs: $LOG_DIR"
}

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd -- "$SCRIPT_DIR/../.." && pwd)"
CALLER_DIR="$(pwd)"
PROVIDER_TEMPLATE_DIR="$REPO_ROOT/template/provider"
DEMO_FIXTURE_DIR="$SCRIPT_DIR/demo-project"
SEED_SOURCE="$SCRIPT_DIR/seed/seed.sql.gz"

HOSTENV_HOSTNAME="demo.hostenv.test"
PROVIDER_API_VM_GATEWAY="10.0.2.2"
PROVIDER_HTTP_PORT=8080
NODE_A_HOST_IP="127.0.0.2"
NODE_B_HOST_IP="127.0.0.3"
NODE_SSH_PORT=2222
NODE_HTTP_PORT=8080

WORKDIR=""
AUTO_CLEANUP=0
TEARDOWN_ONLY=0
MODE="wizard"
ABORTED=0

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
    --cleanup)
      AUTO_CLEANUP=1
      ;;
    --teardown)
      TEARDOWN_ONLY=1
      ;;
    --automated)
      MODE="automated"
      ;;
    --no-color)
      NO_COLOR=1
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

if [[ "$NO_COLOR" -eq 1 ]]; then
  export NO_COLOR=1
fi

setup_colors

ensure_demo_tools_available "${ORIGINAL_ARGS[@]}"

require_cmd hostctl
configure_hostctl_command

if [[ "$TEARDOWN_ONLY" -eq 1 ]]; then
  run_teardown "$WORKDIR"
  exit 0
fi

for cmd in nix jq git ssh ssh-keygen sops age-keygen curl sed find awk pv gunzip gzip; do
  require_cmd "$cmd"
done
if [[ "$MODE" == "wizard" ]]; then
  require_cmd gum
  USE_GUM=1
fi

[[ -e /dev/kvm ]] || fail "/dev/kvm not found; this demo requires Linux with KVM"
[[ -f "$SEED_SOURCE" ]] || fail "missing seed file: $SEED_SOURCE"

choose_free_port "$NODE_SSH_PORT"
NODE_SSH_PORT="$CHOSEN_PORT"
choose_free_port "$NODE_HTTP_PORT"
NODE_HTTP_PORT="$CHOSEN_PORT"

log "Using forwarded endpoints: node-a=${NODE_A_HOST_IP} ssh:${NODE_SSH_PORT} http:${NODE_HTTP_PORT}, node-b=${NODE_B_HOST_IP} ssh:${NODE_SSH_PORT} http:${NODE_HTTP_PORT}"

if [[ -z "$WORKDIR" ]]; then
  WORKDIR="$(mktemp -d /tmp/hostenv-local-vm-demo-XXXXXX)"
else
  mkdir -p "$WORKDIR"
fi
WORKDIR="$(cd -- "$WORKDIR" && pwd)"

PROVIDER_DIR="$WORKDIR/provider"
PROJECT_DIR="$WORKDIR/demo-project"
SHARED_DIR="$WORKDIR/shared"
SSH_DIR="$WORKDIR/ssh"
SSH_KEY="$SSH_DIR/id_ed25519"
SSH_CONFIG="$SSH_DIR/config"
NIX_SIGNING_KEY_FILE="$PROVIDER_DIR/secrets/nix-signing.key"
LOG_DIR="$WORKDIR/logs"
PIDS_DIR="$WORKDIR/pids"
WRAPPER_DIR="$WORKDIR/bin"
PLAN_PATH="$PROVIDER_DIR/generated/plan.json"
DEMO_LINK_PATH="$CALLER_DIR/hostenv-demo"
HOSTCTL_PROFILE="$(profile_name_for_workdir "$WORKDIR")"
HOSTCTL_SOURCE_FILE="$WORKDIR/hostctl-profile.hosts"
EVIDENCE_DIR="$REPO_ROOT/.sisyphus/evidence"
TASK8_ACTIVATION_LOG="$EVIDENCE_DIR/task-8-activation-script.log"
TASK8_API_LOG="$EVIDENCE_DIR/task-8-provider-api.log"
TASK8_MANUAL_TRIGGER_LOG="$EVIDENCE_DIR/task-8-manual-comin-trigger.log"
TASK10_RUNNER_LOG="$EVIDENCE_DIR/task-10-run-demo-hook.log"
TASK10_WEBHOOK_LOG="$EVIDENCE_DIR/task-10-webhook-trigger.log"
TASK10_DEPLOY_INTENT_LOG="$EVIDENCE_DIR/task-10-deploy-intent.log"
TASK11_PROVIDER_SERVICE_LOG="$EVIDENCE_DIR/task-11-provider-service.log"
TASK11_NODE_A_STATUS_LOG="$EVIDENCE_DIR/task-11-node-a-job-status.log"
TASK11_NODE_B_STATUS_LOG="$EVIDENCE_DIR/task-11-node-b-job-status.log"
TASK12_BACKUP_LOG="$EVIDENCE_DIR/task-12-backup.log"
TASK12_MIGRATION_VERIFY_LOG="$EVIDENCE_DIR/task-12-migration-verify.log"

printf '%s\n' "hostenv-local-vm-demo" > "$WORKDIR/.hostenv-local-vm-demo"

VM_PIDS=()
ENV_USER=""
RUNTIME_DIR=""
DATA_DIR=""
ENV_HOST=""
VHOST=""
MIGRATE_BACKUP=""
PROJECT_HASH=""
PROVIDER_SERVICE_RUNTIME_DIR="$WORKDIR/provider-dev"
PROVIDER_SERVICE_SOCKET="$PROVIDER_SERVICE_RUNTIME_DIR/hostenv-provider.sock"
PROVIDER_SERVICE_PID=""
NODE_A_AUTH_TOKEN=""
NODE_B_AUTH_TOKEN=""
NODE_A_DEPLOY_SHA=""
NODE_B_DEPLOY_SHA=""
NODE_A_JOB_ID=""
NODE_B_JOB_ID=""
FOUND_JOB_ID=""
TASK12_MIGRATION_SOURCE_PLAN="$PROVIDER_DIR/generated/task-12-migration-source-plan.json"

cleanup() {
  local code=$?
  trap - EXIT INT TERM

  for pid in "${VM_PIDS[@]:-}"; do
    kill_pid_if_running "$pid"
  done

  kill_pid_if_running "$PROVIDER_SERVICE_PID"

  if [[ -d "$PIDS_DIR" ]]; then
    for pidfile in "$PIDS_DIR"/*.pid; do
      [[ -e "$pidfile" ]] || continue
      kill_pid_if_running "$(cat "$pidfile" 2>/dev/null || true)"
    done
  fi

  if [[ -L "$DEMO_LINK_PATH" ]]; then
    local target
    target="$(readlink -f "$DEMO_LINK_PATH" || true)"
    if [[ "$target" == "$WORKDIR" ]]; then
      rm -f "$DEMO_LINK_PATH"
    fi
  fi

  remove_hostctl_profile "$HOSTCTL_PROFILE"

  if [[ "$AUTO_CLEANUP" -eq 1 || "$ABORTED" -eq 1 || "$DEMO_FAILED" -eq 1 || "$code" -ne 0 ]]; then
    rm -rf "$WORKDIR"
    log "Removed workdir $WORKDIR"
  else
    log "Workdir kept at $WORKDIR"
  fi

  exit "$code"
}
trap cleanup EXIT
trap abort_demo INT TERM

rm -rf "$PROVIDER_DIR" "$PROJECT_DIR" "$SHARED_DIR" "$SSH_DIR" "$LOG_DIR" "$PIDS_DIR" "$WRAPPER_DIR"
mkdir -p "$PROVIDER_DIR" "$PROJECT_DIR" "$SHARED_DIR" "$SSH_DIR" "$LOG_DIR" "$PIDS_DIR" "$WRAPPER_DIR"
chmod 700 "$SSH_DIR"
mkdir -p "$SHARED_DIR/backups"
chmod 0777 "$SHARED_DIR" "$SHARED_DIR/backups"

remove_hostctl_profile "$HOSTCTL_PROFILE"

log "Copying demo fixture and provider template"
cp -a "$DEMO_FIXTURE_DIR/." "$PROJECT_DIR/"
cp -a "$PROVIDER_TEMPLATE_DIR/." "$PROVIDER_DIR/"
mkdir -p "$PROVIDER_DIR/secrets" "$PROVIDER_DIR/generated"
printf '{}\n' > "$PROVIDER_DIR/generated/state.json"

log "Copying Drupal seed file into project"
cp "$SEED_SOURCE" "$PROJECT_DIR/.hostenv/seed.sql.gz"

log "Generating SSH keypair for deploy and environment access"
rm -f "$SSH_KEY" "$SSH_KEY.pub"
ssh-keygen -q -t ed25519 -N "" -f "$SSH_KEY"
SSH_PUBLIC_KEY="$(cat "$SSH_KEY.pub")"

log "Generating age key for sops secrets"
rm -f "$PROVIDER_DIR/secrets/demo-age.key"
age-keygen -o "$PROVIDER_DIR/secrets/demo-age.key" >/dev/null
AGE_PRIVATE_KEY="$(cat "$PROVIDER_DIR/secrets/demo-age.key")"
export SOPS_AGE_KEY="$AGE_PRIVATE_KEY"
AGE_PUBLIC_KEY="$(grep '^# public key:' "$PROVIDER_DIR/secrets/demo-age.key" | awk '{print $4}')"
[[ -n "$AGE_PUBLIC_KEY" ]] || fail "failed to read age public key"

log "Generating local Nix signing key for VM deploy trust"
nix key generate-secret --key-name "hostenv-demo-local" > "$NIX_SIGNING_KEY_FILE"
NIX_SIGNING_PUBLIC_KEY="$(nix key convert-secret-to-public < "$NIX_SIGNING_KEY_FILE")"
[[ -n "$NIX_SIGNING_PUBLIC_KEY" ]] || fail "failed to read Nix signing public key"

log "Patching demo project placeholders"
escaped_hostenv_path="$(escape_sed "path:${REPO_ROOT}")"
escaped_ssh_key="$(escape_sed "$SSH_PUBLIC_KEY")"
sed -i "s|HOSTENV_INPUT_URL_PLACEHOLDER|${escaped_hostenv_path}|g" "$PROJECT_DIR/.hostenv/flake.nix"
sed -i "s|SSH_PUBLIC_KEY_PLACEHOLDER|${escaped_ssh_key}|g" "$PROJECT_DIR/.hostenv/hostenv.nix"

log "Initializing demo project git repository"
init_git_repo "$PROJECT_DIR"

log "Writing provider configuration for initial deploy on node-a"
write_provider_flake "node-a"
write_node_config "node-a" "$NODE_A_HOST_IP" "$NODE_SSH_PORT" "$NODE_HTTP_PORT"
write_node_config "node-b" "$NODE_B_HOST_IP" "$NODE_SSH_PORT" "$NODE_HTTP_PORT"

log "Encrypting provider secrets with sops"
cat > "$PROVIDER_DIR/secrets/provider.plain.yaml" <<'EOF_SECRETS'
access_tokens: ""
demo:
  backups_secret: "hostenv-demo-password"
  backups_env: "RESTIC_COMPRESSION=off"
EOF_SECRETS
SOPS_AGE_RECIPIENTS="$AGE_PUBLIC_KEY" \
  sops --encrypt --input-type yaml --output-type yaml \
  "$PROVIDER_DIR/secrets/provider.plain.yaml" > "$PROVIDER_DIR/secrets/provider.yaml"
rm -f "$PROVIDER_DIR/secrets/provider.plain.yaml"

log "Initializing provider git repository"
init_git_repo "$PROVIDER_DIR"

log "Writing local SSH config"
cat > "$SSH_CONFIG" <<EOF_SSH
Host *.${HOSTENV_HOSTNAME}
  Port ${NODE_SSH_PORT}
  UserKnownHostsFile /dev/null
  StrictHostKeyChecking no
  IdentityFile ${SSH_KEY}

Host *
  BatchMode yes
  IdentitiesOnly yes
  UserKnownHostsFile /dev/null
  StrictHostKeyChecking no
  IdentityFile ${SSH_KEY}
EOF_SSH
chmod 600 "$SSH_CONFIG"

write_ssh_wrapper

export PATH="$WRAPPER_DIR:$PATH"
export GIT_SSH_COMMAND="ssh -F $SSH_CONFIG"
if [[ -n "${NIX_CONFIG:-}" ]]; then
  export NIX_CONFIG="${NIX_CONFIG}"$'\n'"secret-key-files = ${NIX_SIGNING_KEY_FILE}"
else
  export NIX_CONFIG="secret-key-files = ${NIX_SIGNING_KEY_FILE}"
fi

write_demo_shell_flake
create_demo_symlink

prepare_node_a_baseline

if [[ "$MODE" == "automated" ]]; then
  run_automated_flow
else
  run_wizard_flow
fi
