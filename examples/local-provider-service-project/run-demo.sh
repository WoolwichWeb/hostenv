#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd -- "$SCRIPT_DIR/../.." && pwd)"
CALLER_DIR="$(pwd)"

PROVIDER_TEMPLATE_DIR="$REPO_ROOT/template/provider"
APP_FIXTURE_DIR="$SCRIPT_DIR/demo-app-project"
PROVIDER_SERVICE_FIXTURE_DIR="$SCRIPT_DIR/provider-service-project"
SEED_SOURCE="$SCRIPT_DIR/seed/seed.sql.gz"
PROVIDER_FLAKE_TEMPLATE="$SCRIPT_DIR/provider-flake.nix.tmpl"
NODE_CONFIG_TEMPLATE="$SCRIPT_DIR/node-configuration.nix.tmpl"
EVIDENCE_DIR="$REPO_ROOT/.sisyphus/evidence"

HOSTENV_HOSTNAME="demo.hostenv.test"
PROVIDER_SERVICE_HOST="provider.demo.hostenv.test"
PROVIDER_SERVICE_PROJECT_SELECTOR="providerservice"
NODE_A_HOST_IP="127.0.0.2"
NODE_B_HOST_IP="127.0.0.3"
NODE_SSH_PORT=2222
NODE_HTTP_PORT=8080
PROVIDER_HTTP_PORT=18080
PROVIDER_API_VM_GATEWAY="10.0.2.2"
JOB_COMPLETION_TIMEOUT=600
JOB_ACTION_TIMEOUT=1200
PLAN_COMMAND_TIMEOUT=1800
NIX_BUILD_TIMEOUT=3600
FLAKE_ARCHIVE_TIMEOUT=1800
SEED_IMPORT_TIMEOUT=1800

AUTOMATED=0
CLEANUP=0
DEMO_FAILED=0
NO_COLOR=0
USE_GUM=1

WORKDIR=""
PROVIDER_DIR=""
APP_PROJECT_DIR=""
PROVIDER_SERVICE_PROJECT_DIR=""
HOSTENV_SOURCE_DIR="" SSH_DIR=""
SSH_KEY=""
SSH_CONFIG=""
SHARED_DIR=""
LOG_DIR=""
PIDS_DIR=""
PLAN_PATH=""
APP_ENV_NAME=""
APP_ENV_USER=""
APP_ENV_UID=""
APP_RUNTIME_DIR=""
APP_VHOST=""
APP_PROJECT_HASH=""

PROVIDER_ENV_NAME=""
PROVIDER_ENV_USER=""
PROVIDER_ENV_UID=""
PROVIDER_RUNTIME_DIR=""
PROVIDER_VHOST=""

AGE_PRIVATE_KEY=""
AGE_PUBLIC_KEY=""
SSH_PUBLIC_KEY=""
NIX_SIGNING_KEY_FILE=""
NIX_SIGNING_PUBLIC_KEY=""

NODE_A_AUTH_TOKEN=""
NODE_B_AUTH_TOKEN=""
CACHE_AUTH_PASSWORD=""
APP_WEBHOOK_SECRET="hostenv-demo-webhook"
NODE_A_JOB_ID=""
NODE_B_JOB_ID=""

PROVIDER_BRIDGE_PID=""
VM_PIDS=()

usage() {
  cat <<'USAGE'
Usage: run-demo.sh [--automated] [--cleanup] [--no-color]

Modes:
  default       Interactive mode with pauses
  --automated   Fully non-interactive demo run

Options:
  --cleanup     Remove workdir after successful completion
  --no-color    Disable gum-based styling
  --help, -h    Show this help text
USAGE
}

log() { printf '[%s] %s\n' "$(date +%H:%M:%S)" "$*"; }

plain() { printf '%s\n' "$*"; }

info() {
  if [[ "$USE_GUM" -eq 1 ]]; then
    gum style --foreground 212 "$*"
  else
    plain "$*"
  fi
}

warn() {
  if [[ "$USE_GUM" -eq 1 ]]; then
    gum style --foreground 208 "$*"
  else
    plain "$*"
  fi
}

success() {
  if [[ "$USE_GUM" -eq 1 ]]; then
    gum style --foreground 82 --bold "$*"
  else
    plain "$*"
  fi
}

fail() {
  DEMO_FAILED=1
  if [[ "$USE_GUM" -eq 1 ]]; then
    gum style --foreground 196 --bold "ERROR: $*" >&2
  else
    printf 'ERROR: %s\n' "$*" >&2
  fi
  exit 1
}

stage() {
  if [[ "$USE_GUM" -eq 1 ]]; then
    gum style --bold --foreground 63 --border rounded --border-foreground 240 --padding "0 1" "== $* =="
  else
    printf '== %s ==\n' "$*"
  fi
}

pause_for_exploration() {
  local message="$1"
  shift

  if [[ "$AUTOMATED" -eq 1 ]]; then
    return 0
  fi

  if [[ "$USE_GUM" -eq 1 ]]; then
    {
      printf '%s\n\n' "$message"
      if [[ "$#" -gt 0 ]]; then
        printf 'Things to explore:\n'
        for item in "$@"; do
          printf -- '- %s\n' "$item"
        done
      fi
    } | gum format
    gum confirm --default=true "Continue?" || exit 130
  else
    printf '%s\n' "$message"
    for item in "$@"; do
      printf -- '- %s\n' "$item"
    done
    read -r -p 'Continue? [Y/n] ' reply
    [[ -z "$reply" || "$reply" =~ ^[Yy]$ ]] || exit 130
  fi
}

require_cmd() {
  local cmd="$1"
  command -v "$cmd" >/dev/null 2>&1 || fail "Missing '$cmd'. Run from the repo Nix environment (for example: nix develop)."
}

run_with_timeout() {
  local timeout_seconds="$1"
  local label="$2"
  shift 2

  if timeout --foreground "$timeout_seconds" "$@"; then
    return 0
  fi

  local rc=$?
  if [[ "$rc" -eq 124 ]]; then
    fail "$label timed out after ${timeout_seconds}s"
  fi
  return "$rc"
}

run_with_timeout_capture() {
  local timeout_seconds="$1"
  local label="$2"
  shift 2
  local output

  if output="$(timeout --foreground "$timeout_seconds" "$@")"; then
    printf '%s\n' "$output"
    return 0
  fi

  local rc=$?
  if [[ "$rc" -eq 124 ]]; then
    fail "$label timed out after ${timeout_seconds}s"
  fi
  return "$rc"
}

port_in_use() {
  local address="$1"
  local port="$2"
  local endpoints
  endpoints="$(ss -H -ltn | awk '{print $4}')"
  grep -Eq "(^|[[:space:]])(${address}:${port}|0\.0\.0\.0:${port}|\*:${port}|\[::\]:${port})([[:space:]]|$)" <<<"$endpoints"
}

choose_free_port() {
  local address="$1"
  local port="$2"
  while port_in_use "$address" "$port"; do
    port=$((port + 1))
  done
  printf '%s\n' "$port"
}

escape_sed_replacement() {
  printf '%s' "$1" | sed -e 's/[|&\\]/\\&/g'
}

kill_pid_if_running() {
  local pid="$1"
  local _
  [[ "$pid" =~ ^[0-9]+$ ]] || return 0

  if ! kill -0 "$pid" >/dev/null 2>&1; then
    return 0
  fi

  kill "$pid" >/dev/null 2>&1 || true
  for _ in 1 2 3 4 5; do
    if ! kill -0 "$pid" >/dev/null 2>&1; then
      wait "$pid" >/dev/null 2>&1 || true
      return 0
    fi
    sleep 1
  done

  kill -9 "$pid" >/dev/null 2>&1 || true
  wait "$pid" >/dev/null 2>&1 || true
}

kill_demo_processes_for_workdir() {
  local dir="$1"
  local pid
  local cmd
  local line
  local _

  command -v ps >/dev/null 2>&1 || return 0

  for _ in 1 2; do
    while IFS= read -r line; do
      [[ "$line" =~ ^[[:space:]]*([0-9]+)[[:space:]]+(.*)$ ]] || continue
      pid="${BASH_REMATCH[1]}"
      cmd="${BASH_REMATCH[2]}"
      [[ "$cmd" == *"$dir"* ]] || continue

      case "$cmd" in
        *qemu-system*|*socat*)
          kill_pid_if_running "$pid"
          ;;
      esac
    done < <(ps -eo pid=,args= 2>/dev/null || true)
    sleep 1
  done
}

cleanup() {
  local code=$?
  trap - EXIT INT TERM

  for pid in "${VM_PIDS[@]:-}"; do
    kill_pid_if_running "$pid"
  done

  if [[ -d "$PIDS_DIR" ]]; then
    local pidfile
    for pidfile in "$PIDS_DIR"/*.pid; do
      [[ -e "$pidfile" ]] || continue
      kill_pid_if_running "$(<"$pidfile")"
    done
  fi

  kill_pid_if_running "$PROVIDER_BRIDGE_PID"
  if [[ -n "$WORKDIR" ]]; then
    kill_demo_processes_for_workdir "$WORKDIR"
  fi

  if [[ -L "$CALLER_DIR/hostenv-demo" ]] && [[ "$(readlink -f "$CALLER_DIR/hostenv-demo" || true)" == "$WORKDIR" ]]; then
    rm -f "$CALLER_DIR/hostenv-demo"
  fi

  if [[ "$code" -ne 0 || "$DEMO_FAILED" -eq 1 ]]; then
    [[ -n "$WORKDIR" ]] && log "Workdir kept for debugging: $WORKDIR"
  elif [[ "$CLEANUP" -eq 1 ]]; then
    rm -rf "$WORKDIR"
    log "Removed workdir: $WORKDIR"
  else
    log "Workdir kept: $WORKDIR"
  fi

  exit "$code"
}

wait_for_ssh() {
  local host_alias="$1"
  local timeout="${2:-420}"
  local start
  start="$(date +%s)"

  while true; do
    if ssh -vv -F "$SSH_CONFIG" -o ConnectTimeout=2 "root@${host_alias}" true ; then
      return 0
    fi
    if (( "$(date +%s)" - start > timeout )); then
      return 1
    fi
    sleep 2
  done
}

wait_for_remote_socket() {
  local host_alias="$1"
  local socket_path="$2"
  local timeout="${3:-180}"
  local start
  start="$(date +%s)"

  while true; do
    if ssh -F "$SSH_CONFIG" "root@${host_alias}" "test -S '$socket_path'" >/dev/null 2>&1; then
      return 0
    fi
    if (( "$(date +%s)" - start >= timeout )); then
      return 1
    fi
    sleep 2
  done
}

provider_user_ssh() {
  local remote_cmd="$1"
  local max_attempts=3
  local attempt=1
  local delay=1
  local ssh_exit
  
  while true; do
    ssh -vv -F "$SSH_CONFIG" "${PROVIDER_ENV_USER}@node-a.${HOSTENV_HOSTNAME}" \
      "set -euo pipefail; export HOME=\$HOME; export XDG_RUNTIME_DIR=/run/user/\$(id -u); export XDG_CACHE_HOME=\$HOME/.cache; export XDG_CONFIG_HOME=\$HOME/.config; export XDG_DATA_HOME=\$HOME/.local/share; export XDG_STATE_HOME=\$HOME/.local/state; ${remote_cmd}"
    ssh_exit=$?
    
    if (( ssh_exit == 0 )); then
      return 0
    fi
    
    if (( ssh_exit != 255 )); then
      return "$ssh_exit"
    fi
    
    if (( attempt >= max_attempts )); then
      return "$ssh_exit"
    fi
    
    sleep "$delay"
    attempt=$((attempt + 1))
    delay=$((delay * 2))
  done
}

sanitize_remote_gitconfig() {
  local log_file="$LOG_DIR/provider-service-gitconfig-fix.log"

  ssh -F "$SSH_CONFIG" "root@node-a.${HOSTENV_HOSTNAME}" \
    "set -euo pipefail; if [[ -e /etc/gitconfig ]]; then cp -f /etc/gitconfig /tmp/hostenv-demo-etc-gitconfig.bak || true; rm -f /etc/gitconfig; fi; printf '%s\n' '[safe]' $'\tdirectory = *' > /etc/gitconfig; chmod 0644 /etc/gitconfig" \
    > "$log_file" 2>&1
}

wait_for_provider_user_services() {
  local timeout="${1:-180}"
  local start
  start="$(date +%s)"

  while true; do
    # Single SSH call to check all service statuses at once
    if provider_user_ssh "systemctl --user is-active --quiet postgresql.service hostenv-provider.service" >/dev/null 2>&1; then
      return 0
    fi
    if (( "$(date +%s)" - start >= timeout )); then
      return 1
    fi
    sleep 2
  done
}

capture_provider_bootstrap_diagnostics() {
  local provider_socket="$1"
  local bootstrap_log="$2"
  local user_manager_log="$LOG_DIR/provider-service-user-manager.log"
  local user_status_log="$LOG_DIR/provider-service-user-status.log"
  local user_journal_log="$LOG_DIR/provider-service-user-journal.log"
  local runtime_log="$LOG_DIR/provider-service-runtime-dir.log"

  ssh -F "$SSH_CONFIG" "root@node-a.${HOSTENV_HOSTNAME}" \
    "systemctl --no-pager --full status user@${PROVIDER_ENV_UID}.service" \
    > "$user_manager_log" 2>&1 || true

  provider_user_ssh "systemctl --user --no-pager --full status default.target sockets.target postgresql.service hostenv-provider-cache-auth.service fcgiwrap.socket fcgiwrap.service harmonia.service nginx.service hostenv-provider.service" \
    > "$user_status_log" 2>&1 || true

  provider_user_ssh "journalctl --user --no-pager -b -n 200 -u postgresql.service -u hostenv-provider-cache-auth.service -u fcgiwrap.socket -u fcgiwrap.service -u harmonia.service -u nginx.service -u hostenv-provider.service" \
    > "$user_journal_log" 2>&1 || true

  ssh -F "$SSH_CONFIG" "root@node-a.${HOSTENV_HOSTNAME}" \
    "set -euo pipefail; ls -lah '${PROVIDER_RUNTIME_DIR}'; printf '\n'; ls -lah '/run/secrets/${PROVIDER_ENV_USER}' 2>/dev/null || true; printf '\n'; stat '${provider_socket}' 2>/dev/null || true" \
    > "$runtime_log" 2>&1 || true

  {
    printf 'bootstrap_stdout_log=%s\n' "$LOG_DIR/provider-service-bootstrap.log"
    printf 'user_manager_status_log=%s\n' "$user_manager_log"
    printf 'user_unit_status_log=%s\n' "$user_status_log"
    printf 'user_journal_log=%s\n' "$user_journal_log"
    printf 'runtime_dir_listing_log=%s\n' "$runtime_log"
  } >> "$bootstrap_log"
}

capture_provider_cache_diagnostics() {
  local unauth_code="$1"
  local auth_code="$2"
  local body_file="$3"
  local summary_log="$LOG_DIR/provider-service-cache-diagnostics.log"
  local unauth_body_log="$LOG_DIR/provider-service-cache-unauth-body.log"
  local auth_body_log="$LOG_DIR/provider-service-cache-auth-body.log"
  local status_log="$LOG_DIR/provider-service-cache-status.log"
  local journal_log="$LOG_DIR/provider-service-cache-journal.log"
  local runtime_log="$LOG_DIR/provider-service-cache-runtime.log"
  local cache_url="http://${NODE_A_HOST_IP}:${NODE_HTTP_PORT}/cache/nix-cache-info"

  curl --connect-timeout 3 --max-time 10 -sS -H "Host: ${PROVIDER_VHOST}" "$cache_url" > "$unauth_body_log" 2>&1 || true
  curl --connect-timeout 3 --max-time 10 -sS -H "Host: ${PROVIDER_VHOST}" -u "cache:${CACHE_AUTH_PASSWORD}" "$cache_url" > "$auth_body_log" 2>&1 || true

  provider_user_ssh "systemctl --user --no-pager --full status hostenv-provider-cache-auth.service fcgiwrap.socket fcgiwrap.service harmonia.service nginx.service" \
    > "$status_log" 2>&1 || true

  provider_user_ssh "journalctl --user --no-pager -b -n 200 -u hostenv-provider-cache-auth.service -u fcgiwrap.socket -u fcgiwrap.service -u harmonia.service -u nginx.service" \
    > "$journal_log" 2>&1 || true

  ssh -F "$SSH_CONFIG" "root@node-a.${HOSTENV_HOSTNAME}" \
    "set -euo pipefail; ls -lah '${PROVIDER_RUNTIME_DIR}'; printf '\n'; stat '${PROVIDER_RUNTIME_DIR}/harmonia.sock' 2>/dev/null || true; printf '\n'; stat '${PROVIDER_RUNTIME_DIR}/fcgiwrap.sock' 2>/dev/null || true" \
    > "$runtime_log" 2>&1 || true

  {
    printf 'cache_url=%s\n' "$cache_url"
    printf 'cache_unauth_http=%s\n' "$unauth_code"
    printf 'cache_auth_http=%s\n' "$auth_code"
    printf 'phase_d_body_file=%s\n' "$body_file"
    printf 'unauth_body_log=%s\n' "$unauth_body_log"
    printf 'auth_body_log=%s\n' "$auth_body_log"
    printf 'status_log=%s\n' "$status_log"
    printf 'journal_log=%s\n' "$journal_log"
    printf 'runtime_log=%s\n' "$runtime_log"
  } > "$summary_log"
}

capture_provider_webhook_diagnostics() {
  local response_code="$1"
  local response_body_file="$2"
  local summary_log="$LOG_DIR/provider-service-webhook-diagnostics.log"
  local status_log="$LOG_DIR/provider-service-webhook-status.log"
  local journal_log="$LOG_DIR/provider-service-webhook-journal.log"
  local db_log="$LOG_DIR/provider-service-webhook-db.log"
  local worktree_log="$LOG_DIR/provider-service-webhook-worktree.log"
  local remote_worktree="/home/${PROVIDER_ENV_USER}/.local/share/hostenv-provider/work/provider"

  provider_user_ssh "systemctl --user --no-pager --full status postgresql.service hostenv-provider.service nginx.service harmonia.service fcgiwrap.socket" \
    > "$status_log" 2>&1 || true

  provider_user_ssh "journalctl --user --no-pager -b -n 300 -u postgresql.service -u hostenv-provider.service -u nginx.service -u harmonia.service -u fcgiwrap.socket" \
    > "$journal_log" 2>&1 || true

  provider_db_query "
SELECT 'jobs' AS section, id::text, kind, status, COALESCE(error_summary, '')
FROM jobs
ORDER BY created_at DESC
LIMIT 20;

SELECT 'events' AS section, job_id, node, phase, status, COALESCE(message, '')
FROM deploy_node_events
ORDER BY id DESC
LIMIT 40;

SELECT 'actions' AS section, job_id, node, op, status, user_name
FROM deploy_actions
ORDER BY updated_at DESC, node, action_idx DESC
LIMIT 40;
" > "$db_log" 2>&1 || true

  provider_user_ssh "set -euo pipefail; git_bin=\$(command -v git || printf '%s' /run/current-system/sw/bin/git); worktree='${remote_worktree}'; printf 'worktree=%s\n' \"\$worktree\"; if [[ -d \"\$worktree\" ]]; then printf '\n== git status ==\n'; \"\$git_bin\" -C \"\$worktree\" status --short; printf '\n== git log ==\n'; \"\$git_bin\" -C \"\$worktree\" log --oneline -n 5; printf '\n== git remote -v ==\n'; \"\$git_bin\" -C \"\$worktree\" remote -v; printf '\n== generated listing ==\n'; ls -lah \"\$worktree/generated\"; fi" \
    > "$worktree_log" 2>&1 || true

  {
    printf 'webhook_http=%s\n' "$response_code"
    printf 'webhook_body_file=%s\n' "$response_body_file"
    printf 'status_log=%s\n' "$status_log"
    printf 'journal_log=%s\n' "$journal_log"
    printf 'db_log=%s\n' "$db_log"
    printf 'worktree_log=%s\n' "$worktree_log"
  } > "$summary_log"
}

capture_provider_job_diagnostics() {
  local job_id="$1"
  local node="$2"
  local label="$3"
  local summary_log="$LOG_DIR/provider-service-${label}.log"
  local db_log="$LOG_DIR/provider-service-${label}-db.log"
  local journal_log="$LOG_DIR/provider-service-${label}-journal.log"

  provider_db_query "
SELECT 'job' AS section, id, kind, status, COALESCE(error_summary, '')
FROM jobs
WHERE id = '${job_id}';

SELECT 'event' AS section, job_id, node, COALESCE(phase, ''), status, COALESCE(message, '')
FROM deploy_node_events
WHERE job_id = '${job_id}'
ORDER BY id;

SELECT 'action' AS section, job_id, node, op, status, user_name
FROM deploy_actions
WHERE job_id = '${job_id}'
ORDER BY node, action_idx;
" > "$db_log" 2>&1 || true

  provider_user_ssh "journalctl --user --no-pager -b -n 300 -u hostenv-provider.service -u postgresql.service" \
    > "$journal_log" 2>&1 || true

  {
    printf 'job_id=%s\n' "$job_id"
    printf 'node=%s\n' "$node"
    printf 'db_log=%s\n' "$db_log"
    printf 'journal_log=%s\n' "$journal_log"
  } > "$summary_log"
}

capture_app_site_diagnostics() {
  local label="$1"
  local node_alias="$2"
  local node_ip="$3"
  local summary_log="$LOG_DIR/${label}.log"
  local host_http_log="$LOG_DIR/${label}-host-http.log"
  local node_http_log="$LOG_DIR/${label}-node-http.log"
  local root_status_log="$LOG_DIR/${label}-root-status.log"
  local app_units_log="$LOG_DIR/${label}-app-units.log"
  local app_unit_files_log="$LOG_DIR/${label}-app-unit-files.log"
  local app_failed_log="$LOG_DIR/${label}-app-failed.log"
  local app_journal_log="$LOG_DIR/${label}-app-journal.log"
  local runtime_log="$LOG_DIR/${label}-runtime.log"
  local deploy_journal_log="$LOG_DIR/${label}-provider-deploy-journal.log"
  local user_state_cmd
  local host_code

  user_state_cmd="$(printf '%q' "set -euo pipefail; ls -lah \$HOME; printf '\\n'; ls -lah \$HOME/.config 2>/dev/null || true; printf '\\n'; ls -lah \$HOME/.config/systemd 2>/dev/null || true; printf '\\n'; ls -lah \$HOME/.config/systemd/user 2>/dev/null || true; printf '\\n'; ls -lah \$HOME/.local/state 2>/dev/null || true; printf '\\n'; ls -lah \$HOME/.local/state/hostenv/current-state 2>/dev/null || true; printf '\\n'; ls -lah \$HOME/.local/state/hostenv/current-state/systemd 2>/dev/null || true; printf '\\n'; ls -lah \$HOME/.local/state/hostenv/current-state/systemd/user 2>/dev/null || true")"

  host_code="$(http_status "$APP_VHOST" "http://${node_ip}:${NODE_HTTP_PORT}/")"
  curl --connect-timeout 3 --max-time 10 -sS -D - -o /dev/null -H "Host: ${APP_VHOST}" "http://${node_ip}:${NODE_HTTP_PORT}/" > "$host_http_log" 2>&1 || true

  ssh -F "$SSH_CONFIG" "root@${node_alias}.${HOSTENV_HOSTNAME}" \
    "set -euo pipefail; curl --connect-timeout 3 --max-time 10 -sS -D - -o /dev/null -H 'Host: ${APP_VHOST}' 'http://127.0.0.1:${NODE_HTTP_PORT}/'" \
    > "$node_http_log" 2>&1 || true

  ssh -F "$SSH_CONFIG" "root@${node_alias}.${HOSTENV_HOSTNAME}" \
    "systemctl --no-pager --full status nginx.service hostenv-deploy-agent.service sshd.service" \
    > "$root_status_log" 2>&1 || true

  ssh -F "$SSH_CONFIG" "root@${node_alias}.${HOSTENV_HOSTNAME}" \
    "journalctl --no-pager -b -n 200 -u hostenv-deploy-agent.service" \
    > "$deploy_journal_log" 2>&1 || true

  ssh -F "$SSH_CONFIG" "${APP_ENV_USER}@${node_alias}.${HOSTENV_HOSTNAME}" \
    "set -euo pipefail; export HOME=\$HOME; export XDG_RUNTIME_DIR=/run/user/\$(id -u); export XDG_CACHE_HOME=\$HOME/.cache; export XDG_CONFIG_HOME=\$HOME/.config; export XDG_DATA_HOME=\$HOME/.local/share; export XDG_STATE_HOME=\$HOME/.local/state; systemctl --user --no-pager --full list-units --type=service --all" \
    > "$app_units_log" 2>&1 || true

  ssh -F "$SSH_CONFIG" "${APP_ENV_USER}@${node_alias}.${HOSTENV_HOSTNAME}" \
    "set -euo pipefail; export HOME=\$HOME; export XDG_RUNTIME_DIR=/run/user/\$(id -u); export XDG_CACHE_HOME=\$HOME/.cache; export XDG_CONFIG_HOME=\$HOME/.config; export XDG_DATA_HOME=\$HOME/.local/share; export XDG_STATE_HOME=\$HOME/.local/state; systemctl --user --no-pager --full list-unit-files" \
    > "$app_unit_files_log" 2>&1 || true

  ssh -F "$SSH_CONFIG" "${APP_ENV_USER}@${node_alias}.${HOSTENV_HOSTNAME}" \
    "set -euo pipefail; export HOME=\$HOME; export XDG_RUNTIME_DIR=/run/user/\$(id -u); export XDG_CACHE_HOME=\$HOME/.cache; export XDG_CONFIG_HOME=\$HOME/.config; export XDG_DATA_HOME=\$HOME/.local/share; export XDG_STATE_HOME=\$HOME/.local/state; systemctl --user --no-pager --full --failed" \
    > "$app_failed_log" 2>&1 || true

  ssh -F "$SSH_CONFIG" "${APP_ENV_USER}@${node_alias}.${HOSTENV_HOSTNAME}" \
    "set -euo pipefail; export HOME=\$HOME; export XDG_RUNTIME_DIR=/run/user/\$(id -u); export XDG_CACHE_HOME=\$HOME/.cache; export XDG_CONFIG_HOME=\$HOME/.config; export XDG_DATA_HOME=\$HOME/.local/share; export XDG_STATE_HOME=\$HOME/.local/state; journalctl --user --no-pager -b -n 200" \
    > "$app_journal_log" 2>&1 || true

  ssh -F "$SSH_CONFIG" "root@${node_alias}.${HOSTENV_HOSTNAME}" \
    "set -euo pipefail; ls -lah '${APP_RUNTIME_DIR}'; printf '\n== runtime contents ==\n'; find '${APP_RUNTIME_DIR}' -maxdepth 2 \( -type s -o -type f \) | sort; printf '\n== user config systemd ==\n'; runuser -u '${APP_ENV_USER}' -- bash -lc ${user_state_cmd}" \
    > "$runtime_log" 2>&1 || true

  {
    printf 'node=%s\n' "$node_alias"
    printf 'node_ip=%s\n' "$node_ip"
    printf 'host_http_code=%s\n' "$host_code"
    printf 'host_http_log=%s\n' "$host_http_log"
    printf 'node_http_log=%s\n' "$node_http_log"
    printf 'root_status_log=%s\n' "$root_status_log"
    printf 'app_units_log=%s\n' "$app_units_log"
    printf 'app_unit_files_log=%s\n' "$app_unit_files_log"
    printf 'app_failed_log=%s\n' "$app_failed_log"
    printf 'app_journal_log=%s\n' "$app_journal_log"
    printf 'runtime_log=%s\n' "$runtime_log"
    printf 'provider_deploy_journal_log=%s\n' "$deploy_journal_log"
  } > "$summary_log"
}

sync_provider_service_user_secrets() {
  local tmp_dir="$WORKDIR/provider-service-user-secrets"
  local secrets_json="$tmp_dir/provider-secrets.json"
  local secrets_dir="/run/secrets/${PROVIDER_ENV_USER}"
  local secrets_log="$LOG_DIR/provider-service-user-secrets.log"

  mkdir -p "$tmp_dir"
  : > "$secrets_log"

  sops -d --output-type json "$PROVIDER_DIR/secrets/provider.yaml" > "$secrets_json"
  jq -c '.provider_node_tokens // empty' "$secrets_json" > "$tmp_dir/provider_node_tokens.yaml"
  jq -r '.cache_auth_password // empty' "$secrets_json" > "$tmp_dir/cache_auth_password"
  jq -r '.cache_signing_key // empty' "$secrets_json" > "$tmp_dir/cache_signing_key"

  [[ -s "$tmp_dir/provider_node_tokens.yaml" ]] || fail "Missing provider_node_tokens in provider secrets"
  [[ -s "$tmp_dir/cache_auth_password" ]] || fail "Missing cache_auth_password in provider secrets"
  [[ -s "$tmp_dir/cache_signing_key" ]] || fail "Missing cache_signing_key in provider secrets"

  ssh -F "$SSH_CONFIG" "root@node-a.${HOSTENV_HOSTNAME}" \
    "set -euo pipefail; mkdir -p '${secrets_dir}'; chown '${PROVIDER_ENV_USER}:${PROVIDER_ENV_USER}' '${secrets_dir}'; chmod 0700 '${secrets_dir}'" \
    >> "$secrets_log" 2>&1

  while IFS='|' read -r local_path remote_name; do
    printf 'syncing %s\n' "$remote_name" >> "$secrets_log"
    ssh -F "$SSH_CONFIG" "root@node-a.${HOSTENV_HOSTNAME}" \
      "set -euo pipefail; cat > '${secrets_dir}/${remote_name}'; chown '${PROVIDER_ENV_USER}:${PROVIDER_ENV_USER}' '${secrets_dir}/${remote_name}'; chmod 0400 '${secrets_dir}/${remote_name}'" \
      < "$local_path" >> "$secrets_log" 2>&1
  done <<EOF_PROVIDER_SECRET_FILES
$tmp_dir/provider_node_tokens.yaml|provider_node_tokens.yaml
$tmp_dir/cache_auth_password|cache_auth_password
$tmp_dir/cache_signing_key|cache_signing_key
EOF_PROVIDER_SECRET_FILES
}

seed_provider_service_repo() {
  local seed_log="$LOG_DIR/provider-service-repo-seed.log"
  local remote_data_dir="/home/${PROVIDER_ENV_USER}/.local/share/hostenv-provider"
  local remote_bare_repo="${remote_data_dir}/git/provider.git"
  local remote_worktree="${remote_data_dir}/work/provider"
  local local_seed_root="$WORKDIR/provider-service-repo-seed"
  local local_seed_src="${local_seed_root}/src"
  local local_seed_bare="${local_seed_root}/provider.git"
  local local_seed_worktree="${local_seed_root}/worktree"

  : > "$seed_log"

  rm -rf "$local_seed_root"
  mkdir -p "$local_seed_src"
  tar -C "$PROVIDER_DIR" --exclude='./.git' --exclude='./vm-*' --exclude='./result' --exclude='./result-*' -cf - . | tar -C "$local_seed_src" -xf -
  rm -f "$local_seed_src/generated/deploy-intent.json"
  rm -f "$local_seed_src/node-a.qcow2"
  jq --arg hash "$APP_PROJECT_HASH" '
    .environments |= with_entries(
      if (.value.hostenv.projectNameHash // "") == $hash
      then .value |= del(.node)
      else .
      end
    )
  ' "$local_seed_src/generated/plan.json" > "$local_seed_src/generated/plan.json.tmp"
  mv "$local_seed_src/generated/plan.json.tmp" "$local_seed_src/generated/plan.json"

  git -C "$local_seed_src" init -b main >> "$seed_log" 2>&1
  git -C "$local_seed_src" config user.email "demo@example.test" >> "$seed_log" 2>&1
  git -C "$local_seed_src" config user.name "Hostenv Demo" >> "$seed_log" 2>&1
  git -C "$local_seed_src" add . >> "$seed_log" 2>&1
  git -C "$local_seed_src" commit -m "Bootstrap provider repository" >> "$seed_log" 2>&1
  git clone --bare "$local_seed_src" "$local_seed_bare" >> "$seed_log" 2>&1
  git clone "$local_seed_bare" "$local_seed_worktree" >> "$seed_log" 2>&1
  git -C "$local_seed_worktree" remote set-url origin "$remote_bare_repo" >> "$seed_log" 2>&1

  ssh -F "$SSH_CONFIG" "root@node-a.${HOSTENV_HOSTNAME}" \
    "set -euo pipefail; rm -rf '${remote_bare_repo}' '${remote_worktree}'; mkdir -p '${remote_data_dir}/git' '${remote_data_dir}/work' '${remote_worktree}'; chown -R '${PROVIDER_ENV_USER}:${PROVIDER_ENV_USER}' '${remote_data_dir}'" \
    >> "$seed_log" 2>&1

  tar -C "$local_seed_bare" -cf - . | ssh -F "$SSH_CONFIG" "root@node-a.${HOSTENV_HOSTNAME}" "set -euo pipefail; mkdir -p '${remote_bare_repo}'; tar -C '${remote_bare_repo}' -xf -" >> "$seed_log" 2>&1
  tar -C "$local_seed_worktree" -cf - . | ssh -F "$SSH_CONFIG" "root@node-a.${HOSTENV_HOSTNAME}" "set -euo pipefail; mkdir -p '${remote_worktree}'; tar -C '${remote_worktree}' -xf -; chown -R '${PROVIDER_ENV_USER}:${PROVIDER_ENV_USER}' '${remote_data_dir}'" \
    >> "$seed_log" 2>&1
}

start_provider_user_services() {
  local service_log="$LOG_DIR/provider-service-user-services.log"
  local -a units=(
    postgresql.service
    hostenv-provider-cache-auth.service
    fcgiwrap.socket
    harmonia.service
    nginx.service
    hostenv-provider.service
  )

  : > "$service_log"
  printf 'provider_env=%s\n' "$PROVIDER_ENV_USER" >> "$service_log"

  # Clean up stale cache_htpasswd file that may be root-owned and cause permission errors
  ssh -F "$SSH_CONFIG" "root@node-a.${HOSTENV_HOSTNAME}" \
    "set -euo pipefail; rm -f '${PROVIDER_RUNTIME_DIR}/cache_htpasswd'" >> "$service_log" 2>&1 || true

  # Ensure dig command is available in provider-service runtime environment
  # This is required by hostenv-provider dns-gate functionality
  ssh -F "$SSH_CONFIG" "root@node-a.${HOSTENV_HOSTNAME}" \
    "set -euo pipefail; mkdir -p '${PROVIDER_RUNTIME_DIR}/bin'; if ! command -v dig >/dev/null 2>&1; then ln -sf /run/current-system/sw/bin/dig '${PROVIDER_RUNTIME_DIR}/bin/dig' || true; fi; chown -R '${PROVIDER_ENV_USER}:${PROVIDER_ENV_USER}' '${PROVIDER_RUNTIME_DIR}/bin' 2>/dev/null || true" >> "$service_log" 2>&1 || true

  # Batch all service operations into a single SSH call to reduce connection churn
  local batch_cmd
  batch_cmd="set -euo pipefail; export HOME=\$HOME; export XDG_RUNTIME_DIR=/run/user/\$(id -u); export XDG_CACHE_HOME=\$HOME/.cache; export XDG_CONFIG_HOME=\$HOME/.config; export XDG_DATA_HOME=\$HOME/.local/share; export XDG_STATE_HOME=\$HOME/.local/state; systemctl --user set-environment HOSTENV_PROVIDER_FORCE_INITIAL_INTENTS=1 GIT_CONFIG_NOSYSTEM=1 PATH=${PROVIDER_RUNTIME_DIR}/bin:\$PATH; systemctl --user reset-failed || true; systemctl --user daemon-reload"
  
  for unit in "${units[@]}"; do
    batch_cmd+="; printf 'starting %s\n' '$unit'; systemctl --user restart '$unit' || systemctl --user start '$unit'; systemctl --user --no-pager --full status '$unit' || true"
  done

  provider_user_ssh "$batch_cmd" >> "$service_log" 2>&1
}

seed_app_project_webhook() {
  local seed_log="$LOG_DIR/provider-service-webhook-seed.log"
  local sql

  sql="WITH upsert_project AS (INSERT INTO projects (org, project, git_host, repo_id, repo_url, repo_path, flake_input, default_env_hash) VALUES ('demo', 'app', 'gitlab.com', 1, 'file://${APP_PROJECT_DIR}', 'demo/app', 'demo__app', '${APP_PROJECT_HASH}') ON CONFLICT (org, project) DO UPDATE SET git_host = EXCLUDED.git_host, repo_id = EXCLUDED.repo_id, repo_url = EXCLUDED.repo_url, repo_path = EXCLUDED.repo_path, flake_input = EXCLUDED.flake_input, default_env_hash = EXCLUDED.default_env_hash, updated_at = now() RETURNING id) INSERT INTO webhooks (project_id, secret, webhook_id, webhook_url) SELECT id, '${APP_WEBHOOK_SECRET}', NULL, 'https://${PROVIDER_VHOST}/webhook/${APP_PROJECT_HASH}' FROM upsert_project ON CONFLICT (project_id) DO UPDATE SET secret = EXCLUDED.secret, webhook_id = EXCLUDED.webhook_id, webhook_url = EXCLUDED.webhook_url, updated_at = now();"

  : > "$seed_log"
  provider_user_ssh "pg_pid=\$(systemctl --user show -p MainPID --value postgresql.service); pg_exe=\$(readlink -f /proc/\$pg_pid/exe); db_bin=\"\${pg_exe%/*}/psql\"; export PGHOST='${PROVIDER_RUNTIME_DIR}'; \"\$db_bin\" -v ON_ERROR_STOP=1 -d hostenv-provider --command \"${sql}\"" >> "$seed_log" 2>&1
}

provider_db_query() {
  local sql="$1"
  printf '%s' "$sql" | provider_user_ssh "pg_pid=\$(systemctl --user show -p MainPID --value postgresql.service); pg_exe=\$(readlink -f /proc/\$pg_pid/exe); db_bin=\"\${pg_exe%/*}/psql\"; export PGHOST='${PROVIDER_RUNTIME_DIR}'; \"\$db_bin\" -X -A -t -q -v ON_ERROR_STOP=1 -d hostenv-provider"
}

write_job_actions_snapshot() {
  local job_id="$1"
  local node="$2"
  local body_file="$3"
  local sql

  sql="WITH action_rows AS (SELECT node, action_idx AS \"actionIndex\", op, user_name AS \"user\", status, message, started_at AS \"startedAt\", finished_at AS \"finishedAt\", updated_at AS \"updatedAt\" FROM deploy_actions WHERE job_id = '${job_id}' AND node = '${node}' ORDER BY action_idx) SELECT json_build_object('jobId', '${job_id}', 'node', '${node}', 'actions', COALESCE((SELECT json_agg(row_to_json(action_rows)) FROM action_rows), '[]'::json));"

  provider_db_query "$sql" > "$body_file"
}

write_job_statuses_snapshot() {
  local job_id="$1"
  local node="$2"
  local body_file="$3"
  local sql

  sql="WITH status_rows AS (SELECT DISTINCT ON (node) id AS \"eventId\", node, status, phase, message, payload, created_at AS \"createdAt\" FROM deploy_node_events WHERE job_id = '${job_id}' ORDER BY node, id DESC) SELECT json_build_object('jobId', '${job_id}', 'node', '${node}', 'statuses', COALESCE((SELECT json_agg(row_to_json(status_rows) ORDER BY node) FROM status_rows), '[]'::json));"

  provider_db_query "$sql" > "$body_file"
}

http_status() {
  local host_header="$1"
  local url="$2"
  shift 2
  curl --connect-timeout 3 --max-time 10 -sS -o /dev/null -w '%{http_code}' -H "Host: ${host_header}" "$@" "$url" 2>/dev/null || true
}

find_vm_runner() {
  local vm_out="$1"
  local runner
  runner="$(find -L "$vm_out" -type f -name 'run-*-vm' | head -n1 || true)"
  [[ -n "$runner" ]] || fail "Could not locate VM runner under $vm_out"
  printf '%s\n' "$runner"
}

init_git_repo() {
  local dir="$1"
  git -C "$dir" init -b main >/dev/null
  git -C "$dir" config user.email "demo@example.test"
  git -C "$dir" config user.name "Hostenv Demo"
  git -C "$dir" add .
  git -C "$dir" commit -m "Initial commit for local demo" >/dev/null
  git -C "$dir" config receive.denyCurrentBranch updateInstead
  git -C "$dir" remote add origin "file://$dir"
  git -C "$dir" config branch.main.remote origin
  git -C "$dir" config branch.main.merge refs/heads/main
}

ensure_local_git_ref() {
  local dir="$1"
  local ref_name="$2"

  if ! git -C "$dir" show-ref --verify --quiet "refs/heads/${ref_name}"; then
    git -C "$dir" branch "$ref_name" main >/dev/null
  fi
}

run_provider_plan() {
  (
    cd "$PROVIDER_DIR"
    run_with_timeout "$PLAN_COMMAND_TIMEOUT" "Provider plan generation" nix run .#hostenv-provider -- plan
    run_with_timeout "$PLAN_COMMAND_TIMEOUT" "Provider dns-gate update" nix run .#hostenv-provider -- dns-gate
  )
}

sanitize_generated_plan() {
  jq -e 'if .nodes then ([.nodes[] | (.provider.cacheServer? // null)] | all(. == null)) else true end' "$PLAN_PATH" >/dev/null \
    || fail "generated plan still contains stale provider.cacheServer entries"
}

commit_generated() {
  git -C "$PROVIDER_DIR" add generated >/dev/null 2>&1 || true
  if ! git -C "$PROVIDER_DIR" diff --cached --quiet; then
    git -C "$PROVIDER_DIR" commit -m "Update generated provider artifacts" >/dev/null
  fi
}

commit_provider_sources() {
  git -C "$PROVIDER_DIR" add flake.nix nodes >/dev/null 2>&1 || true
  if ! git -C "$PROVIDER_DIR" diff --cached --quiet; then
    git -C "$PROVIDER_DIR" commit -m "Update provider node definitions" >/dev/null
  fi
}

sign_deploy_profile() {
  local node="$1"
  local profile="$2"
  local path

  if [[ "$profile" == "system" ]]; then
    path="$(run_with_timeout_capture "$NIX_BUILD_TIMEOUT" "System profile build for ${node}" nix build --no-link --print-out-paths "$PROVIDER_DIR/generated/.#nixosConfigurations.${node}.config.system.build.toplevel")"
  else
    path="$(run_with_timeout_capture "$NIX_BUILD_TIMEOUT" "Environment profile build for ${profile}" nix build --no-link --print-out-paths "$PROVIDER_DIR/generated/.#packages.x86_64-linux.env-${profile}")"
  fi

  nix store sign --key-file "$NIX_SIGNING_KEY_FILE" --recursive "$path" >/dev/null
}

refresh_merged_provider_secrets() {
  local tmp_json
  local decrypted_json
  local token_map_yaml

  tmp_json="$(mktemp "$WORKDIR/provider-secrets-merged-XXXXXX.json")"
  decrypted_json="$(sops -d --output-type json "$PROVIDER_DIR/secrets/provider.yaml")"
  token_map_yaml="$(jq -r '
    (.provider_node_tokens // {})
    | if type == "object" then
        (to_entries | map("\(.key): \(.value | tostring | @json)") | join("\n"))
      elif type == "string" then
        .
      else
        ""
      end
  ' <<<"$decrypted_json")"

  jq --arg provider_node_tokens_yaml "$token_map_yaml" '.provider_node_tokens_yaml = $provider_node_tokens_yaml' <<<"$decrypted_json" > "$tmp_json"
  SOPS_AGE_RECIPIENTS="$AGE_PUBLIC_KEY" sops --encrypt --config /dev/null --input-type json --output-type yaml "$tmp_json" > "$PROVIDER_DIR/generated/secrets.merged.yaml"
  rm -f "$tmp_json"
}

load_provider_node_token() {
  local node="$1"
  sops -d --output-type json "$PROVIDER_DIR/secrets/provider.yaml" | jq -r --arg node "$node" '.provider_node_tokens[$node] // empty'
}

load_cache_auth_password() {
  CACHE_AUTH_PASSWORD="$(sops -d --output-type json "$PROVIDER_DIR/secrets/provider.yaml" | jq -r '.cache_auth_password // empty')"
  [[ -n "$CACHE_AUTH_PASSWORD" ]] || fail "Missing cache_auth_password in provider secrets"
}

load_plan_metadata() {
  local provider_project="$PROVIDER_SERVICE_PROJECT_SELECTOR"
  APP_ENV_NAME="$(jq -r '.environments | to_entries[] | select(.value.hostenv.project == "app") | .key' "$PLAN_PATH" | head -n1)"
  PROVIDER_ENV_NAME="$(jq -r --arg project "$provider_project" '.environments | to_entries[] | select(.value.hostenv.project == $project) | .key' "$PLAN_PATH" | head -n1)"

  [[ -n "$APP_ENV_NAME" ]] || fail "Could not locate demo app environment in $PLAN_PATH"
  [[ -n "$PROVIDER_ENV_NAME" ]] || fail "Could not locate provider-service environment in $PLAN_PATH"

  APP_ENV_USER="$APP_ENV_NAME"
  PROVIDER_ENV_USER="$PROVIDER_ENV_NAME"
  APP_ENV_UID="$(jq -r --arg env "$APP_ENV_NAME" '.environments[$env].uid // empty' "$PLAN_PATH")"
  PROVIDER_ENV_UID="$(jq -r --arg env "$PROVIDER_ENV_NAME" '.environments[$env].uid // empty' "$PLAN_PATH")"
  APP_RUNTIME_DIR="$(jq -r --arg env "$APP_ENV_NAME" '.environments[$env].hostenv.runtimeDir // empty' "$PLAN_PATH")"
  PROVIDER_RUNTIME_DIR="$(jq -r --arg env "$PROVIDER_ENV_NAME" '.environments[$env].hostenv.runtimeDir // empty' "$PLAN_PATH")"
  APP_VHOST="$(jq -r --arg env "$APP_ENV_NAME" '.environments[$env].virtualHosts | keys[0] // empty' "$PLAN_PATH")"
  PROVIDER_VHOST="$(jq -r --arg env "$PROVIDER_ENV_NAME" '.environments[$env].virtualHosts | keys[0] // empty' "$PLAN_PATH")"
  APP_PROJECT_HASH="$(jq -r --arg env "$APP_ENV_NAME" '.environments[$env].hostenv.projectNameHash // empty' "$PLAN_PATH")"

  [[ -n "$APP_ENV_UID" && -n "$PROVIDER_ENV_UID" ]] || fail "Plan is missing environment uid values"
  [[ -n "$APP_RUNTIME_DIR" && -n "$PROVIDER_RUNTIME_DIR" ]] || fail "Plan runtimeDir metadata is incomplete"
  [[ -n "$APP_VHOST" && -n "$PROVIDER_VHOST" ]] || fail "Plan virtual host metadata is incomplete"
  [[ -n "$APP_PROJECT_HASH" ]] || fail "Could not read app project hash from $PLAN_PATH"
}

provider_request() {
  local method="$1"
  local path="$2"
  local body_file="$3"
  local auth_header="${4:-}"
  local data_file="${5:-}"
  local url="http://${NODE_A_HOST_IP}:${NODE_HTTP_PORT}${path}"
  local -a args=(curl -sS -X "$method" -H "Host: ${PROVIDER_VHOST}" -o "$body_file" -w '%{http_code}')

  if [[ -n "$auth_header" ]]; then
    args+=(-H "$auth_header")
  fi
  if [[ -n "$data_file" ]]; then
    args+=(-H 'Content-Type: application/octet-stream' --data-binary "@${data_file}")
  fi

  args+=("$url")
  "${args[@]}" || true
}

archive_flake_inputs_to_node() {
  local node="$1"
  local flake_path="$2"
  local label="$3"
  local log_file="$LOG_DIR/${label}-${node}-flake-archive.log"

  [[ -f "$flake_path/flake.nix" ]] || fail "Cannot archive flake inputs from non-flake path: $flake_path"

  if ! NIX_SSHOPTS="-F $SSH_CONFIG" timeout --foreground "$FLAKE_ARCHIVE_TIMEOUT" nix flake archive --to "ssh://root@${node}.${HOSTENV_HOSTNAME}" "$flake_path" > "$log_file" 2>&1; then
    local rc=$?
    if [[ "$rc" -eq 124 ]]; then
      fail "Archiving flake inputs for ${node} from ${flake_path} timed out after ${FLAKE_ARCHIVE_TIMEOUT}s. See $log_file"
    fi
    fail "Failed to archive flake inputs for ${node} from ${flake_path}. See $log_file"
  fi
}

preseed_deploy_flake_inputs() {
  local node="$1"

  archive_flake_inputs_to_node "$node" "$PROVIDER_DIR" "provider-root"
  archive_flake_inputs_to_node "$node" "$PROVIDER_DIR/generated" "provider-generated"
}

wait_for_job_action_op() {
  local job_id="$1"
  local node="$2"
  local token="$3"
  local op="$4"
  local timeout="$5"
  local start
  local body

  body="$WORKDIR/actions-${job_id}-${node}.json"
  start="$(date +%s)"

  while true; do
    write_job_actions_snapshot "$job_id" "$node" "$body"
    if [[ -s "$body" ]] \
      && jq -e --arg op "$op" '.actions[]? | select(.op == $op)' "$body" >/dev/null 2>&1; then
      return 0
    fi
    if (( "$(date +%s)" - start >= timeout )); then
      capture_provider_job_diagnostics "$job_id" "$node" "job-action-timeout-${job_id}-${node}"
      return 1
    fi
    sleep 2
  done
}

wait_for_provider_job_completion() {
  local job_id="$1"
  local node="$2"
  local token="$3"
  local timeout="$4"
  local body
  local start

  body="$WORKDIR/status-${job_id}-${node}.json"
  start="$(date +%s)"

  while true; do
    write_job_statuses_snapshot "$job_id" "$node" "$body"
    if [[ -s "$body" ]]; then
      if jq -e '.statuses[]? | select(.status == "failed" or .status == "timed_out")' "$body" >/dev/null 2>&1; then
        return 1
      fi
      if jq -e --arg node "$node" '.statuses[]? | select(.node == $node and .phase == "intent" and .status == "success")' "$body" >/dev/null 2>&1; then
        return 0
      fi
    fi
    if (( "$(date +%s)" - start >= timeout )); then
      capture_provider_job_diagnostics "$job_id" "$node" "job-status-timeout-${job_id}-${node}"
      return 1
    fi
    sleep 2
  done
}

trigger_app_webhook_deploy() {
  local sha="$1"
  local payload_file="$WORKDIR/webhook-payload-${sha}.json"
  local body="$WORKDIR/webhook-response-${sha}.json"
  local code

  printf '{"ref":"refs/heads/main","checkout_sha":"%s","project":{"path_with_namespace":"demo/app"}}' "$sha" > "$payload_file"
  code="$(provider_request POST "/webhook/${APP_PROJECT_HASH}" "$body" "X-Gitlab-Token: ${APP_WEBHOOK_SECRET}" "$payload_file")"
  if [[ "$code" != "202" ]]; then
    capture_provider_webhook_diagnostics "$code" "$body"
    fail "Webhook trigger failed (HTTP $code). See $LOG_DIR/provider-service-webhook-diagnostics.log"
  fi
  jq -r '.jobId // empty' "$body"
}

wait_for_site_ready() {
  local node_ip="$1"
  local timeout="$2"
  local start
  local code

  start="$(date +%s)"
  while true; do
    code="$(http_status "$APP_VHOST" "http://${node_ip}:${NODE_HTTP_PORT}/")"
    if [[ "$code" =~ ^[23][0-9][0-9]$ ]]; then
      return 0
    fi
    if (( "$(date +%s)" - start >= timeout )); then
      return 1
    fi
    sleep 2
  done
}

marker_present_on_node() {
  local node_alias="$1"
  ssh -F "$SSH_CONFIG" "${APP_ENV_USER}@${node_alias}" \
    "set -euo pipefail; db_bin=\$(command -v mariadb || command -v mysql || echo /run/current-system/sw/bin/mysql); printf '%s\n' 'SELECT note FROM hostenv_demo_marker WHERE id = 1;' | \"\$db_bin\" --batch --skip-column-names --socket='${APP_RUNTIME_DIR}/mysql.sock' --user='${APP_ENV_USER}' --database=drupal" \
    2>/dev/null | grep -qx 'from-node-a'
}

capture_migration_source_plan() {
  cp "$PLAN_PATH" "$PROVIDER_DIR/generated/migration-source-plan.json"
}

apply_migration_source_plan() {
  cp "$PROVIDER_DIR/generated/migration-source-plan.json" "$PLAN_PATH"
}

write_provider_flake() {
  local production_node="$1"
  local esc_hostenv_source
  local esc_app_project_url
  local esc_provider_project_url
  local esc_hostname
  local esc_signing_key
  local esc_node
  local esc_provider_port
  local esc_provider_gateway

  esc_hostenv_source="$(escape_sed_replacement "path:${HOSTENV_SOURCE_DIR}")"
  esc_app_project_url="$(escape_sed_replacement "git+file://${APP_PROJECT_DIR}?dir=.hostenv&ref=main")"
  esc_provider_project_url="$(escape_sed_replacement "git+file://${PROVIDER_SERVICE_PROJECT_DIR}?dir=.hostenv&ref=main")"
  esc_hostname="$(escape_sed_replacement "$HOSTENV_HOSTNAME")"
  esc_signing_key="$(escape_sed_replacement "$NIX_SIGNING_PUBLIC_KEY")"
  esc_node="$(escape_sed_replacement "$production_node")"
  esc_provider_port="$(escape_sed_replacement "$PROVIDER_HTTP_PORT")"
  esc_provider_gateway="$(escape_sed_replacement "$PROVIDER_API_VM_GATEWAY")"

  cp "$PROVIDER_FLAKE_TEMPLATE" "$PROVIDER_DIR/flake.nix"
  sed -i "s|__HOSTENV_SOURCE_DIR__|${esc_hostenv_source}|g" "$PROVIDER_DIR/flake.nix"
  sed -i "s|__APP_PROJECT_URL__|${esc_app_project_url}|g" "$PROVIDER_DIR/flake.nix"
  sed -i "s|__PROVIDER_SERVICE_PROJECT_URL__|${esc_provider_project_url}|g" "$PROVIDER_DIR/flake.nix"
  sed -i "s|__HOSTENV_HOSTNAME__|${esc_hostname}|g" "$PROVIDER_DIR/flake.nix"
  sed -i "s|__NIX_SIGNING_PUBLIC_KEY__|${esc_signing_key}|g" "$PROVIDER_DIR/flake.nix"
  sed -i "s|__PRODUCTION_NODE__|${esc_node}|g" "$PROVIDER_DIR/flake.nix"
  sed -i "s|__PROVIDER_HTTP_PORT__|${esc_provider_port}|g" "$PROVIDER_DIR/flake.nix"
  sed -i "s|__PROVIDER_API_VM_GATEWAY__|${esc_provider_gateway}|g" "$PROVIDER_DIR/flake.nix"
  sed -i 's|"demo__provider-service"|demo__providerservice|' "$PROVIDER_DIR/flake.nix"
  sed -i "s|project = \"provider-service\";|project = \"${PROVIDER_SERVICE_PROJECT_SELECTOR}\";|" "$PROVIDER_DIR/flake.nix"
}

write_node_config() {
  local node="$1"
  local host_ip="$2"
  local esc_node
  local esc_host_ip
  local esc_ssh_port
  local esc_http_port
  local esc_signing_key
  local esc_shared_dir
  local esc_provider_dir
  local esc_workdir
  local esc_repo_root
  local esc_age_private
  local esc_ssh_public
  local esc_provider_host
  local esc_provider_gateway

  esc_node="$(escape_sed_replacement "$node")"
  esc_host_ip="$(escape_sed_replacement "$host_ip")"
  esc_ssh_port="$(escape_sed_replacement "$NODE_SSH_PORT")"
  esc_http_port="$(escape_sed_replacement "$NODE_HTTP_PORT")"
  esc_signing_key="$(escape_sed_replacement "$NIX_SIGNING_PUBLIC_KEY")"
  esc_shared_dir="$(escape_sed_replacement "$SHARED_DIR")"
  esc_provider_dir="$(escape_sed_replacement "$PROVIDER_DIR")"
  esc_workdir="$(escape_sed_replacement "$WORKDIR")"
  esc_repo_root="$(escape_sed_replacement "$REPO_ROOT")"
  esc_age_private="$(escape_sed_replacement "$AGE_PRIVATE_KEY")"
  esc_ssh_public="$(escape_sed_replacement "$SSH_PUBLIC_KEY")"
  esc_provider_host="$(escape_sed_replacement "$PROVIDER_SERVICE_HOST")"
  esc_provider_gateway="$(escape_sed_replacement "$PROVIDER_API_VM_GATEWAY")"

  mkdir -p "$PROVIDER_DIR/nodes/$node"
  cp "$NODE_CONFIG_TEMPLATE" "$PROVIDER_DIR/nodes/$node/configuration.nix"
  sed -i "s|__NODE_NAME__|${esc_node}|g" "$PROVIDER_DIR/nodes/$node/configuration.nix"
  sed -i "s|__NODE_HOST_IP__|${esc_host_ip}|g" "$PROVIDER_DIR/nodes/$node/configuration.nix"
  sed -i "s|__NODE_SSH_PORT__|${esc_ssh_port}|g" "$PROVIDER_DIR/nodes/$node/configuration.nix"
  sed -i "s|__NODE_HTTP_PORT__|${esc_http_port}|g" "$PROVIDER_DIR/nodes/$node/configuration.nix"
  sed -i "s|__NIX_SIGNING_PUBLIC_KEY__|${esc_signing_key}|g" "$PROVIDER_DIR/nodes/$node/configuration.nix"
  sed -i "s|__SHARED_DIR__|${esc_shared_dir}|g" "$PROVIDER_DIR/nodes/$node/configuration.nix"
  sed -i "s|__PROVIDER_DIR__|${esc_provider_dir}|g" "$PROVIDER_DIR/nodes/$node/configuration.nix"
  sed -i "s|__WORKDIR__|${esc_workdir}|g" "$PROVIDER_DIR/nodes/$node/configuration.nix"
  sed -i "s|__REPO_ROOT__|${esc_repo_root}|g" "$PROVIDER_DIR/nodes/$node/configuration.nix"
  sed -i "s|__AGE_PRIVATE_KEY__|${esc_age_private}|g" "$PROVIDER_DIR/nodes/$node/configuration.nix"
  sed -i "s|__SSH_PUBLIC_KEY__|${esc_ssh_public}|g" "$PROVIDER_DIR/nodes/$node/configuration.nix"
  sed -i "s|__PROVIDER_SERVICE_HOST__|${esc_provider_host}|g" "$PROVIDER_DIR/nodes/$node/configuration.nix"
}

start_vm() {
  local node="$1"
  local vm_out="$PROVIDER_DIR/vm-$node"
  local vm_runner

  (
    cd "$PROVIDER_DIR"
    run_with_timeout "$NIX_BUILD_TIMEOUT" "VM build for ${node}" nix build "./generated#nixosConfigurations.${node}.config.system.build.vm" -o "$vm_out"
  )
  vm_runner="$(find_vm_runner "$vm_out")"

  (cd "$PROVIDER_DIR" && "$vm_runner" > "$LOG_DIR/${node}.log" 2>&1) &
  VM_PIDS+=("$!")
  printf '%s\n' "$!" > "$PIDS_DIR/${node}.pid"

  wait_for_ssh "${node}.${HOSTENV_HOSTNAME}" 420 || fail "Timed out waiting for SSH on ${node}. See $LOG_DIR/${node}.log"
}

start_provider_http_bridge() {
  local provider_service_socket="${PROVIDER_RUNTIME_DIR}/hostenv-provider.sock"
  
  # Start socat to expose the Unix socket on TCP for VM access
  socat TCP-LISTEN:"$PROVIDER_HTTP_PORT",bind=0.0.0.0,fork,reuseaddr TCP:127.0.0.2:${NODE_HTTP_PORT} > "$LOG_DIR/provider-socat.log" 2>&1 &
  PROVIDER_BRIDGE_PID="$!"
  printf '%s\n' "$PROVIDER_BRIDGE_PID" > "$PIDS_DIR/provider-socat.pid"
  
  # Wait for the TCP port to be listening
  local start
  start="$(date +%s)"
  while true; do
    if port_in_use "0.0.0.0" "$PROVIDER_HTTP_PORT"; then
      return 0
    fi
    if (( "$(date +%s)" - start >= 30 )); then
      return 1
    fi
    sleep 1
  done
}

bootstrap_provider_service_environment() {
  local profile_path
  local provider_socket
  local bootstrap_log="$EVIDENCE_DIR/provider-service-project-phase-c-bootstrap.log"
  local bootstrap_status
  profile_path="$(run_with_timeout_capture "$NIX_BUILD_TIMEOUT" "Provider-service environment build" nix build --no-link --print-out-paths "$PROVIDER_DIR/generated/.#packages.x86_64-linux.env-${PROVIDER_ENV_USER}")"
  provider_socket="${PROVIDER_RUNTIME_DIR}/hostenv-provider.sock"

  {
    printf 'provider_env=%s\n' "$PROVIDER_ENV_USER"
    printf 'provider_uid=%s\n' "$PROVIDER_ENV_UID"
    printf 'runtime_dir=%s\n' "$PROVIDER_RUNTIME_DIR"
    printf 'socket=%s\n' "$provider_socket"
    printf 'activate_profile=%s\n' "$profile_path"
    printf 'bridge_port=%s\n' "$PROVIDER_HTTP_PORT"
    printf 'user_secrets_log=%s\n' "$LOG_DIR/provider-service-user-secrets.log"
    printf 'user_services_log=%s\n' "$LOG_DIR/provider-service-user-services.log"
  } > "$bootstrap_log"

  ssh -F "$SSH_CONFIG" "root@node-a.${HOSTENV_HOSTNAME}" "systemctl start user@${PROVIDER_ENV_UID}.service"
  sanitize_remote_gitconfig
  sync_provider_service_user_secrets
  if ! provider_user_ssh "'${profile_path}/bin/activate'" > "$LOG_DIR/provider-service-bootstrap.log" 2>&1; then
    capture_provider_bootstrap_diagnostics "$provider_socket" "$bootstrap_log"
    fail "Provider-service activate failed. See $LOG_DIR/provider-service-bootstrap.log and $LOG_DIR/provider-service-user-status.log"
  fi

  if ! seed_provider_service_repo; then
    capture_provider_bootstrap_diagnostics "$provider_socket" "$bootstrap_log"
    fail "Provider-service repository seed failed. See $LOG_DIR/provider-service-repo-seed.log"
  fi

  if ! start_provider_user_services; then
    capture_provider_bootstrap_diagnostics "$provider_socket" "$bootstrap_log"
    fail "Provider-service bootstrap could not start user services. See $LOG_DIR/provider-service-user-services.log and $LOG_DIR/provider-service-user-status.log"
  fi

  if ! wait_for_provider_user_services 240; then
    capture_provider_bootstrap_diagnostics "$provider_socket" "$bootstrap_log"
    fail "Provider-service user services did not reach active state. See $LOG_DIR/provider-service-user-services.log and $LOG_DIR/provider-service-user-status.log"
  fi

  if ! wait_for_remote_socket "node-a.${HOSTENV_HOSTNAME}" "$provider_socket" 240; then
    capture_provider_bootstrap_diagnostics "$provider_socket" "$bootstrap_log"
    bootstrap_status="$(provider_user_ssh "systemctl --user is-active postgresql.service hostenv-provider.service nginx.service harmonia.service fcgiwrap.socket || true" 2>/dev/null || true)"
    {
      printf 'service_activity_snapshot:\n%s\n' "$bootstrap_status"
    } >> "$bootstrap_log"
    fail "Provider-service socket did not appear at $provider_socket. See $LOG_DIR/provider-service-bootstrap.log, $LOG_DIR/provider-service-user-status.log, and $LOG_DIR/provider-service-user-journal.log"
  fi

  if ! start_provider_http_bridge; then
    capture_provider_bootstrap_diagnostics "$provider_socket" "$bootstrap_log"
    fail "Provider HTTP bridge failed to start. See $LOG_DIR/provider-socat.log"
  fi

  if ! seed_app_project_webhook; then
    capture_provider_bootstrap_diagnostics "$provider_socket" "$bootstrap_log"
    fail "Provider-service webhook seed failed. See $LOG_DIR/provider-service-webhook-seed.log"
  fi
}

run_seed_import() {
  run_with_timeout "$SEED_IMPORT_TIMEOUT" "Seed import" bash -lc '
    set -euo pipefail
    cd "$1"
    pv ./seed.sql.gz | gunzip -c | ssh -F "$2" "$3" "set -euo pipefail; db_bin=\$(command -v mariadb || command -v mysql || echo /run/current-system/sw/bin/mysql); \"\$db_bin\" --socket=\"$4\" --user=\"$5\" --database=drupal"
  ' _ "$APP_PROJECT_DIR/.hostenv" "$SSH_CONFIG" "${APP_ENV_USER}@node-a.${HOSTENV_HOSTNAME}" "${APP_RUNTIME_DIR}/mysql.sock" "$APP_ENV_USER"
}

record_phase_a_evidence() {
  local file="$EVIDENCE_DIR/provider-service-project-phase-a-workspace.log"
  {
    printf 'workdir=%s\n' "$WORKDIR"
    printf 'provider_dir=%s\n' "$PROVIDER_DIR"
    printf 'app_project_dir=%s\n' "$APP_PROJECT_DIR"
    printf 'provider_service_project_dir=%s\n' "$PROVIDER_SERVICE_PROJECT_DIR"
    printf 'seed_source=%s\n' "$SEED_SOURCE"
    printf 'provider_template=%s\n' "$PROVIDER_FLAKE_TEMPLATE"
    printf 'node_template=%s\n' "$NODE_CONFIG_TEMPLATE"
  } > "$file"
}

record_phase_b_evidence() {
  local file="$EVIDENCE_DIR/provider-service-project-phase-b-plan.log"
  local provider_project="$PROVIDER_SERVICE_PROJECT_SELECTOR"
  {
    printf 'plan_path=%s\n' "$PLAN_PATH"
    printf 'app_env=%s\n' "$APP_ENV_NAME"
    printf 'app_node=%s\n' "$(jq -r --arg env "$APP_ENV_NAME" '.environments[$env].node // empty' "$PLAN_PATH")"
    printf 'app_project_hash=%s\n' "$APP_PROJECT_HASH"
    printf 'app_runtime_dir=%s\n' "$APP_RUNTIME_DIR"
    printf 'provider_env=%s\n' "$PROVIDER_ENV_NAME"
    printf 'provider_node=%s\n' "$(jq -r --arg env "$PROVIDER_ENV_NAME" '.environments[$env].node // empty' "$PLAN_PATH")"
    printf 'provider_runtime_dir=%s\n' "$PROVIDER_RUNTIME_DIR"
    printf 'provider_vhost=%s\n' "$PROVIDER_VHOST"
    printf 'provider_service_selected=%s\n' "$(jq -r --arg env "$PROVIDER_ENV_NAME" --arg project "$provider_project" '.environments[$env].hostenv.project == $project' "$PLAN_PATH")"
  } > "$file"
}

record_phase_d_evidence() {
  local file="$EVIDENCE_DIR/provider-service-project-phase-d-cache-readiness.log"
  local unauth_code
  local auth_code
  local body_file="$WORKDIR/provider-cache-info.txt"
  local body_tmp
  local cache_url="http://${NODE_A_HOST_IP}:${NODE_HTTP_PORT}/cache/nix-cache-info"
  local ready=0
  local start
  start="$(date +%s)"
  body_tmp="$(mktemp "$WORKDIR/provider-cache-info-XXXXXX")"

  while true; do
    unauth_code="$(http_status "$PROVIDER_VHOST" "$cache_url")"
    auth_code="$(http_status "$PROVIDER_VHOST" "$cache_url" -u "cache:${CACHE_AUTH_PASSWORD}")"

    if [[ "$unauth_code" =~ ^(401|403)$ ]] \
      && [[ "$auth_code" == "200" ]]; then
      curl --connect-timeout 3 --max-time 10 -sS -H "Host: ${PROVIDER_VHOST}" -u "cache:${CACHE_AUTH_PASSWORD}" "$cache_url" > "$body_tmp" || true
      if grep -q 'StoreDir: /nix/store' "$body_tmp" && grep -q 'WantMassQuery: 1' "$body_tmp"; then
        mv "$body_tmp" "$body_file"
        ready=1
        break
      fi
    fi

    if (( "$(date +%s)" - start >= 240 )); then
      break
    fi
    sleep 2
  done

  if [[ "$ready" -ne 1 ]]; then
    curl --connect-timeout 3 --max-time 10 -sS -H "Host: ${PROVIDER_VHOST}" -u "cache:${CACHE_AUTH_PASSWORD}" "$cache_url" > "$body_file" || true
  fi

  {
    printf 'provider_vhost=%s\n' "$PROVIDER_VHOST"
    printf 'cache_unauth_http=%s\n' "$unauth_code"
    printf 'cache_auth_http=%s\n' "$auth_code"
    printf 'harmonia_socket=%s\n' "${PROVIDER_RUNTIME_DIR}/harmonia.sock"
    printf 'fcgiwrap_socket=%s\n' "${PROVIDER_RUNTIME_DIR}/fcgiwrap.sock"
    printf 'nix_cache_info:\n'
    cat "$body_file"
    printf '\n'
  } > "$file"

  if [[ "$ready" -ne 1 ]]; then
    capture_provider_cache_diagnostics "$unauth_code" "$auth_code" "$body_file"
    fail "Provider cache did not become ready before timeout. See $LOG_DIR/provider-service-cache-diagnostics.log"
  fi

  [[ "$unauth_code" =~ ^(401|403)$ ]] || fail "Expected unauthenticated cache probe to be rejected"
  [[ "$auth_code" == "200" ]] || fail "Authenticated cache probe failed"
}

record_node_job_evidence() {
  local label="$1"
  local job_id="$2"
  local node="$3"
  local token="$4"
  local file="$EVIDENCE_DIR/provider-service-project-${label}-${node}.log"
  local actions_json="$WORKDIR/${label}-${node}-actions.json"
  local statuses_json="$WORKDIR/${label}-${node}-statuses.json"

  write_job_actions_snapshot "$job_id" "$node" "$actions_json"
  write_job_statuses_snapshot "$job_id" "$node" "$statuses_json"

  {
    printf 'job_id=%s\n' "$job_id"
    printf 'node=%s\n' "$node"
    printf 'final_state=%s\n' "$(jq -r --arg node "$node" '.statuses[]? | select(.node == $node and .phase == "intent") | .status' "$statuses_json" | tail -n1)"
    printf 'latest_status=%s\n' "$(jq -c '.statuses[-1] // null' "$statuses_json")"
    printf 'actions=%s\n' "$(jq -c '[.actions[]? | {node, op, user, status}]' "$actions_json")"
    printf 'statuses=%s\n' "$(jq -c '.statuses' "$statuses_json")"
  } > "$file"
}

record_migration_sequence_evidence() {
  local file="$EVIDENCE_DIR/provider-service-project-phase-e-migration-sequence.log"
  local raw_file="$WORKDIR/migration-events.tsv"

  ssh -F "$SSH_CONFIG" "${PROVIDER_ENV_USER}@node-a.${HOSTENV_HOSTNAME}" "set -euo pipefail; export PGHOST='${PROVIDER_RUNTIME_DIR}'; psql -d hostenv-provider -Atqc \"SELECT node || E'\\t' || COALESCE(phase, '') || E'\\t' || status || E'\\t' || COALESCE(message, '') FROM deploy_node_events WHERE job_id = '${NODE_B_JOB_ID}' ORDER BY id\"" > "$raw_file"

  local backup_line restore_line deactivate_line
  backup_line="$(grep -n $'\tbackup\tsuccess\t' "$raw_file" | head -n1 | cut -d: -f1 || true)"
  restore_line="$(grep -n $'\trestore\tsuccess\t' "$raw_file" | head -n1 | cut -d: -f1 || true)"
  deactivate_line="$(grep -n $'\tdeactivate\tsuccess\t' "$raw_file" | head -n1 | cut -d: -f1 || true)"

  {
    printf 'job_id=%s\n' "$NODE_B_JOB_ID"
    printf 'backup_line=%s\n' "$backup_line"
    printf 'restore_line=%s\n' "$restore_line"
    printf 'deactivate_line=%s\n' "$deactivate_line"
    printf 'events:\n'
    cat "$raw_file"
    printf '\n'
  } > "$file"

  [[ -n "$backup_line" && -n "$restore_line" ]] || fail "Migration events missing backup/restore evidence"
  (( backup_line < restore_line )) || fail "Migration event order invalid: restore occurred before backup"
  if [[ -n "$deactivate_line" ]]; then
    (( restore_line < deactivate_line )) || fail "Migration event order invalid: deactivate occurred before restore"
  fi
}

record_cache_client_evidence() {
  local node="$1"
  local file="$EVIDENCE_DIR/provider-service-project-phase-f-${node}-cache-client.log"
  ssh -F "$SSH_CONFIG" "root@${node}.${HOSTENV_HOSTNAME}" "set -euo pipefail; nix show-config | awk '/^substituters = / || /^trusted-public-keys = / || /^netrc-file = / { print } /^require-sigs = / { sub(/^require-sigs/, \"require-signed-binaries\"); print }'; test -f /run/hostenv/provider-cache.netrc" > "$file"
}

prepare_workspace() {
  WORKDIR="$(mktemp -d /var/tmp/hostenv-local-provider-service-demo-XXXXXX)"
  PROVIDER_DIR="$WORKDIR/provider"
  APP_PROJECT_DIR="$WORKDIR/demo-app-project"
  PROVIDER_SERVICE_PROJECT_DIR="$WORKDIR/provider-service-project"
  HOSTENV_SOURCE_DIR="$WORKDIR/hostenv-source"
  SSH_DIR="$WORKDIR/ssh"
  SSH_KEY="$SSH_DIR/id_ed25519"
  SSH_CONFIG="$SSH_DIR/config"
  SHARED_DIR="$WORKDIR/shared"
  LOG_DIR="$WORKDIR/logs"
  PIDS_DIR="$WORKDIR/pids"
  PLAN_PATH="$PROVIDER_DIR/generated/plan.json"

  mkdir -p "$PROVIDER_DIR" "$APP_PROJECT_DIR" "$PROVIDER_SERVICE_PROJECT_DIR" "$HOSTENV_SOURCE_DIR" "$SSH_DIR" "$SHARED_DIR/backups" "$LOG_DIR" "$PIDS_DIR" "$EVIDENCE_DIR"
  mkdir -p "$PROVIDER_DIR/secrets" "$PROVIDER_DIR/generated"
  chmod 700 "$SSH_DIR"
  chmod 0777 "$SHARED_DIR" "$SHARED_DIR/backups"

  # Create disk-backed temp directory for nix/mktemp operations
  local temp_dir="$WORKDIR/tmp"
  mkdir -p "$temp_dir"
  export TMPDIR="$temp_dir"
  export TMP="$temp_dir"
  export TEMP="$temp_dir"

  cp -a "$APP_FIXTURE_DIR/." "$APP_PROJECT_DIR/"
  cp -a "$PROVIDER_SERVICE_FIXTURE_DIR/." "$PROVIDER_SERVICE_PROJECT_DIR/"
  cp -a "$PROVIDER_TEMPLATE_DIR/." "$PROVIDER_DIR/"
  cp -a "$REPO_ROOT/." "$HOSTENV_SOURCE_DIR/"
  rm -rf "$HOSTENV_SOURCE_DIR/.git" "$HOSTENV_SOURCE_DIR/.direnv" "$HOSTENV_SOURCE_DIR/result" "$HOSTENV_SOURCE_DIR"/result-*
  rm -f "$HOSTENV_SOURCE_DIR/hostenv-demo"
  sed -i 's/settings\.require-signed-binaries/settings.require-sigs/' "$HOSTENV_SOURCE_DIR/modules/nixos/provider-common.nix"
            sed -i '/if (env HOSTENV_WS_AUTH_MESSAGE=/i\            # shellcheck disable=SC2016' "$HOSTENV_SOURCE_DIR/modules/nixos/hostenv-deploy-agent.nix"
            sed -i '/local description="\$2"/i\            # shellcheck disable=SC2034' "$HOSTENV_SOURCE_DIR/modules/nixos/hostenv-deploy-agent.nix"
  cp "$SEED_SOURCE" "$APP_PROJECT_DIR/.hostenv/seed.sql.gz"

  ssh-keygen -q -t ed25519 -N "" -f "$SSH_KEY"
  SSH_PUBLIC_KEY="$(<"$SSH_KEY.pub")"

  age-keygen -o "$PROVIDER_DIR/secrets/demo-age.key" >/dev/null
  AGE_PRIVATE_KEY="$(grep '^AGE-SECRET-KEY-' "$PROVIDER_DIR/secrets/demo-age.key")"
  AGE_PUBLIC_KEY="$(grep '^# public key:' "$PROVIDER_DIR/secrets/demo-age.key" | awk '{print $4}')"
  export SOPS_AGE_KEY="$AGE_PRIVATE_KEY"
  export SOPS_AGE_KEY_FILE="$PROVIDER_DIR/secrets/demo-age.key"

  NIX_SIGNING_KEY_FILE="$PROVIDER_DIR/secrets/nix-signing.key"
  nix key generate-secret --key-name "hostenv-demo-local" > "$NIX_SIGNING_KEY_FILE"
  NIX_SIGNING_PUBLIC_KEY="$(nix key convert-secret-to-public < "$NIX_SIGNING_KEY_FILE")"

  sed -i "s|HOSTENV_INPUT_URL_PLACEHOLDER|$(escape_sed_replacement "path:${HOSTENV_SOURCE_DIR}")|g" "$APP_PROJECT_DIR/.hostenv/flake.nix"
  sed -i "s|HOSTENV_INPUT_URL_PLACEHOLDER|$(escape_sed_replacement "path:${HOSTENV_SOURCE_DIR}")|g" "$PROVIDER_SERVICE_PROJECT_DIR/.hostenv/flake.nix"
  sed -i 's|environmentName = null;|environmentName = "provider-service";|' "$PROVIDER_SERVICE_PROJECT_DIR/.hostenv/flake.nix"
  sed -i "s|SSH_PUBLIC_KEY_PLACEHOLDER|$(escape_sed_replacement "$SSH_PUBLIC_KEY")|g" "$APP_PROJECT_DIR/.hostenv/hostenv.nix"
  cat > "$PROVIDER_SERVICE_PROJECT_DIR/.hostenv/hostenv.nix" <<EOF_PROVIDER_SERVICE_HOSTENV
{ lib, ... }:
{
  config = {
    defaultEnvironment = "provider-service";
    hostenv.gitRef = "main";

    provider.serviceResolution = {
      organisation = "demo";
      project = "${PROVIDER_SERVICE_PROJECT_SELECTOR}";
      environmentName = "provider-service";
    };

    services.hostenv-provider = {
      enable = true;
      deploy.enable = true;
      webhookHost = "provider.demo.hostenv.test";
      uiBasePath = "/dashboard";
    };

    environments."provider-service" = {
      enable = true;
      type = "testing";

      virtualHosts."provider.demo.hostenv.test" = {
        enableLetsEncrypt = false;
        globalRedirect = null;
      };

      users.demo = {
        email = "demo@example.test";
        publicKeys = [
          "${SSH_PUBLIC_KEY}"
        ];
      };
    };
  };
}
EOF_PROVIDER_SERVICE_HOSTENV

  cat > "$PROVIDER_DIR/secrets/provider.plain.yaml" <<'EOF_SECRETS'
access_tokens: ""
demo:
  backups_secret: "hostenv-demo-password"
  backups_env: "RESTIC_COMPRESSION=off"
EOF_SECRETS
  SOPS_AGE_RECIPIENTS="$AGE_PUBLIC_KEY" sops --encrypt --input-type yaml --output-type yaml "$PROVIDER_DIR/secrets/provider.plain.yaml" > "$PROVIDER_DIR/secrets/provider.yaml"
  rm -f "$PROVIDER_DIR/secrets/provider.plain.yaml"

  init_git_repo "$APP_PROJECT_DIR"
  init_git_repo "$PROVIDER_SERVICE_PROJECT_DIR"
  ensure_local_git_ref "$PROVIDER_SERVICE_PROJECT_DIR" "provider-service"
  init_git_repo "$PROVIDER_DIR"

  chmod 0755 "$WORKDIR"
  chmod -R a+rX "$APP_PROJECT_DIR" "$PROVIDER_SERVICE_PROJECT_DIR" "$HOSTENV_SOURCE_DIR"

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

  ln -sf "$WORKDIR" "$CALLER_DIR/hostenv-demo"
  record_phase_a_evidence
}

prepare_initial_plan() {
  write_provider_flake "node-a"
  write_node_config "node-a" "$NODE_A_HOST_IP"
  write_node_config "node-b" "$NODE_B_HOST_IP"
  commit_provider_sources

  (
    cd "$PROVIDER_DIR"
    run_with_timeout "$PLAN_COMMAND_TIMEOUT" "Provider cache secret generation" nix run .#hostenv-provider -- cache-secrets
  )
  run_provider_plan
  sanitize_generated_plan
  (
    cd "$PROVIDER_DIR"
    run_with_timeout "$PLAN_COMMAND_TIMEOUT" "Provider node token generation" nix run .#hostenv-provider -- node-tokens
  )
  refresh_merged_provider_secrets
  commit_generated

  load_plan_metadata
  NODE_A_AUTH_TOKEN="$(load_provider_node_token node-a)"
  [[ -n "$NODE_A_AUTH_TOKEN" ]] || fail "Missing node-a token"
  load_cache_auth_password
  record_phase_b_evidence
}

bring_up_nodes_and_bootstrap_provider() {
  sign_deploy_profile "node-a" "system"
  sign_deploy_profile "node-a" "$APP_ENV_USER"
  sign_deploy_profile "node-a" "$PROVIDER_ENV_USER"

  start_vm "node-a"
  bootstrap_provider_service_environment


  local readiness_code
  readiness_code="$(http_status "$PROVIDER_VHOST" "http://${NODE_A_HOST_IP}:${NODE_HTTP_PORT}/api/deploy-jobs/next?node=node-a" -H "Authorization: Bearer ${NODE_A_AUTH_TOKEN}")"
  [[ "$readiness_code" =~ ^(200|404)$ ]] || fail "Provider-service HTTP readiness probe failed (HTTP $readiness_code)"

  preseed_deploy_flake_inputs "node-a"
}

deploy_app_to_node_a() {
  local sha
  sha="deploy-node-a-$(date +%s)"
  NODE_A_JOB_ID="$(trigger_app_webhook_deploy "$sha")"
  [[ -n "$NODE_A_JOB_ID" ]] || fail "Missing node-a job id"

  if ! wait_for_job_action_op "$NODE_A_JOB_ID" "node-a" "$NODE_A_AUTH_TOKEN" "activate" "$JOB_ACTION_TIMEOUT"; then
    wait_for_job_action_op "$NODE_A_JOB_ID" "node-a" "$NODE_A_AUTH_TOKEN" "reload" 180 || fail "No activate/reload action for node-a"
  fi

  if ! wait_for_provider_job_completion "$NODE_A_JOB_ID" "node-a" "$NODE_A_AUTH_TOKEN" "$JOB_COMPLETION_TIMEOUT"; then
    record_node_job_evidence "phase-e-node-a-job" "$NODE_A_JOB_ID" "node-a" "$NODE_A_AUTH_TOKEN"
    fail "Node-a deployment job failed"
  fi
  if ! wait_for_site_ready "$NODE_A_HOST_IP" 1200; then
    capture_provider_job_diagnostics "$NODE_A_JOB_ID" "node-a" "job-status-timeout-${NODE_A_JOB_ID}-node-a"
    capture_app_site_diagnostics "node-a-site-timeout" "node-a" "$NODE_A_HOST_IP"
    fail "Node-a site did not become ready. See $LOG_DIR/node-a-site-timeout.log"
  fi
  record_node_job_evidence "phase-e-node-a-job" "$NODE_A_JOB_ID" "node-a" "$NODE_A_AUTH_TOKEN"
}

prepare_migration_plan() {
  capture_migration_source_plan
  write_provider_flake "node-b"
  commit_provider_sources
  run_provider_plan
  sanitize_generated_plan
  (
    cd "$PROVIDER_DIR"
    run_with_timeout "$PLAN_COMMAND_TIMEOUT" "Provider node token regeneration" nix run .#hostenv-provider -- node-tokens
  )
  refresh_merged_provider_secrets
  commit_generated

  load_plan_metadata
  NODE_A_AUTH_TOKEN="$(load_provider_node_token node-a)"
  NODE_B_AUTH_TOKEN="$(load_provider_node_token node-b)"
  [[ -n "$NODE_A_AUTH_TOKEN" ]] || fail "Missing refreshed node-a token"
  [[ -n "$NODE_B_AUTH_TOKEN" ]] || fail "Missing refreshed node-b token"

  sign_deploy_profile "node-a" "system"
  sign_deploy_profile "node-b" "system"
  sign_deploy_profile "node-b" "$APP_ENV_USER"
  start_vm "node-b"

  preseed_deploy_flake_inputs "node-a"
  preseed_deploy_flake_inputs "node-b"
}

migrate_app_to_node_b() {
  local sha
  sha="migrate-node-b-$(date +%s)"
  apply_migration_source_plan
  NODE_B_JOB_ID="$(trigger_app_webhook_deploy "$sha")"
  [[ -n "$NODE_B_JOB_ID" ]] || fail "Missing node-b migration job id"

  wait_for_job_action_op "$NODE_B_JOB_ID" "node-a" "$NODE_A_AUTH_TOKEN" "backup" "$JOB_ACTION_TIMEOUT" || fail "No backup action for node-a"
  wait_for_job_action_op "$NODE_B_JOB_ID" "node-b" "$NODE_B_AUTH_TOKEN" "restore" "$JOB_ACTION_TIMEOUT" || fail "No restore action for node-b"
  if ! wait_for_provider_job_completion "$NODE_B_JOB_ID" "node-a" "$NODE_A_AUTH_TOKEN" "$JOB_COMPLETION_TIMEOUT"; then
    record_node_job_evidence "phase-e-migration-job" "$NODE_B_JOB_ID" "node-a" "$NODE_A_AUTH_TOKEN"
    fail "Node-a migration status failed"
  fi
  if ! wait_for_provider_job_completion "$NODE_B_JOB_ID" "node-b" "$NODE_B_AUTH_TOKEN" "$JOB_COMPLETION_TIMEOUT"; then
    record_node_job_evidence "phase-e-migration-job" "$NODE_B_JOB_ID" "node-b" "$NODE_B_AUTH_TOKEN"
    fail "Node-b migration status failed"
  fi
  if ! wait_for_site_ready "$NODE_B_HOST_IP" 1200; then
    capture_app_site_diagnostics "node-b-site-timeout" "node-b" "$NODE_B_HOST_IP"
    fail "Node-b site did not become ready. See $LOG_DIR/node-b-site-timeout.log"
  fi
  marker_present_on_node "node-b.${HOSTENV_HOSTNAME}" || fail "Migration marker not found on node-b"

  record_node_job_evidence "phase-e-migration-job" "$NODE_B_JOB_ID" "node-a" "$NODE_A_AUTH_TOKEN"
  record_node_job_evidence "phase-e-migration-job" "$NODE_B_JOB_ID" "node-b" "$NODE_B_AUTH_TOKEN"
  record_migration_sequence_evidence

  {
    printf 'marker_found=true\n'
    printf 'http_code=%s\n' "$(http_status "$APP_VHOST" "http://${NODE_B_HOST_IP}:${NODE_HTTP_PORT}/")"
  } > "$EVIDENCE_DIR/provider-service-project-phase-e-migration-verify.log"
}

main() {
  while (($# > 0)); do
    case "$1" in
      --automated) AUTOMATED=1 ;;
      --cleanup) CLEANUP=1 ;;
      --no-color) NO_COLOR=1 ;;
      --help|-h) usage; exit 0 ;;
      *) fail "Unknown option: $1 (usage: run-demo.sh [--automated] [--cleanup] [--no-color])" ;;
    esac
    shift
  done

  USE_GUM=1
  [[ "$NO_COLOR" -eq 1 ]] && USE_GUM=0

  if [[ "$USE_GUM" -eq 1 ]] && ! command -v gum >/dev/null 2>&1; then
    USE_GUM=0
  fi

  for cmd in nix git jq ssh ssh-keygen sops age-keygen curl pv gunzip socat find sed awk ss grep; do
    require_cmd "$cmd"
  done
  [[ -e /dev/kvm ]] || fail "This demo requires Linux KVM (/dev/kvm)."
  [[ -f "$SEED_SOURCE" ]] || fail "Missing seed file: $SEED_SOURCE"

  NODE_SSH_PORT="$(choose_free_port "$NODE_A_HOST_IP" "$NODE_SSH_PORT")"
  NODE_SSH_PORT="$(choose_free_port "$NODE_B_HOST_IP" "$NODE_SSH_PORT")"
  NODE_HTTP_PORT="$(choose_free_port "$NODE_A_HOST_IP" "$NODE_HTTP_PORT")"
  NODE_HTTP_PORT="$(choose_free_port "$NODE_B_HOST_IP" "$NODE_HTTP_PORT")"
  PROVIDER_HTTP_PORT="$(choose_free_port "0.0.0.0" "$PROVIDER_HTTP_PORT")"

  trap cleanup EXIT INT TERM

  stage "Phase A: Workspace bootstrap"
  prepare_workspace

  stage "Phase B: Plan generation"
  prepare_initial_plan
  pause_for_exploration "Provider topology generated." \
    "App project repo: $APP_PROJECT_DIR" \
    "Provider-service project repo: $PROVIDER_SERVICE_PROJECT_DIR" \
    "Plan evidence: $EVIDENCE_DIR/provider-service-project-phase-b-plan.log"

  stage "Phase C: Node bring-up"
  bring_up_nodes_and_bootstrap_provider
  pause_for_exploration "Provider-service is now running inside its Hostenv environment on node-a." \
    "Provider-service runtime dir: $PROVIDER_RUNTIME_DIR" \
    "Bootstrap evidence: $EVIDENCE_DIR/provider-service-project-phase-c-bootstrap.log" \
    "App probe: curl -H 'Host: $APP_VHOST' http://${NODE_A_HOST_IP}:${NODE_HTTP_PORT}/"

  stage "Phase D: Cache and control-plane readiness"
  record_phase_d_evidence

  stage "Phase E: App deployment and migration"
  deploy_app_to_node_a
  run_seed_import
  marker_present_on_node "node-a.${HOSTENV_HOSTNAME}" || fail "Seed marker not found on node-a"
  prepare_migration_plan
  migrate_app_to_node_b

  stage "Phase F: Cache client proof"
  record_cache_client_evidence "node-a"
  record_cache_client_evidence "node-b"

  stage "Demo completed"
  pause_for_exploration "Demo complete." \
    "Evidence directory: $EVIDENCE_DIR" \
    "Node-b probe: curl -H 'Host: $APP_VHOST' http://${NODE_B_HOST_IP}:${NODE_HTTP_PORT}/" \
    "Logs: $LOG_DIR"

  success "Provider-service project demo completed."
}

main "$@"
