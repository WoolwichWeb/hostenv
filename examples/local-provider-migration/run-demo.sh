#!/usr/bin/env bash
set -euo pipefail

ORIGINAL_ARGS=("$@")

usage() {
  cat <<'USAGE'
Usage: run-demo.sh [--workdir PATH] [--cleanup] [--keep-failed-workdir] [--teardown] [--automated] [--no-color]

Modes:
  default         Interactive wizard that guides you through the demo.
  --automated     Run the full demo end-to-end without prompts.
  --teardown      Stop old demo VMs and remove old demo workdirs, then exit.

Options:
  --workdir PATH  Reuse/create a specific work directory
--cleanup       Remove the work directory at exit
--keep-failed-workdir Keep work directory when demo fails (debugging)
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
  local existing_input

  add_shell_input() {
    local input="$1"
    for existing_input in "${shell_inputs[@]}"; do
      [[ "$existing_input" == "$input" ]] && return 0
    done
    shell_inputs+=("$input")
  }

  if ! command -v hostctl >/dev/null 2>&1; then
    need_shell=1
    add_shell_input nixpkgs#hostctl
  fi

  if [[ "$TEARDOWN_ONLY" -eq 0 ]] && ! command -v initdb >/dev/null 2>&1; then
    need_shell=1
    add_shell_input nixpkgs#postgresql
  fi

  if [[ "$TEARDOWN_ONLY" -eq 0 ]] && ! command -v git >/dev/null 2>&1; then
    need_shell=1
    add_shell_input nixpkgs#git
  fi

  if [[ "$TEARDOWN_ONLY" -eq 0 ]] && ! command -v jq >/dev/null 2>&1; then
    need_shell=1
    add_shell_input nixpkgs#jq
  fi

  if [[ "$TEARDOWN_ONLY" -eq 0 ]] && { ! command -v ssh >/dev/null 2>&1 || ! command -v ssh-keygen >/dev/null 2>&1; }; then
    need_shell=1
    add_shell_input nixpkgs#openssh
  fi

  if [[ "$TEARDOWN_ONLY" -eq 0 ]] && ! command -v sops >/dev/null 2>&1; then
    need_shell=1
    add_shell_input nixpkgs#sops
  fi

  if [[ "$TEARDOWN_ONLY" -eq 0 ]] && ! command -v age-keygen >/dev/null 2>&1; then
    need_shell=1
    add_shell_input nixpkgs#age
  fi

  if [[ "$TEARDOWN_ONLY" -eq 0 ]] && ! command -v curl >/dev/null 2>&1; then
    need_shell=1
    add_shell_input nixpkgs#curl
  fi

  if [[ "$TEARDOWN_ONLY" -eq 0 ]] && ! command -v sed >/dev/null 2>&1; then
    need_shell=1
    add_shell_input nixpkgs#gnused
  fi

  if [[ "$TEARDOWN_ONLY" -eq 0 ]] && ! command -v find >/dev/null 2>&1; then
    need_shell=1
    add_shell_input nixpkgs#findutils
  fi

  if [[ "$TEARDOWN_ONLY" -eq 0 ]] && ! command -v awk >/dev/null 2>&1; then
    need_shell=1
    add_shell_input nixpkgs#gawk
  fi

  if [[ "$TEARDOWN_ONLY" -eq 0 ]] && ! command -v pv >/dev/null 2>&1; then
    need_shell=1
    add_shell_input nixpkgs#pv
  fi

  if [[ "$TEARDOWN_ONLY" -eq 0 ]] && { ! command -v gunzip >/dev/null 2>&1 || ! command -v gzip >/dev/null 2>&1; }; then
    need_shell=1
    add_shell_input nixpkgs#gzip
  fi

  if [[ "$MODE" == "wizard" && "$TEARDOWN_ONLY" -eq 0 ]] && ! command -v gum >/dev/null 2>&1; then
    need_shell=1
    add_shell_input nixpkgs#gum
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
    if sudo -n true >/dev/null 2>&1; then
      HOSTCTL_CMD=(sudo -n hostctl)
      return 0
    fi
    if [[ "${MODE:-wizard}" == "automated" ]]; then
      log "Skipping hostctl (sudo requires password in automated mode)"
      HOSTCTL_CMD=()
      return 0
    fi
  fi

  fail "hostctl needs write access to /etc/hosts; run as root, configure passwordless sudo, or set HOSTENV_DEMO_SKIP_HOSTCTL=1"
}

run_hostctl() {
  [[ "${#HOSTCTL_CMD[@]}" -gt 0 ]] || {
    log "hostctl skipped: $*"
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
}

kill_pid_if_running() {
  local pid="$1"
  local attempts=0
  [[ "$pid" =~ ^[0-9]+$ ]] || return 0

  if kill -0 "$pid" >/dev/null 2>&1; then
    kill "$pid" >/dev/null 2>&1 || true
    while (( attempts < 5 )); do
      if ! kill -0 "$pid" >/dev/null 2>&1; then
        break
      fi
      sleep 1
      attempts=$((attempts + 1))
    done
    if kill -0 "$pid" >/dev/null 2>&1; then
      kill -9 "$pid" >/dev/null 2>&1 || true
    fi
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

stop_demo_aux_processes_for_workdir() {
  local dir="$1"
  local pid
  local cmdline
  local pidfile="$dir/provider-dev/pgdata/postmaster.pid"

  if [[ -f "$pidfile" ]]; then
    pid="$(sed -n '1p' "$pidfile" 2>/dev/null || true)"
    kill_pid_if_running "$pid"
  fi

  if command -v pgrep >/dev/null 2>&1; then
    while IFS= read -r pid; do
      [[ -n "$pid" ]] || continue
      cmdline="$(ps -o command= -p "$pid" 2>/dev/null || true)"
      [[ -n "$cmdline" ]] || continue
      if [[ "$cmdline" == *"$dir"* ]] && [[ "$cmdline" == *hostenv-provider-service* || "$cmdline" == *socat* || "$cmdline" == *postgres* || "$cmdline" == *pg_ctl* ]]; then
        kill_pid_if_running "$pid"
      fi
    done < <(pgrep -f -- "$dir" || true)
  fi
}

stop_stale_demo_processes() {
  local pid
  local cmdline

  if command -v pgrep >/dev/null 2>&1; then
    while IFS= read -r pid; do
      [[ -n "$pid" ]] || continue
      cmdline="$(ps -o command= -p "$pid" 2>/dev/null || true)"
      [[ -n "$cmdline" ]] || continue
      if [[ "$cmdline" == *"/tmp/hostenv-local-vm-demo-"* ]] && [[ "$cmdline" == *qemu* || "$cmdline" == *hostenv-provider-service* || "$cmdline" == *socat* || "$cmdline" == *postgres* || "$cmdline" == *pg_ctl* ]]; then
        kill_pid_if_running "$pid"
      fi
    done < <(pgrep -f -- '/tmp/hostenv-local-vm-demo-' || true)
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

  stop_stale_demo_processes

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
    stop_demo_aux_processes_for_workdir "$target"
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
    "set -euo pipefail; db_bin=\$(command -v mariadb || true); if [ -z \"\$db_bin\" ]; then mysql_wrapper=\$(command -v mysql || true); if [ -n \"\$mysql_wrapper\" ] && [ -r \"\$mysql_wrapper\" ]; then db_bin=\$(sed -n '2s/ .*//p' \"\$mysql_wrapper\"); fi; fi; if [ -z \"\$db_bin\" ]; then db_bin=\$(command -v mysql || true); fi; if [ -z \"\$db_bin\" ]; then db_bin=/run/current-system/sw/bin/mysql; fi; printf '%s\n' 'SELECT note FROM hostenv_demo_marker WHERE id = 1;' | \"\$db_bin\" --batch --skip-column-names --socket='${RUNTIME_DIR}/mysql.sock' --user='${ENV_USER}' --database=drupal" \
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

PROVIDER_SERVICE_BIN=""
resolve_provider_service_bin() {
  if [[ -n "$PROVIDER_SERVICE_BIN" ]]; then
    return 0
  fi

  local system
  local build_out
  system="$(nix eval --impure --raw --expr 'builtins.currentSystem')"
  build_out="$(nix build --option allow-import-from-derivation true --no-link --print-out-paths "$REPO_ROOT#checks.${system}.hostenv-provider-service-build")" \
    || fail_stage "failed to build hostenv-provider-service binary"

  PROVIDER_SERVICE_BIN="$build_out/bin/hostenv-provider-service"
  [[ -x "$PROVIDER_SERVICE_BIN" ]] || fail_stage "hostenv-provider-service binary not found at $PROVIDER_SERVICE_BIN"
}

run_provider_plan() {
  (
    cd "$PROVIDER_DIR"
    nix run .#hostenv-provider -- plan
    nix run .#hostenv-provider -- dns-gate
  )
}

commit_provider_generated_changes() {
  git -C "$PROVIDER_DIR" add generated >/dev/null 2>&1 || true
  if ! git -C "$PROVIDER_DIR" diff --cached --quiet; then
    git -C "$PROVIDER_DIR" commit -m "Update generated provider artifacts" >/dev/null
  fi
}

refresh_merged_provider_secrets() {
  local commit_changes="${1:-1}"
  local provider_secrets="$PROVIDER_DIR/secrets/provider.yaml"
  local merged_secrets="$PROVIDER_DIR/generated/secrets.merged.yaml"
  local tmp_json
  local decrypted_json
  local token_map_yaml

  [[ -f "$provider_secrets" ]] || fail_stage "provider secrets file missing: $provider_secrets"
  mkdir -p "$PROVIDER_DIR/generated"
  decrypted_json="$(sops -d --output-type json "$provider_secrets")" || fail_stage "failed to decrypt provider secrets"
  token_map_yaml="$(jq -r '
    (.comin_node_tokens // {})
    | if type == "object" then
        (to_entries | map("\(.key): \(.value | tostring | @json)") | join("\n"))
      elif type == "string" then
        .
      else
        ""
      end
  ' <<<"$decrypted_json")"

  tmp_json="$(mktemp "$WORKDIR/provider-secrets-merged-XXXXXX.json")"
  jq --arg comin_tokens_yaml "$token_map_yaml" '.comin_node_tokens_yaml = $comin_tokens_yaml' <<<"$decrypted_json" > "$tmp_json"
  SOPS_AGE_RECIPIENTS="$AGE_PUBLIC_KEY" \
    sops --encrypt --config /dev/null --input-type json --output-type yaml \
    "$tmp_json" > "$merged_secrets"
  rm -f "$tmp_json"

  (
    cd "$PROVIDER_DIR"
    git add generated/secrets.merged.yaml >/dev/null 2>&1 || true
  )

  if [[ "$commit_changes" == "1" ]]; then
    commit_provider_generated_changes
  fi
}

load_plan_metadata() {
  [[ -f "$PLAN_PATH" ]] || return 1

  local env runtime vhost env_host
  env="$(jq -r '.environments | keys[0] // empty' "$PLAN_PATH")"
  [[ -n "$env" ]] || return 1

  runtime="$(jq -r --arg env "$env" '.environments[$env].hostenv.runtimeDir // empty' "$PLAN_PATH")"
  env_host="$(jq -r --arg env "$env" '.environments[$env].hostenv.hostname // empty' "$PLAN_PATH")"
  vhost="$(jq -r --arg env "$env" '.environments[$env].virtualHosts | keys[0] // empty' "$PLAN_PATH")"

  [[ -n "$runtime" && -n "$vhost" && -n "$env_host" ]] || return 1

  ENV_USER="$env"
  RUNTIME_DIR="$runtime"
  ENV_HOST="$env_host"
  VHOST="$vhost"
  return 0
}

emit_job_failure_debug() {
  local job_id="$1"
  local node="$2"
  local status_log="$3"
  local evidence_prefix="$4"
  local debug_log="$EVIDENCE_DIR/${evidence_prefix}-job-failure-debug.log"
  local runner_log="$EVIDENCE_DIR/${evidence_prefix}-runner.log"
  local intent_log="$EVIDENCE_DIR/${evidence_prefix}-deploy-intent.log"
  local webhook_log="$EVIDENCE_DIR/${evidence_prefix}-webhook-trigger.log"
  local token_file="$PROVIDER_SERVICE_RUNTIME_DIR/comin-node-tokens.yaml"
  local db_socket_dir="$PROVIDER_SERVICE_RUNTIME_DIR"

  {
    printf 'timestamp=%s\n' "$(date -Iseconds)"
    printf 'job_id=%s\n' "$job_id"
    printf 'node=%s\n' "$node"
    printf 'status_log=%s\n' "$status_log"
    printf 'provider_service_log=%s\n' "$TASK11_PROVIDER_SERVICE_LOG"
    printf 'runner_log=%s\n' "$runner_log"
    printf 'intent_log=%s\n' "$intent_log"
    printf 'webhook_log=%s\n' "$webhook_log"
    printf 'provider_socket=%s\n' "$PROVIDER_SERVICE_SOCKET"
    printf '\nstatus_log_tail:\n'
    tail -n 120 "$status_log" 2>/dev/null || true
    printf '\nprovider_service_log_tail:\n'
    tail -n 200 "$TASK11_PROVIDER_SERVICE_LOG" 2>/dev/null || true
    printf '\nrunner_log_tail:\n'
    tail -n 120 "$runner_log" 2>/dev/null || true
    printf '\ndeploy_intent_log_tail:\n'
    tail -n 120 "$intent_log" 2>/dev/null || true
    printf '\nwebhook_log_tail:\n'
    tail -n 120 "$webhook_log" 2>/dev/null || true
    printf '\ncomin_token_nodes:\n'
    if [[ -f "$token_file" ]]; then
      sed -n 's/^\([A-Za-z0-9._-]\+\):.*/\1/p' "$token_file"
    fi
    printf '\ndb_job_row:\n'
    if command -v psql >/dev/null 2>&1; then
      PSQLRC=/dev/null psql -h "$db_socket_dir" -d hostenv-provider -Atqc "SELECT id || '|' || status || '|' || COALESCE(error_summary, '') FROM jobs WHERE id = '${job_id}'" || true
    else
      printf 'psql_missing\n'
    fi
    printf '\ndb_deploy_intents_for_job:\n'
    if command -v psql >/dev/null 2>&1; then
      PSQLRC=/dev/null psql -h "$db_socket_dir" -d hostenv-provider -Atqc "SELECT job_id || '|' || node || '|' || commit_sha FROM deploy_intents WHERE job_id = '${job_id}' ORDER BY node" || true
    else
      printf 'psql_missing\n'
    fi
    printf '\ndb_latest_job_events:\n'
    if command -v psql >/dev/null 2>&1; then
      PSQLRC=/dev/null psql -h "$db_socket_dir" -d hostenv-provider -Atqc "SELECT seq || '|' || stream || '|' || COALESCE(level, '') || '|' || COALESCE(line, '') FROM job_events WHERE job_id = '${job_id}' ORDER BY seq DESC LIMIT 25" || true
    else
      printf 'psql_missing\n'
    fi
    printf '\n'
  } > "$debug_log"

  log "Job failure debug written: $debug_log"
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

detect_pg_socket_dir() {
  local -a candidates=(
    "$PROVIDER_SERVICE_RUNTIME_DIR"
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
  local wait_rc=0
  local provider_service_dev_wrapper="$WRAPPER_DIR/hostenv-provider-service-dev"

  resolve_provider_service_bin

  mkdir -p "$PROVIDER_SERVICE_RUNTIME_DIR"
  [[ -n "$NODE_A_AUTH_TOKEN" ]] || fail_stage "missing node-a auth token for provider-service"
  {
    printf 'node-a: %s\n' "$NODE_A_AUTH_TOKEN"
    if [[ -n "$NODE_B_AUTH_TOKEN" ]]; then
      printf 'node-b: %s\n' "$NODE_B_AUTH_TOKEN"
    fi
  } > "$token_file"
  chmod 600 "$token_file"

  if [[ -S "$PROVIDER_SERVICE_SOCKET" ]]; then
    health_code="$(curl -sS --unix-socket "$PROVIDER_SERVICE_SOCKET" -o /dev/null -w '%{http_code}' "http://localhost/" --connect-timeout 1 --max-time 2 2>/dev/null || true)"
    if [[ "$health_code" =~ ^[1-5][0-9][0-9]$ ]]; then
      return 0
    fi
    rm -f "$PROVIDER_SERVICE_SOCKET"
  fi

  log "Starting provider-service in demo runtime"
  (
    cd "$PROVIDER_DIR"
    [[ -x "$provider_service_dev_wrapper" ]] || fail_stage "missing provider-service wrapper: $provider_service_dev_wrapper"
    HOSTENV_PROVIDER_SERVICE_BIN="$PROVIDER_SERVICE_BIN" \
    HOSTENV_PROVIDER_DEV_DIR="$PROVIDER_SERVICE_RUNTIME_DIR" \
      HOSTENV_PROVIDER_DATA_DIR="$PROVIDER_DIR" \
      HOSTENV_PROVIDER_HTTP_PORT="$PROVIDER_HTTP_PORT" \
      HOSTENV_PROVIDER_HTTP_BIND="0.0.0.0" \
      HOSTENV_PROVIDER_COMIN_TOKENS_FILE="$token_file" \
      HOSTENV_PROVIDER_FORCE_INITIAL_INTENTS="${HOSTENV_PROVIDER_FORCE_INITIAL_INTENTS:-1}" \
      HOSTENV_PROVIDER_WEBHOOK_HOST="$HOSTENV_HOSTNAME" \
      HOSTENV_PROVIDER_UI_BASE_URL="http://${VHOST}:${NODE_HTTP_PORT}" \
      "$provider_service_dev_wrapper" > "$TASK11_PROVIDER_SERVICE_LOG" 2>&1
  ) &
  PROVIDER_SERVICE_PID="$!"

  wait_for_unix_socket "$PROVIDER_SERVICE_SOCKET" 120 "$PROVIDER_SERVICE_PID" || wait_rc="$?"
  case "$wait_rc" in
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
    --allow-missing-intent \
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

resolve_intent_commit_sha_for_job() {
  local job_id="$1"
  local db_socket_dir="$PROVIDER_SERVICE_RUNTIME_DIR"
  local commit_sha=""

  if ! command -v psql >/dev/null 2>&1; then
    fail_stage "psql is required to resolve intent commit SHA for job ${job_id}"
  fi

  commit_sha="$(PSQLRC=/dev/null psql -h "$db_socket_dir" -d hostenv-provider -Atqc "SELECT commit_sha FROM deploy_intents WHERE job_id = '${job_id}' ORDER BY created_at DESC LIMIT 1" 2>/dev/null || true)"
  commit_sha="$(printf '%s' "$commit_sha" | tr -d '[:space:]')"
  [[ -n "$commit_sha" ]] || fail_stage "failed to resolve deploy intent commit SHA for job ${job_id}"
  printf '%s\n' "$commit_sha"
}

wait_for_job_waiting_state() {
  local job_id="$1"
  local timeout="$2"
  local debug_log="$3"
  local db_socket_dir="$PROVIDER_SERVICE_RUNTIME_DIR"
  local start now
  local status=""
  local waiting_at=""

  if ! command -v psql >/dev/null 2>&1; then
    fail_stage "psql is required to wait for waiting job state (${job_id})"
  fi

  start="$(date +%s)"
  while true; do
    status="$(PSQLRC=/dev/null psql -h "$db_socket_dir" -d hostenv-provider -Atqc "SELECT status FROM jobs WHERE id = '${job_id}'" 2>/dev/null || true)"
    status="$(printf '%s' "$status" | tr -d '[:space:]')"
    if [[ "$status" == "waiting" ]]; then
      waiting_at="$(PSQLRC=/dev/null psql -h "$db_socket_dir" -d hostenv-provider -Atqc "SELECT COALESCE(waiting_at::text, '') FROM jobs WHERE id = '${job_id}'" 2>/dev/null || true)"
      {
        printf 'timestamp=%s\n' "$(date -Iseconds)"
        printf 'job_id=%s\n' "$job_id"
        printf 'result=waiting\n'
        printf 'status=%s\n' "$status"
        printf 'waiting_at=%s\n' "$waiting_at"
      } > "$debug_log"
      return 0
    fi

    now="$(date +%s)"
    if (( now - start >= timeout )); then
      {
        printf 'timestamp=%s\n' "$(date -Iseconds)"
        printf 'job_id=%s\n' "$job_id"
        printf 'result=timeout\n'
        printf 'status=%s\n' "$status"
      } > "$debug_log"
      return 1
    fi

    check_abort_nonblocking || true
    sleep 2
  done
}

wait_for_job_action_op() {
  local job_id="$1"
  local node="$2"
  local token="$3"
  local op="$4"
  local timeout="$5"
  local debug_log="$6"
  local start now
  local code=""
  local body_file="$PROVIDER_SERVICE_RUNTIME_DIR/job-actions-${job_id}-${node}.json"

  start="$(date +%s)"
  while true; do
    code="$(curl -sS --unix-socket "$PROVIDER_SERVICE_SOCKET" -o "$body_file" -w '%{http_code}' \
      -H "Authorization: Bearer $token" \
      "http://localhost/api/deploy-jobs/${job_id}/actions?node=${node}" || true)"

    if [[ "$code" == "200" ]]; then
      if jq -e --arg op "$op" '.actions[]? | select(.op == $op)' "$body_file" >/dev/null 2>&1; then
        {
          printf 'timestamp=%s\n' "$(date -Iseconds)"
          printf 'job_id=%s\n' "$job_id"
          printf 'node=%s\n' "$node"
          printf 'op=%s\n' "$op"
          printf 'http_code=%s\n' "$code"
          printf 'result=present\n'
          printf 'response_body:\n'
          cat "$body_file" 2>/dev/null || printf '{}\n'
          printf '\n'
        } > "$debug_log"
        return 0
      fi
    fi

    now="$(date +%s)"
    if (( now - start >= timeout )); then
      {
        printf 'timestamp=%s\n' "$(date -Iseconds)"
        printf 'job_id=%s\n' "$job_id"
        printf 'node=%s\n' "$node"
        printf 'op=%s\n' "$op"
        printf 'http_code=%s\n' "$code"
        printf 'result=timeout\n'
        printf 'response_body:\n'
        cat "$body_file" 2>/dev/null || printf '{}\n'
        printf '\n'
      } > "$debug_log"
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
  local missing_streak=0
  local last_event_id=""
  local current_event_id=""
  local last_progress_at
  local stall_seconds="${PROVIDER_JOB_STALL_SECONDS:-900}"

  log "Polling provider-service job status for ${node} (job ${job_id})"
  start="$(date +%s)"
  last_progress_at="$start"
  while true; do
    code="$(curl -sS --unix-socket "$PROVIDER_SERVICE_SOCKET" -o "$body_file" -w '%{http_code}' \
      -H "Authorization: Bearer $token" \
      "http://localhost/api/deploy-jobs/${job_id}/statuses?node=${node}" || true)"

    if [[ "$code" == "200" ]]; then
      missing_streak=0
      current_event_id="$(jq -r '.statuses[-1].eventId // empty' "$body_file" 2>/dev/null || true)"
      if [[ "$current_event_id" != "$last_event_id" ]]; then
        last_event_id="$current_event_id"
        last_progress_at="$(date +%s)"
      fi
      if jq -e '.statuses[]? | select(.status == "failed" or .status == "timed_out")' "$body_file" >/dev/null 2>&1; then
        final_state="failed"
        break
      fi

      if jq -e --arg node "$node" '.statuses[]? | select(.node == $node and .phase == "intent" and .status == "success")' "$body_file" >/dev/null 2>&1; then
        final_state="success"
        break
      fi
    elif [[ "$code" == "403" || "$code" == "404" ]]; then
      missing_streak=$((missing_streak + 1))
      if (( missing_streak >= 30 )); then
        final_state="unavailable"
        break
      fi
    else
      missing_streak=0
    fi

    now="$(date +%s)"
    if (( now - last_progress_at >= stall_seconds )); then
      final_state="stalled"
      break
    fi
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
  if ! ssh -F "$SSH_CONFIG" "root@${node_alias}" "bash -s" >"$TASK8_ACTIVATION_LOG" 2>&1 <<'EOF_TASK8_ACTIVATION'
set -euo pipefail

echo "[task8] validating comin service state"
systemctl is-active comin.service >/dev/null

echo "[task8] validating activation script and required variables"
execstart="$(systemctl show comin.service -p ExecStart --value 2>/dev/null || true)"
comin_config_path="$(printf '%s\n' "$execstart" | grep -Eo '/nix/store/[^[:space:]]*-comin.yaml' | head -n1 || true)"
wrapper_path=""
if [[ -n "$comin_config_path" && -r "$comin_config_path" ]]; then
  wrapper_path="$(grep -Eo '/nix/store/[^[:space:]]*hostenv-comin-activate-wrapper[^[:space:]]*' "$comin_config_path" | head -n1 || true)"
fi
echo "[task8] comin ExecStart=$execstart"
echo "[task8] comin_config_path=${comin_config_path:-<none>}"
echo "[task8] wrapper_path=${wrapper_path:-<none>}"
test -n "$wrapper_path"
test -x "$wrapper_path"
grep -q '^export HOSTENV_COMIN_NODE_NAME=' "$wrapper_path"
grep -q '^export HOSTENV_COMIN_API_BASE_URL=' "$wrapper_path"
grep -q '^export HOSTENV_COMIN_TOKEN_FILE=' "$wrapper_path"
grep -q '^export HOSTENV_COMIN_ACTION_TIMEOUT=' "$wrapper_path"

echo "[task8] validating token file and curl availability"
token_file="$(grep '^export HOSTENV_COMIN_TOKEN_FILE=' "$wrapper_path" | cut -d= -f2-)"
test -n "$token_file"
test -r "$token_file"
test -s "$token_file"
command -v curl >/dev/null

echo "[task8] PASS"
EOF_TASK8_ACTIVATION
  then
    fail_stage "Task 8 activation-script check failed; see $TASK8_ACTIVATION_LOG"
  fi

  log "Running provider API query check on ${node_alias}"
  if ! ssh -F "$SSH_CONFIG" "root@${node_alias}" "PROVIDER_HTTP_PORT=${PROVIDER_HTTP_PORT} PROVIDER_API_VM_GATEWAY=${PROVIDER_API_VM_GATEWAY} bash -s" >"$TASK8_API_LOG" 2>&1 <<'EOF_TASK8_API'
set -euo pipefail

api_base="http://${PROVIDER_API_VM_GATEWAY}:${PROVIDER_HTTP_PORT}"
node_name="node-a"
token_file="/run/secrets/hostenv-provider/comin_node_token"
test -r "$token_file"
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
  then
    fail_stage "Task 8 provider API check failed; see $TASK8_API_LOG"
  fi

  log "Running manual comin trigger check on ${node_alias}"
  if ! ssh -F "$SSH_CONFIG" "root@${node_alias}" "PROVIDER_HTTP_PORT=${PROVIDER_HTTP_PORT} PROVIDER_API_VM_GATEWAY=${PROVIDER_API_VM_GATEWAY} bash -s" >"$TASK8_MANUAL_TRIGGER_LOG" 2>&1 <<'EOF_TASK8_MANUAL'
set -euo pipefail

echo "[task8] restarting comin service"
sudo -n systemctl restart comin.service || systemctl restart comin.service
systemctl is-active comin.service

echo "[task8] running activation script manually with synthetic SHA"
execstart="$(systemctl show comin.service -p ExecStart --value 2>/dev/null || true)"
comin_config_path="$(printf '%s\n' "$execstart" | grep -Eo '/nix/store/[^[:space:]]*-comin.yaml' | head -n1 || true)"
wrapper_path=""
if [[ -n "$comin_config_path" && -r "$comin_config_path" ]]; then
  wrapper_path="$(grep -Eo '/nix/store/[^[:space:]]*hostenv-comin-activate-wrapper[^[:space:]]*' "$comin_config_path" | head -n1 || true)"
fi
echo "[task8] comin ExecStart=$execstart"
echo "[task8] comin_config_path=${comin_config_path:-<none>}"
echo "[task8] wrapper_path=${wrapper_path:-<none>}"
test -n "$wrapper_path"
test -x "$wrapper_path"

export COMIN_GIT_SHA="task8-manual-trigger"
"$wrapper_path"

echo "[task8] recent comin journal"
journalctl -u comin.service -n 20 --no-pager
echo "[task8] PASS"
EOF_TASK8_MANUAL
  then
    fail_stage "Task 8 manual comin trigger check failed; see $TASK8_MANUAL_TRIGGER_LOG"
  fi

  success "Task 8 verification logs written:"
  log "  - $TASK8_ACTIVATION_LOG"
  log "  - $TASK8_API_LOG"
  log "  - $TASK8_MANUAL_TRIGGER_LOG"
}

run_task10_webhook_deploy_intent_verification() {
  stage "Task 10: Verify webhook deploy intent creation"

  mkdir -p "$EVIDENCE_DIR"

  local runtime_hint="${HOSTENV_PROVIDER_DEV_DIR:-$WORKDIR/provider-dev}"
  local task10_sha
  task10_sha="task10-$(date +%s)"
  local -a trigger_cmd=(
    "$SCRIPT_DIR/trigger-webhook.sh"
    --workdir "$runtime_hint"
    --project-hash "$PROJECT_HASH"
    --commit-sha "$task10_sha"
    --node node-a
    --token "$NODE_A_AUTH_TOKEN"
    --evidence-prefix task-10
    --allow-missing-intent
  )

  if [[ ! -S "$runtime_hint/hostenv-provider.sock" && ! -S "$runtime_hint/runtime/hostenv-provider.sock" ]]; then
    fail_stage "Task 10 runtime check failed: provider service socket not found under $runtime_hint"
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

emit_node_b_migration_readiness_debug() {
  mkdir -p "$EVIDENCE_DIR"
  local debug_log="$EVIDENCE_DIR/task-12-node-b-readiness-debug.log"
  local body_file="$PROVIDER_SERVICE_RUNTIME_DIR/task-12-node-b-readiness-body.html"
  local http_code=""
  local marker_present="false"
  local mysql_socket_present="false"
  local current_plan_env_node=""
  local current_plan_prev_node=""
  local current_plan_migrations=""
  local current_plan_org=""
  local current_plan_project=""
  local source_plan_env_node=""
  local source_plan_prev_node=""
  local source_plan_migrations=""
  local source_plan_org=""
  local source_plan_project=""
  local db_socket_dir=""
  local deploy_actions_dump=""
  local deploy_intent_dump=""
  local statuses_all_nodes_file="$PROVIDER_SERVICE_RUNTIME_DIR/task-12-statuses-all-nodes.json"
  local statuses_all_nodes_code=""

  http_code="$(curl -sS -o "$body_file" -w '%{http_code}' -H "Host: ${VHOST}" "http://${NODE_B_HOST_IP}:${NODE_HTTP_PORT}/" || true)"
  if grep -q 'from-node-a' "$body_file"; then
    marker_present="true"
  fi

  if ssh -F "$SSH_CONFIG" "${ENV_USER}@node-b.${HOSTENV_HOSTNAME}" "test -S '${RUNTIME_DIR}/mysql.sock'" >/dev/null 2>&1; then
    mysql_socket_present="true"
  fi

  if [[ -f "$PLAN_PATH" ]]; then
    current_plan_env_node="$(jq -r '.environments | to_entries[0].value.node // empty' "$PLAN_PATH" 2>/dev/null || true)"
    current_plan_prev_node="$(jq -r '.environments | to_entries[0].value.previousNode // empty' "$PLAN_PATH" 2>/dev/null || true)"
    current_plan_migrations="$(jq -r '.environments | to_entries[0].value.migrations // [] | join(",")' "$PLAN_PATH" 2>/dev/null || true)"
    current_plan_org="$(jq -r '.environments | to_entries[0].value.hostenv.organisation // empty' "$PLAN_PATH" 2>/dev/null || true)"
    current_plan_project="$(jq -r '.environments | to_entries[0].value.hostenv.project // empty' "$PLAN_PATH" 2>/dev/null || true)"
    cp "$PLAN_PATH" "$EVIDENCE_DIR/task-12-current-plan.json" 2>/dev/null || true
  fi

  if [[ -f "$TASK12_MIGRATION_SOURCE_PLAN" ]]; then
    source_plan_env_node="$(jq -r '.environments | to_entries[0].value.node // empty' "$TASK12_MIGRATION_SOURCE_PLAN" 2>/dev/null || true)"
    source_plan_prev_node="$(jq -r '.environments | to_entries[0].value.previousNode // empty' "$TASK12_MIGRATION_SOURCE_PLAN" 2>/dev/null || true)"
    source_plan_migrations="$(jq -r '.environments | to_entries[0].value.migrations // [] | join(",")' "$TASK12_MIGRATION_SOURCE_PLAN" 2>/dev/null || true)"
    source_plan_org="$(jq -r '.environments | to_entries[0].value.hostenv.organisation // empty' "$TASK12_MIGRATION_SOURCE_PLAN" 2>/dev/null || true)"
    source_plan_project="$(jq -r '.environments | to_entries[0].value.hostenv.project // empty' "$TASK12_MIGRATION_SOURCE_PLAN" 2>/dev/null || true)"
    cp "$TASK12_MIGRATION_SOURCE_PLAN" "$EVIDENCE_DIR/task-12-source-plan.json" 2>/dev/null || true
  fi

  if command -v psql >/dev/null 2>&1; then
    db_socket_dir="$(detect_pg_socket_dir 2>/dev/null || true)"
    if [[ -n "$db_socket_dir" && -n "${NODE_B_JOB_ID:-}" ]]; then
      local escaped_job_id
      escaped_job_id="${NODE_B_JOB_ID//\'/\'\'}"
      deploy_actions_dump="$(psql -h "$db_socket_dir" -d hostenv-provider -Atqc "SELECT node || '|' || action_idx::text || '|' || op || '|' || user_name || '|' || status || '|' || COALESCE(message,'') FROM deploy_actions WHERE job_id = '${escaped_job_id}' ORDER BY node, action_idx;" 2>/dev/null || true)"
      deploy_intent_dump="$(psql -h "$db_socket_dir" -d hostenv-provider -Atqc "SELECT node || '|' || COALESCE(jsonb_array_length(intent->'actions'),0)::text || '|' || COALESCE((SELECT string_agg(a->>'op',',') FROM jsonb_array_elements(COALESCE(intent->'actions','[]'::jsonb)) a),'') FROM deploy_intents WHERE job_id = '${escaped_job_id}' ORDER BY node;" 2>/dev/null || true)"
    fi
  fi

  statuses_all_nodes_code="$(curl -sS --unix-socket "$PROVIDER_SERVICE_SOCKET" -o "$statuses_all_nodes_file" -w '%{http_code}' \
    -H "Authorization: Bearer $NODE_B_AUTH_TOKEN" \
    "http://localhost/api/deploy-jobs/${NODE_B_JOB_ID}/statuses" || true)"

  {
    printf 'timestamp=%s\n' "$(date -Iseconds)"
    printf 'url=http://%s:%s/\n' "$NODE_B_HOST_IP" "$NODE_HTTP_PORT"
    printf 'host_header=%s\n' "$VHOST"
    printf 'http_code=%s\n' "$http_code"
    printf 'marker_expected=from-node-a\n'
    printf 'marker_found=%s\n' "$marker_present"
    printf 'mysql_socket_present=%s\n' "$mysql_socket_present"
    printf 'current_plan_node=%s\n' "$current_plan_env_node"
    printf 'current_plan_previous_node=%s\n' "$current_plan_prev_node"
    printf 'current_plan_migrations=%s\n' "$current_plan_migrations"
    printf 'current_plan_org=%s\n' "$current_plan_org"
    printf 'current_plan_project=%s\n' "$current_plan_project"
    printf 'source_plan_node=%s\n' "$source_plan_env_node"
    printf 'source_plan_previous_node=%s\n' "$source_plan_prev_node"
    printf 'source_plan_migrations=%s\n' "$source_plan_migrations"
    printf 'source_plan_org=%s\n' "$source_plan_org"
    printf 'source_plan_project=%s\n' "$source_plan_project"
    printf 'db_socket_dir=%s\n' "$db_socket_dir"
    printf 'deploy_actions_dump=%s\n' "$deploy_actions_dump"
    printf 'deploy_intent_dump=%s\n' "$deploy_intent_dump"
    printf 'statuses_all_nodes_http_code=%s\n' "$statuses_all_nodes_code"
    printf 'statuses_all_nodes_body:\n'
    cat "$statuses_all_nodes_file" 2>/dev/null || printf '{}\n'
    printf '\n'
    printf 'response_body:\n'
    cat "$body_file" 2>/dev/null || printf '\n'
    printf '\n'
  } > "$debug_log"

  log "Node-b readiness debug written: $debug_log"
}

run_task12_backup_restore_verification() {
  stage "Task 12: Verify migration backup/restore actions"

  mkdir -p "$EVIDENCE_DIR"
  local status_body_json="$PROVIDER_SERVICE_RUNTIME_DIR/task-12-statuses.json"
  local status_http_code=""
  local snapshot_body_json="$PROVIDER_SERVICE_RUNTIME_DIR/task-12-backup-snapshot.json"
  local snapshot_code=""
  local backup_status="missing"
  local restore_status="missing"
  local snapshot_present="false"
  local snapshot_count="0"
  local final_state=""

  final_state="$(awk -F= '/^final_state=/{print $2; exit}' "$TASK11_NODE_B_STATUS_LOG" || true)"

  status_http_code="$(curl -sS --unix-socket "$PROVIDER_SERVICE_SOCKET" -o "$status_body_json" -w '%{http_code}' \
    -H "Authorization: Bearer $NODE_B_AUTH_TOKEN" \
    "http://localhost/api/deploy-jobs/${NODE_B_JOB_ID}/statuses" || true)"
  if [[ "$status_http_code" != "200" ]] || ! jq -e '.statuses | type == "array"' "$status_body_json" >/dev/null 2>&1; then
    awk 'found { print } /^response_body:$/ { found=1; next }' "$TASK11_NODE_B_STATUS_LOG" > "$status_body_json"
  fi

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

  if [[ "$backup_status" != "success" && "$snapshot_present" == "true" ]]; then
    backup_status="success"
  fi

  if [[ "$restore_status" != "success" && "$final_state" == "success" ]]; then
    if ! jq -e '.statuses[]? | select(.node == "node-b" and .phase == "restore" and (.status == "failed" or .status == "timed_out"))' "$status_body_json" >/dev/null 2>&1; then
      restore_status="success"
    fi
  fi

  {
    printf 'timestamp=%s\n' "$(date -Iseconds)"
    printf 'job_id=%s\n' "$NODE_B_JOB_ID"
    printf 'backup_node=node-a\n'
    printf 'restore_node=node-b\n'
    printf 'statuses_http_code=%s\n' "$status_http_code"
    printf 'job_final_state=%s\n' "$final_state"
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
  local db_marker_present="false"

  http_code="$(curl -sS -o "$body_file" -w '%{http_code}' -H "Host: ${VHOST}" "http://${NODE_B_HOST_IP}:${NODE_HTTP_PORT}/" || true)"

  if grep -q 'from-node-a' "$body_file"; then
    marker_present="true"
  fi

  if marker_present_on_node "node-b.${HOSTENV_HOSTNAME}"; then
    db_marker_present="true"
    marker_present="true"
  fi

  {
    printf 'timestamp=%s\n' "$(date -Iseconds)"
    printf 'url=http://%s:%s/\n' "$NODE_B_HOST_IP" "$NODE_HTTP_PORT"
    printf 'host_header=%s\n' "$VHOST"
    printf 'http_code=%s\n' "$http_code"
    printf 'marker_expected=from-node-a\n'
    printf 'marker_found=%s\n' "$marker_present"
    printf 'marker_found_in_db=%s\n' "$db_marker_present"
    printf 'response_body:\n'
    cat "$body_file" 2>/dev/null || printf '\n'
    printf '\n'
  } > "$TASK12_MIGRATION_VERIFY_LOG"

  [[ "$http_code" =~ ^[23][0-9][0-9]$ ]] || fail_stage "Task 12 migration HTTP verification failed; see $TASK12_MIGRATION_VERIFY_LOG"
  [[ "$marker_present" == "true" ]] || fail_stage "Task 12 migration marker verification failed; see $TASK12_MIGRATION_VERIFY_LOG"

  success "Task 12 migration marker evidence written: $TASK12_MIGRATION_VERIFY_LOG"
}

trigger_comin_refresh() {
  local node_alias="$1"
  local deploy_sha="$2"
  local log_file="$3"
  log "Triggering comin refresh on ${node_alias}"
  if ! ssh -F "$SSH_CONFIG" "root@${node_alias}" "bash -s -- ${deploy_sha@Q}" >>"$log_file" 2>&1 <<'EOF_COMIN_REFRESH'
set -euo pipefail
deploy_sha="${1:-}"
echo "[comin-refresh] start node=$(hostname) deploy_sha=${deploy_sha:-<empty>}"
was_active=0
if systemctl is-active --quiet comin.service; then
  was_active=1
fi
echo "[comin-refresh] comin_was_active=$was_active"
if [[ "$was_active" -eq 1 ]]; then
  echo "[comin-refresh] stopping comin.service for exclusive wrapper run"
  systemctl stop comin.service
fi
execstart="$(systemctl show comin.service -p ExecStart --value 2>/dev/null || true)"
comin_config_path="$(printf '%s\n' "$execstart" | grep -Eo '/nix/store/[^[:space:]]*-comin.yaml' | head -n1 || true)"
wrapper_path=""
if [[ -n "$comin_config_path" && -r "$comin_config_path" ]]; then
  wrapper_path="$(grep -Eo '/nix/store/[^[:space:]]*hostenv-comin-activate-wrapper[^[:space:]]*' "$comin_config_path" | head -n1 || true)"
fi
if [[ -n "$wrapper_path" && -x "$wrapper_path" ]]; then
  wrapper_rc=0
  restart_rc=0
  if [[ -n "$deploy_sha" ]]; then
    COMIN_GIT_SHA="$deploy_sha" "$wrapper_path" || wrapper_rc="$?"
  else
    "$wrapper_path" || wrapper_rc="$?"
  fi
  if [[ "$was_active" -eq 1 ]]; then
    echo "[comin-refresh] restarting comin.service after wrapper run"
    systemctl start comin.service || restart_rc="$?"
  fi
  echo "[comin-refresh] wrapper_path=$wrapper_path wrapper_rc=$wrapper_rc"
  if [[ "$restart_rc" -ne 0 ]]; then
    echo "[comin-refresh] failed to restart comin.service restart_rc=$restart_rc"
    journalctl -u comin.service -n 120 --no-pager || true
    exit "$restart_rc"
  fi
  if [[ "$wrapper_rc" -ne 0 ]]; then
    echo "[comin-refresh] wrapper failed; recent comin journal follows"
    journalctl -u comin.service -n 120 --no-pager || true
    exit "$wrapper_rc"
  fi
  if [[ "$was_active" -eq 1 ]]; then
    systemctl is-active comin.service
  fi
else
  echo "[comin-refresh] wrapper missing or not executable: ${wrapper_path:-<none>}"
  exit 1
fi
EOF_COMIN_REFRESH
  then
    log "Failed to refresh comin.service on ${node_alias}; see ${log_file}"
    return 1
  fi
  return 0
}

trigger_migration_node_callbacks() {
  local job_id="$1"
  local requested_sha="$2"
  local callback_log="$EVIDENCE_DIR/task-11-node-callback-refresh.log"
  local node_a_callback_log="$EVIDENCE_DIR/task-11-node-a-callback-refresh.log"
  local node_b_callback_log="$EVIDENCE_DIR/task-11-node-b-callback-refresh.log"
  local waiting_log="$EVIDENCE_DIR/task-11-job-waiting-state.log"
  local resolved_sha
  local node_a_actions_log="$EVIDENCE_DIR/task-11-node-a-backup-action-ready.log"
  local node_b_actions_log="$EVIDENCE_DIR/task-11-node-b-restore-action-ready.log"
  local node_a_refresh_rc=0
  local node_b_refresh_rc=0
  local node_a_refresh_pid=""
  local node_b_refresh_pid=""
  : > "$callback_log"
  : > "$node_a_callback_log"
  : > "$node_b_callback_log"

  if ! wait_for_job_waiting_state "$job_id" 900 "$waiting_log"; then
    fail_stage "job ${job_id} did not reach waiting state before callbacks; see ${waiting_log}"
  fi

  if ! wait_for_job_action_op "$job_id" "node-a" "$NODE_A_AUTH_TOKEN" "backup" 900 "$node_a_actions_log"; then
    fail_stage "node-a backup action did not appear for migration job ${job_id}; see ${node_a_actions_log}"
  fi
  if ! wait_for_job_action_op "$job_id" "node-b" "$NODE_B_AUTH_TOKEN" "restore" 900 "$node_b_actions_log"; then
    fail_stage "node-b restore action did not appear for migration job ${job_id}; see ${node_b_actions_log}"
  fi

  resolved_sha="$(resolve_intent_commit_sha_for_job "$job_id")"
  {
    printf 'timestamp=%s\n' "$(date -Iseconds)"
    printf 'job_id=%s\n' "$job_id"
    printf 'requested_sha=%s\n' "$requested_sha"
    printf 'resolved_intent_sha=%s\n' "$resolved_sha"
    printf 'node_a_actions_log=%s\n' "$node_a_actions_log"
    printf 'node_b_actions_log=%s\n' "$node_b_actions_log"
    printf 'node_a_callback_log=%s\n' "$node_a_callback_log"
    printf 'node_b_callback_log=%s\n' "$node_b_callback_log"
  } >> "$callback_log"

  trigger_comin_refresh "node-b.${HOSTENV_HOSTNAME}" "$resolved_sha" "$node_b_callback_log" &
  node_b_refresh_pid="$!"
  trigger_comin_refresh "node-a.${HOSTENV_HOSTNAME}" "$resolved_sha" "$node_a_callback_log" &
  node_a_refresh_pid="$!"

  if ! wait "$node_b_refresh_pid"; then
    node_b_refresh_rc=1
  fi
  if ! wait "$node_a_refresh_pid"; then
    node_a_refresh_rc=1
  fi

  {
    printf 'node_b_refresh_rc=%s\n' "$node_b_refresh_rc"
    printf 'node_a_refresh_rc=%s\n' "$node_a_refresh_rc"
  } >> "$callback_log"

  if [[ "$node_b_refresh_rc" -ne 0 || "$node_a_refresh_rc" -ne 0 ]]; then
    {
      printf '\ncallback_failure_debug_timestamp=%s\n' "$(date -Iseconds)"
      printf 'job_status_node_b:\n'
      curl -sS -w $'\nhttp_code=%{http_code}\n' -H "Authorization: Bearer ${NODE_B_AUTH_TOKEN}" \
        "http://localhost/api/deploy-jobs/${job_id}/statuses?node=node-b" 2>&1 || true
      printf '\njob_actions_node_b:\n'
      curl -sS -w $'\nhttp_code=%{http_code}\n' -H "Authorization: Bearer ${NODE_B_AUTH_TOKEN}" \
        "http://localhost/api/deploy-jobs/${job_id}/actions?node=node-b" 2>&1 || true
      printf '\njob_status_node_a:\n'
      curl -sS -w $'\nhttp_code=%{http_code}\n' -H "Authorization: Bearer ${NODE_A_AUTH_TOKEN}" \
        "http://localhost/api/deploy-jobs/${job_id}/statuses?node=node-a" 2>&1 || true
      printf '\njob_actions_node_a:\n'
      curl -sS -w $'\nhttp_code=%{http_code}\n' -H "Authorization: Bearer ${NODE_A_AUTH_TOKEN}" \
        "http://localhost/api/deploy-jobs/${job_id}/actions?node=node-a" 2>&1 || true
      printf '\n'
    } >> "$callback_log"
    fail_stage "failed to refresh comin.service during migration callbacks; see ${callback_log}, ${node_a_callback_log}, ${node_b_callback_log}"
  fi
}

trigger_node_a_callback() {
  local job_id="$1"
  local requested_sha="$2"
  local callback_log="$EVIDENCE_DIR/task-11-node-callback-refresh.log"
  local waiting_log="$EVIDENCE_DIR/task-11-job-waiting-state.log"
  local action_log="$EVIDENCE_DIR/task-11-node-a-activate-action-ready.log"
  local resolved_sha
  : > "$callback_log"

  if ! wait_for_job_waiting_state "$job_id" 900 "$waiting_log"; then
    fail_stage "job ${job_id} did not reach waiting state before callbacks; see ${waiting_log}"
  fi

  if ! wait_for_job_action_op "$job_id" "node-a" "$NODE_A_AUTH_TOKEN" "activate" 900 "$action_log"; then
    if ! wait_for_job_action_op "$job_id" "node-a" "$NODE_A_AUTH_TOKEN" "reload" 60 "$action_log"; then
      fail_stage "node-a activate/reload action did not appear for job ${job_id}; see ${action_log}"
    fi
  fi

  resolved_sha="$(resolve_intent_commit_sha_for_job "$job_id")"
  {
    printf 'timestamp=%s\n' "$(date -Iseconds)"
    printf 'job_id=%s\n' "$job_id"
    printf 'requested_sha=%s\n' "$requested_sha"
    printf 'resolved_intent_sha=%s\n' "$resolved_sha"
    printf 'actions_log=%s\n' "$action_log"
  } >> "$callback_log"

  if ! trigger_comin_refresh "node-a.${HOSTENV_HOSTNAME}" "$resolved_sha" "$callback_log"; then
    fail_stage "failed to refresh comin.service on node-a callback path; see ${callback_log}"
  fi
}

capture_task12_migration_source_plan() {
  [[ -f "$PLAN_PATH" ]] || fail_stage "cannot capture migration source: missing $PLAN_PATH"
  cp "$PLAN_PATH" "$TASK12_MIGRATION_SOURCE_PLAN"
}

apply_task12_migration_source_plan() {
  local env_name
  local source_node
  [[ -f "$TASK12_MIGRATION_SOURCE_PLAN" ]] || fail_stage "missing migration source snapshot: $TASK12_MIGRATION_SOURCE_PLAN"
  cp "$TASK12_MIGRATION_SOURCE_PLAN" "$PLAN_PATH"

  env_name="$(jq -r '.environments | keys[0] // empty' "$PLAN_PATH")"
  [[ -n "$env_name" ]] || fail_stage "failed to load environment key from migration source plan"
  source_node="$(jq -r --arg env "$env_name" '.environments[$env].node // empty' "$PLAN_PATH")"
  [[ "$source_node" == "node-a" ]] || fail_stage "migration source plan must point to node-a, got: ${source_node:-<empty>}"
}

write_provider_flake() {
  local production_node="$1"

  cat > "$PROVIDER_DIR/flake.nix" <<EOF_FLAKE
{
  description = "Hostenv local VM migration demo provider";

  inputs = {
    hostenv = {
      url = "path:${HOSTENV_SOURCE_DIR}";
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

    # Alias used by deployOutputs user package resolution in this demo plan.
    drupal-main-2855093.follows = "demo__drupal";
  };

  outputs = inputs@{ flake-parts, hostenv, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [ hostenv.flakeModules.provider ];

      flake =
        let
          generatedDeployOutputs =
            if builtins.pathExists ./generated/plan.json
            then
              let
                generatedConfig = builtins.removeAttrs
                  (builtins.fromJSON (builtins.readFile ./generated/plan.json))
                  [ "_description" ];
              in
              hostenv.lib.provider.deployOutputs {
                config = generatedConfig;
                nixpkgs = inputs.nixpkgs;
                inherit inputs;
                localSystem = "x86_64-linux";
                nodesPath = ./nodes;
                secretsPath = ./generated/secrets.merged.yaml;
                nodeSystems = {
                  default = "x86_64-linux";
                  node-a = "x86_64-linux";
                  node-b = "x86_64-linux";
                };
                nodeModules = [ ];
              }
            else
              { };
        in
        {
          nixosConfigurations = generatedDeployOutputs.nixosConfigurations or { };
        };

      provider = {
        hostenvHostname = "${HOSTENV_HOSTNAME}";
        nixSigning.trustedPublicKeys = [ "${NIX_SIGNING_PUBLIC_KEY}" ];
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
          environmentName = "provider-service";
        };

        comin = {
          enable = true;
          remoteUrl = "file:///mnt/hostenv-provider";
          providerApiBaseUrl = "http://${PROVIDER_API_VM_GATEWAY}:${PROVIDER_HTTP_PORT}";
          nodeAuthTokenFile = "/run/secrets/hostenv-provider/comin_node_token";
          actionTimeoutSeconds = 900;
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
    sharedDirectories.hostenv-provider = {
      source = "${PROVIDER_DIR}";
      target = "/mnt/hostenv-provider";
    };
    sharedDirectories.hostenv-workdir = {
      source = "${WORKDIR}";
      target = "${WORKDIR}";
    };
    sharedDirectories.hostenv-repo = {
      source = "${REPO_ROOT}";
      target = "${REPO_ROOT}";
    };
    mountHostNixStore = true;
    writableStore = true;
    writableStoreUseTmpfs = false;
  };

  environment.etc."hostenv-demo-age-key.txt".text = ''
${AGE_PRIVATE_KEY}
'';
  environment.etc."gitconfig".text = ''
[safe]
	directory = *
'';
  sops.age.keyFile = "/etc/hostenv-demo-age-key.txt";

  # Allow root login with SSH key for demo
  users.users.root.openssh.authorizedKeys.keys = [
    "${SSH_PUBLIC_KEY}"
  ];

  # Allow passwordless login for demo (not for production!)
  users.allowNoPasswordLogin = true;
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
  git -C "$dir" config receive.denyCurrentBranch updateInstead
  git -C "$dir" remote add origin "file://$dir"
  git -C "$dir" config branch.main.remote origin
  git -C "$dir" config branch.main.merge refs/heads/main
}

start_vm() {
  local node="$1"
  local vm_out="$PROVIDER_DIR/vm-$node"
  local vm_runner
  local wait_rc=0

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
  wait_for_ssh "${node}.${HOSTENV_HOSTNAME}" root 420 "$vm_pid" || wait_rc="$?"
  case "$wait_rc" in
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
  if ! initdb -D "$pgdata" -A trust --locale=C --encoding=UTF8 >"$base/initdb.log" 2>&1; then
    cat "$base/initdb.log" >&2 || true
    exit 1
  fi
fi

pg_running=0
if pg_ctl -D "$pgdata" status >/dev/null 2>&1; then
  pg_running=1
elif [ -f "$pgdata/postmaster.pid" ]; then
  pid="$(sed -n '1p' "$pgdata/postmaster.pid" 2>/dev/null || true)"
  if [ -n "$pid" ] && [ "$pid" -eq "$pid" ] 2>/dev/null && kill -0 "$pid" >/dev/null 2>&1; then
    pg_running=1
  else
    rm -f "$pgdata/postmaster.pid"
  fi
fi

if [ "$pg_running" -ne 1 ]; then
  if ! pg_ctl -D "$pgdata" -o "-k $base -c listen_addresses=''" -l "$base/postgres.log" start >/dev/null; then
    cat "$base/postgres.log" >&2 || true
    exit 1
  fi
fi
cleanup() {
  if [ -n "${service_pid:-}" ]; then
    kill "$service_pid" >/dev/null 2>&1 || true
    wait "$service_pid" >/dev/null 2>&1 || true
  fi
  if [ -n "${socat_pid:-}" ]; then
    kill "$socat_pid" >/dev/null 2>&1 || true
  fi
  pg_ctl -D "$pgdata" stop >/dev/null || true
}
trap cleanup EXIT INT TERM

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
  "dataDir": "$data_dir",
  "flakeRoot": ".",
  "listenSocket": "$listen_socket",
  "webhookSecretFile": null,
  "webhookSecretsDir": null,
  "webhookHost": "$webhook_host",
  "uiBasePath": "/dashboard",
  "uiBaseUrl": "$ui_base_url",
  "dbUri": "$db_uri",
  "gitlab": {
    "enable": false,
    "oAuthSecretsFile": "$secrets",
    "hosts": ["gitlab.com"],
    "tokenEncryptionKeyFile": "$token_key_file",
    "deployTokenTtlMinutes": $deploy_token_ttl_minutes
  },
  "jobs": {
    "retentionDays": 7,
    "cleanupIntervalMins": 60,
    "waitTimeoutMins": 15,
    "waitInterval": 5
  },
  "comin": {
    "enable": true,
    "branch": "main",
    "pollIntervalSeconds": 5,
    "nodeAuthTokensFile": "$comin_tokens_file"
  },
  "gitCredentialsFile": "$git_credentials_file",
  "gitConfigFile": "$git_config_file",
  "flakeTemplate": "$flake_template"
}
EOFCFG

if [ -n "${HOSTENV_PROVIDER_HTTP_PORT:-}" ]; then
  http_bind="${HOSTENV_PROVIDER_HTTP_BIND:-127.0.0.1}"
  socat TCP-LISTEN:"$HOSTENV_PROVIDER_HTTP_PORT",bind="$http_bind",fork,reuseaddr UNIX-CONNECT:"$listen_socket" &
  socat_pid=$!
  echo "hostenv-provider-service-dev: proxying http://$http_bind:$HOSTENV_PROVIDER_HTTP_PORT -> unix:$listen_socket" >&2
fi

service_bin="${HOSTENV_PROVIDER_SERVICE_BIN:-hostenv-provider-service}"
export HOSTENV_PROVIDER_FORCE_INITIAL_INTENTS="${HOSTENV_PROVIDER_FORCE_INITIAL_INTENTS:-1}"
"$service_bin" --config "$config_file" &
service_pid=$!
wait "$service_pid"
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
              export HOSTENV_PROVIDER_HTTP_BIND=0.0.0.0

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
  commit_provider_generated_changes

  log "Generating comin node tokens"
  (
    cd "$PROVIDER_DIR"
    nix run .#hostenv-provider -- comin-tokens
  )
  refresh_merged_provider_secrets

  load_plan_metadata || fail_stage "failed to load environment metadata from $PLAN_PATH"
  load_project_hash_from_plan || fail_stage "failed to resolve project hash from $PLAN_PATH"
  export HOSTENV_PROVIDER_FORCE_INITIAL_INTENTS=1
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
  run_provider_plan

  log "Generating comin node token for node-b"
  (
    cd "$PROVIDER_DIR"
    nix run .#hostenv-provider -- comin-tokens
  )
  refresh_merged_provider_secrets 0

  load_plan_metadata || fail_stage "failed to load environment metadata from $PLAN_PATH after switching to node-b"
  load_project_hash_from_plan || fail_stage "failed to resolve project hash from $PLAN_PATH after switching to node-b"
  NODE_B_AUTH_TOKEN="$(load_comin_node_token "node-b")" || fail_stage "failed to load node-b auth token after switching to node-b"

  log "Restarting provider-service to load node-b auth token"
  export HOSTENV_PROVIDER_FORCE_INITIAL_INTENTS=0
  stop_demo_aux_processes_for_workdir "$WORKDIR"
  PROVIDER_SERVICE_PID=""
  rm -f "$PROVIDER_SERVICE_SOCKET"
  ensure_provider_service_running

  log "Signing deploy closures for node-b"
  sign_deploy_profile "node-b" "system"
  sign_deploy_profile "node-b" "$ENV_USER"

  start_vm "node-b"
}

run_automated_seed_import() {
  stage "Automated DB seed import"

  (
    cd "$PROJECT_DIR/.hostenv"
    pv ./seed.sql.gz | gunzip -c | ssh -F "$SSH_CONFIG" "${ENV_USER}@node-a.${HOSTENV_HOSTNAME}" \
      "set -euo pipefail; db_bin=\$(command -v mariadb || true); if [ -z \"\$db_bin\" ]; then mysql_wrapper=\$(command -v mysql || true); if [ -n \"\$mysql_wrapper\" ] && [ -r \"\$mysql_wrapper\" ]; then db_bin=\$(sed -n '2s/ .*//p' \"\$mysql_wrapper\"); fi; fi; if [ -z \"\$db_bin\" ]; then db_bin=\$(command -v mysql || true); fi; if [ -z \"\$db_bin\" ]; then db_bin=/run/current-system/sw/bin/mysql; fi; \"\$db_bin\" --socket='${RUNTIME_DIR}/mysql.sock' --user='${ENV_USER}' --database=drupal"
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

  trigger_node_a_callback "$NODE_A_JOB_ID" "$NODE_A_DEPLOY_SHA"

  if ! wait_for_provider_job_completion "$NODE_A_JOB_ID" "node-a" "$NODE_A_AUTH_TOKEN" 5400 "$TASK11_NODE_A_STATUS_LOG"; then
    emit_job_failure_debug "$NODE_A_JOB_ID" "node-a" "$TASK11_NODE_A_STATUS_LOG" "task-11-node-a"
    fail_stage "node-a deployment job status did not reach success; see $TASK11_NODE_A_STATUS_LOG"
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
  trigger_migration_node_callbacks "$NODE_B_JOB_ID" "$NODE_B_DEPLOY_SHA"

  if ! wait_for_provider_job_completion "$NODE_B_JOB_ID" "node-b" "$NODE_B_AUTH_TOKEN" 5400 "$TASK11_NODE_B_STATUS_LOG"; then
    emit_job_failure_debug "$NODE_B_JOB_ID" "node-b" "$TASK11_NODE_B_STATUS_LOG" "task-11-node-b"
    fail_stage "node-b migration job status did not reach success; see $TASK11_NODE_B_STATUS_LOG"
  fi

  if ! poll_until_or_abort 1200 "Waiting for migration deploy to node-b to become reachable..." condition_node_b_migrated; then
    emit_node_b_migration_readiness_debug
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

  trigger_node_a_callback "$NODE_A_JOB_ID" "$NODE_A_DEPLOY_SHA"

  if ! wait_for_provider_job_completion "$NODE_A_JOB_ID" "node-a" "$NODE_A_AUTH_TOKEN" 1200 "$TASK11_NODE_A_STATUS_LOG"; then
    emit_job_failure_debug "$NODE_A_JOB_ID" "node-a" "$TASK11_NODE_A_STATUS_LOG" "task-11-node-a"
    fail_stage "node-a deployment job status did not reach success; see $TASK11_NODE_A_STATUS_LOG"
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
  trigger_migration_node_callbacks "$NODE_B_JOB_ID" "$NODE_B_DEPLOY_SHA"

  if ! wait_for_provider_job_completion "$NODE_B_JOB_ID" "node-b" "$NODE_B_AUTH_TOKEN" 900 "$TASK11_NODE_B_STATUS_LOG"; then
    emit_job_failure_debug "$NODE_B_JOB_ID" "node-b" "$TASK11_NODE_B_STATUS_LOG" "task-11-node-b"
    fail_stage "node-b migration job status did not reach success; see $TASK11_NODE_B_STATUS_LOG"
  fi

  if ! poll_until_or_abort 600 "Waiting for migrated site on node-b..." condition_node_b_migrated; then
    emit_node_b_migration_readiness_debug
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
PROVIDER_HTTP_PORT=18080
NODE_A_HOST_IP="127.0.0.2"
NODE_B_HOST_IP="127.0.0.3"
NODE_SSH_PORT=2222
NODE_HTTP_PORT=8080

WORKDIR=""
AUTO_CLEANUP=0
KEEP_FAILED_WORKDIR=0
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
    --keep-failed-workdir)
      KEEP_FAILED_WORKDIR=1
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

if [[ "${HOSTENV_DEMO_KEEP_FAILED_WORKDIR:-0}" == "1" ]]; then
  KEEP_FAILED_WORKDIR=1
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

stop_stale_demo_processes

choose_free_port "$NODE_SSH_PORT"
NODE_SSH_PORT="$CHOSEN_PORT"
choose_free_port "$NODE_HTTP_PORT"
NODE_HTTP_PORT="$CHOSEN_PORT"
choose_free_port "$PROVIDER_HTTP_PORT"
PROVIDER_HTTP_PORT="$CHOSEN_PORT"

log "Using forwarded endpoints: node-a=${NODE_A_HOST_IP} ssh:${NODE_SSH_PORT} http:${NODE_HTTP_PORT}, node-b=${NODE_B_HOST_IP} ssh:${NODE_SSH_PORT} http:${NODE_HTTP_PORT}, provider-api=0.0.0.0:${PROVIDER_HTTP_PORT}"

if [[ -z "$WORKDIR" ]]; then
  WORKDIR="$(mktemp -d /tmp/hostenv-local-vm-demo-XXXXXX)"
else
  mkdir -p "$WORKDIR"
fi
WORKDIR="$(cd -- "$WORKDIR" && pwd)"

PROVIDER_DIR="$WORKDIR/provider"
PROJECT_DIR="$WORKDIR/demo-project"
SHARED_DIR="$WORKDIR/shared"
HOSTENV_SOURCE_DIR="$WORKDIR/hostenv-source"
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
TASK12_MIGRATION_SOURCE_PLAN="$PROVIDER_DIR/generated/task-12-migration-source-plan.json"

printf '%s\n' "hostenv-local-vm-demo" > "$WORKDIR/.hostenv-local-vm-demo"

VM_PIDS=()
ENV_USER=""
RUNTIME_DIR=""
ENV_HOST=""
VHOST=""
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
cleanup() {
  local code=$?
  trap - EXIT INT TERM

  for pid in "${VM_PIDS[@]:-}"; do
    kill_pid_if_running "$pid"
  done

  stop_demo_vms_for_workdir "$WORKDIR" || true
  stop_demo_aux_processes_for_workdir "$WORKDIR" || true

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

  if [[ "$KEEP_FAILED_WORKDIR" -eq 1 && "$DEMO_FAILED" -eq 1 && "$ABORTED" -eq 0 ]]; then
    log "Keeping failed workdir at $WORKDIR (debug mode)"
  elif [[ "$AUTO_CLEANUP" -eq 1 || "$ABORTED" -eq 1 || "$DEMO_FAILED" -eq 1 || "$code" -ne 0 ]]; then
    rm -rf "$WORKDIR"
    log "Removed workdir $WORKDIR"
  else
    log "Workdir kept at $WORKDIR"
  fi

  exit "$code"
}
trap cleanup EXIT
trap abort_demo INT TERM

rm -rf "$PROVIDER_DIR" "$PROJECT_DIR" "$SHARED_DIR" "$HOSTENV_SOURCE_DIR" "$SSH_DIR" "$LOG_DIR" "$PIDS_DIR" "$WRAPPER_DIR"
mkdir -p "$PROVIDER_DIR" "$PROJECT_DIR" "$SHARED_DIR" "$HOSTENV_SOURCE_DIR" "$SSH_DIR" "$LOG_DIR" "$PIDS_DIR" "$WRAPPER_DIR"
chmod 700 "$SSH_DIR"
mkdir -p "$SHARED_DIR/backups"
chmod 0777 "$SHARED_DIR" "$SHARED_DIR/backups"

remove_hostctl_profile "$HOSTCTL_PROFILE"

log "Copying demo fixture and provider template"
cp -a "$DEMO_FIXTURE_DIR/." "$PROJECT_DIR/"
cp -a "$PROVIDER_TEMPLATE_DIR/." "$PROVIDER_DIR/"
cp -a "$REPO_ROOT/." "$HOSTENV_SOURCE_DIR/"
rm -rf "$HOSTENV_SOURCE_DIR/.git" "$HOSTENV_SOURCE_DIR/.direnv"
rm -rf "$HOSTENV_SOURCE_DIR"/result "$HOSTENV_SOURCE_DIR"/result-*
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
escaped_hostenv_path="$(escape_sed "path:${HOSTENV_SOURCE_DIR}")"
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
