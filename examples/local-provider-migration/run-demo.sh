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
  echo "ERROR: $*" >&2
  exit 1
}

fail_stage() {
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
  [[ "${#HOSTCTL_CMD[@]}" -gt 0 ]] || fail "hostctl command not configured"
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
  local start
  start="$(date +%s)"

  while true; do
    if ssh -F "$SSH_CONFIG" -o ConnectTimeout=2 "${user}@${host_alias}" true >/dev/null 2>&1; then
      return 0
    fi
    if (( "$(date +%s)" - start > timeout )); then
      return 1
    fi
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

  profile_path="$(nix build --no-link --print-out-paths "$PROVIDER_DIR/generated/.#deploy.nodes.${node}.profiles.${profile}.path")"
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

condition_node_a_deployed() {
  load_plan_metadata || return 1

  ssh -F "$SSH_CONFIG" -o ConnectTimeout=2 "${ENV_USER}@node-a.${HOSTENV_HOSTNAME}" true >/dev/null 2>&1 || return 1
  ssh -F "$SSH_CONFIG" "${ENV_USER}@node-a.${HOSTENV_HOSTNAME}" "test -S '${RUNTIME_DIR}/mysql.sock'" >/dev/null 2>&1 || return 1

  local code
  code="$(http_status "$VHOST" "http://${NODE_A_HOST_IP}:${NODE_HTTP_PORT}/")"
  [[ "$code" != "000" ]]
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

write_provider_flake() {
  local production_node="$1"
  cat > "$PROVIDER_DIR/flake.nix" <<EOF_FLAKE
{
  description = "Hostenv local VM migration demo provider";

  inputs = {
    hostenv = {
      url = "path:${REPO_ROOT}";
    };

    nixpkgs.follows = "hostenv/nixpkgs";
    flake-parts.follows = "hostenv/flake-parts";
    deploy-rs.follows = "hostenv/deploy-rs";
    sops-nix.follows = "hostenv/sops-nix";
    phps.follows = "hostenv/phps";

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
        deployPublicKeys = [ "${SSH_PUBLIC_KEY}" ];
        nodeSystems = {
          default = "x86_64-linux";
          node-a = "x86_64-linux";
          node-b = "x86_64-linux";
        };
        nodeAddresses = {
          node-a = "node-a.${HOSTENV_HOSTNAME}";
          node-b = "node-b.${HOSTENV_HOSTNAME}";
        };
        nodeSshPorts = {
          node-a = ${NODE_SSH_PORT};
          node-b = ${NODE_SSH_PORT};
        };
        nodeSshOpts = {
          node-a = [
            "-o" "StrictHostKeyChecking=no"
            "-o" "UserKnownHostsFile=/dev/null"
            "-o" "IdentitiesOnly=yes"
            "-i" "${SSH_KEY}"
          ];
          node-b = [
            "-o" "StrictHostKeyChecking=no"
            "-o" "UserKnownHostsFile=/dev/null"
            "-o" "IdentitiesOnly=yes"
            "-i" "${SSH_KEY}"
          ];
        };
        nodeMagicRollback = {
          node-a = false;
          node-b = false;
        };
        nodeAutoRollback = {
          node-a = false;
          node-b = false;
        };
        nodeFor = {
          default = "${production_node}";
          production = "${production_node}";
          testing = "${production_node}";
          development = "${production_node}";
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
  wait_for_ssh "${node}.${HOSTENV_HOSTNAME}" deploy 420 || fail_stage "timed out waiting for ${node} SSH; see $LOG_DIR/${node}.log"
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
  \"gitCredentialsFile\": \"$git_credentials_file\",
  \"gitConfigFile\": \"$git_config_file\",
  \"flakeTemplate\": \"$flake_template\"
}
EOFCFG

if [ -n "${HOSTENV_PROVIDER_HTTP_PORT:-}" ]; then
  socat TCP-LISTEN:"$HOSTENV_PROVIDER_HTTP_PORT",fork,reuseaddr UNIX-CONNECT:"$listen_socket" &
  socat_pid=$!
  echo "hostenv-provider-service-dev: proxying http://localhost:$HOSTENV_PROVIDER_HTTP_PORT -> unix:$listen_socket" >&2
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

  load_plan_metadata || fail_stage "failed to load environment metadata from $PLAN_PATH"
  sync_hostctl_profile "node-a"

  log "Signing deploy closures for node-a"
  sign_deploy_profile "node-a" "system"
  sign_deploy_profile "node-a" "$ENV_USER"

  start_vm "node-a"
}

prepare_node_b_baseline() {
  stage "Preparing node-b"
  log "Switching provider placement to node-b"
  write_provider_flake "node-b"

  log "Generating provider plan for node-b"
  run_provider_plan

  load_plan_metadata || fail_stage "failed to load environment metadata from $PLAN_PATH after switching to node-b"

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
The demo has prepared a local provider workspace and started VM node-a.
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
  cd ${PROVIDER_DIR}
  nix run .#hostenv-provider -- plan
  nix run .#hostenv-provider -- dns-gate
  nix run .#hostenv-provider -- deploy --node node-a

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
  cd ${PROVIDER_DIR}
  nix run .#hostenv-provider -- plan
  nix run .#hostenv-provider -- dns-gate
  nix run .#hostenv-provider -- deploy --node node-b --migration-source ${ENV_USER}=node-a

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

  show_node_a_deploy_instructions

  if ! poll_until_or_abort 5400 "Waiting for node-a deployment to become ready..." condition_node_a_deployed; then
    fail_stage "timed out waiting for node-a deployment"
  fi

  success "node-a deployment detected."
  log "View node-a installer page with: curl http://${VHOST}:${NODE_HTTP_PORT}/"
  prompt_continue_or_abort

  show_seed_instructions

  if ! poll_until_or_abort 5400 "Waiting for seed import to complete on node-a..." condition_seed_imported_node_a; then
    fail_stage "timed out waiting for Drupal seed import on node-a"
  fi

  success "Drupal seed import detected on node-a."
  log "View node-a site with: curl http://${VHOST}:${NODE_HTTP_PORT}/"

  prompt_continue_or_abort
  prepare_node_b_baseline

  show_node_b_migration_instructions

  if ! poll_until_or_abort 5400 "Waiting for migration deploy to node-b..." condition_node_b_migrated; then
    fail_stage "timed out waiting for migrated site on node-b"
  fi

  sync_hostctl_profile "node-b"
  success "Migration to node-b detected."
  assert_not_installer_page "$VHOST" "http://${NODE_B_HOST_IP}:${NODE_HTTP_PORT}/"
  prompt_continue_or_abort

  stage "Demo completed"
  log "View node-b site with: curl http://${VHOST}:${NODE_HTTP_PORT}/"
  log "Provider workspace: $PROVIDER_DIR"
  log "VM logs: $LOG_DIR"
}

run_automated_flow() {
  stage "Automated deploy: node-a"
  (
    cd "$PROVIDER_DIR"
    nix run .#hostenv-provider -- deploy --node node-a
  )

  if ! poll_until_or_abort 1800 "Waiting for node-a deployment to become ready..." condition_node_a_deployed; then
    fail_stage "node-a deployment did not become ready"
  fi

  run_automated_seed_import

  if ! poll_until_or_abort 1800 "Waiting for seed import to complete on node-a..." condition_seed_imported_node_a; then
    fail_stage "seed import did not complete on node-a"
  fi

  prepare_node_b_baseline

  stage "Automated deploy: node-b migration"
  (
    cd "$PROVIDER_DIR"
    nix run .#hostenv-provider -- deploy --node node-b --migration-source "${ENV_USER}=node-a"
  )

  if ! poll_until_or_abort 1800 "Waiting for migrated site on node-b..." condition_node_b_migrated; then
    fail_stage "node-b migration did not become ready"
  fi

  sync_hostctl_profile "node-b"
  assert_not_installer_page "$VHOST" "http://${NODE_B_HOST_IP}:${NODE_HTTP_PORT}/"

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

printf '%s\n' "hostenv-local-vm-demo" > "$WORKDIR/.hostenv-local-vm-demo"

VM_PIDS=()
ENV_USER=""
RUNTIME_DIR=""
DATA_DIR=""
ENV_HOST=""
VHOST=""
MIGRATE_BACKUP=""

cleanup() {
  local code=$?
  trap - EXIT INT TERM

  for pid in "${VM_PIDS[@]:-}"; do
    kill_pid_if_running "$pid"
  done

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

  if [[ "$HOSTCTL_IN_USE" -eq 1 ]]; then
    remove_hostctl_profile "$HOSTCTL_PROFILE"
  fi

  if [[ "$AUTO_CLEANUP" -eq 1 || "$ABORTED" -eq 1 ]]; then
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
