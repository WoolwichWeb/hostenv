#!/usr/bin/env bash
set -euo pipefail

# =============================================================================
# Hostenv Local Migration Demo
# =============================================================================
#
# PURPOSE:
# This script demonstrates the complete Hostenv provider control-plane lifecycle
# in a disposable local environment. It's designed as a learning tool for users
# who want to understand how Hostenv works before running their own provider.
#
# WHAT IS HOSTENV?
# Hostenv is a Nix-based PaaS (Platform as a Service) that enables declarative,
# reproducible application deployments. Your application configuration lives
# in your project's git repository, and Hostenv translates that configuration
# into running infrastructure.
#
# THE PROVIDER MODEL:
# A Hostenv provider:
# 1. Reads your project's hostenv.nix configuration
# 2. Generates a deployment plan (plan.json) describing what should run where
# 3. Accepts webhook calls when you push changes
# 4. Orchestrates deployments across nodes using hostenv-deploy-agent (the node agent)
#
# DEMO OVERVIEW:
# This demo creates TWO local VMs (node-a and node-b) and demonstrates:
# - Phase 1: Provider setup and initial deployment to node-a
# - Phase 2: Webhook-driven deployment through provider-service
# - Phase 3: Data seeding to create migration state
# - Phase 4: Backup/restore migration from node-a to node-b
# - Phase 5: Verification and exploration
#
# WHY LOCAL VMS?
# Using QEMU VMs on loopback IPs lets you experience the full provider workflow
# without needing cloud infrastructure. Each VM runs NixOS and acts like a real
# Hostenv node would in production.
#
# ARCHITECTURE FLOW:
# 1. Projects have a .hostenv/hostenv.nix, which defines environments
# 2. The provider generates plan.json from that configuration
# 3. provider-service webhooks create deploy jobs
# 4. Each node runs provider-deploy, which polls provider-service for actions
# 5. Nodes authenticate using tokens stored in provider secrets
# 6. Deployments are atomic and can be rolled back
#
# KEY CONCEPTS:
# - plan.json: The "compiled" deployment configuration
# - provider-service: The control plane API that receives webhooks
# - provider-deploy: The node-side agent that executes deployments
# - node tokens: Bearer credentials nodes use to authenticate with provider-service
# - signing: Nix closures are signed so nodes trust artifacts from the provider
# - environments: Logical deployment targets (production, staging, etc.)
# - nodes: Physical or virtual machines that run your workloads
#
# PREREQUISITES:
# - Linux with KVM support
# - Nix with flakes enabled
#
# USAGE:
#   ./run-demo.sh                    # Interactive mode with exploration pauses
#   ./run-demo.sh --automated        # Non-interactive mode
#   ./run-demo.sh --automated --cleanup  # Clean up on success
#
# =============================================================================

# -----------------------------------------------------------------------------
# PATH SETUP
# -----------------------------------------------------------------------------
# We need to know where to find templates and the hostenv repo.
# SCRIPT_DIR: Where this script lives
# REPO_ROOT: The hostenv repository root (for copying source files into our demo provider)
# CALLER_DIR: Where the user ran the script from (for creating convenience symlink)

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd -- "$SCRIPT_DIR/../.." && pwd)"
CALLER_DIR="$(pwd)"

# -----------------------------------------------------------------------------
# TEMPLATE PATHS
# -----------------------------------------------------------------------------
# Templates are the "cookie cutter" files we use to create the demo environment.
# - PROVIDER_TEMPLATE_DIR: Base provider flake structure
# - DEMO_FIXTURE_DIR: Sample Drupal project with hostenv.nix
# - SEED_SOURCE: SQL dump to seed the database
# - *_TMPL: Template files with placeholders we'll substitute

PROVIDER_TEMPLATE_DIR="$REPO_ROOT/template/provider"
DEMO_FIXTURE_DIR="$SCRIPT_DIR/demo-project"
SEED_SOURCE="$SCRIPT_DIR/seed/seed.sql.gz"
PROVIDER_FLAKE_TEMPLATE="$SCRIPT_DIR/provider-flake.nix.tmpl"
NODE_CONFIG_TEMPLATE="$SCRIPT_DIR/node-configuration.nix.tmpl"

# -----------------------------------------------------------------------------
# NETWORK CONFIGURATION
# -----------------------------------------------------------------------------
# We use loopback addresses to simulate a multi-node cluster on one machine.
# 
# WHY LOOPBACK IPs?
# Normally, each Hostenv node would have its own IP address on a network. For
# this demo, we use 127.0.0.2 and 127.0.0.3 (distinct from localhost 127.0.0.1)
# so each "node" can have its own SSH and HTTP ports without conflicts.
#
# HOSTENV_HOSTNAME:
# The base domain for all demo services. Nodes will be node-a.demo.hostenv.test
# and node-b.demo.hostenv.test. This simulates how real Hostenv environments
# use subdomains for node addressing.
#
# PROVIDER_API_VM_GATEWAY (10.0.2.2):
# When QEMU runs VMs with user-mode networking (the default), the host is
# reachable at 10.0.2.2 from inside the VM. This lets the VMs contact the
# provider-service running on the host.

HOSTENV_HOSTNAME="demo.hostenv.test"
NODE_A_HOST_IP="127.0.0.2"  # Distinct loopback for node-a
NODE_B_HOST_IP="127.0.0.3"  # Distinct loopback for node-b
NODE_SSH_PORT=2222          # SSH access to VMs (one port, different IPs)
NODE_HTTP_PORT=8080         # HTTP access to deployed apps
PROVIDER_HTTP_PORT=18080    # Provider dashboard HTTP port
PROVIDER_API_VM_GATEWAY="10.0.2.2"  # QEMU user-mode networking gateway

# -----------------------------------------------------------------------------
# RUNTIME FLAGS
# -----------------------------------------------------------------------------
# These control demo behavior:
# - AUTOMATED=1: Skip interactive gum prompts (for CI)
# - CLEANUP=1: Remove workdir on successful completion
# - DEMO_FAILED=1: Set by fail() so cleanup knows to preserve logs

AUTOMATED=0
CLEANUP=0
DEMO_FAILED=0

# -----------------------------------------------------------------------------
# WORKSPACE VARIABLES (populated in main())
# -----------------------------------------------------------------------------
# These will point to directories inside our temporary workdir.
# We use a temp directory so the demo is completely disposable.

WORKDIR=""           # Root temp directory
PROVIDER_DIR=""      # The provider workspace (like a real provider repo)
PROJECT_DIR=""       # The project being deployed (Drupal in this demo)
HOSTENV_SOURCE_DIR=""  # Copy of hostenv repo (for flakes to reference)
SSH_DIR=""           # SSH keys for VM access
SSH_KEY=""           # Path to private key
SSH_CONFIG=""        # SSH client configuration
SHARED_DIR=""        # Shared mount between host and VMs
LOG_DIR=""           # Where we collect all logs
PIDS_DIR=""          # PID files for process tracking

# -----------------------------------------------------------------------------
# DEPLOYMENT METADATA (loaded from plan.json)
# -----------------------------------------------------------------------------
# plan.json is the "contract" between your project config and the provider.
# It contains:
# - Which environments exist
# - Runtime directories
# - Virtual host configurations
# - Project hash (for webhook routing)

PLAN_PATH=""         # Path to generated/plan.json
ENV_USER=""          # Unix user the app runs as
RUNTIME_DIR=""       # Where runtime files (sockets, etc.) live
ENV_HOST=""          # The hostname for this environment
VHOST=""             # Virtual host (subdomain) for the app
PROJECT_HASH=""      # Unique hash for webhook routing

# -----------------------------------------------------------------------------
# PROVIDER SERVICE STATE
# -----------------------------------------------------------------------------
# provider-service is the control plane that receives webhooks and coordinates
# deployments. In production, this would run on a server. Here we run it locally.

PROVIDER_SERVICE_RUNTIME_DIR=""  # Where postgres and socket live
PROVIDER_SERVICE_SOCKET=""       # Unix socket path
PROVIDER_SERVICE_PID=""          # Process ID for cleanup
PROVIDER_SERVICE_BIN=""          # Path to binary
FORCE_INITIAL_INTENTS=1          # Create deploy intents on first run

# -----------------------------------------------------------------------------
# NODE AUTHENTICATION
# -----------------------------------------------------------------------------
# Each node needs a bearer token to authenticate with provider-service.
# These tokens are generated by the provider and stored encrypted in secrets.
# They're like API keys - nodes use them to poll for deployment actions.

NODE_A_AUTH_TOKEN=""  # Token for node-a to talk to provider-service
NODE_B_AUTH_TOKEN=""  # Token for node-b to talk to provider-service
NODE_A_JOB_ID=""      # Deploy job ID for node-a
NODE_B_JOB_ID=""      # Deploy job ID for node-b

# -----------------------------------------------------------------------------
# VM PROCESS TRACKING
# -----------------------------------------------------------------------------
# We keep track of QEMU PIDs so we can clean them up on exit.

VM_PIDS=()

# =============================================================================
# USER INTERFACE FUNCTIONS
# =============================================================================

usage() {
  cat <<'USAGE'
Usage: run-demo.sh [--automated] [--cleanup]

Modes:
  default       Interactive mode with gum pauses at interesting moments
  --automated   Fully non-interactive demo run

Options:
  --cleanup     Remove workdir at successful completion
  --help, -h    Show this help text
USAGE
}

# log: Timestamped output for debugging
log() { printf '[%s] %s\n' "$(date +%H:%M:%S)" "$*"; }

# info/warn/success: Styled output using gum
info() { gum style --foreground 212 "$*"; }
warn() { gum style --foreground 208 "$*"; }
success() { gum style --foreground 82 --bold "$*"; }

# fail: Fatal error handler
# Sets DEMO_FAILED so cleanup knows to preserve the workdir for debugging
fail() {
  DEMO_FAILED=1
  gum style --foreground 196 --bold "ERROR: $*" >&2
  exit 1
}

# stage: Visual separator for demo phases
stage() {
  gum style --bold --foreground 63 --border rounded --border-foreground 240 --padding "0 1" "== $* =="
}

# -----------------------------------------------------------------------------
# pause_for_exploration: Interactive learning checkpoints
# -----------------------------------------------------------------------------
# In interactive mode, we pause at key moments so users can:
# - View the provider dashboard
# - Check VM status
# - Understand what's happening before proceeding
#
# WHY THIS MATTERS:
# Hostenv deployments are asynchronous. The pause lets users observe the
# provider-service UI and see jobs moving through states (pending -> running ->
# success) before we proceed to the next phase.

pause_for_exploration() {
  local message="$1"
  shift

  if [[ "$AUTOMATED" -eq 1 ]]; then
    return 0
  fi

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
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

# require_cmd: Verify dependencies are available
# The script assumes a Nix environment where all tools are pre-installed.
require_cmd() {
  local cmd="$1"
  command -v "$cmd" >/dev/null 2>&1 || fail "Missing '$cmd'. Run from the repo Nix environment (for example: nix develop)."
}

# -----------------------------------------------------------------------------
# Port Management
# -----------------------------------------------------------------------------
# We need to find free ports to avoid conflicts with other services.
# This is especially important if you run the demo multiple times or have
# other services using the default ports.

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

# escape_sed_replacement: Make strings safe for sed substitution
escape_sed_replacement() {
  printf '%s' "$1" | sed -e 's/[|&\\]/\\&/g'
}

# =============================================================================
# PROCESS MANAGEMENT
# =============================================================================

# kill_pid_if_running: Graceful process termination with fallback
# Tries SIGTERM first, waits 5 seconds, then SIGKILL if needed.
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

# -----------------------------------------------------------------------------
# kill_demo_processes_for_workdir: Scavenger hunt for orphaned processes
# -----------------------------------------------------------------------------
# Sometimes processes get detached from our PID tracking (especially QEMU VMs).
# This function scans the process list for anything tied to our workdir and
# kills it. This is important for --cleanup to actually clean up everything.

kill_demo_processes_for_workdir() {
  local dir="$1"
  local pid
  local cmd
  local line

  command -v ps >/dev/null 2>&1 || return 0

  for _ in 1 2; do
    while IFS= read -r line; do
      [[ "$line" =~ ^[[:space:]]*([0-9]+)[[:space:]]+(.*)$ ]] || continue
      pid="${BASH_REMATCH[1]}"
      cmd="${BASH_REMATCH[2]}"
      [[ "$cmd" == *"$dir"* ]] || continue

      case "$cmd" in
        *qemu-system*|*hostenv-provider-service*|*postgres*|*pg_ctl*|*socat*)
          kill_pid_if_running "$pid"
          ;;
      esac
    done < <(ps -eo pid=,args= 2>/dev/null || true)
    sleep 1
  done
}

# -----------------------------------------------------------------------------
# cleanup: The EXIT trap handler
# -----------------------------------------------------------------------------
# This runs when the script exits (success or failure), on interrupt, or on
# terminate signal. It ensures we don't leave VMs running or temp files behind.
#
# BEHAVIOR:
# - If demo failed: Keep workdir for debugging
# - If --cleanup and success: Remove workdir
# - If no --cleanup: Keep workdir for exploration
#
# WHY THIS MATTERS:
# QEMU VMs consume significant resources. This ensures demos don't leak VMs
# if the user hits Ctrl-C or an error occurs.

cleanup() {
  local code=$?
  trap - EXIT INT TERM

  # Kill tracked VM processes
  for pid in "${VM_PIDS[@]:-}"; do
    kill_pid_if_running "$pid"
  done

  # Kill any processes recorded in pidfiles
  if [[ -d "$PIDS_DIR" ]]; then
    for pidfile in "$PIDS_DIR"/*.pid; do
      [[ -e "$pidfile" ]] || continue
      kill_pid_if_running "$(cat "$pidfile" 2>/dev/null || true)"
    done
  fi

  # Kill provider service and scan for orphaned demo processes
  kill_pid_if_running "$PROVIDER_SERVICE_PID"
  kill_demo_processes_for_workdir "$WORKDIR"

  # Remove the convenience symlink
  if [[ -L "$CALLER_DIR/hostenv-demo" ]] && [[ "$(readlink -f "$CALLER_DIR/hostenv-demo" || true)" == "$WORKDIR" ]]; then
    rm -f "$CALLER_DIR/hostenv-demo"
  fi

  # Decide whether to keep or remove the workdir
  if [[ "$code" -ne 0 || "$DEMO_FAILED" -eq 1 ]]; then
    log "Workdir kept for debugging: $WORKDIR"
  elif [[ "$CLEANUP" -eq 1 ]]; then
    rm -rf "$WORKDIR"
    log "Removed workdir: $WORKDIR"
  else
    log "Workdir kept: $WORKDIR"
  fi

  exit "$code"
}

# =============================================================================
# HOSTENV DATA LOADING
# =============================================================================

# -----------------------------------------------------------------------------
# load_plan_metadata: Extract deployment configuration from plan.json
# -----------------------------------------------------------------------------
# plan.json is the "compiled" output of your hostenv.nix configuration. It tells
# the provider (and this demo) everything needed to deploy:
# - Which user runs the application
# - Where runtime files live
# - What hostname and virtual hosts to use
#
# In production, the provider generates this file from your project's
# hostenv.nix. Here we load it to extract the values we need.

load_plan_metadata() {
  local env
  env="$(jq -r '.environments | keys[0] // empty' "$PLAN_PATH")"
  [[ -n "$env" ]] || fail "Could not load environment key from $PLAN_PATH"

  ENV_USER="$env"
  RUNTIME_DIR="$(jq -r --arg env "$env" '.environments[$env].hostenv.runtimeDir // empty' "$PLAN_PATH")"
  ENV_HOST="$(jq -r --arg env "$env" '.environments[$env].hostenv.hostname // empty' "$PLAN_PATH")"
  VHOST="$(jq -r --arg env "$env" '.environments[$env].virtualHosts | keys[0] // empty' "$PLAN_PATH")"

  [[ -n "$RUNTIME_DIR" && -n "$ENV_HOST" && -n "$VHOST" ]] || fail "Plan metadata is incomplete in $PLAN_PATH"
}

# load_project_hash_from_plan: Get the unique project identifier
# The project hash is used in webhook URLs to route deploy requests.
load_project_hash_from_plan() {
  PROJECT_HASH="$(jq -r 'first((.environments // {} | to_entries[]? | .value.hostenv.projectNameHash | if type == "array" then .[] else . end) // empty)' "$PLAN_PATH")"
  [[ -n "$PROJECT_HASH" ]] || fail "Could not read projectNameHash from $PLAN_PATH"
}

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Each node needs a bearer token to authenticate with provider-service. Think of
# this like an API key. The node includes this token in HTTP headers when polling
# for deployment actions.
#
# Tokens are generated by the provider CLI and stored encrypted in secrets.yaml.
# We use sops (Secrets OPerationS) to decrypt them.

load_provider_node_token() {
  local node="$1"
  sops -d --output-type json "$PROVIDER_DIR/secrets/provider.yaml" | jq -r --arg node "$node" '.provider_node_tokens[$node] // empty'
}

# -----------------------------------------------------------------------------
# refresh_merged_provider_secrets: Rebuild secrets after token updates
# -----------------------------------------------------------------------------
# The provider maintains two forms of secrets:
# 1. provider.yaml - The encrypted source of truth (human-editable)
# 2. secrets.merged.yaml - A processed version (machine-readable)
#
# When we generate new node tokens, we need to regenerate the merged version
# so the provider templates can reference the updated values.

refresh_merged_provider_secrets() {
  local provider_secrets="$PROVIDER_DIR/secrets/provider.yaml"
  local merged_secrets="$PROVIDER_DIR/generated/secrets.merged.yaml"
  local tmp_json
  local decrypted_json
  local token_map_yaml

  decrypted_json="$(sops -d --output-type json "$provider_secrets")"
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

  tmp_json="$(mktemp "$WORKDIR/provider-secrets-merged-XXXXXX.json")"
  jq --arg provider_node_tokens_yaml "$token_map_yaml" '.provider_node_tokens_yaml = $provider_node_tokens_yaml' <<<"$decrypted_json" > "$tmp_json"
  SOPS_AGE_RECIPIENTS="$AGE_PUBLIC_KEY" sops --encrypt --config /dev/null --input-type json --output-type yaml "$tmp_json" > "$merged_secrets"
  rm -f "$tmp_json"
}

# =============================================================================
# WAIT HELPERS
# =============================================================================

# wait_for_ssh: Wait for a VM to accept SSH connections
# VMs take time to boot. We poll SSH until it's ready or timeout.
wait_for_ssh() {
  local host_alias="$1"
  local timeout="${2:-420}"
  local start
  start="$(date +%s)"

  while true; do
    if ssh -F "$SSH_CONFIG" -o ConnectTimeout=2 "root@${host_alias}" true >/dev/null 2>&1; then
      return 0
    fi
    if (( "$(date +%s)" - start > timeout )); then
      return 1
    fi
    sleep 2
  done
}

# wait_for_unix_socket: Wait for a service to create its socket
# provider-service creates a Unix socket when it's ready to accept requests.
wait_for_unix_socket() {
  local socket_path="$1"
  local timeout="${2:-120}"
  local start
  start="$(date +%s)"

  while true; do
    [[ -S "$socket_path" ]] && return 0
    if (( "$(date +%s)" - start > timeout )); then
      return 1
    fi
    sleep 1
  done
}

# http_status: Check HTTP response code for a URL
# Used to verify the deployed application is responding.
http_status() {
  local host_header="$1"
  local url="$2"
  curl --connect-timeout 2 --max-time 4 -sS -o /dev/null -w '%{http_code}' -H "Host: ${host_header}" "$url" 2>/dev/null || true
}

# =============================================================================
# VM MANAGEMENT
# =============================================================================

# find_vm_runner: Locate the QEMU launch script in nix build output
# When you build a NixOS VM, it creates a run-*-vm script. We find it.
find_vm_runner() {
  local vm_out="$1"
  local runner
  runner="$(find -L "$vm_out" -type f -name 'run-*-vm' | head -n1 || true)"
  [[ -n "$runner" ]] || fail "Could not locate VM runner under $vm_out"
  printf '%s\n' "$runner"
}

# init_git_repo: Initialize a git repo with demo-friendly settings
# Hostenv is git-centric. We set up repos with appropriate config.
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

# run_provider_plan: Generate the deployment plan
# This runs `hostenv-provider plan` which reads your hostenv.nix and writes
# generated/plan.json describing what should be deployed.
run_provider_plan() {
  (cd "$PROVIDER_DIR" && nix run .#hostenv-provider -- plan && nix run .#hostenv-provider -- dns-gate)
}

# commit_generated: Commit plan artifacts to git
# Hostenv tracks plan.json in git for reproducibility.
commit_generated() {
  git -C "$PROVIDER_DIR" add generated >/dev/null 2>&1 || true
  if ! git -C "$PROVIDER_DIR" diff --cached --quiet; then
    git -C "$PROVIDER_DIR" commit -m "Update generated provider artifacts" >/dev/null
  fi
}

# commit_provider_sources: Commit provider config changes
# When we switch node placement, we need to commit the updated flake.nix.
commit_provider_sources() {
  git -C "$PROVIDER_DIR" add flake.nix nodes >/dev/null 2>&1 || true
  if ! git -C "$PROVIDER_DIR" diff --cached --quiet; then
    git -C "$PROVIDER_DIR" commit -m "Update provider node definitions" >/dev/null
  fi
}

# =============================================================================
# SECURITY: NIX SIGNING
# =============================================================================

# -----------------------------------------------------------------------------
# sign_deploy_profile: Sign Nix closures for VM trust
# -----------------------------------------------------------------------------
# In Nix, "closures" are the complete dependency trees needed to run software.
# When a provider builds deploy artifacts, those closures live in the provider's
# Nix store. For VMs to use them, the VMs must trust the provider's signatures.
#
# WHY THIS MATTERS:
# Without signing, VMs would reject closures from the provider as untrusted.
# This simulates production where providers have signing keys that nodes trust.
#
# We sign two profiles:
# - system: The NixOS system closure (kernel, init, services)
# - env-${user}: The application environment closure (Drupal, PHP, etc.)

sign_deploy_profile() {
  local node="$1"
  local profile="$2"
  local path

  if [[ "$profile" == "system" ]]; then
    path="$(nix build --no-link --print-out-paths "$PROVIDER_DIR/generated/.#nixosConfigurations.${node}.config.system.build.toplevel")"
  else
    path="$(nix build --no-link --print-out-paths "$PROVIDER_DIR/generated/.#packages.x86_64-linux.env-${profile}")"
  fi

  nix store sign --key-file "$NIX_SIGNING_KEY_FILE" --recursive "$path" >/dev/null
}

# =============================================================================
# PROVIDER SERVICE
# =============================================================================

# resolve_provider_service_bin: Build and locate the provider service binary
# We build it from the hostenv repo so we're always using the latest code.
resolve_provider_service_bin() {
  local system
  local out
  system="$(nix eval --impure --raw --expr 'builtins.currentSystem')"
  out="$(nix build --option allow-import-from-derivation true --no-link --print-out-paths "$REPO_ROOT#checks.${system}.hostenv-provider-service-build")"
  PROVIDER_SERVICE_BIN="$out/bin/hostenv-provider-service"
  [[ -x "$PROVIDER_SERVICE_BIN" ]] || fail "hostenv-provider-service binary missing at $PROVIDER_SERVICE_BIN"
}

# -----------------------------------------------------------------------------
# ensure_provider_service_running: Start the control plane
# -----------------------------------------------------------------------------
# provider-service is the brain of the operation. It:
# 1. Runs a PostgreSQL database for state
# 2. Exposes a Unix socket API (we bridge to HTTP with socat)
# 3. Accepts webhooks to create deploy jobs
# 4. Serves per-node action APIs that hostenv-deploy-agent polls
#
# CONFIGURATION:
# - dataDir: Where the provider keeps its files
# - listenSocket: Unix socket path
# - webhookHost: Hostname expected in webhooks
# - uiBaseUrl: Where the dashboard should redirect for node HTTP
# - dbUri: PostgreSQL connection string
# - deploy: Configuration for the node agent integration

ensure_provider_service_running() {
  local pgdata="$PROVIDER_SERVICE_RUNTIME_DIR/pgdata"
  local token_file="$PROVIDER_SERVICE_RUNTIME_DIR/provider-node-tokens.yaml"
  local config_file="$PROVIDER_SERVICE_RUNTIME_DIR/provider-config.json"
  resolve_provider_service_bin
  mkdir -p "$PROVIDER_SERVICE_RUNTIME_DIR"

  # Initialize PostgreSQL if needed
  if [[ ! -s "$pgdata/PG_VERSION" ]]; then
    initdb -D "$pgdata" -A trust --locale=C --encoding=UTF8 >"$LOG_DIR/initdb.log" 2>&1
  fi
  if ! pg_ctl -D "$pgdata" status >/dev/null 2>&1; then
    pg_ctl -D "$pgdata" -o "-k $PROVIDER_SERVICE_RUNTIME_DIR -c listen_addresses=''" -l "$LOG_DIR/postgres.log" start >/dev/null
  fi
  createdb -h "$PROVIDER_SERVICE_RUNTIME_DIR" hostenv-provider >/dev/null 2>&1 || true

  # Write the provider node tokens file
  {
    printf 'node-a: %s\n' "$NODE_A_AUTH_TOKEN"
    if [[ -n "$NODE_B_AUTH_TOKEN" ]]; then
      printf 'node-b: %s\n' "$NODE_B_AUTH_TOKEN"
    fi
  } > "$token_file"
  chmod 600 "$token_file"

  # Write the provider service configuration
  cat > "$config_file" <<EOF_CFG
{
  "dataDir": "$PROVIDER_DIR",
  "flakeRoot": ".",
  "listenSocket": "$PROVIDER_SERVICE_SOCKET",
  "webhookSecretFile": null,
  "webhookSecretsDir": null,
  "webhookHost": "$HOSTENV_HOSTNAME",
  "uiBasePath": "/dashboard",
  "uiBaseUrl": "http://${NODE_A_HOST_IP}:${NODE_HTTP_PORT}",
  "dbUri": "host=$PROVIDER_SERVICE_RUNTIME_DIR dbname=hostenv-provider",
  "gitCredentialsFile": "$PROVIDER_DIR/git-credentials",
  "gitConfigFile": "$PROVIDER_DIR/gitconfig",
  "flakeTemplate": "flake.template.nix",
  "deploy": {
    "enable": true,
    "nodeAuthTokensFile": "$token_file"
  }
}
EOF_CFG

  # Start socat to expose the Unix socket on TCP for VM access
  socat TCP-LISTEN:"$PROVIDER_HTTP_PORT",bind=0.0.0.0,fork,reuseaddr UNIX-CONNECT:"$PROVIDER_SERVICE_SOCKET" > "$LOG_DIR/provider-socat.log" 2>&1 &
  printf '%s\n' "$!" > "$PIDS_DIR/provider-socat.pid"

  # Start the provider service
  (
    cd "$PROVIDER_DIR"
    HOSTENV_PROVIDER_FORCE_INITIAL_INTENTS="$FORCE_INITIAL_INTENTS" "$PROVIDER_SERVICE_BIN" --config "$config_file" > "$LOG_DIR/provider-service.log" 2>&1
  ) &
  PROVIDER_SERVICE_PID="$!"

  wait_for_unix_socket "$PROVIDER_SERVICE_SOCKET" 120 || fail "Provider service socket did not appear: $PROVIDER_SERVICE_SOCKET"
}

# =============================================================================
# DEPLOYMENT ORCHESTRATION
# =============================================================================

# -----------------------------------------------------------------------------
# wait_for_job_action_op: Poll for a specific deployment action
# -----------------------------------------------------------------------------
# When provider-service creates a deploy job, it materializes node-specific
# actions (like "activate", "backup", "restore"). Nodes poll the action API
# to see what they should do.
#
# This function waits until a specific action appears for a node, which means
# the provider has decided what the node should do.

wait_for_job_action_op() {
  local job_id="$1"
  local node="$2"
  local token="$3"
  local op="$4"
  local timeout="$5"
  local start
  local code
  local body

  start="$(date +%s)"
  body="$PROVIDER_SERVICE_RUNTIME_DIR/actions-${job_id}-${node}.json"

  while true; do
    code="$(curl -sS --unix-socket "$PROVIDER_SERVICE_SOCKET" -o "$body" -w '%{http_code}' -H "Authorization: Bearer $token" "http://localhost/api/deploy-jobs/${job_id}/actions?node=${node}" || true)"
    if [[ "$code" == "200" ]] && jq -e --arg op "$op" '.actions[]? | select(.op == $op)' "$body" >/dev/null 2>&1; then
      return 0
    fi
    if (( "$(date +%s)" - start >= timeout )); then
      return 1
    fi
    sleep 2
  done
}

# -----------------------------------------------------------------------------
# wait_for_provider_job_completion: Wait for deployment success
# -----------------------------------------------------------------------------
# Deployments are asynchronous. The job goes through states:
# pending -> running -> success (or failed/timed_out)
# We poll the status API until the node reports success.

wait_for_provider_job_completion() {
  local job_id="$1"
  local node="$2"
  local token="$3"
  local timeout="$4"
  local body
  local code
  local start

  body="$PROVIDER_SERVICE_RUNTIME_DIR/status-${job_id}-${node}.json"
  start="$(date +%s)"

  while true; do
    code="$(curl -sS --unix-socket "$PROVIDER_SERVICE_SOCKET" -o "$body" -w '%{http_code}' -H "Authorization: Bearer $token" "http://localhost/api/deploy-jobs/${job_id}/statuses?node=${node}" || true)"

    if [[ "$code" == "200" ]]; then
      if jq -e '.statuses[]? | select(.status == "failed" or .status == "timed_out")' "$body" >/dev/null 2>&1; then
        return 1
      fi
      if jq -e --arg node "$node" '.statuses[]? | select(.node == $node and .phase == "intent" and .status == "success")' "$body" >/dev/null 2>&1; then
        return 0
      fi
    fi

    if (( "$(date +%s)" - start >= timeout )); then
      return 1
    fi
    sleep 2
  done
}

# resolve_intent_commit_sha_for_job: Get the deployed commit from database
# provider-service tracks which git commit was deployed in its database.
resolve_intent_commit_sha_for_job() {
  local job_id="$1"
  local sha
  sha="$(PSQLRC=/dev/null psql -h "$PROVIDER_SERVICE_RUNTIME_DIR" -d hostenv-provider -Atqc "SELECT commit_sha FROM deploy_intents WHERE job_id = '${job_id}' ORDER BY created_at DESC LIMIT 1" 2>/dev/null || true)"
  sha="$(printf '%s' "$sha" | tr -d '[:space:]')"
  [[ -n "$sha" ]] || fail "Could not resolve deploy intent SHA for job $job_id"
  printf '%s\n' "$sha"
}

# =============================================================================
# NODE DEPLOY INTEGRATION
# =============================================================================


# =============================================================================
# WEBHOOK SIMULATION
# =============================================================================

# -----------------------------------------------------------------------------
# trigger_webhook_deploy: Simulate a git push webhook
# -----------------------------------------------------------------------------
# In production, your git forge (GitLab, GitHub) sends webhooks to
# provider-service when you push. This function simulates that by POSTing
# a webhook payload directly to provider-service.
#
# PAYLOAD STRUCTURE:
# - ref: The git ref (e.g., refs/heads/main)
# - checkout_sha: The commit SHA to deploy
# - project.path_with_namespace: Project identifier
#
# ROUTING:
# The webhook URL includes PROJECT_HASH, which provider-service uses to route
# to the correct project configuration.
#
# RESPONSE:
# Returns the jobId, which we use to track deployment progress.

trigger_webhook_deploy() {
  local node="$1"
  local sha="$2"
  local body="$PROVIDER_SERVICE_RUNTIME_DIR/webhook-${node}.json"
  local payload
  local code

  payload="{\"ref\":\"refs/heads/main\",\"checkout_sha\":\"${sha}\",\"project\":{\"path_with_namespace\":\"acme/demo\"}}"
  code="$(curl -sS --unix-socket "$PROVIDER_SERVICE_SOCKET" -o "$body" -w '%{http_code}' -H 'Content-Type: application/octet-stream' --data-binary "$payload" "http://localhost/webhook/${PROJECT_HASH}" || true)"

  [[ "$code" == "202" ]] || fail "Webhook trigger failed for ${node} (HTTP $code)"
  jq -r '.jobId // empty' "$body"
}

# =============================================================================
# HEALTH CHECKS
# =============================================================================

# wait_for_site_ready: Check if the deployed app responds to HTTP
# We poll until we get a 2xx or 3xx response, indicating the app is up.
wait_for_site_ready() {
  local node_ip="$1"
  local timeout="$2"
  local start
  local code

  start="$(date +%s)"
  while true; do
    code="$(http_status "$VHOST" "http://${node_ip}:${NODE_HTTP_PORT}/")"
    if [[ "$code" =~ ^[23][0-9][0-9]$ ]]; then
      return 0
    fi
    if (( "$(date +%s)" - start >= timeout )); then
      return 1
    fi
    sleep 2
  done
}

# marker_present_on_node: Verify the database migration worked
# We inserted a marker row during seeding. This checks if it's present,
# proving the database was properly migrated from node-a to node-b.
marker_present_on_node() {
  local node_alias="$1"
  ssh -F "$SSH_CONFIG" "${ENV_USER}@${node_alias}" \
    "set -euo pipefail; db_bin=\$(command -v mariadb || command -v mysql || echo /run/current-system/sw/bin/mysql); printf '%s\n' 'SELECT note FROM hostenv_demo_marker WHERE id = 1;' | \"\$db_bin\" --batch --skip-column-names --socket='${RUNTIME_DIR}/mysql.sock' --user='${ENV_USER}' --database=drupal" \
    2>/dev/null | grep -qx 'from-node-a'
}

# =============================================================================
# MIGRATION HELPERS
# =============================================================================

# capture_migration_source_plan: Save the current plan before switching nodes
# When we migrate, we need the plan from the SOURCE node to restore on the
# DESTINATION. This saves it before we switch placement.
capture_migration_source_plan() {
  cp "$PLAN_PATH" "$PROVIDER_DIR/generated/migration-source-plan.json"
}

# apply_migration_source_plan: Restore the source plan on destination
# This tricks provider-service into thinking we're deploying the same thing,
# triggering backup on node-a and restore on node-b.
apply_migration_source_plan() {
  cp "$PROVIDER_DIR/generated/migration-source-plan.json" "$PLAN_PATH"
}

# =============================================================================
# TEMPLATE RENDERING
# =============================================================================

# -----------------------------------------------------------------------------
# write_provider_flake: Generate the provider flake.nix
# -----------------------------------------------------------------------------
# The provider flake defines the infrastructure. Key parts:
# - inputs: References to hostenv library and your project
# - provider settings: Node placement, deploy config, service endpoints
# - nodeSystems: Which architectures each node uses
#
# We use templates with placeholders that we substitute with actual values.

write_provider_flake() {
  local production_node="$1"
  local esc_hostenv_source
  local esc_project_url
  local esc_hostname
  local esc_signing_key
  local esc_node
  local esc_gateway
  local esc_provider_port

  esc_hostenv_source="$(escape_sed_replacement "path:${HOSTENV_SOURCE_DIR}")"
  esc_project_url="$(escape_sed_replacement "git+file://${PROJECT_DIR}?dir=.hostenv&ref=main")"
  esc_hostname="$(escape_sed_replacement "$HOSTENV_HOSTNAME")"
  esc_signing_key="$(escape_sed_replacement "$NIX_SIGNING_PUBLIC_KEY")"
  esc_node="$(escape_sed_replacement "$production_node")"
  esc_gateway="$(escape_sed_replacement "$PROVIDER_API_VM_GATEWAY")"
  esc_provider_port="$(escape_sed_replacement "$PROVIDER_HTTP_PORT")"

  cp "$PROVIDER_FLAKE_TEMPLATE" "$PROVIDER_DIR/flake.nix"
  sed -i "s|__HOSTENV_SOURCE_DIR__|${esc_hostenv_source}|g" "$PROVIDER_DIR/flake.nix"
  sed -i "s|__PROJECT_URL__|${esc_project_url}|g" "$PROVIDER_DIR/flake.nix"
  sed -i "s|__HOSTENV_HOSTNAME__|${esc_hostname}|g" "$PROVIDER_DIR/flake.nix"
  sed -i "s|__NIX_SIGNING_PUBLIC_KEY__|${esc_signing_key}|g" "$PROVIDER_DIR/flake.nix"
  sed -i "s|__PRODUCTION_NODE__|${esc_node}|g" "$PROVIDER_DIR/flake.nix"
  sed -i "s|__PROVIDER_API_VM_GATEWAY__|${esc_gateway}|g" "$PROVIDER_DIR/flake.nix"
  sed -i "s|__PROVIDER_HTTP_PORT__|${esc_provider_port}|g" "$PROVIDER_DIR/flake.nix"
}

# -----------------------------------------------------------------------------
# write_node_config: Generate NixOS configuration for a VM node
# -----------------------------------------------------------------------------
# Each node needs a NixOS configuration defining:
# - Hostname and networking
# - SSH authorized keys
# - Shared filesystem mounts (9p via QEMU)
# - Nix settings (substituters, trusted keys)
#
# The VMs use NixOS's QEMU support with 9p filesystem passthrough for shared
# directories between host and guests.

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
}

# =============================================================================
# VM LIFECYCLE
# =============================================================================

# -----------------------------------------------------------------------------
# start_vm: Build and launch a NixOS VM
# -----------------------------------------------------------------------------
# This builds the NixOS system for a node and launches it as a QEMU VM.
# The VM runs headless (no GUI) and we interact with it via SSH.
#
# BUILD PROCESS:
# 1. nix build generates the VM runner script
# 2. The runner script launches QEMU with proper arguments
# 3. We wait for SSH to become available
#
# QEMU ARGUMENTS:
# - accel=kvm:tcg: Use KVM if available, otherwise TCG (slow but works)
# - m 4096: 4GB RAM
# - smp 2: 2 CPU cores
# - virtio-rng: Hardware RNG for better entropy
# - 9p mounts: Shared directories between host and guest

start_vm() {
  local node="$1"
  local vm_out="$PROVIDER_DIR/vm-$node"
  local vm_runner

  (cd "$PROVIDER_DIR" && nix build "./generated#nixosConfigurations.${node}.config.system.build.vm" -o "$vm_out")
  vm_runner="$(find_vm_runner "$vm_out")"

  (cd "$PROVIDER_DIR" && "$vm_runner" > "$LOG_DIR/${node}.log" 2>&1) &
  VM_PIDS+=("$!")
  printf '%s\n' "$!" > "$PIDS_DIR/${node}.pid"

  wait_for_ssh "${node}.${HOSTENV_HOSTNAME}" 420 || fail "Timed out waiting for SSH on ${node}. See $LOG_DIR/${node}.log"
}

# =============================================================================
# DATABASE SEEDING
# =============================================================================

# run_seed_import: Import the database dump into Drupal
# The seed.sql.gz contains a Drupal database with a marker row we'll use
# to verify migration worked. We pipe it through SSH to node-a's database.
run_seed_import() {
  (
    cd "$PROJECT_DIR/.hostenv"
    pv ./seed.sql.gz | gunzip -c | ssh -F "$SSH_CONFIG" "${ENV_USER}@node-a.${HOSTENV_HOSTNAME}" \
      "set -euo pipefail; db_bin=\$(command -v mariadb || command -v mysql || echo /run/current-system/sw/bin/mysql); \"\$db_bin\" --socket='${RUNTIME_DIR}/mysql.sock' --user='${ENV_USER}' --database=drupal"
  )
}

# =============================================================================
# DEMO PHASES
# =============================================================================

# -----------------------------------------------------------------------------
# PHASE 1: Setup node-a
# -----------------------------------------------------------------------------
# This prepares the initial infrastructure:
# 1. Generate provider configuration (flake.nix)
# 2. Create VM configurations for both nodes
# 3. Run provider plan to generate plan.json
# 4. Generate provider node authentication tokens
# 5. Sign Nix closures for VM trust
# 6. Start node-a VM and provider-service
#
# WHY NODE-B CONFIG NOW?
# We create both node configs upfront so the provider knows about all nodes
# from the start. This simplifies the migration phase later.

prepare_baseline_node_a() {
  write_provider_flake "node-a"
  write_node_config "node-a" "$NODE_A_HOST_IP"
  write_node_config "node-b" "$NODE_B_HOST_IP"
  commit_provider_sources

  run_provider_plan
  (cd "$PROVIDER_DIR" && nix run .#hostenv-provider -- node-tokens --quiet)
  refresh_merged_provider_secrets
  commit_generated

  load_plan_metadata
  load_project_hash_from_plan
  NODE_A_AUTH_TOKEN="$(load_provider_node_token node-a)"; [[ -n "$NODE_A_AUTH_TOKEN" ]] || fail "Missing node-a token"

  sign_deploy_profile "node-a" "system"
  sign_deploy_profile "node-a" "$ENV_USER"

  start_vm "node-a"
  ensure_provider_service_running
}

# -----------------------------------------------------------------------------
# PHASE 2: Prepare node-b for migration
# -----------------------------------------------------------------------------
# This switches the infrastructure to use node-b:
# 1. Capture the current plan (for migration source)
# 2. Update provider flake to place production on node-b
# 3. Regenerate tokens and artifacts
# 4. Restart provider-service (to pick up new config)
# 5. Start node-b VM
#
# WHY RESTART PROVIDER-SERVICE?
# The service caches configuration. We restart it to ensure it picks up the
# new node placement and tokens.

prepare_baseline_node_b() {
  capture_migration_source_plan
  write_provider_flake "node-b"
  commit_provider_sources
  run_provider_plan
  (cd "$PROVIDER_DIR" && nix run .#hostenv-provider -- node-tokens --quiet)
  refresh_merged_provider_secrets
  commit_generated

  load_plan_metadata
  load_project_hash_from_plan
  NODE_B_AUTH_TOKEN="$(load_provider_node_token node-b)"; [[ -n "$NODE_B_AUTH_TOKEN" ]] || fail "Missing node-b token"

  sign_deploy_profile "node-b" "system"
  sign_deploy_profile "node-b" "$ENV_USER"

  kill_pid_if_running "$PROVIDER_SERVICE_PID"
  PROVIDER_SERVICE_PID=""
  FORCE_INITIAL_INTENTS=0
  rm -f "$PROVIDER_SERVICE_SOCKET"
  ensure_provider_service_running
  start_vm "node-b"
}

# -----------------------------------------------------------------------------
# PHASE 3: Deploy to node-a
# -----------------------------------------------------------------------------
# This simulates a production deployment:
# 1. Send webhook to trigger deploy job
# 2. Wait for provider-service to schedule activation action
# 3. Trigger hostenv-deploy-agent refresh to apply the deployment
# 4. Wait for job completion
# 5. Verify the site responds to HTTP
#
# THE WEBHOOK FLOW:
# 1. Webhook arrives at provider-service
# 2. Service creates deploy intent in database
# 3. Service materializes node-specific actions
# 4. Node polls action API, finds "activate" action
# 5. Node downloads closure, activates system
# 6. Node reports status back to provider-service

deploy_to_node_a() {
  local sha
  sha="deploy-node-a-$(date +%s)"
  NODE_A_JOB_ID="$(trigger_webhook_deploy node-a "$sha")"
  [[ -n "$NODE_A_JOB_ID" ]] || fail "Missing node-a job id"

  if ! wait_for_job_action_op "$NODE_A_JOB_ID" "node-a" "$NODE_A_AUTH_TOKEN" "activate" 900; then
    wait_for_job_action_op "$NODE_A_JOB_ID" "node-a" "$NODE_A_AUTH_TOKEN" "reload" 120 || fail "No activate/reload action for node-a"
  fi

  wait_for_provider_job_completion "$NODE_A_JOB_ID" "node-a" "$NODE_A_AUTH_TOKEN" 1800 || fail "Node-a deployment job failed"
  wait_for_site_ready "$NODE_A_HOST_IP" 1200 || fail "Node-a site did not become ready"
}

# -----------------------------------------------------------------------------
# PHASE 4: Migrate to node-b
# -----------------------------------------------------------------------------
# This demonstrates Hostenv's migration capability:
# 1. Restore the source plan (triggers backup on node-a)
# 2. Send webhook to create migration job
# 3. Wait for backup action on node-a
# 4. Wait for restore action on node-b
# 5. Trigger hostenv-deploy-agent on both nodes
# 6. Verify migration marker is present
#
# THE MIGRATION FLOW:
# When we restore the source plan, provider-service sees the environment was
# previously on node-a but should now be on node-b. It orchestrates:
# 1. Backup action on node-a (dumps database to shared storage)
# 2. Restore action on node-b (loads database from shared storage)
# 3. Both nodes activate with the same intent SHA

migrate_to_node_b() {
  local sha
  sha="migrate-node-b-$(date +%s)"
  apply_migration_source_plan
  NODE_B_JOB_ID="$(trigger_webhook_deploy node-b "$sha")"
  [[ -n "$NODE_B_JOB_ID" ]] || fail "Missing node-b job id"

  wait_for_job_action_op "$NODE_B_JOB_ID" "node-a" "$NODE_A_AUTH_TOKEN" "backup" 900 || fail "No backup action for node-a"
  wait_for_job_action_op "$NODE_B_JOB_ID" "node-b" "$NODE_B_AUTH_TOKEN" "restore" 900 || fail "No restore action for node-b"

  local intent_sha
  intent_sha="$(resolve_intent_commit_sha_for_job "$NODE_B_JOB_ID")"

  wait_for_provider_job_completion "$NODE_B_JOB_ID" "node-b" "$NODE_B_AUTH_TOKEN" 1800 || fail "Node-b migration job failed"
  wait_for_site_ready "$NODE_B_HOST_IP" 1200 || fail "Node-b site did not become ready"
  marker_present_on_node "node-b.${HOSTENV_HOSTNAME}" || fail "Migration marker not found on node-b"
}

# =============================================================================
# MAIN ENTRY POINT
# =============================================================================

main() {
  # Parse command line arguments
  while (($# > 0)); do
    case "$1" in
      --automated) AUTOMATED=1 ;;
      --cleanup) CLEANUP=1 ;;
      --help|-h) usage; exit 0 ;;
      *) fail "Unknown option: $1 (usage: run-demo.sh [--automated] [--cleanup])" ;;
    esac
    shift
  done

  # Verify all required tools are available
  # The script assumes a Nix environment where dependencies are pre-installed
  require_cmd gum
  for cmd in nix git jq ssh ssh-keygen sops age-keygen curl pv gunzip psql initdb pg_ctl createdb socat find sed awk ss; do
    require_cmd "$cmd"
  done
  [[ -e /dev/kvm ]] || fail "This demo requires Linux KVM (/dev/kvm)."
  [[ -f "$SEED_SOURCE" ]] || fail "Missing seed file: $SEED_SOURCE"

  # Find free ports to avoid conflicts with other services
  # This is important if you run multiple demos or have services on default ports
  NODE_SSH_PORT="$(choose_free_port "$NODE_A_HOST_IP" "$NODE_SSH_PORT")"
  NODE_SSH_PORT="$(choose_free_port "$NODE_B_HOST_IP" "$NODE_SSH_PORT")"
  NODE_HTTP_PORT="$(choose_free_port "$NODE_A_HOST_IP" "$NODE_HTTP_PORT")"
  NODE_HTTP_PORT="$(choose_free_port "$NODE_B_HOST_IP" "$NODE_HTTP_PORT")"
  PROVIDER_HTTP_PORT="$(choose_free_port "0.0.0.0" "$PROVIDER_HTTP_PORT")"

  # Create the temporary workspace
  # Everything lives in here, making cleanup trivial
  WORKDIR="$(mktemp -d /tmp/hostenv-local-vm-demo-XXXXXX)"
  PROVIDER_DIR="$WORKDIR/provider"
  PROJECT_DIR="$WORKDIR/demo-project"
  HOSTENV_SOURCE_DIR="$WORKDIR/hostenv-source"
  SSH_DIR="$WORKDIR/ssh"
  SSH_KEY="$SSH_DIR/id_ed25519"
  SSH_CONFIG="$SSH_DIR/config"
  SHARED_DIR="$WORKDIR/shared"
  LOG_DIR="$WORKDIR/logs"
  PIDS_DIR="$WORKDIR/pids"
  PLAN_PATH="$PROVIDER_DIR/generated/plan.json"
  PROVIDER_SERVICE_RUNTIME_DIR="$WORKDIR/provider-dev"
  PROVIDER_SERVICE_SOCKET="$PROVIDER_SERVICE_RUNTIME_DIR/hostenv-provider.sock"

  # Ensure cleanup runs on exit
  # This trap ensures we don't leave VMs running if the script fails
  trap cleanup EXIT INT TERM

  # Create directory structure
  mkdir -p "$PROVIDER_DIR" "$PROJECT_DIR" "$HOSTENV_SOURCE_DIR" "$SSH_DIR" "$SHARED_DIR/backups" "$LOG_DIR" "$PIDS_DIR"
  mkdir -p "$PROVIDER_DIR/secrets" "$PROVIDER_DIR/generated"
  chmod 700 "$SSH_DIR"
  chmod 0777 "$SHARED_DIR" "$SHARED_DIR/backups"

  # Copy template files and fixtures
  cp -a "$DEMO_FIXTURE_DIR/." "$PROJECT_DIR/"
  cp -a "$PROVIDER_TEMPLATE_DIR/." "$PROVIDER_DIR/"
  cp -a "$REPO_ROOT/." "$HOSTENV_SOURCE_DIR/"
  rm -rf "$HOSTENV_SOURCE_DIR/.git" "$HOSTENV_SOURCE_DIR/.direnv" "$HOSTENV_SOURCE_DIR/result" "$HOSTENV_SOURCE_DIR"/result-*
  cp "$SEED_SOURCE" "$PROJECT_DIR/.hostenv/seed.sql.gz"

  # Generate SSH key for VM access
  # Each demo run gets fresh keys for security isolation
  ssh-keygen -q -t ed25519 -N "" -f "$SSH_KEY"
  SSH_PUBLIC_KEY="$(cat "$SSH_KEY.pub")"

  # Generate age key for secret encryption
  # Age is a modern encryption tool. We use it via sops for secret management.
  age-keygen -o "$PROVIDER_DIR/secrets/demo-age.key" >/dev/null
  AGE_PRIVATE_KEY="$(grep '^AGE-SECRET-KEY-' "$PROVIDER_DIR/secrets/demo-age.key")"
  AGE_PUBLIC_KEY="$(grep '^# public key:' "$PROVIDER_DIR/secrets/demo-age.key" | awk '{print $4}')"
  export SOPS_AGE_KEY="$AGE_PRIVATE_KEY"
  export SOPS_AGE_KEY_FILE="$PROVIDER_DIR/secrets/demo-age.key"

  # Generate Nix signing key for closure trust
  # This lets VMs trust closures from this provider instance
  NIX_SIGNING_KEY_FILE="$PROVIDER_DIR/secrets/nix-signing.key"
  nix key generate-secret --key-name "hostenv-demo-local" > "$NIX_SIGNING_KEY_FILE"
  NIX_SIGNING_PUBLIC_KEY="$(nix key convert-secret-to-public < "$NIX_SIGNING_KEY_FILE")"

  # Substitute placeholders in the project flake and config
  # Templates use __PLACEHOLDER__ syntax that we replace with actual values
  sed -i "s|HOSTENV_INPUT_URL_PLACEHOLDER|$(escape_sed_replacement "path:${HOSTENV_SOURCE_DIR}")|g" "$PROJECT_DIR/.hostenv/flake.nix"
  sed -i "s|SSH_PUBLIC_KEY_PLACEHOLDER|$(escape_sed_replacement "$SSH_PUBLIC_KEY")|g" "$PROJECT_DIR/.hostenv/hostenv.nix"

  # Create and encrypt provider secrets
  # Secrets include backup passwords and API tokens
  cat > "$PROVIDER_DIR/secrets/provider.plain.yaml" <<'EOF_SECRETS'
access_tokens: ""
demo:
  backups_secret: "hostenv-demo-password"
  backups_env: "RESTIC_COMPRESSION=off"
EOF_SECRETS
  SOPS_AGE_RECIPIENTS="$AGE_PUBLIC_KEY" sops --encrypt --input-type yaml --output-type yaml "$PROVIDER_DIR/secrets/provider.plain.yaml" > "$PROVIDER_DIR/secrets/provider.yaml"
  rm -f "$PROVIDER_DIR/secrets/provider.plain.yaml"

  # Initialize git repositories
  # Hostenv is git-centric; everything must be tracked
  init_git_repo "$PROJECT_DIR"
  init_git_repo "$PROVIDER_DIR"

  # Create SSH config for VM access
  # This configures SSH to use our generated keys and skip host key checking
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

  # Create convenience symlink so users can easily find their workdir
  ln -s "$WORKDIR" "$CALLER_DIR/hostenv-demo"

  # =============================================================================
  # DEMO EXECUTION
  # =============================================================================
  
  stage "Hostenv Local Migration Demo"

  # PHASE 1: Bootstrap provider and node-a
  pause_for_exploration "About to generate provider plan and boot node-a."
  prepare_baseline_node_a

  # PHASE 2: Deploy to node-a
  pause_for_exploration "About to deploy Drupal to node-a." \
    "Provider UI: http://localhost:${PROVIDER_HTTP_PORT}/dashboard" \
    "Site probe: curl -H 'Host: ${VHOST}' http://${NODE_A_HOST_IP}:${NODE_HTTP_PORT}/"
  deploy_to_node_a
  success "Node-a deployment completed."

  # PHASE 3: Seed database with migration marker
  pause_for_exploration "About to import seed database on node-a." \
    "This creates visible marker data used for migration verification."
  run_seed_import
  marker_present_on_node "node-a.${HOSTENV_HOSTNAME}" || fail "Seed marker not found on node-a"
  success "Seed import completed on node-a."

  # PHASE 4: Migrate to node-b
  pause_for_exploration "About to switch placement to node-b and run migration." \
    "Watch backup/restore in provider UI: http://localhost:${PROVIDER_HTTP_PORT}/dashboard"
  prepare_baseline_node_b
  migrate_to_node_b
  success "Migration completed on node-b."

  # PHASE 5: Demo complete
  pause_for_exploration "Demo complete." \
    "Provider UI: http://localhost:${PROVIDER_HTTP_PORT}/dashboard" \
    "Node-b probe: curl -H 'Host: ${VHOST}' http://${NODE_B_HOST_IP}:${NODE_HTTP_PORT}/" \
    "Logs: ${LOG_DIR}"
}

main "$@"
