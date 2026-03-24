{ pkgs }:
pkgs.writeShellApplication {
  name = "local-provider-service-project-test";
  runtimeInputs = with pkgs; [
    bash
    coreutils
    curl
    findutils
    gawk
    gnused
    gnugrep
    git
    gzip
    hostctl
    jq
    nix
    openssh
    pv
    sops
    age
  ];
  text = ''
    set -euo pipefail

    fail() {
      printf 'ERROR: %s\n' "$*" >&2
      exit 1
    }

    require_match() {
      local pattern="$1"
      local file="$2"
      if ! grep -Eq "$pattern" "$file"; then
        fail "missing expected pattern '$pattern' in $file"
      fi
    }

    require_fresh_file() {
      local file="$1"
      local mtime=""

      [ -s "$file" ] || fail "missing expected evidence file: $file"
      mtime="$(stat -c %Y "$file" 2>/dev/null || true)"
      [[ "$mtime" =~ ^[0-9]+$ ]] || fail "failed to read mtime for $file"
      [ "$mtime" -ge "$start_epoch" ] || fail "stale evidence file (not from this run): $file"
    }

    read_key_value() {
      local key="$1"
      local file="$2"
      grep -E "^$key=" "$file" | tail -n1 | cut -d= -f2- || true
    }

    require_key_value() {
      local key="$1"
      local file="$2"
      local value=""

      value="$(read_key_value "$key" "$file")"
      [ -n "$value" ] || fail "missing expected key '$key' in $file"
      printf '%s\n' "$value"
    }

    repo_root="$(git rev-parse --show-toplevel 2>/dev/null || true)"
    [ -n "$repo_root" ] || fail "run inside the hostenv git repository"
    cd "$repo_root"

    script_path="$repo_root/examples/local-provider-service-project/run-demo.sh"
    [ -x "$script_path" ] || fail "missing executable demo script: $script_path"

    evidence_dir="$repo_root/.sisyphus/evidence"
    mkdir -p "$evidence_dir"

    log_file="$evidence_dir/provider-service-project-e2e-test.log"
    plan_log="$evidence_dir/provider-service-project-phase-b-plan.log"
    bootstrap_log="$evidence_dir/provider-service-project-phase-c-bootstrap.log"
    cache_readiness_log="$evidence_dir/provider-service-project-phase-d-cache-readiness.log"
    node_a_job_log="$evidence_dir/provider-service-project-phase-e-node-a-job-node-a.log"
    migration_job_node_a_log="$evidence_dir/provider-service-project-phase-e-migration-job-node-a.log"
    migration_job_node_b_log="$evidence_dir/provider-service-project-phase-e-migration-job-node-b.log"
    migration_sequence_log="$evidence_dir/provider-service-project-phase-e-migration-sequence.log"
    migration_verify_log="$evidence_dir/provider-service-project-phase-e-migration-verify.log"
    cache_client_node_a_log="$evidence_dir/provider-service-project-phase-f-node-a-cache-client.log"
    cache_client_node_b_log="$evidence_dir/provider-service-project-phase-f-node-b-cache-client.log"
    start_epoch="$(date +%s)"

    printf '[provider-service-project-test] %s\n' "starting automated provider-service project demo" | tee "$log_file"
    if ! "$script_path" --automated --cleanup --no-color >>"$log_file" 2>&1; then
      fail "demo flow failed; see $log_file"
    fi

    require_match '^== Phase A: Workspace bootstrap ==$' "$log_file"
    require_match '^== Phase B: Plan generation ==$' "$log_file"
    require_match '^== Phase C: Node bring-up ==$' "$log_file"
    require_match '^== Phase D: Cache and control-plane readiness ==$' "$log_file"
    require_match '^== Phase E: App deployment and migration ==$' "$log_file"
    require_match '^== Phase F: Cache client proof ==$' "$log_file"
    require_match '^== Demo completed ==$' "$log_file"
    require_match '^Provider-service project demo completed\.$' "$log_file"

    require_fresh_file "$plan_log"
    require_fresh_file "$bootstrap_log"
    require_fresh_file "$cache_readiness_log"
    require_fresh_file "$node_a_job_log"
    require_fresh_file "$migration_job_node_a_log"
    require_fresh_file "$migration_job_node_b_log"
    require_fresh_file "$migration_sequence_log"
    require_fresh_file "$migration_verify_log"
    require_fresh_file "$cache_client_node_a_log"
    require_fresh_file "$cache_client_node_b_log"

    require_match '^provider_service_selected=true$' "$plan_log"
    plan_provider_env="$(require_key_value provider_env "$plan_log")"
    plan_runtime_dir="$(require_key_value provider_runtime_dir "$plan_log")"
    bootstrap_provider_env="$(require_key_value provider_env "$bootstrap_log")"
    bootstrap_runtime_dir="$(require_key_value runtime_dir "$bootstrap_log")"
    bootstrap_socket="$(require_key_value socket "$bootstrap_log")"
    activate_profile="$(require_key_value activate_profile "$bootstrap_log")"

    [ "$plan_provider_env" = "$bootstrap_provider_env" ] || fail "provider env mismatch between plan and bootstrap evidence"
    [ "$plan_runtime_dir" = "$bootstrap_runtime_dir" ] || fail "provider runtime dir mismatch between plan and bootstrap evidence"
    [ "$bootstrap_socket" = "$bootstrap_runtime_dir/hostenv-provider.sock" ] || fail "provider socket was not recorded under the project runtime dir"
    [ "$bootstrap_socket" != "/run/hostenv-provider.sock" ] || fail "provider socket evidence fell back to a host-only path"
    [[ "$activate_profile" == /nix/store/* ]] || fail "provider activate profile was not captured from a built environment"

    require_match '^cache_unauth_http=(401|403)$' "$cache_readiness_log"
    require_match '^cache_auth_http=200$' "$cache_readiness_log"
    require_match '^harmonia_socket=.*/harmonia\.sock$' "$cache_readiness_log"
    require_match '^fcgiwrap_socket=.*/fcgiwrap\.sock$' "$cache_readiness_log"
    require_match 'StoreDir: /nix/store' "$cache_readiness_log"
    require_match 'WantMassQuery: 1' "$cache_readiness_log"

    require_match '^final_state=success$' "$node_a_job_log"
    require_match '"op":"(activate|reload)"' "$node_a_job_log"
    require_match '^final_state=success$' "$migration_job_node_a_log"
    require_match '^final_state=success$' "$migration_job_node_b_log"
    require_match '"op":"backup"' "$migration_job_node_a_log"
    require_match '"op":"restore"' "$migration_job_node_b_log"

    backup_line="$(require_key_value backup_line "$migration_sequence_log")"
    restore_line="$(require_key_value restore_line "$migration_sequence_log")"
    deactivate_line="$(read_key_value deactivate_line "$migration_sequence_log")"
    [[ "$backup_line" =~ ^[0-9]+$ ]] || fail "backup_line was not numeric in $migration_sequence_log"
    [[ "$restore_line" =~ ^[0-9]+$ ]] || fail "restore_line was not numeric in $migration_sequence_log"
    (( backup_line < restore_line )) || fail "migration sequence did not record backup before restore"
    if [ -n "$deactivate_line" ]; then
      [[ "$deactivate_line" =~ ^[0-9]+$ ]] || fail "deactivate_line was not numeric in $migration_sequence_log"
      (( restore_line < deactivate_line )) || fail "migration sequence did not record restore before deactivate"
    fi

    require_match '^marker_found=true$' "$migration_verify_log"
    require_match '^http_code=[23][0-9][0-9]$' "$migration_verify_log"

    require_match '^substituters = .*/cache .*cache\.nixos\.org/?( .*)?$' "$cache_client_node_a_log"
    require_match '^trusted-public-keys = .*hostenv-demo-local:.*$' "$cache_client_node_a_log"
    require_match '^require-sigs = true$' "$cache_client_node_a_log"
    require_match '^netrc-file = /run/hostenv/provider-cache\.netrc$' "$cache_client_node_a_log"

    require_match '^substituters = .*/cache .*cache\.nixos\.org/?( .*)?$' "$cache_client_node_b_log"
    require_match '^trusted-public-keys = .*hostenv-demo-local:.*$' "$cache_client_node_b_log"
    require_match '^require-sigs = true$' "$cache_client_node_b_log"
    require_match '^netrc-file = /run/hostenv/provider-cache\.netrc$' "$cache_client_node_b_log"

    printf '[provider-service-project-test] %s\n' "PASS provider-service project demo verified" | tee -a "$log_file"
  '';
}
