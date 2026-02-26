{ pkgs }:
pkgs.writeShellApplication {
  name = "local-provider-migration-test";
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

    repo_root="$(git rev-parse --show-toplevel 2>/dev/null || true)"
    [ -n "$repo_root" ] || fail "run inside the hostenv git repository"
    cd "$repo_root"

    script_path="$repo_root/examples/local-provider-migration/run-demo.sh"
    [ -x "$script_path" ] || fail "missing executable demo script: $script_path"

    evidence_dir="$repo_root/.sisyphus/evidence"
    mkdir -p "$evidence_dir"

    log_file="$evidence_dir/task-13-e2e-test.log"
    node_a_status_log="$evidence_dir/task-11-node-a-job-status.log"
    node_b_status_log="$evidence_dir/task-11-node-b-job-status.log"
    provider_service_log="$evidence_dir/task-11-provider-service.log"
    backup_log="$evidence_dir/task-12-backup.log"
    migration_verify_log="$evidence_dir/task-12-migration-verify.log"
    start_epoch="$(date +%s)"

    printf '[task-13] %s\n' "starting automated local provider migration demo" | tee "$log_file"
    if ! "$script_path" --automated --cleanup --no-color >>"$log_file" 2>&1; then
      fail "demo flow failed; see $log_file"
    fi

    require_match '== Automated deploy: node-a ==' "$log_file"
    require_match '== Automated DB seed import ==' "$log_file"
    require_match '== Automated deploy: node-b migration ==' "$log_file"
    require_match '== Demo completed ==' "$log_file"

    require_fresh_file "$provider_service_log"
    require_fresh_file "$node_a_status_log"
    require_fresh_file "$node_b_status_log"
    require_fresh_file "$backup_log"
    require_fresh_file "$migration_verify_log"

    require_match '^final_state=success$' "$node_a_status_log"
    require_match '^final_state=success$' "$node_b_status_log"
    require_match '^backup_phase_status=success$' "$backup_log"
    require_match '^restore_phase_status=success$' "$backup_log"
    require_match '^backup_snapshot_present=true$' "$backup_log"
    require_match '^marker_found=true$' "$migration_verify_log"
    require_match '^http_code=[23][0-9][0-9]$' "$migration_verify_log"

    printf '[task-13] %s\n' "PASS full migration flow verified" | tee -a "$log_file"
  '';
}
