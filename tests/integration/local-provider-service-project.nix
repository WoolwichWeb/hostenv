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

    repo_root="$(git rev-parse --show-toplevel 2>/dev/null || true)"
    [ -n "$repo_root" ] || fail "run inside the hostenv git repository"
    cd "$repo_root"

    script_path="$repo_root/examples/local-provider-service-project/run-demo.sh"
    [ -x "$script_path" ] || fail "missing executable demo script: $script_path"

    log_file="$(mktemp -t provider-service-project-e2e.XXXXXX.log)"

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

    printf '[provider-service-project-test] %s\n' "PASS provider-service project demo verified" | tee -a "$log_file"
  '';
}
