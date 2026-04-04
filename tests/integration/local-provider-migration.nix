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

    repo_root="$(git rev-parse --show-toplevel 2>/dev/null || true)"
    [ -n "$repo_root" ] || fail "run inside the hostenv git repository"
    cd "$repo_root"

    script_path="$repo_root/examples/local-provider-migration/run-demo.sh"
    [ -x "$script_path" ] || fail "missing executable demo script: $script_path"

    log_file="$(mktemp -t local-provider-migration.XXXXXX.log)"

    printf '[task-13] %s\n' "starting automated local provider migration demo" | tee "$log_file"
    if ! "$script_path" --automated --cleanup --no-color >>"$log_file" 2>&1; then
      fail "demo flow failed; see $log_file"
    fi

    require_match '== Automated deploy: node-a ==' "$log_file"
    require_match '== Automated DB seed import ==' "$log_file"
    require_match '== Automated deploy: node-b migration ==' "$log_file"
    require_match '== Demo completed ==' "$log_file"

    printf '[task-13] %s\n' "PASS full migration flow verified" | tee -a "$log_file"
  '';
}
