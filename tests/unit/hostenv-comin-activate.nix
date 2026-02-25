{ pkgs, ... }:
{
  comin-activate-migration-backup = pkgs.runCommand "comin-activate-migration-backup" { } ''
    set -euo pipefail

    test_dir="$(mktemp -d)"
    trap 'rm -rf "$test_dir"' EXIT

    export HOME="$test_dir/home"
    export PATH="$test_dir/bin:${pkgs.jq}/bin:${pkgs.coreutils}/bin:${pkgs.bash}/bin"
    mkdir -p "$HOME/.local/bin" "$test_dir/bin"

    events_file="$test_dir/events.jsonl"
    systemctl_file="$test_dir/systemctl.jsonl"
    touch "$events_file" "$systemctl_file"

    cat > "$test_dir/deploy-intent.json" <<'JSON'
    {
      "jobId": "job-123",
      "intent": {
        "schemaVersion": 1,
        "actions": [
          {
            "user": "org-proj-main",
            "op": "backup",
            "migrations": ["drupal-migrate"]
          }
        ]
      }
    }
    JSON

    cat > "$test_dir/bin/curl" <<'EOF'
    #!${pkgs.bash}/bin/bash
    set -euo pipefail

    url="''${!#}"
    data=""
    while [ "$#" -gt 0 ]; do
      case "$1" in
        -d)
          data="$2"
          shift 2
          ;;
        *)
          shift
          ;;
      esac
    done

    if [[ "$url" == *"/api/deploy-intents/by-sha"* ]]; then
      cat "$HOSTENV_TEST_DEPLOY_INTENT"
      printf '\n200\n'
      exit 0
    fi

    if [[ "$url" == *"/events" ]]; then
      printf '%s\n' "$data" >> "$HOSTENV_TEST_EVENTS_FILE"
      exit 0
    fi

    echo "unexpected curl url: $url" >&2
    exit 1
    EOF
    chmod +x "$test_dir/bin/curl"

    cat > "$test_dir/bin/runuser" <<'EOF'
    #!${pkgs.bash}/bin/bash
    set -euo pipefail
    while [ "$#" -gt 0 ]; do
      case "$1" in
        -u)
          shift 2
          ;;
        --)
          shift
          break
          ;;
        *)
          break
          ;;
      esac
    done
    exec "$@"
    EOF
    chmod +x "$test_dir/bin/runuser"

    cat > "$test_dir/bin/id" <<'EOF'
    #!${pkgs.bash}/bin/bash
    set -euo pipefail
    if [ "''${1:-}" = "-u" ]; then
      echo 1000
      exit 0
    fi
    echo "unsupported id invocation" >&2
    exit 1
    EOF
    chmod +x "$test_dir/bin/id"

    cat > "$test_dir/bin/systemctl" <<'EOF'
    #!${pkgs.bash}/bin/bash
    set -euo pipefail

    if [ "$1" = "--user" ] && [ "$2" = "show" ] && [ "$3" = "-p" ] && [ "$4" = "LoadState" ]; then
      unit="$5"
      printf '{"op":"show","unit":"%s"}\n' "$unit" >> "$HOSTENV_TEST_SYSTEMCTL_FILE"
      echo "LoadState=loaded"
      exit 0
    fi

    if [ "$1" = "--user" ] && [ "$2" = "start" ] && [ "$3" = "--wait" ]; then
      unit="$4"
      printf '{"op":"start","unit":"%s"}\n' "$unit" >> "$HOSTENV_TEST_SYSTEMCTL_FILE"
      exit 0
    fi

    echo "unexpected systemctl invocation: $*" >&2
    exit 1
    EOF
    chmod +x "$test_dir/bin/systemctl"

    cat > "$HOME/.local/bin/restic-drupal-migrate" <<'EOF'
    #!${pkgs.bash}/bin/bash
    set -euo pipefail
    cat <<'JSON'
    [
      {"id":"snap-older","time":"2026-01-01T00:00:00Z"},
      {"id":"snap-latest","time":"2026-01-02T00:00:00Z"}
    ]
    JSON
    EOF
    chmod +x "$HOME/.local/bin/restic-drupal-migrate"

    token_file="$test_dir/token"
    printf 'token\n' > "$token_file"

    export COMIN_GIT_SHA="abc123"
    export HOSTENV_COMIN_NODE_NAME="node-a"
    export HOSTENV_COMIN_API_BASE_URL="https://hostenv.test"
    export HOSTENV_COMIN_TOKEN_FILE="$token_file"
    export HOSTENV_COMIN_ACTION_TIMEOUT="30"
    export HOSTENV_TEST_DEPLOY_INTENT="$test_dir/deploy-intent.json"
    export HOSTENV_TEST_EVENTS_FILE="$events_file"
    export HOSTENV_TEST_SYSTEMCTL_FILE="$systemctl_file"

    bash ${../../modules/nixos/hostenv-comin-activate.sh}

    ${pkgs.jq}/bin/jq -s -e '
      map(select(.op == "show" and .unit == "restic-backups-drupal-migrate.service")) | length == 1
    ' "$systemctl_file" >/dev/null

    ${pkgs.jq}/bin/jq -s -e '
      map(select(.op == "start" and .unit == "restic-backups-drupal-migrate.service")) | length == 1
    ' "$systemctl_file" >/dev/null

    ${pkgs.jq}/bin/jq -s -e '
      map(select(.phase == "backup" and .status == "success"))
      | last
      | .payload.snapshots["drupal-migrate"] == "snap-latest"
    ' "$events_file" >/dev/null

    echo ok > "$out"
  '';
}
