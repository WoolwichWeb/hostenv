{ pkgs }:
let
  lib = pkgs.lib;
  src = ../../provider;
  cliPkg = pkgs.haskellPackages.callCabal2nix "hostenv-provider-cli" src { };
  ghc = pkgs.haskell.packages.ghc912.ghcWithPackages (p: [
    p.aeson
    p.containers
    p.process
    p.scientific
    p.text
  ]);
in
{
  provider-cli-typecheck = cliPkg;
  provider-cli-signing-targets = pkgs.runCommand "provider-cli-signing-targets" { } ''
    set -euo pipefail
    ${ghc}/bin/runghc -i${src} ${src}/TestSigningTargets.hs
    echo ok > "$out"
  '';
  provider-cli-dns-gate-filtering = pkgs.runCommand "provider-cli-dns-gate-filtering" { } ''
    set -euo pipefail
    ${ghc}/bin/runghc -i${src} ${src}/TestDnsGateFilter.hs
    echo ok > "$out"
  '';
  provider-cli-dry-run-help = pkgs.runCommand "provider-cli-dry-run-help" { } ''
    set -euo pipefail
    for subcmd in plan dns-gate deploy; do
      ${cliPkg}/bin/hostenv-provider "$subcmd" --help | ${pkgs.gnugrep}/bin/grep -q -- "--dry-run"
    done
    echo ok > "$out"
  '';
  provider-cli-secrets-merge = pkgs.runCommand "provider-cli-secrets-merge" { } ''
    set -euo pipefail
    export PATH="${lib.makeBinPath [ pkgs.age pkgs.sops pkgs.yq-go pkgs.jq pkgs.gnugrep pkgs.gawk pkgs.coreutils pkgs.bash ]}:$PATH"

    root="$TMPDIR/repo"
    projectRoot="$root/project"
    mkdir -p "$root/generated" "$root/secrets" "$projectRoot/.hostenv/secrets"

    cat > "$projectRoot/.hostenv/hostenv.nix" <<'EOF'
    { ... }: { }
    EOF

    cat > "$projectRoot/.hostenv/secrets/project.yaml" <<'EOF'
    api_token: "project-secret"
    EOF

    cat > "$projectRoot/.hostenv/secrets/main.yaml" <<'EOF'
    oauth_client: "env-secret"
    EOF

    age-keygen -o "$root/age.key" >/dev/null
    recipient="$(grep '^# public key:' "$root/age.key" | awk '{print $4}')"

    cat > "$root/provider-plain.yaml" <<'EOF'
    acme__demo-main:
      backups_secret: "base-secret"
    acme_demo:
      existing_project: "keep-me"
    EOF
    sops --encrypt --input-type yaml --output-type yaml --age "$recipient" "$root/provider-plain.yaml" > "$root/secrets/secrets.yaml"

    cat > "$root/generated/plan.json" <<EOF
    {
      "hostenvHostname": "hosting.test",
      "environments": {
        "acme__demo-main": {
          "node": "node-a",
          "type": "development",
          "users": {},
          "virtualHosts": {},
          "secrets": {
            "enable": true,
            "keys": ["oauth_client"]
          },
          "hostenv": {
            "userName": "acme__demo-main",
            "organisation": "acme",
            "project": "demo",
            "root": "$projectRoot",
            "environmentName": "main",
            "safeEnvironmentName": "main",
            "runtimeDir": "/run/hostenv/nginx/acme__demo-main",
            "projectSecrets": {
              "enable": true,
              "keys": ["api_token"]
            }
          }
        }
      }
    }
    EOF

    cd "$root"
    export SOPS_AGE_KEY_FILE="$root/age.key"
    ${cliPkg}/bin/hostenv-provider deploy --dry-run --node no-such >/dev/null

    test -f "$root/generated/secrets.merged.yaml"
    sops --decrypt --output-type json "$root/generated/secrets.merged.yaml" > "$root/merged.json"

    jq -e '.["acme_demo"].api_token == "project-secret"' "$root/merged.json" >/dev/null
    jq -e '.["acme__demo-main"].oauth_client == "env-secret"' "$root/merged.json" >/dev/null
    jq -e '.["acme__demo-main"].backups_secret == "base-secret"' "$root/merged.json" >/dev/null
    jq -e '.["acme_demo"].existing_project == "keep-me"' "$root/merged.json" >/dev/null
    echo ok > "$out"
  '';
  provider-cli-secrets-merge-all-keys = pkgs.runCommand "provider-cli-secrets-merge-all-keys" { } ''
    set -euo pipefail
    export PATH="${lib.makeBinPath [ pkgs.age pkgs.sops pkgs.yq-go pkgs.jq pkgs.gnugrep pkgs.gawk pkgs.coreutils pkgs.bash ]}:$PATH"

    root="$TMPDIR/repo"
    projectRoot="$root/project"
    mkdir -p "$root/generated" "$root/secrets" "$projectRoot/.hostenv/secrets"

    cat > "$projectRoot/.hostenv/hostenv.nix" <<'EOF'
    { ... }: { }
    EOF

    cat > "$projectRoot/.hostenv/secrets/project.yaml" <<'EOF'
    api_token: "project-token"
    api_secret: "project-secret"
    EOF

    cat > "$projectRoot/.hostenv/secrets/main.yaml" <<'EOF'
    oauth_client: "env-client"
    env_extra: "env-extra"
    EOF

    age-keygen -o "$root/age.key" >/dev/null
    recipient="$(grep '^# public key:' "$root/age.key" | awk '{print $4}')"

    cat > "$root/provider-plain.yaml" <<'EOF'
    acme__demo-main:
      backups_secret: "base-secret"
      backups_env: "RESTIC_PASSWORD=base-env"
      provider_only: "keep-private"
    acme_demo:
      provider_project_only: "keep-private-project"
    EOF
    sops --encrypt --input-type yaml --output-type yaml --age "$recipient" "$root/provider-plain.yaml" > "$root/secrets/secrets.yaml"

    cat > "$root/generated/plan.json" <<EOF
    {
      "hostenvHostname": "hosting.test",
      "environments": {
        "acme__demo-main": {
          "node": "node-a",
          "type": "development",
          "users": {},
          "virtualHosts": {},
          "secrets": {
            "enable": true
          },
          "hostenv": {
            "userName": "acme__demo-main",
            "organisation": "acme",
            "project": "demo",
            "root": "$projectRoot",
            "environmentName": "main",
            "safeEnvironmentName": "main",
            "runtimeDir": "/run/hostenv/nginx/acme__demo-main",
            "projectSecrets": {
              "enable": true
            }
          }
        }
      }
    }
    EOF

    cd "$root"
    export SOPS_AGE_KEY_FILE="$root/age.key"
    ${cliPkg}/bin/hostenv-provider deploy --dry-run --node no-such >/dev/null

    test -f "$root/generated/secrets.merged.yaml"
    sops --decrypt --output-type json "$root/generated/secrets.merged.yaml" > "$root/merged.json"

    jq -e '.["acme_demo"].api_token == "project-token"' "$root/merged.json" >/dev/null
    jq -e '.["acme_demo"].api_secret == "project-secret"' "$root/merged.json" >/dev/null
    jq -e '.["acme__demo-main"].oauth_client == "env-client"' "$root/merged.json" >/dev/null
    jq -e '.["acme__demo-main"].env_extra == "env-extra"' "$root/merged.json" >/dev/null
    jq -e '.["acme__demo-main"].provider_only == "keep-private"' "$root/merged.json" >/dev/null
    jq -e '.["acme_demo"].provider_project_only == "keep-private-project"' "$root/merged.json" >/dev/null

    jq -e '(.["__hostenv_selected_keys"]["acme__demo-main"].project | sort) == ["api_secret", "api_token"]' "$root/merged.json" >/dev/null
    jq -e '(.["__hostenv_selected_keys"]["acme__demo-main"].environment | sort) == ["env_extra", "oauth_client"]' "$root/merged.json" >/dev/null
    echo ok > "$out"
  '';
  provider-cli-secrets-merge-tracks-file = pkgs.runCommand "provider-cli-secrets-merge-tracks-file" { } ''
    set -euo pipefail
    export PATH="${lib.makeBinPath [ pkgs.age pkgs.git pkgs.sops pkgs.yq-go pkgs.jq pkgs.gnugrep pkgs.gawk pkgs.coreutils pkgs.bash ]}:$PATH"

    root="$TMPDIR/repo"
    projectRoot="$root/project"
    mkdir -p "$root/generated" "$root/secrets" "$projectRoot/.hostenv/secrets"

    cat > "$projectRoot/.hostenv/hostenv.nix" <<'EOF'
    { ... }: { }
    EOF

    cat > "$projectRoot/.hostenv/secrets/project.yaml" <<'EOF'
    api_token: "project-secret"
    EOF

    cat > "$projectRoot/.hostenv/secrets/main.yaml" <<'EOF'
    oauth_client: "env-secret"
    EOF

    age-keygen -o "$root/age.key" >/dev/null
    recipient="$(grep '^# public key:' "$root/age.key" | awk '{print $4}')"

    cat > "$root/provider-plain.yaml" <<'EOF'
    acme__demo-main:
      backups_secret: "base-secret"
    acme_demo:
      existing_project: "keep-me"
    EOF
    sops --encrypt --input-type yaml --output-type yaml --age "$recipient" "$root/provider-plain.yaml" > "$root/secrets/secrets.yaml"

    cat > "$root/generated/plan.json" <<EOF
    {
      "hostenvHostname": "hosting.test",
      "environments": {
        "acme__demo-main": {
          "node": "node-a",
          "type": "development",
          "users": {},
          "virtualHosts": {},
          "secrets": {
            "enable": true,
            "keys": ["oauth_client"]
          },
          "hostenv": {
            "userName": "acme__demo-main",
            "organisation": "acme",
            "project": "demo",
            "root": "$projectRoot",
            "environmentName": "main",
            "safeEnvironmentName": "main",
            "runtimeDir": "/run/hostenv/nginx/acme__demo-main",
            "projectSecrets": {
              "enable": true,
              "keys": ["api_token"]
            }
          }
        }
      }
    }
    EOF

    git -C "$root" init -q

    if git -C "$root" ls-files --error-unmatch -- generated/secrets.merged.yaml >/dev/null 2>&1; then
      echo "generated/secrets.merged.yaml unexpectedly tracked before deploy" >&2
      exit 1
    fi

    cd "$root"
    export SOPS_AGE_KEY_FILE="$root/age.key"
    ${cliPkg}/bin/hostenv-provider deploy --dry-run --node no-such >/dev/null

    git ls-files --error-unmatch -- generated/secrets.merged.yaml >/dev/null
    echo ok > "$out"
  '';
  provider-cli-secrets-merge-does-not-readd-tracked = pkgs.runCommand "provider-cli-secrets-merge-does-not-readd-tracked" { } ''
    set -euo pipefail
    export PATH="${lib.makeBinPath [ pkgs.age pkgs.git pkgs.sops pkgs.yq-go pkgs.jq pkgs.gnugrep pkgs.gawk pkgs.coreutils pkgs.bash ]}:$PATH"

    root="$TMPDIR/repo"
    projectRoot="$root/project"
    mkdir -p "$root/generated" "$root/secrets" "$projectRoot/.hostenv/secrets" "$root/bin"

    cat > "$projectRoot/.hostenv/hostenv.nix" <<'EOF'
    { ... }: { }
    EOF

    cat > "$projectRoot/.hostenv/secrets/project.yaml" <<'EOF'
    api_token: "project-secret"
    EOF

    cat > "$projectRoot/.hostenv/secrets/main.yaml" <<'EOF'
    oauth_client: "env-secret"
    EOF

    age-keygen -o "$root/age.key" >/dev/null
    recipient="$(grep '^# public key:' "$root/age.key" | awk '{print $4}')"

    cat > "$root/provider-plain.yaml" <<'EOF'
    acme__demo-main:
      backups_secret: "base-secret"
    acme_demo:
      existing_project: "keep-me"
    EOF
    sops --encrypt --input-type yaml --output-type yaml --age "$recipient" "$root/provider-plain.yaml" > "$root/secrets/secrets.yaml"

    cat > "$root/generated/plan.json" <<EOF
    {
      "hostenvHostname": "hosting.test",
      "environments": {
        "acme__demo-main": {
          "node": "node-a",
          "type": "development",
          "users": {},
          "virtualHosts": {},
          "secrets": {
            "enable": true,
            "keys": ["oauth_client"]
          },
          "hostenv": {
            "userName": "acme__demo-main",
            "organisation": "acme",
            "project": "demo",
            "root": "$projectRoot",
            "environmentName": "main",
            "safeEnvironmentName": "main",
            "runtimeDir": "/run/hostenv/nginx/acme__demo-main",
            "projectSecrets": {
              "enable": true,
              "keys": ["api_token"]
            }
          }
        }
      }
    }
    EOF

    cat > "$root/generated/secrets.merged.yaml" <<'EOF'
    placeholder: true
    EOF

    ${pkgs.git}/bin/git -C "$root" init -q
    ${pkgs.git}/bin/git -C "$root" config user.email "test@example.com"
    ${pkgs.git}/bin/git -C "$root" config user.name "hostenv test"
    ${pkgs.git}/bin/git -C "$root" add generated/secrets.merged.yaml
    ${pkgs.git}/bin/git -C "$root" commit -qm "seed tracked merged secrets file"

    cat > "$root/bin/git" <<EOF
    #!/usr/bin/env bash
    set -euo pipefail
    echo "\$*" >> "$root/git-calls.log"
    exec ${pkgs.git}/bin/git "\$@"
    EOF
    chmod +x "$root/bin/git"

    cd "$root"
    export PATH="$root/bin:$PATH"
    export SOPS_AGE_KEY_FILE="$root/age.key"
    ${cliPkg}/bin/hostenv-provider deploy --dry-run --node no-such >/dev/null

    if grep -Fxq "add generated/secrets.merged.yaml" "$root/git-calls.log"; then
      echo "hostenv-provider unexpectedly re-added tracked generated/secrets.merged.yaml" >&2
      cat "$root/git-calls.log" >&2
      exit 1
    fi

    ${pkgs.git}/bin/git -C "$root" diff --cached --exit-code -- generated/secrets.merged.yaml >/dev/null
    if ${pkgs.git}/bin/git -C "$root" diff --quiet -- generated/secrets.merged.yaml; then
      echo "expected generated/secrets.merged.yaml to change but remain unstaged" >&2
      exit 1
    fi
    echo ok > "$out"
  '';
  provider-cli-secrets-missing-key = pkgs.runCommand "provider-cli-secrets-missing-key" { } ''
    set -euo pipefail
    export PATH="${lib.makeBinPath [ pkgs.age pkgs.sops pkgs.yq-go pkgs.jq pkgs.gnugrep pkgs.gawk pkgs.coreutils pkgs.bash ]}:$PATH"

    root="$TMPDIR/repo"
    projectRoot="$root/project"
    mkdir -p "$root/generated" "$root/secrets" "$projectRoot/.hostenv/secrets"

    cat > "$projectRoot/.hostenv/hostenv.nix" <<'EOF'
    { ... }: { }
    EOF

    cat > "$projectRoot/.hostenv/secrets/project.yaml" <<'EOF'
    other_key: "not-the-one"
    EOF

    age-keygen -o "$root/age.key" >/dev/null
    recipient="$(grep '^# public key:' "$root/age.key" | awk '{print $4}')"

    cat > "$root/provider-plain.yaml" <<'EOF'
    acme__demo-main:
      backups_secret: "base-secret"
    EOF
    sops --encrypt --input-type yaml --output-type yaml --age "$recipient" "$root/provider-plain.yaml" > "$root/secrets/secrets.yaml"

    cat > "$root/generated/plan.json" <<EOF
    {
      "hostenvHostname": "hosting.test",
      "environments": {
        "acme__demo-main": {
          "node": "node-a",
          "type": "development",
          "users": {},
          "virtualHosts": {},
          "hostenv": {
            "userName": "acme__demo-main",
            "organisation": "acme",
            "project": "demo",
            "root": "$projectRoot",
            "environmentName": "main",
            "safeEnvironmentName": "main",
            "runtimeDir": "/run/hostenv/nginx/acme__demo-main",
            "projectSecrets": {
              "enable": true,
              "keys": ["api_token"]
            }
          }
        }
      }
    }
    EOF

    cd "$root"
    export SOPS_AGE_KEY_FILE="$root/age.key"

    if ${cliPkg}/bin/hostenv-provider deploy --dry-run --node no-such >"$root/stdout.log" 2>"$root/stderr.log"; then
      echo "expected deploy to fail for missing declared secret key" >&2
      exit 1
    fi

    grep -q "declared project secret key 'api_token'" "$root/stderr.log"
    echo ok > "$out"
  '';
  provider-cli-dns-backoff = pkgs.runCommand "provider-cli-dns-backoff" { } ''
    set -euo pipefail
    ${ghc}/bin/runghc -i${src} ${src}/TestDnsBackoff.hs
    echo ok > "$out"
  '';
  provider-cli-prev-node-discovery = pkgs.runCommand "provider-cli-prev-node-discovery" { } ''
    set -euo pipefail
    ${ghc}/bin/runghc -i${src} ${src}/TestPrevNodeDiscovery.hs
    echo ok > "$out"
  '';
  provider-cli-deploy-verification = pkgs.runCommand "provider-cli-deploy-verification" { } ''
    set -euo pipefail
    ${ghc}/bin/runghc -i${src} ${src}/TestDeployVerification.hs
    echo ok > "$out"
  '';
}
