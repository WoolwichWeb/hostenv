{ pkgs }:
let
  lib = pkgs.lib;
  src = ../../provider;
  cliPkg = pkgs.haskellPackages.callCabal2nix "hostenv-provider-cli" src { };
  ghc = pkgs.haskellPackages.ghcWithPackages (p: [
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
