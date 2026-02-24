{ inputs, lib, config, hostenvInputs, ... }:
let
  fp = inputs.flake-parts.lib;
  cfgTop = config;
  providerEnabled = config.provider.enable or false;
  addressableContentInput =
    hostenvInputs.requireInput {
      inherit inputs;
      name = "addressable-content";
      context = "hostenv-provider-service";
    };
in
{
  options.perSystem = fp.mkPerSystemOption ({ config, pkgs, ... }:
    let
      providerService = cfgTop.flake.lib.provider.service;
      packageNames = providerService.haskellDeps;
      serviceSrc = providerService.src;
      providerHaskellPackages = pkgs.haskell.packages.ghc912.override {
        overrides = self: super: {
          addressable-content = self.callCabal2nix "addressable-content" addressableContentInput.outPath { };
        };
      };
      ghc = providerHaskellPackages.ghcWithPackages (p: map (name: p.${name}) packageNames);
      servicePkg = pkgs.writeShellScriptBin "hostenv-provider-service" ''
        exec ${ghc}/bin/runghc -i${serviceSrc} ${serviceSrc}/Main.hs "$@"
      '';

      # Quick hacky way to try out the provider service using a browser on local.
      servicePkgDev = pkgs.writeShellScriptBin "hostenv-provider-service-dev" ''
        set -euo pipefail

        base="''${HOSTENV_PROVIDER_DEV_DIR:-/tmp/hostenv-provider-dev}"
        pgdata="$base/pgdata"
        mkdir -p "$base"

        if [ ! -s "$pgdata/PG_VERSION" ]; then
          initdb -D "$pgdata" -A trust >/dev/null
        fi

        pg_ctl -D "$pgdata" -o "-k $base" -l "$base/postgres.log" start >/dev/null
        cleanup() {
          if [ -n "''${socat_pid:-}" ]; then
            kill "$socat_pid" >/dev/null 2>&1 || true
          fi
          pg_ctl -D "$pgdata" stop >/dev/null || true
        }
        trap cleanup EXIT

        createdb -h "$base" hostenv-provider >/dev/null 2>&1 || true

        data_dir="''${HOSTENV_PROVIDER_DATA_DIR:-$base/data}"
        listen_socket="''${HOSTENV_PROVIDER_LISTEN_SOCKET:-$base/hostenv-provider.sock}"
        webhook_host="''${HOSTENV_PROVIDER_WEBHOOK_HOST:-localhost}"
        ui_base_url="''${HOSTENV_PROVIDER_UI_BASE_URL:-http://localhost}"
        db_uri="host=$base dbname=hostenv-provider"
        git_credentials_file="''${HOSTENV_PROVIDER_GIT_CREDENTIALS_FILE:-$data_dir/git-credentials}"
        git_config_file="''${HOSTENV_PROVIDER_GIT_CONFIG_FILE:-$data_dir/gitconfig}"
        flake_template="''${HOSTENV_PROVIDER_FLAKE_TEMPLATE:-flake.template.nix}"
        deploy_token_ttl_minutes="''${HOSTENV_PROVIDER_GITLAB_DEPLOY_TOKEN_TTL_MINUTES:-15}"
        gitlab_hosts_raw="''${HOSTENV_PROVIDER_GITLAB_HOSTS:-gitlab.com}"
        gitlab_hosts_json=$(printf '%s' "$gitlab_hosts_raw" | ${pkgs.gawk}/bin/awk -v RS=',' 'BEGIN { printf "[" } { gsub(/^[[:space:]]+|[[:space:]]+$/, "", $0); if (length($0) > 0) { if (n++) printf ","; gsub(/"/, "\\\"", $0); printf "\"%s\"", $0 } } END { printf "]" }')
        token_key_file="''${HOSTENV_PROVIDER_GITLAB_TOKEN_KEY_FILE:-$base/gitlab_token_key}"

        secrets="''${HOSTENV_PROVIDER_GITLAB_SECRETS_FILE:-$base/gitlab_oauth}"
        if [ ! -f "$secrets" ]; then
          printf "client_id=dev\nclient_secret=dev\n" > "$secrets"
        fi
        if [ ! -f "$token_key_file" ]; then
          printf "key=0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\n" > "$token_key_file"
          chmod 600 "$token_key_file"
        fi

        config_file="$base/provider-config.json"
        cat > "$config_file" <<EOF
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
            "enable": true,
            "oAuthSecretsFile": "$secrets",
            "hosts": $gitlab_hosts_json,
            "tokenEncryptionKeyFile": "$token_key_file",
            "deployTokenTtlMinutes": $deploy_token_ttl_minutes
          },
          "gitCredentialsFile": "$git_credentials_file",
          "gitConfigFile": "$git_config_file",
          "flakeTemplate": "$flake_template"
        }
        EOF

        if [ -n "''${HOSTENV_PROVIDER_HTTP_PORT:-}" ]; then
          socat TCP-LISTEN:"$HOSTENV_PROVIDER_HTTP_PORT",fork,reuseaddr UNIX-CONNECT:"$listen_socket" &
          socat_pid=$!
          echo "hostenv-provider-service-dev: proxying http://localhost:$HOSTENV_PROVIDER_HTTP_PORT -> unix:$listen_socket" >&2
        fi

        ${servicePkg}/bin/hostenv-provider-service --config "$config_file"
      '';
    in
    {
      config = lib.mkIf providerEnabled {
        packages.hostenv-provider-service = servicePkg;
        apps.hostenv-provider-service = {
          type = "app";
          program = "${servicePkg}/bin/hostenv-provider-service";
          meta.description = "Hostenv provider service (webhooks + admin UI) app";
        };
        hostenv.haskell.devPackages = packageNames;
        devshells.default.devshell.packages = [ servicePkg servicePkgDev ];
      };
    });
}
