{ inputs, lib, config, self, ... }:
let
  flakeParts = inputs.flake-parts.lib;
  cfg = config.provider;
  providerPlan = config.flake.lib.provider.plan;
  hostenvInputs = config.flake.lib.hostenvInputs;
  hostenvInput =
    hostenvInputs.requireInput {
      inherit inputs;
      name = "hostenv";
      context = "provider tooling";
    };

  hostenvRoot = hostenvInput.outPath;
  providerRoot = hostenvRoot + "/provider";

in
{
  options.perSystem = flakeParts.mkPerSystemOption ({ lib, ... }: {
    options.provider.haskellDevPackages = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [
        "aeson"
        "aeson-pretty"
        "containers"
        "text"
        "text-conversions"
        "bytestring"
        "optparse-applicative"
        "turtle"
      ];
      description = "Haskell package names to include in the dev shell for provider tooling.";
    };

    options.provider.planPaths = lib.mkOption {
      type = lib.types.attrsOf lib.types.path;
      default = { };
      description = "Generated plan/state/flake store paths for provider tooling.";
    };
  });

  config = lib.mkIf cfg.enable {
    perSystem = { system, pkgs, config, ... }:
      let
        providerHsDeps = p: map (name: p.${name}) config.provider.haskellDevPackages;
        providerGhc = pkgs.haskell.packages.ghc912.ghcWithPackages providerHsDeps;
        cliPkg = pkgs.haskellPackages.callCabal2nix "hostenv-provider-cli" providerRoot { };
        hostenvProviderCLI = pkgs.writeShellApplication {
          name = "hostenv-provider";
          runtimeInputs = [ pkgs.jq pkgs.sops pkgs.yq-go pkgs.openssl pkgs.dnsutils ];
          text = ''
            set -euo pipefail
            export HOSTENV_PROVIDER_DIG_PATH="${pkgs.dnsutils}/bin/dig"
            exec ${cliPkg}/bin/hostenv-provider "$@"
          '';
        };

        providerGenerator =
          providerPlan
            {
              inputs = inputs // { hostenv = hostenvInput; };
              repoRoot = self.outPath;
              inherit system;
              lib = pkgs.lib;
              pkgs = pkgs;
              letsEncrypt = cfg.letsEncrypt;
              nixSigning = cfg.nixSigning;
              deploy = cfg.deploy;
              hostenvHostname = cfg.hostenvHostname;
              nodeFor = cfg.nodeFor;
              nodeSystems = cfg.nodeSystems;
              nodeModules = cfg.nodeModules;
              cloudflare = cfg.cloudflare;
              generatedFlake = cfg.generatedFlake;
              service = cfg.serviceResolution;
            };
      in
      {
        packages = {
          hostenv-provider = hostenvProviderCLI;
        };
        provider.planPaths = {
          plan = providerGenerator.plan;
          state = providerGenerator.state;
          flake = providerGenerator.flake;
        };

        # Add to dev packages for hostenv developers.
        hostenv.haskell.devPackages = lib.mkDefault (
          [ "haskell-language-server" ] ++ config.provider.haskellDevPackages
        );

        devshells.default = {
          devshell.packages = [
            hostenvProviderCLI
            providerGhc
            pkgs.sops
            pkgs.age
            pkgs.jq
            pkgs.yq-go
            pkgs.dnsutils
            pkgs.postgresql
            pkgs.haskellPackages.cabal-install
            pkgs.gum
          ];

          devshell.startup.provider-state-file = {
            text = ''
              mkdir -p generated
              if [ ! -f generated/state.json ]; then
                printf '{}\n' > generated/state.json
                git add generated/state.json >/dev/null 2>&1 || true
              fi
            '';
          };

          devshell.startup.provider-node-tokens = lib.mkIf cfg.deploy.enable {
            text = ''
              provider_secrets="secrets/provider.yaml"
              plan_file="generated/plan.json"

              if [ ! -f "$plan_file" ]; then
                nix run .#hostenv-provider -- plan
                git add "$plan_file" >/dev/null 2>&1 || true
              fi

              has_provider_tokens=false
              if [ -f "$provider_secrets" ]; then
                if sops -d "$provider_secrets" 2>/dev/null | yq -e '.provider_node_tokens != null' >/dev/null 2>&1; then
                  has_provider_tokens=true
                fi
              fi

              if [ "$has_provider_tokens" != "true" ]; then
                if [ -t 0 ] && [ -t 1 ]; then
                  if command -v gum >/dev/null 2>&1; then
                    gum confirm \
                      --default=true \
                      --affirmative="Generate" \
                      --negative="Skip" \
                      "Hostenv could not find 'provider_node_tokens' in $provider_secrets. Generate them now?" \
                      && nix run .#hostenv-provider -- node-tokens
                  else
                    echo "Info: provider node tokens are missing. Run: nix run .#hostenv-provider -- node-tokens"
                  fi
                fi
                if [ ! -t 0 ] || [ ! -t 1 ]; then
                  echo "Info: provider node tokens are missing. Run: nix run .#hostenv-provider -- node-tokens"
                fi
              fi

              missing_cache_signing_key=false
              missing_cache_auth_password=false
              missing_cache_public_key=false

              if [ -f "$provider_secrets" ]; then
                if ! sops -d "$provider_secrets" 2>/dev/null | yq -e '.cache_signing_key != null and .cache_signing_key != ""' >/dev/null 2>&1; then
                  missing_cache_signing_key=true
                fi
                if ! sops -d "$provider_secrets" 2>/dev/null | yq -e '.cache_auth_password != null and .cache_auth_password != ""' >/dev/null 2>&1; then
                  missing_cache_auth_password=true
                fi
              fi

              if [ ! -s generated/cache-public-key.txt ]; then
                missing_cache_public_key=true
              fi

              if [ "$missing_cache_signing_key" = "true" ] || [ "$missing_cache_auth_password" = "true" ] || [ "$missing_cache_public_key" = "true" ]; then
                if [ -t 0 ] && [ -t 1 ]; then
                  if command -v gum >/dev/null 2>&1; then
                    gum confirm \
                      --default=true \
                      --affirmative="Generate" \
                      --negative="Skip" \
                      "Hostenv could not find complete cache signing/auth secrets. Generate them now?" \
                      && nix run .#hostenv-provider -- cache-secrets
                  else
                    echo "Info: cache signing/auth secrets are incomplete. Run: nix run .#hostenv-provider -- cache-secrets"
                  fi
                fi
                if [ ! -t 0 ] || [ ! -t 1 ]; then
                  echo "Info: cache signing/auth secrets are incomplete. Run: nix run .#hostenv-provider -- cache-secrets"
                fi
              fi
            '';
          };

          env = [
            {
              name = "DEVSHELL_NO_MOTD";
              value = 1;
            }
          ];
        };
      };

    flake.lib.provider.planPaths = lib.genAttrs config.systems (system:
      config.allSystems.${system}.provider.planPaths);
  };
}
