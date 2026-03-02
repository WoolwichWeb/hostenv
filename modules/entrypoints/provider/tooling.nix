{ inputs, lib, config, ... }:
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
          runtimeInputs = [ pkgs.jq pkgs.sops pkgs.yq-go pkgs.openssl ];
          text = ''
            set -euo pipefail
            exec ${cliPkg}/bin/hostenv-provider "$@"
          '';
        };

        providerGenerator =
          providerPlan
            {
              inputs = inputs // { hostenv = hostenvInput; };
              inherit system;
              lib = pkgs.lib;
              pkgs = pkgs;
              letsEncrypt = cfg.letsEncrypt;
              nixSigning = cfg.nixSigning;
              comin = cfg.comin;
              hostenvHostname = cfg.hostenvHostname;
              nodeFor = cfg.nodeFor;
              nodeSystems = cfg.nodeSystems;
              nodeModules = cfg.nodeModules;
              statePath = cfg.statePath;
              planPath = cfg.planPath;
              cloudflare = cfg.cloudflare;
              planSource = cfg.planSource;
              generatedFlake = cfg.generatedFlake;
              service = cfg.service;
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
            pkgs.bind
            pkgs.postgresql
            pkgs.haskellPackages.cabal-install
          ];

          devshell.startup.comin-tokens = lib.mkIf cfg.comin.enable {
            text = ''
              provider_secrets="secrets/provider.yaml"
              plan_file="generated/plan.json"

              if [ ! -f "$plan_file" ]; then
                mkdir -p generated
                printf '{}\n' >|"$plan_file"
                git add "$plan_file" >/dev/null 2>&1 || true
              fi

              has_comin_tokens=false
              if [ -f "$provider_secrets" ]; then
                if sops -d "$provider_secrets" 2>/dev/null | yq -e '.comin_node_tokens != null' >/dev/null 2>&1; then
                  has_comin_tokens=true
                fi
              fi

              if [ "$has_comin_tokens" != "true" ]; then
                if [ -t 0 ] && [ -t 1 ]; then
                  if command -v gum >/dev/null 2>&1; then
                    gum confirm \
                      --default=true \
                      --affirmative="Generate" \
                      --negative="Skip" \
                      "Hostenv could not find 'comin_node_tokens' in $provider_secrets. Generate them now?" \
                      && nix run .#hostenv-provider -- comin-tokens
                  else
                    echo "Info: comin tokens are missing. Run: nix run .#hostenv-provider -- comin-tokens"
                  fi
                fi
                if [ ! -t 0 ] || [ ! -t 1 ]; then
                  echo "Info: comin tokens are missing. Run: nix run .#hostenv-provider -- comin-tokens"
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
