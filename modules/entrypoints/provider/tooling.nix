{ inputs, lib, config, ... }:
let
  flakeParts = inputs.flake-parts.lib;
  cfg = config.provider;
  providerPlan = config.flake.lib.provider.plan;

  hostenvInput =
    if inputs ? hostenv then inputs.hostenv
    else if inputs ? self then inputs.self
    else throw "provider tooling requires a hostenv input";

  hostenvRoot = hostenvInput.outPath;
  providerRoot = hostenvRoot + "/provider";
  cliPath = providerRoot + "/cli.hs";

in
{
  options.perSystem = flakeParts.mkPerSystemOption ({ lib, ... }: {
    options.provider.haskellDevPackages = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [
          "aeson"
          "aeson-pretty"
          "text"
          "text-conversions"
          "bytestring"
          "optparse-applicative"
          "turtle"
        ];
      description = "Haskell package names to include in the dev shell for provider tooling.";
    };
  });

  config.perSystem = { system, pkgs, config, ... }:
    lib.mkIf cfg.enable
      (let
        providerHsDeps = p: map (name: p.${name}) config.provider.haskellDevPackages;
        providerGhc = pkgs.haskellPackages.ghcWithPackages providerHsDeps;
        cliSrc = builtins.path { path = cliPath; name = "hostenv-provider-cli"; };
        hostenvProviderCLI = pkgs.writeShellApplication {
          name = "hostenv-provider";
          runtimeInputs = [ pkgs.jq ];
          text = ''
            set -euo pipefail
            exec ${providerGhc}/bin/runghc ${cliSrc} "$@"
          '';
        };

        providerGenerator =
          if cfg.enable then
            providerPlan {
              inputs = inputs // { hostenv = hostenvInput; };
              inherit system;
              lib = pkgs.lib;
              pkgs = pkgs;
              letsEncrypt = cfg.letsEncrypt;
              deployPublicKeys = cfg.deployPublicKeys;
              hostenvHostname = cfg.hostenvHostname;
              nodeFor = cfg.nodeFor;
              nodeSystems = cfg.nodeSystems;
              nodeModules = cfg.nodeModules;
              statePath = cfg.statePath;
              planPath = cfg.planPath;
              cloudflare = cfg.cloudflare;
              planSource = cfg.planSource;
              generatedFlake = cfg.generatedFlake;
            }
          else
            null;

        hostenvProviderPlan =
          if cfg.enable then
            pkgs.writeShellScriptBin "hostenv-provider-plan" ''
              set -euo pipefail
              dest=generated
              mkdir -p "$dest"

              # The following are:
              # - a sub-Flake with per-environment inputs (inputs in
              #   provider Flakes are per-project);
              cp ${providerGenerator.flake} "$dest/flake.nix"

              # - state retained between runs, like unique UNIX user uids.
              cat ${providerGenerator.state} > "$dest/state.json"

              # - a plan for the next deployment, containing Nix config and
              #   and environment metadata. This file is more ephemeral than
              #   the other two, and does not need to be committed to git (but
              #   still needs to be git add-ed for Nix flakes to see it).)
              ${pkgs.jq}/bin/jq -S '.' ${providerGenerator.plan} > "$dest/plan.json"

              chmod ug+w "$dest"/{state.json,plan.json,flake.nix}
              (cd "$dest" && nix --extra-experimental-features "nix-command flakes" flake update)
              git add "$dest" || true

              echo "✅ Infrastructure configuration written to $dest"
            ''
          else
            pkgs.writeShellScriptBin "hostenv-provider-plan" ''
              echo "hostenv-provider-plan: provider configuration is missing." >&2
              echo "Import hostenv modules via hostenv.flakeModules.provider, then set provider.* options." >&2
              exit 1
            '';
      in
      {
        packages = {
          hostenv-provider = hostenvProviderCLI;
          hostenv-provider-plan = hostenvProviderPlan;
        };

        apps.hostenv-provider-plan = {
          type = "app";
          program = "${hostenvProviderPlan}/bin/hostenv-provider-plan";
          meta.description = "Generate provider plan/state/flake";
        };

        # Add to dev packages for hostenv developers.
        hostenv.haskell.devPackages = lib.mkDefault (
          [ "haskell-language-server" ] ++ config.provider.haskellDevPackages
        );

        devshells.default = {
          devshell.packages = [
            hostenvProviderCLI
            hostenvProviderPlan
            providerGhc
            pkgs.sops
            pkgs.age
            pkgs.jq
            pkgs.bind
            pkgs.deploy-rs
            pkgs.postgresql
            pkgs.haskellPackages.cabal-install
          ];
          env = [
            {
              name = "DEVSHELL_NO_MOTD";
              value = 1;
            }
          ];
        };
      });
}
