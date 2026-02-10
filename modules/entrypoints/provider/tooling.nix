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
        providerGhc = pkgs.haskellPackages.ghcWithPackages providerHsDeps;
        cliPkg = pkgs.haskellPackages.callCabal2nix "hostenv-provider-cli" providerRoot { };
        hostenvProviderCLI = pkgs.writeShellApplication {
          name = "hostenv-provider";
          runtimeInputs = [ pkgs.jq ];
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
              deployPublicKeys = cfg.deployPublicKeys;
              hostenvHostname = cfg.hostenvHostname;
              nodeFor = cfg.nodeFor;
              nodeSystems = cfg.nodeSystems;
              nodeAddresses = cfg.nodeAddresses;
              nodeSshPorts = cfg.nodeSshPorts;
              nodeSshOpts = cfg.nodeSshOpts;
              nodeRemoteBuild = cfg.nodeRemoteBuild;
              nodeMagicRollback = cfg.nodeMagicRollback;
              nodeAutoRollback = cfg.nodeAutoRollback;
              nodeModules = cfg.nodeModules;
              statePath = cfg.statePath;
              planPath = cfg.planPath;
              cloudflare = cfg.cloudflare;
              planSource = cfg.planSource;
              generatedFlake = cfg.generatedFlake;
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
      };

    flake.lib.provider.planPaths = lib.genAttrs config.systems (system:
      config.allSystems.${system}.provider.planPaths);
  };
}
