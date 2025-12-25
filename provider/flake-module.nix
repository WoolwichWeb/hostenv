# Flake-parts module: provider tooling (plan, deploy, dns-gate, CLI)
{ inputs, lib, config, ... }:
let
  inherit (lib) mkOption types;
  fp = inputs.flake-parts.lib;
  hostenvInput =
    if inputs ? hostenv then inputs.hostenv
    else if inputs ? hostenv-platform then inputs.hostenv-platform
    else throw "provider module requires a hostenv input";
in
{
  options = {
    provider = {
      hostenvHostname = mkOption { type = types.str; default = "example.invalid"; description = "Hostenv control-plane hostname (must be set by provider)."; };
      letsEncrypt = mkOption { type = types.attrs; default = { adminEmail = "admin@example.invalid"; acceptTerms = true; }; };
      deployPublicKey = mkOption { type = types.nullOr types.str; default = null; description = "SSH public key for deploy user; must be set by provider."; };
      warnInvalidDeployKey = mkOption { type = types.bool; default = true; description = "Warn when deployPublicKey is invalid or empty."; };
      nodeFor = mkOption {
        type = types.attrs;
        default = { default = null; };
      };
      nodeSystems = mkOption {
        type = types.attrs;
        default = { };
        description = "Map of node name -> system string (e.g. x86_64-linux, aarch64-linux).";
      };
      statePath = mkOption {
        type = types.path;
        default =
          if inputs ? self
          then inputs.self + /generated/state.json
          else builtins.throw "provider.statePath: inputs.self is required to resolve defaults; set provider.statePath explicitly.";
      };
      planPath = mkOption {
        type = types.path;
        default =
          if inputs ? self
          then inputs.self + /generated/plan.json
          else builtins.throw "provider.planPath: inputs.self is required to resolve defaults; set provider.planPath explicitly.";
      };
      planSource = mkOption { type = types.enum [ "disk" "eval" ]; default = "eval"; };
      cloudflare = mkOption {
        type = types.submodule {
          options = {
            enable = mkOption { type = types.bool; default = false; };
            zoneId = mkOption { type = types.nullOr types.str; default = null; };
            apiTokenFile = mkOption { type = types.nullOr types.path; default = null; };
          };
        };
        default = { enable = false; zoneId = null; apiTokenFile = null; };
      };
    };

    perSystem = fp.mkPerSystemOption ({ lib, ... }: {
      options.provider.haskellDevPackages = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Extra Haskell package names to include in the dev shell for provider tooling.";
      };
    });

  };

  config =
    let
      cfgTop =
        if config ? provider then config.provider
        else throw ''provider flake module: set the `provider.*` options (e.g. by importing provider/flake-module.nix in flake-parts and defining provider.hostenvHostname, nodeFor, nodeSystems, paths, etc.)'';
    in
    {
      flake.lib.provider.nixosSystem = ./nixos-system.nix;
      flake.lib.provider.deployOutputs = ./deploy-outputs.nix;
      flake.flakeModules.provider = ./flake-module.nix;

      perSystem = { system, pkgs, ... }:
        let
          cfg = cfgTop;
          providerHsPackageNames = [
            "aeson"
            "aeson-pretty"
            "text"
            "text-conversions"
            "bytestring"
            "optparse-applicative"
            "turtle"
          ];
          providerHsDeps = p: map (name: p.${name}) providerHsPackageNames;
          providerGhc = pkgs.haskellPackages.ghcWithPackages providerHsDeps;

          providerGenerator = import ./plan.nix {
            inputs = inputs // { hostenv = hostenvInput; };
            inherit system;
            lib = pkgs.lib;
            pkgs = pkgs;
            letsEncrypt = cfg.letsEncrypt;
            deployPublicKey = cfg.deployPublicKey;
            warnInvalidDeployKey = cfg.warnInvalidDeployKey;
            hostenvHostname = cfg.hostenvHostname;
            nodeFor = cfg.nodeFor;
            nodeSystems = cfg.nodeSystems;
            statePath = cfg.statePath;
            planPath = cfg.planPath;
            cloudflare = cfg.cloudflare;
            planSource = cfg.planSource;
          };

          hostenvProviderPlan = pkgs.writeShellScriptBin "hostenv-provider-plan" ''
            set -euo pipefail
            dest=generated
            mkdir -p "$dest"
            
            # The following are:
            # - a sub-Flake with per-environment inputs (inputs in this Flake are per-project);
            cp ${providerGenerator.flake} "$dest/flake.nix"
            
            # - state retained between runs, like unique UNIX user uids.
            cat ${providerGenerator.state} > "$dest/state.json"
            
            # - a plan for the next deployment, containing Nix config and
            #   and environment metadata. This file is more ephemeral than
            #   the other two, and does not need to be committed to git (but
            #   still needs to be git add-ed for Nix flakes to see it).)
            cat ${providerGenerator.plan} > "$dest/plan.json"

            chmod ug+w "$dest"/{state.json,plan.json,flake.nix}
            (cd "$dest" && nix --extra-experimental-features "nix-command flakes" flake update)
            git add "$dest" || true

            echo "✅ Infrastructure configuration written to $dest"
          '';

          cliSrc = builtins.path { path = ./cli.hs; name = "hostenv-provider-cli"; };

          hostenvProviderCLI = pkgs.writeShellScriptBin "hostenv-provider" ''
            set -euo pipefail
            exec ${providerGhc}/bin/runghc ${cliSrc} "$@"
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

          checks = { };

          provider.haskellDevPackages = providerHsPackageNames;
        };
    };
}
