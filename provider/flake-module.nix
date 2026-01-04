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
      deployPublicKeys = mkOption { type = types.listOf types.str; default = [ ]; description = "SSH public keys for deploy user; must be set by provider."; };
      nodeFor = mkOption {
        type = types.attrs;
        default = { default = null; };
      };
      nodeSystems = mkOption {
        type = types.attrs;
        default = { };
        description = "Map of node name -> system string (e.g. x86_64-linux, aarch64-linux).";
      };
      nodeModules = mkOption {
        type = types.listOf (types.oneOf [ types.path types.str ]);
        default = [ ];
        description = "Extra NixOS modules applied to every node. Strings are paths relative to the provider root.";
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
      generatedFlake = mkOption {
        type = types.submodule {
          options = {
            inputs = mkOption {
              type = types.attrsOf types.unspecified;
              default = { };
              description = "Extra inputs injected into generated/flake.nix (appended to allow overrides).";
            };
            envInputs = mkOption {
              type = types.submodule {
                options = {
                  follows = mkOption {
                    type = types.nullOr (types.attrsOf types.str);
                    default = null;
                    description = "Override the inputs.<name>.follows mapping for every environment input (null = default mapping).";
                  };
                  extra = mkOption {
                    type = types.functionTo (types.attrsOf types.unspecified);
                    default = _: { };
                    description = "Function env -> attrs merged into each environment input spec.";
                  };
                };
              };
              default = { };
            };
          };
        };
        default = { };
        description = "Customization for generated/flake.nix inputs and per-environment inputs.";
      };
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
      flake.lib.provider.deployPublicKeys = cfgTop.deployPublicKeys;
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
          };

          hostenvProviderPlan = pkgs.writeShellScriptBin "hostenv-provider-plan" ''
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
