# Flake-parts module: provider tooling (plan, deploy, dns-gate, CLI)
{ inputs, lib, config, ... }:
let
  inherit (lib) mkOption types;
  hostenvInput =
    if inputs ? hostenv then inputs.hostenv
    else if inputs ? hostenv-platform then inputs.hostenv-platform
    else throw "provider module requires a hostenv input";
in
{
  options.provider = {
    hostenvHostname = mkOption { type = types.str; default = "example.invalid"; description = "Hostenv control-plane hostname (must be set by provider)."; };
    letsEncrypt = mkOption { type = types.attrs; default = { adminEmail = "admin@example.invalid"; acceptTerms = true; }; };
    deployPublicKey = mkOption { type = types.str; default = ""; description = "SSH public key for deploy user; must be set by provider."; };
    nodeFor = mkOption {
      type = types.attrs;
      default = { default = null; };
    };
    nodeSystems = mkOption {
      type = types.attrs;
      default = { };
      description = "Map of node name -> system string (e.g. x86_64-linux, aarch64-linux).";
    };
    # Default to the repository root `nodes/` directory; provider repos can override.
    nodesPath = mkOption {
      type = types.path;
      default =
        if inputs ? self
        then inputs.self + /nodes
        else builtins.throw "provider.nodesPath: inputs.self is required to resolve defaults; set provider.nodesPath explicitly.";
    };
    secretsPath = mkOption {
      type = types.path;
      default =
        if inputs ? self
        then inputs.self + /secrets/secrets.yaml
        else builtins.throw "provider.secretsPath: inputs.self is required to resolve defaults; set provider.secretsPath explicitly.";
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

  config =
    let
      cfgTop =
        if config ? provider then config.provider
        else throw ''provider flake module: set the `provider.*` options (e.g. by importing provider/flake-module.nix in flake-parts and defining provider.hostenvHostname, nodeFor, nodeSystems, paths, etc.)'';
    in
    {

      perSystem = { system, pkgs, config, ... }:
        let
          cfg = cfgTop;

          providerHsDeps = p: [
            p.aeson
            p.aeson-pretty
            p.text
            p.text-conversions
            p.bytestring
            p.optparse-applicative
            p.turtle
          ];
          providerGhc = pkgs.haskellPackages.ghcWithPackages providerHsDeps;

          providerGenerator = import ./plan.nix {
            inputs = inputs // { hostenv = hostenvInput; };
            inherit system;
            lib = pkgs.lib;
            pkgs = pkgs;
            letsEncrypt = cfg.letsEncrypt;
            deployPublicKey = cfg.deployPublicKey;
            hostenvHostname = cfg.hostenvHostname;
            nodeFor = cfg.nodeFor;
            nodeSystems = cfg.nodeSystems;
            nodesPath = cfg.nodesPath;
            secretsPath = cfg.secretsPath;
            statePath = cfg.statePath;
            planPath = cfg.planPath;
            cloudflare = cfg.cloudflare;
            planSource = cfg.planSource;
          };

          hostenvProviderPlan = pkgs.writeShellScriptBin "hostenv-provider-plan" ''
            set -euo pipefail
            dest=''${HOSTENV_PROVIDER_OUT:-generated}
            mkdir -p "$dest"
            cp ${providerGenerator.flake} "$dest/flake.nix"
            cat ${providerGenerator.state} > "$dest/state.json"
            cat ${providerGenerator.plan} > "$dest/plan.json"
            chmod ug+w "$dest"/{state.json,plan.json,flake.nix}
            (cd "$dest" && nix --extra-experimental-features "nix-command flakes" flake lock)
            git add "$dest" || true
            echo "✅ provider plan/state/flake written to $dest"
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
        };
    };
}
