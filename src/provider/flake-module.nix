# Flake-parts module: provider tooling (plan, deploy, dns-gate, CLI)
{ inputs, lib, config, ... }:
let
  inherit (lib) mkOption types;
  hostenvInput =
    if inputs ? hostenv then inputs.hostenv
    else if inputs ? hostenv-internal then inputs.hostenv-internal
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
    hostenvProjectDir = mkOption {
      type = types.str;
      default = ".hostenv";
      description = "Default flake dir for client projects when dir is absent in flake.lock (e.g. .hostenv).";
    };
    nodesPath = mkOption { type = types.path; default = ./nodes; };
    secretsPath = mkOption { type = types.path; default = ./secrets/secrets.yaml; };
    statePath = mkOption { type = types.path; default = ./generated/state.json; };
    planPath = mkOption { type = types.path; default = ./generated/plan.json; };
    planSource = mkOption { type = types.enum [ "disk" "eval" ]; default = "eval"; };
    goldenPlanPath = mkOption { type = types.nullOr types.path; default = null; };
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
      cfgTop = if config ? provider then config.provider
               else throw ''provider flake module: set the `provider.*` options (e.g. by importing src/provider/flake-module.nix in flake-parts and defining provider.hostenvHostname, nodeFor, nodeSystems, paths, etc.)'';
      pkgsLocal = import inputs.nixpkgs { system = "x86_64-linux"; };
      pkgsAll = inputs.nixpkgs.legacyPackages;

      mkPlan = { system, pkgs' }:
        let
          gen = import ./plan.nix {
            inputs = inputs // { hostenv = hostenvInput; };
            inherit system;
            lib = pkgs'.lib;
            pkgs = pkgs';
            letsEncrypt = cfgTop.letsEncrypt;
            deployPublicKey = cfgTop.deployPublicKey;
            hostenvHostname = cfgTop.hostenvHostname;
            nodeFor = cfgTop.nodeFor;
            nodeSystems = cfgTop.nodeSystems;
            nodesPath = cfgTop.nodesPath;
            secretsPath = cfgTop.secretsPath;
            statePath = cfgTop.statePath;
            cloudflare = cfgTop.cloudflare;
            hostenvProjectDir = cfgTop.hostenvProjectDir;
          };
        in {
          json = if cfgTop.planSource == "eval" then pkgs'.lib.importJSON gen.plan
                 else if builtins.pathExists cfgTop.planPath then pkgs'.lib.importJSON cfgTop.planPath
                 else { };
          drv = gen.plan;
        };

      planTop = mkPlan { system = "x86_64-linux"; pkgs' = pkgsLocal; };
      planJSONTop = planTop.json;
      hasPlanTop = planJSONTop != { };

      nixosSystemTop = node: import ./nixos-system.nix {
        config = planJSONTop;
        inherit node inputs;
        nixpkgs = inputs.nixpkgs;
        pkgs = pkgsAll;
        localSystem = "x86_64-linux";
        nodesPath = cfgTop.nodesPath;
        secretsPath = cfgTop.secretsPath;
        nodeSystems = cfgTop.nodeSystems;
      };

      deployNodesTop = if hasPlanTop then builtins.mapAttrs (n: _: nixosSystemTop n) planJSONTop.nodes else { };

      deployWithTop = node:
        if hasPlanTop then
          pkgsLocal.lib.filterAttrs (name: _env: node == planJSONTop.environments.${name}.node)
            (builtins.mapAttrs
              (name: environment: pkgsLocal.buildEnv {
                inherit name;
                paths = [ inputs.${name}.packages."x86_64-linux".${environment.hostenv.environmentName} ];
              })
              planJSONTop.environments)
        else { };

      deploySpecTop =
        if hasPlanTop then builtins.mapAttrs
          (node: nodeConfig: {
            hostname = node + "." + planJSONTop.hostenvHostname;
            fastConnection = true;
            remoteBuild = true;
            profilesOrder = [ "system" ] ++ builtins.attrNames (deployWithTop node);
            profiles =
              let
                remoteSystem = deployNodesTop.${node}.config.nixpkgs.hostPlatform.system;
              in
              {
                system = {
                  sshUser = "deploy";
                  user = "root";
                  path = inputs.deploy-rs.lib.${remoteSystem}.activate.nixos deployNodesTop.${node};
                };
              } // builtins.mapAttrs
                (name: environment: {
                  user = name;
                  sshUser = name;
                  path = inputs.deploy-rs.lib.${remoteSystem}.activate.custom environment "./bin/activate";
                })
                (deployWithTop node);
            checks = { };
          })
          planJSONTop.nodes
        else { };
    in
    {
  flake = {
    nixosConfigurations = deployNodesTop;
  };

      perSystem = { system, pkgs, config, ... }:
        let
          # Use top-level provider config (already validated) to avoid per-system fallback defaults.
          cfg = cfgTop;

          plan = mkPlan { system = system; pkgs' = pkgs; };
          planJSON = plan.json;
          hasPlan = planJSON != { };

          providerHsDeps = p: [
            p.aeson p.aeson-pretty p.text p.text-conversions p.bytestring
            p.optparse-applicative p.turtle
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
            cloudflare = cfg.cloudflare;
            hostenvProjectDir = cfg.hostenvProjectDir;
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

          nixosSystem = node: import ./nixos-system.nix {
            inherit node inputs;
            config = planJSON;
            nixpkgs = inputs.nixpkgs;
            pkgs = inputs.nixpkgs.legacyPackages;
            localSystem = system;
            nodesPath = cfg.nodesPath;
            secretsPath = cfg.secretsPath;
            nodeSystems = cfg.nodeSystems;
          };

          deployNodes =
            if hasPlan then builtins.mapAttrs (n: _: nixosSystem n) planJSON.nodes else { };

          deployEnvs =
            if hasPlan then builtins.mapAttrs
              (name: environment:
                let
                  inputName = "${environment.hostenv.organisation}__${environment.hostenv.project}";
                in pkgs.buildEnv {
                  inherit name;
                  paths = [ inputs.${inputName}.packages.${system}.${environment.hostenv.environmentName} ];
                })
              planJSON.environments
            else { };

          deployWith = node:
            if hasPlan then
              pkgs.lib.filterAttrs
                (name: _env: node == planJSON.environments.${name}.node)
                deployEnvs
            else { };

          deploySpec =
            if hasPlan then builtins.mapAttrs
              (node: nodeConfig: {
                hostname = node + "." + planJSON.hostenvHostname;
                fastConnection = true;
                remoteBuild = true;
                profilesOrder = [ "system" ] ++ builtins.attrNames (deployWith node);
                profiles =
                  let
                    remoteSystem = deployNodes.${node}.config.nixpkgs.hostPlatform.system;
                  in
                  {
                    system = {
                      sshUser = "deploy";
                      user = "root";
                      path = inputs.deploy-rs.lib.${remoteSystem}.activate.nixos deployNodes.${node};
                    };
                  } // builtins.mapAttrs
                    (name: environment: {
                      user = name;
                      sshUser = name;
                      path = inputs.deploy-rs.lib.${remoteSystem}.activate.custom environment "./bin/activate";
                    })
                    (deployWith node);
                checks = { };
              })
              planJSON.nodes
            else { };

          goldenPlan =
            let
              candidate = cfg.goldenPlanPath;
            in if candidate != null && builtins.pathExists candidate then candidate else null;

          planGoldenCheck =
            if hasPlan && goldenPlan != null then
              pkgs.runCommand "plan-golden" { buildInputs = [ pkgs.diffutils ]; } ''
                cat ${providerGenerator.plan} > plan-current.json
                diff -u ${goldenPlan} plan-current.json
                touch $out
              ''
            else pkgs.writeText "plan-golden-skipped" "no plan or golden; skipped";
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

          checks.plan-golden = planGoldenCheck;
        };
    };
}
