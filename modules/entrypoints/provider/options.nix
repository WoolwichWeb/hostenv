{ inputs, lib, config, ... }:
let
  inherit (lib) mkOption types;
  hostenvLib = config.flake.lib.hostenv;
  providerNixosModules = [
    config.flake.modules.nixos.provider-common
    config.flake.modules.nixos.hostenv-top-level
    config.flake.modules.nixos.nginx-frontdoor
    config.flake.modules.nixos.nginx-tuning
    config.flake.modules.nixos.monitoring
  ];

  providerNixosSystem =
    { config
    , nixpkgs
    , pkgs
    , node
    , inputs
    , localSystem
    , nodesPath
    , secretsPath
    , nodeModules ? [ ]
    , nodeSystems ? { }
    , ...
    }:
    let
      plan = config;
      nodesBase =
        if builtins.isPath nodesPath
        then nodesPath
        else builtins.path { path = nodesPath; name = "nodes"; };
      nodePath =
        if builtins.typeOf node == "path"
        then node
        else (nodesBase + "/${node}");

      system =
        if builtins.hasAttr node nodeSystems then nodeSystems.${node}
        else builtins.throw "No system specified for node '${node}' (set provider.nodeSystems.${node})";

      nodeConfig = plan.nodes.${node};
      envsAll = plan.environments or { };
      envsForNode = lib.filterAttrs (_: env: (env.node or null) == node) envsAll;
      environmentWith = userName: envsForNode.${userName};
      packages = pkgs.${system};
      envUsers = builtins.attrNames envsForNode;

      hostenvEnvModule = {
        hostenv = {
          environments = envsForNode;
          defaultEnvironment = plan.defaultEnvironment or "main";
        };
      };

      userPackages = userInfo:
        let
          envOnly = packages.lib.filterAttrs (name: _: builtins.elem name envUsers) userInfo.users.users;
        in
        {
          users.users = packages.lib.concatMapAttrs
            (name: _user:
              let
                userPackage = inputs.${name}.packages.${system}.${(environmentWith name).hostenv.environmentName};
              in
              { ${name}.packages = [ userPackage ]; }
            )
            envOnly;
        };

      envUserMismatch =
        let
          envs = envsForNode;
          mismatched =
            packages.lib.filterAttrs
              (name: env: (env.hostenv.userName or name) != name)
              envs;
        in
        builtins.attrNames mismatched;

      sopsSecrets = userInfo:
        let
          hostPkgs = pkgs.${localSystem};
          readYaml = hostenvLib.readYaml;
          sopsKeys = readYaml hostPkgs secretsPath;
          orgFromName = name: (environmentWith name).hostenv.organisation;
          orgProjectFromName = name:
            (environmentWith name).hostenv.organisation
            + "_" + (environmentWith name).hostenv.project;
          envOnly = packages.lib.filterAttrs (name: _: builtins.elem name envUsers) userInfo.users.users;

          scopeKeys = scope:
            if (scope.enable or false) then (scope.keys or [ ]) else [ ];

          hasSecretKey = bucket: key:
            builtins.hasAttr bucket sopsKeys
            && builtins.isAttrs sopsKeys.${bucket}
            && builtins.hasAttr key sopsKeys.${bucket};

          resolveSecretSource = name: key:
            let
              projectBucket = orgProjectFromName name;
              orgBucket = orgFromName name;
            in
            if hasSecretKey name key then
              name
            else if hasSecretKey projectBucket key then
              projectBucket
            else if hasSecretKey orgBucket key then
              orgBucket
            else
              throw ''
                The secrets file does not contain '${key}' for '${name}'.

                From the hosting root directory, run `sops secrets/secrets.yaml` and add:

                - '${name}/${key}' (this environment) or
                - '${projectBucket}/${key}' (this project) or
                - '${orgBucket}/${key}' (this organisation).
              '';
        in
        {
          sops.secrets = packages.lib.concatMapAttrs
            (
              name: _user:
                let
                  envCfg = environmentWith name;
                  hostenvCfg = envCfg.hostenv or { };
                  projectSecretKeys = scopeKeys (hostenvCfg.projectSecrets or { });
                  envSecretKeys = scopeKeys ((envCfg.secrets or { }) // (hostenvCfg.secrets or { }));
                  secretKeys = lib.unique ([ "backups_secret" "backups_env" ] ++ projectSecretKeys ++ envSecretKeys);
                in
                builtins.listToAttrs (map
                  (secretKey: {
                    name = "${name}/${secretKey}";
                    value = {
                      owner = name;
                      group = name;
                      key = "${resolveSecretSource name secretKey}/${secretKey}";
                    };
                  })
                  secretKeys)
            )
            envOnly;
        };

    in
    if envUserMismatch == [ ] then
      nixpkgs.lib.nixosSystem
        {
          inherit system;
          specialArgs = { inherit inputs system; };
          modules =
            [
              {
                _module.args = {
                  inherit inputs system;
                };
              }
              inputs.sops-nix.nixosModules.sops
              { sops.defaultSopsFile = secretsPath; }
            ]
            ++ providerNixosModules
            ++ [
              hostenvEnvModule
            ]
            ++ nodeModules
            ++ [
              (nodePath + "/configuration.nix")
              nodeConfig
              (sopsSecrets nodeConfig)
              (userPackages nodeConfig)
            ];
        }
    else
      throw "hostenv provider: environment keys must match hostenv.userName (mismatched: ${builtins.toString envUserMismatch})";

  providerDeployOutputs =
    { inputs
    , config
    , nixpkgs
    , deploy-rs
    , systems
    , localSystem
    , nodesPath
    , secretsPath
    , nodeModules ? [ ]
    , nodeSystems ? { }
    , nodeAddresses ? { }
    , nodeSshPorts ? { }
    , nodeSshOpts ? { }
    , nodeRemoteBuild ? { }
    , nodeMagicRollback ? { }
    , nodeAutoRollback ? { }
    }:
    let
      forEachSystem = nixpkgs.lib.genAttrs (import systems);
      pkgs = forEachSystem (system: import nixpkgs { inherit system; });

      nixosSystem = node: providerNixosSystem {
        inherit config node nixpkgs pkgs inputs localSystem nodesPath secretsPath nodeSystems nodeModules;
      };

      nodes = builtins.mapAttrs
        (node: _: nixosSystem node)
        config.nodes;

      environments = forEachSystem (system: builtins.mapAttrs
        (name: environment: pkgs.${system}.buildEnv {
          inherit name;
          paths = [ inputs.${name}.packages.${system}.${environment.hostenv.environmentName} ];
        })
        config.environments
      );

      environmentsWith = node: forEachSystem (system: (
        nixpkgs.lib.filterAttrs
          (name: environment: node == config.environments.${name}.node)
          environments.${system}
      ));
    in
    {
      inherit inputs config environmentsWith;

      packages = forEachSystem (system:
        let
          nodeSystemOf = sys: sys.pkgs.stdenv.hostPlatform.system;

          envPkgs = nixpkgs.lib.mapAttrs'
            (name: drv: { name = "env-${name}"; value = drv; })
            environments.${system};

          nodePkgs = nixpkgs.lib.mapAttrs'
            (node: sys: {
              name = "node-${node}";
              value = sys.config.system.build.toplevel;
            })
            (nixpkgs.lib.filterAttrs
              (_: sys: nodeSystemOf sys == system)
              nodes);
        in
        envPkgs // nodePkgs
      );

      deploy.nodes = builtins.mapAttrs
        (node: _: {
          # Allow local demos and non-standard network topologies to override
          # default `<node>.<hostenvHostname>` SSH addressing.
          hostname =
            if builtins.hasAttr node nodeAddresses
            then nodeAddresses.${node}
            else node + "." + config.hostenvHostname;
          # Keep port override and raw ssh options separate so callers can use
          # either `nodeSshPorts` or `nodeSshOpts` without duplicating flags.
          sshOpts =
            let
              portOpts =
                if builtins.hasAttr node nodeSshPorts
                then [ "-p" (builtins.toString nodeSshPorts.${node}) ]
                else [ ];
              extraOpts =
                if builtins.hasAttr node nodeSshOpts
                then nodeSshOpts.${node}
                else [ ];
            in
            portOpts ++ extraOpts;
          fastConnection = true;
          remoteBuild =
            if builtins.hasAttr node nodeRemoteBuild
            then nodeRemoteBuild.${node}
            else false;
          magicRollback =
            if builtins.hasAttr node nodeMagicRollback
            then nodeMagicRollback.${node}
            else true;
          autoRollback =
            if builtins.hasAttr node nodeAutoRollback
            then nodeAutoRollback.${node}
            else true;
          profilesOrder = [ "system" ] ++ builtins.attrNames (environmentsWith node).${localSystem};
          profiles =
            let
              remoteSystem = nodes.${node}.pkgs.stdenv.hostPlatform.system;
            in
            {
              system = {
                sshUser = config.deployUser or "deploy";
                user = "root";
                path = deploy-rs.lib.${remoteSystem}.activate.nixos nodes.${node};
              };
            } // builtins.mapAttrs
              (name: environment: {
                user = name;
                # Environment activation must run in a real user session so
                # systemd --user units can start reliably.
                sshUser = name;
                path = deploy-rs.lib.${remoteSystem}.activate.custom environment "./bin/activate";
              })
              (environmentsWith node).${remoteSystem};
          checks = { };
        })
        config.nodes;

      nixosConfigurations = nodes;
    };
in
{
  options.provider = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Enable provider outputs and tooling.";
    };

    hostenvHostname = mkOption {
      type = types.str;
      default = "example.invalid";
      description = "Hostenv control-plane hostname (must be set by provider).";
    };
    deployUser = mkOption {
      type = types.str;
      default = "deploy";
      description = "SSH/sudo user used for deploy-rs operations.";
    };
    letsEncrypt = mkOption { type = types.attrs; default = { adminEmail = "admin@example.invalid"; acceptTerms = true; }; };
    deployPublicKeys = mkOption { type = types.listOf types.str; default = [ ]; description = "SSH public keys for deploy user; must be set by provider."; };
    nixSigning = mkOption {
      type = types.submodule {
        options = {
          trustedPublicKeys = mkOption {
            type = types.listOf types.str;
            default = [ ];
            description = "Public Nix signing keys trusted by provider nodes for deploy-rs copy checks.";
          };
        };
      };
      default = { };
      description = "Nix signing key configuration shared by generated plan and node configuration.";
    };
    nodeFor = mkOption {
      type = types.attrs;
      default = { default = null; };
    };
    nodeSystems = mkOption {
      type = types.attrs;
      default = { };
      description = "Map of node name -> system string (e.g. x86_64-linux, aarch64-linux).";
    };
    nodeAddresses = mkOption {
      type = types.attrsOf types.str;
      default = { };
      description = "Optional map of node name -> SSH hostname/IP override for deploy-rs.";
    };
    nodeSshPorts = mkOption {
      type = types.attrsOf types.int;
      default = { };
      description = "Optional map of node name -> SSH port override for deploy-rs.";
    };
    nodeSshOpts = mkOption {
      type = types.attrsOf (types.listOf types.str);
      default = { };
      description = "Optional map of node name -> extra SSH options for deploy-rs.";
    };
    nodeRemoteBuild = mkOption {
      type = types.attrsOf types.bool;
      default = { };
      description = "Optional map of node name -> whether deploy-rs should build on the remote host.";
    };
    nodeMagicRollback = mkOption {
      type = types.attrsOf types.bool;
      default = { };
      description = "Optional map of node name -> deploy-rs magicRollback override.";
    };
    nodeAutoRollback = mkOption {
      type = types.attrsOf types.bool;
      default = { };
      description = "Optional map of node name -> deploy-rs autoRollback override.";
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

  config.flake.lib.provider = {
    nixosModules = providerNixosModules;
    nixosSystem = providerNixosSystem;
    deployOutputs = providerDeployOutputs;
    deployPublicKeys = config.provider.deployPublicKeys;
    deployUser = config.provider.deployUser;
    nixSigning = config.provider.nixSigning;
  };
}
