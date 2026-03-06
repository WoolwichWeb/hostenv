{ inputs, lib, config, ... }:
let
  inherit (lib) mkOption types;
  hostenvLib = config.flake.lib.hostenv;
  providerNixosModules = [
    config.flake.modules.nixos."provider-deploy"
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

      userPackages = _userInfo: { };

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
          keysByEnv =
            if builtins.hasAttr "__hostenv_selected_keys" sopsKeys
              && builtins.isAttrs sopsKeys.__hostenv_selected_keys
            then
              sopsKeys.__hostenv_selected_keys
            else
              { };
          orgFromName = name: (environmentWith name).hostenv.organisation;
          orgProjectFromName = name:
            (environmentWith name).hostenv.organisation
            + "_" + (environmentWith name).hostenv.project;
          envOnly = packages.lib.filterAttrs (name: _: builtins.elem name envUsers) userInfo.users.users;
          providerService = userInfo.provider.service or null;
          providerConfigured = providerService != null;
          providerCacheServerEnabled = userInfo.provider.cacheServer.enable or false;
          selectedServiceEnv =
            if providerService == null then
              null
            else
              let
                matches = packages.lib.filterAttrs
                  (_: env:
                    env.hostenv.organisation == providerService.organisation
                    && env.hostenv.project == providerService.project
                    && env.hostenv.environmentName == providerService.environmentName)
                  envsForNode;
                values = builtins.attrValues matches;
              in
              if values == [ ] then null else builtins.head values;
          selectedServiceUser =
            if selectedServiceEnv == null then null else selectedServiceEnv.hostenv.userName;
          providerServiceSecrets =
            if providerConfigured && providerCacheServerEnabled && selectedServiceUser != null then
              {
                "${selectedServiceUser}/provider_node_tokens.yaml" = {
                  owner = selectedServiceUser;
                  group = selectedServiceUser;
                  key = "provider_node_tokens";
                  mode = "0400";
                };
                "${selectedServiceUser}/cache_signing_key" = {
                  owner = selectedServiceUser;
                  group = selectedServiceUser;
                  key = "cache_signing_key";
                  mode = "0400";
                };
                "${selectedServiceUser}/cache_auth_password" = {
                  owner = selectedServiceUser;
                  group = selectedServiceUser;
                  key = "cache_auth_password";
                  mode = "0400";
                };
              }
            else
              { };

          keysForEnv = name:
            if builtins.hasAttr name keysByEnv
              && builtins.isAttrs keysByEnv.${name}
            then
              keysByEnv.${name}
            else
              { };

          keysFromManifest = name: keyGroup:
            let
              envSelection = keysForEnv name;
              raw =
                if builtins.hasAttr keyGroup envSelection
                then envSelection.${keyGroup}
                else [ ];
            in
            if builtins.isList raw
            then builtins.filter builtins.isString raw
            else [ ];

          effectiveKeys = name: keyGroup: secretCfg:
            if !(secretCfg.enable or false)
            then
              [ ]
            else
              let declared = secretCfg.keys or [ ];
              in if declared == [ ] then keysFromManifest name keyGroup else declared;

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

                From the hosting root directory, run `sops secrets/provider.yaml` and add:

                - '${name}/${key}' (this environment) or
                - '${projectBucket}/${key}' (this project) or
                - '${orgBucket}/${key}' (this organisation).
              '';
        in
        {
          sops.secrets =
            (packages.lib.concatMapAttrs
              (
                name: _user:
                  let
                    envCfg = environmentWith name;
                    hostenvCfg = envCfg.hostenv or { };
                    projectSecretKeys = effectiveKeys name "project" (hostenvCfg.projectSecrets or { });
                    envSecretKeys = effectiveKeys name "environment" (envCfg.secrets or { });
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
              envOnly)
            // providerServiceSecrets;
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
    , localSystem
    , nodesPath
    , secretsPath
    , nodeModules ? [ ]
    , nodeSystems ? { }
    }:
    let
      forEachSystem = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed;
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
    letsEncrypt = mkOption { type = types.attrs; default = { adminEmail = "admin@example.invalid"; acceptTerms = true; }; };
    nixSigning = mkOption {
      type = types.submodule {
        options = {
          trustedPublicKeys = mkOption {
            type = types.listOf types.str;
            default = [ ];
            description = "Public Nix signing keys trusted by provider nodes.";
          };
        };
      };
      default = { };
      description = "Nix signing key configuration shared by generated plan and node configuration.";
    };
    deploy = mkOption {
      type = types.submodule {
        options = {
          enable = lib.mkEnableOption "provider-deploy node agent";
          providerApiBaseUrl = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = "Base URL for provider deploy APIs used by node workers.";
          };
          nodeAuthTokenFile = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = "Default path for node bearer token used in provider-deploy requests.";
          };
          nodeAuthTokenFiles = mkOption {
            type = types.attrsOf types.str;
            default = { };
            description = "Optional map of node name -> provider-deploy bearer token file path.";
          };
          reconnectSeconds = mkOption {
            type = types.int;
            default = 5;
            description = "Provider-deploy reconnect delay in seconds.";
          };
        };
      };
      default = { };
      description = "Provider-deploy settings propagated to provider nodes.";
    };
    service = mkOption {
      type = types.nullOr (types.submodule {
        options = {
          organisation = mkOption {
            type = types.str;
            description = "Organisation that owns the provider-service environment.";
          };
          project = mkOption {
            type = types.str;
            description = "Project that owns the provider-service environment.";
          };
          environmentName = mkOption {
            type = types.str;
            description = "Environment name that runs the provider service and receives provider secrets.";
          };
        };
      });
      default = null;
      description = "Provider environment selector used for provider-service secrets.";
    };
    cacheServer = mkOption {
      type = types.submodule {
        options = {
          enable = lib.mkEnableOption "provider-service cache server secret wiring";
        };
      };
      default = { };
      description = "Controls provider-service cache secret projection to the selected service environment.";
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



    nodes = mkOption {
      type = types.attrsOf (types.submodule {
        options.configuration = mkOption {
          type = types.path;
        };
      });
      default = {};
      description = "Declarative node configuration. Each attribute defines a node with a configuration.nix path.";
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
    plan.autoInit = (lib.mkEnableOption "automatically setup necessary config for tracking state and secrets") // { default = true; };
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
    nixSigning = config.provider.nixSigning;
    deploy = config.provider.deploy;
  };
}
