{ pkgs, inputs }:
let
  lib = pkgs.lib;
  asserts = import ../support/assert.nix { inherit pkgs lib; };

  system = pkgs.stdenv.hostPlatform.system;
  envName = "acme__demo-main";
  hostName = "${envName}.hostenv.test";
  nodeName = "node-a";
  trustedSigningKey = "hostenv-provider-test-1:AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  cacheSigningKey = "hostenv-cache-test-1:BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB=";

  nodesPath = pkgs.runCommand "nodes-stub" { } ''
    mkdir -p "$out/${nodeName}"
    cat > "$out/${nodeName}/configuration.nix" <<'EOF'
    { ... }: {
      system.stateVersion = "24.11";
      networking.hostName = "node-a";
    }
    EOF
  '';

  secretsPath = pkgs.writeText "secrets.yaml" ''
    ${envName}:
      backups_secret: "dummy"
      backups_env: "dummy"
      env_only: "env"
      env_provider_only: "provider-env-only"
    acme_demo:
      project_only: "project"
      project_provider_only: "provider-project-only"
    acme:
      org_only: "org"
    provider_node_tokens:
      ${nodeName}: "node-token"
    cache_auth_password: "cache-password"
    __hostenv_selected_keys:
      ${envName}:
        project:
          - "project_only"
        environment:
          - "env_only"
  '';

  envInput = {
    packages.${system}.main = pkgs.hello;
  };

  nodeSystems = { "${nodeName}" = system; };

  config = {
    nixSigning.trustedPublicKeys = [ trustedSigningKey ];
    nodes = {
      "${nodeName}" = {
        users.users = {
          "${envName}" = { };
        };
        provider = {
          nixSigning.trustedPublicKeys = [ trustedSigningKey ];
          service = {
            organisation = "acme";
            project = "demo";
            environmentName = "main";
          };
          deploy = {
            enable = true;
            providerApiBaseUrl = "https://hosting.test";
            nodeName = nodeName;
            nodeAuthTokenFile = "/run/secrets/hostenv/provider_node_token";
            reconnectSeconds = 5;
          };
          cache = {
            enable = true;
            url = "https://hosting.test/cache";
            publicKey = cacheSigningKey;
            netrcFile = "/run/secrets/hostenv/cache_netrc";
          };
        };
      };
    };
    environments = {
      "${envName}" = {
        enable = true;
        type = "production";
        uid = 1001;
        node = nodeName;
        secrets = {
          enable = true;
          keys = [ ];
        };
        hostenv = {
          organisation = "acme";
          project = "demo";
          environmentName = "main";
          userName = envName;
          hostname = hostName;
          hostenvHostname = "hosting.test";
          upstreamRuntimeDir = "/run/hostenv/nginx/${envName}";
          root = "/src/demo";
          projectSecrets = {
            enable = true;
            keys = [ ];
          };
        };
      };
    };
    defaultEnvironment = "main";
  };

  configMismatch = config // {
    environments = config.environments // {
      "${envName}" = (config.environments.${envName} // {
        hostenv = (config.environments.${envName}.hostenv // { userName = "wrong-user"; });
      });
    };
  };

  providerFlake = inputs.flake-parts.lib.mkFlake { inherit inputs; } {
    systems = [ system ];
    imports =
      let
        modules = inputs.import-tree ../../modules;
        moduleList = if builtins.isList modules then modules else [ modules ];
      in
      [ inputs.devshell.flakeModule ] ++ moduleList;
    provider.enable = true;
    project.enable = false;
    provider = {
      hostenvHostname = "hosting.test";
      nixSigning.trustedPublicKeys = [ trustedSigningKey ];
      nodeFor = { default = nodeName; };
      nodeSystems = nodeSystems;
      service = {
        organisation = "acme";
        project = "demo";
        environmentName = "main";
      };
      deploy = {
        enable = true;
        providerApiBaseUrl = "https://hosting.test";
        nodeAuthTokenFile = "/run/secrets/hostenv/provider_node_token";
        reconnectSeconds = 5;
      };
    };
  };

  pkgsBySystem = lib.genAttrs [ system ] (_: pkgs);
  inputsForSystem = inputs // {
    "${envName}" = envInput;
    parent = providerFlake;
  };

  nixosSystem = providerFlake.lib.provider.nixosSystem;
  systemEval = nixosSystem {
    inherit config nodeSystems nodesPath secretsPath;
    node = nodeName;
    inputs = inputsForSystem;
    nixpkgs = inputs.nixpkgs;
    pkgs = pkgsBySystem;
    localSystem = system;
  };

  systemMismatch = builtins.tryEval (nixosSystem {
    config = configMismatch;
    inherit nodeSystems nodesPath secretsPath;
    node = nodeName;
    inputs = inputsForSystem;
    nixpkgs = inputs.nixpkgs;
    pkgs = pkgsBySystem;
    localSystem = system;
  });

  makeHostenv = providerFlake.makeHostenv.${system};
  providerServiceMismatchModules = [
    ({ ... }: {
      hostenv = {
        organisation = "acme";
        project = "demo";
        hostenvHostname = "hosting.test";
        root = ./.;
      };

      environments.main = {
        enable = true;
        type = "testing";
      };

      services.hostenv-provider.enable = true;
      services.hostenv-provider.deploy.enable = true;
    })
  ];

  providerServiceMismatchEnvNoCheck = makeHostenv (providerServiceMismatchModules ++ [ ({ ... }: { _module.check = false; }) ]) null;
  providerServiceMismatchEval =
    let env = makeHostenv providerServiceMismatchModules null;
    in builtins.tryEval env.config.activate;

  nginxOk = systemEval.config.services.nginx.enable == true;
  vhostOk = builtins.hasAttr hostName systemEval.config.services.nginx.virtualHosts;
  trustedPublicKeysOk =
    let keys = systemEval.config.nix.settings.trusted-public-keys or [ ];
    in lib.elem trustedSigningKey keys && lib.elem cacheSigningKey keys;
  deployEnabled = systemEval.config.services.provider-deploy.enable or false;
  deployApiBaseConfigured = (systemEval.config.services.provider-deploy.providerApiBaseUrl or "") == "https://hosting.test";
  substituters = systemEval.config.nix.settings.substituters or [ ];
  providerCacheFirst = substituters != [ ] && builtins.head substituters == "https://hosting.test/cache";
  requireSignedBinaries = systemEval.config.nix.settings.require-signed-binaries or false;
  netrcFileConfigured = (systemEval.config.nix.settings.netrc-file or "") == "/run/secrets/hostenv/cache_netrc";
  allowedUsers = systemEval.config.nix.settings.allowed-users or [ ];
  providerServiceAllowed = lib.elem envName allowedUsers;
  secretsOk =
    builtins.hasAttr "${envName}/backups_secret" systemEval.config.sops.secrets
    && builtins.hasAttr "${envName}/backups_env" systemEval.config.sops.secrets
    && builtins.hasAttr "${envName}/env_only" systemEval.config.sops.secrets
    && builtins.hasAttr "${envName}/project_only" systemEval.config.sops.secrets
    && systemEval.config.sops.secrets."${envName}/env_only".key == "${envName}/env_only"
    && systemEval.config.sops.secrets."${envName}/project_only".key == "acme_demo/project_only"
    && !(builtins.hasAttr "${envName}/org_only" systemEval.config.sops.secrets)
    && !(builtins.hasAttr "${envName}/env_provider_only" systemEval.config.sops.secrets)
    && !(builtins.hasAttr "${envName}/project_provider_only" systemEval.config.sops.secrets)
    && !(builtins.hasAttr "deploy/backups_secret" systemEval.config.sops.secrets)
    && !(builtins.hasAttr "deploy/backups_env" systemEval.config.sops.secrets);
  wheelGroupExists = systemEval.config.users.groups ? wheel;
  wheelPasswordless =
    let
      rules = systemEval.config.security.sudo.extraRules or [ ];
      hasWheelGroup = rule: builtins.any (g: g == "wheel") (rule.groups or [ ]);
      cmdHasNopasswd = cmd: builtins.any (opt: opt == "NOPASSWD") (cmd.options or [ ]);
      ruleHasNopasswd = rule: builtins.any cmdHasNopasswd (rule.commands or [ ]);
      wheelNeedsPassword = systemEval.config.security.sudo.wheelNeedsPassword or true;
    in
    (!wheelNeedsPassword) || builtins.any (rule: hasWheelGroup rule && ruleHasNopasswd rule) rules;
  sessionVars = systemEval.config.environment.sessionVariables or { };
  xdgVarsOk =
    (sessionVars.XDG_CACHE_HOME or null) == "$HOME/.cache"
    && (sessionVars.XDG_CONFIG_HOME or null) == "$HOME/.config"
    && (sessionVars.XDG_DATA_HOME or null) == "$HOME/.local/share"
    && (sessionVars.XDG_STATE_HOME or null) == "$HOME/.local/state";
  firewallPorts = systemEval.config.networking.firewall.allowedTCPPorts or [ ];
  firewallPortsOk = lib.all (port: lib.elem port firewallPorts) [ 22 80 443 ];

  providerServiceMismatchMessageOk =
    let
      assertions = providerServiceMismatchEnvNoCheck.config.assertions or [ ];
      matches = a:
        (!(a.assertion or true))
        && lib.strings.hasInfix "services.hostenv-provider.deploy.enable" (a.message or "")
        && lib.strings.hasInfix "provider.service" (a.message or "");
    in builtins.any matches assertions;
in
{
  provider-nixos-system-eval =
    asserts.assertTrue "provider-nixos-system-eval"
      (nginxOk && vhostOk && trustedPublicKeysOk && secretsOk && firewallPortsOk && deployEnabled && deployApiBaseConfigured && providerCacheFirst && requireSignedBinaries && providerServiceAllowed && netrcFileConfigured && ! systemMismatch.success)
      "provider nixosSystem should enforce env key/userName alignment";
  provider-nixos-system-wheel-sudo =
    asserts.assertTrue "provider-nixos-system-wheel-sudo"
      (wheelGroupExists && wheelPasswordless)
      "provider nixosSystem should keep wheel group and passwordless sudo";
  provider-nixos-system-session-vars =
    asserts.assertTrue "provider-nixos-system-session-vars"
      xdgVarsOk
      "provider nixosSystem should set XDG session variables";

  provider-nixos-system-provider-service-mismatch =
    asserts.assertTrue "provider-nixos-system-provider-service-mismatch"
      (!providerServiceMismatchEval.success && providerServiceMismatchMessageOk)
      "provider nixosSystem should reject services.hostenv-provider.deploy.enable when provider.service is unset";
}
