{ pkgs, inputs }:
let
  lib = pkgs.lib;
  asserts = import ../support/assert.nix { inherit pkgs lib; };

  system = pkgs.stdenv.hostPlatform.system;
  envName = "acme__demo-main";
  hostName = "${envName}.hostenv.test";
  nodeName = "node-a";
  trustedSigningKey = "hostenv-provider-test-1:AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";

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
    comin_node_tokens:
      ${nodeName}: "node-token"
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
          comin = {
            enable = true;
            remoteUrl = "https://gitlab.com/acme/provider.git";
            branch = "main";
            pollIntervalSeconds = 30;
            actionTimeoutSeconds = 45;
            providerApiBaseUrl = "https://hosting.test";
            nodeName = nodeName;
            nodeAuthTokenFile = "/run/secrets/hostenv/comin_node_token";
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
      comin = {
        enable = true;
        remoteUrl = "https://gitlab.com/acme/provider.git";
        branch = "main";
        pollIntervalSeconds = 30;
        actionTimeoutSeconds = 45;
        providerApiBaseUrl = "https://hosting.test";
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
  cominMismatchModules = [
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

      provider.service = {
        organisation = "acme";
        project = "demo";
        environmentName = "main";
      };

      provider.comin.enable = false;
      services.hostenv-provider.enable = true;
      services.hostenv-provider.comin.enable = true;
    })
  ];

  cominMismatchEnvNoCheck = makeHostenv (cominMismatchModules ++ [ ({ ... }: { _module.check = false; }) ]) null;
  cominMismatchEval =
    let env = makeHostenv cominMismatchModules null;
    in builtins.tryEval env.config.activate;

  nginxOk = systemEval.config.services.nginx.enable == true;
  vhostOk = builtins.hasAttr hostName systemEval.config.services.nginx.virtualHosts;
  trustedPublicKeysOk =
    lib.elem trustedSigningKey (systemEval.config.nix.settings.trusted-public-keys or [ ]);
  cominEnabled = systemEval.config.services.comin.enable or false;
  cominRemoteConfigured =
    let remotes = systemEval.config.services.comin.remotes or [ ];
    in builtins.any (remote: (remote.url or "") == "https://gitlab.com/acme/provider.git") remotes;
  cominPostDeployHookConfigured =
    lib.strings.hasInfix "hostenv-comin-activate" (systemEval.config.services.comin.postDeploymentCommand or "");
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

  cominMismatchMessageOk =
    let
      assertions = cominMismatchEnvNoCheck.config.assertions or [ ];
      matches = a:
        (!(a.assertion or true))
        && lib.strings.hasInfix "services.hostenv-provider.comin.enable" (a.message or "")
        && lib.strings.hasInfix "provider.comin.enable" (a.message or "");
    in builtins.any matches assertions;
in
{
  provider-nixos-system-eval =
    asserts.assertTrue "provider-nixos-system-eval"
      (nginxOk && vhostOk && trustedPublicKeysOk && secretsOk && firewallPortsOk && cominEnabled && cominRemoteConfigured && cominPostDeployHookConfigured && ! systemMismatch.success)
      "provider nixosSystem should enforce env key/userName alignment";
  provider-nixos-system-wheel-sudo =
    asserts.assertTrue "provider-nixos-system-wheel-sudo"
      (wheelGroupExists && wheelPasswordless)
      "provider nixosSystem should keep wheel group and passwordless sudo";
  provider-nixos-system-session-vars =
    asserts.assertTrue "provider-nixos-system-session-vars"
      xdgVarsOk
      "provider nixosSystem should set XDG session variables";

  provider-nixos-system-comin-mismatch =
    asserts.assertTrue "provider-nixos-system-comin-mismatch"
      (!cominMismatchEval.success && cominMismatchMessageOk)
      "provider nixosSystem should reject services.hostenv-provider.comin.enable when provider.comin.enable is false";
}
