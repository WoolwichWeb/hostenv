{ pkgs, inputs }:
let
  lib = pkgs.lib;
  asserts = import ../support/assert.nix { inherit pkgs lib; };

  system = pkgs.stdenv.hostPlatform.system;
  envName = "acme__demo-main";
  hostName = "${envName}.hostenv.test";
  nodeName = "node-a";
  deployUser = "shipper";
  trustedSigningKey = "hostenv-provider-test-1:AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  systemsInput = pkgs.writeText "systems.nix" ''[ "${system}" ]'';

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
  '';

  envInput = {
    packages.${system}.main = pkgs.hello;
  };

  nodeSystems = { "${nodeName}" = system; };

  config = {
    deployUser = deployUser;
    nixSigning.trustedPublicKeys = [ trustedSigningKey ];
    nodes = {
      "${nodeName}" = {
        users.users = {
          "${envName}" = { };
        };
        provider = {
          deployPublicKeys = [ "ssh-ed25519 test" ];
          deployUser = deployUser;
          nixSigning.trustedPublicKeys = [ trustedSigningKey ];
        };
      };
    };
    environments = {
      "${envName}" = {
        enable = true;
        type = "production";
        uid = 1001;
        node = nodeName;
        hostenv = {
          organisation = "acme";
          project = "demo";
          environmentName = "main";
          userName = envName;
          hostname = hostName;
          hostenvHostname = "hosting.test";
          upstreamRuntimeDir = "/run/hostenv/nginx/${envName}";
          root = "/src/demo";
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
      deployPublicKeys = [ "ssh-ed25519 test" ];
      deployUser = deployUser;
      nixSigning.trustedPublicKeys = [ trustedSigningKey ];
      nodeFor = { default = nodeName; };
      nodeSystems = nodeSystems;
    };
  };

  pkgsBySystem = lib.genAttrs [ system ] (_: pkgs);
  inputsForSystem = inputs // {
    "${envName}" = envInput;
    parent = providerFlake;
  };

  nixosSystem = providerFlake.lib.provider.nixosSystem;
  deployOutputs = providerFlake.lib.provider.deployOutputs {
    inherit config nodeSystems nodesPath secretsPath;
    inputs = inputsForSystem;
    nixpkgs = inputs.nixpkgs;
    deploy-rs = inputs.deploy-rs;
    systems = systemsInput;
    localSystem = system;
  };
  deployProfiles = deployOutputs.deploy.nodes.${nodeName}.profiles;
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

  nginxOk = systemEval.config.services.nginx.enable == true;
  vhostOk = builtins.hasAttr hostName systemEval.config.services.nginx.virtualHosts;
  deployKeysOk =
    lib.elem "ssh-ed25519 test" (systemEval.config.users.users.${deployUser}.openssh.authorizedKeys.keys or [ ]);
  trustedPublicKeysOk =
    lib.elem trustedSigningKey (systemEval.config.nix.settings.trusted-public-keys or [ ]);
  secretsOk =
    builtins.hasAttr "${envName}/backups_secret" systemEval.config.sops.secrets
    && builtins.hasAttr "${envName}/backups_env" systemEval.config.sops.secrets
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
  deploySystemSshUserOk = (deployProfiles.system.sshUser or null) == deployUser;
  deployEnvSshUserOk = (deployProfiles.${envName}.sshUser or null) == deployUser;
  deployEnvProfileUserOk = (deployProfiles.${envName}.user or null) == envName;
in
{
  provider-nixos-system-eval =
    asserts.assertTrue "provider-nixos-system-eval"
      (nginxOk && vhostOk && deployKeysOk && trustedPublicKeysOk && secretsOk && deploySystemSshUserOk && deployEnvSshUserOk && deployEnvProfileUserOk && ! systemMismatch.success)
      "provider nixosSystem should enforce env key/userName alignment";
  provider-nixos-system-wheel-sudo =
    asserts.assertTrue "provider-nixos-system-wheel-sudo"
      (wheelGroupExists && wheelPasswordless)
      "provider nixosSystem should keep wheel group and passwordless sudo";
  provider-nixos-system-session-vars =
    asserts.assertTrue "provider-nixos-system-session-vars"
      xdgVarsOk
      "provider nixosSystem should set XDG session variables";
}
