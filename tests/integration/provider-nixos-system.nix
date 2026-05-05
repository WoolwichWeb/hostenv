{ pkgs, makeHostenv, inputs }:
let
  lib = pkgs.lib;
  asserts = import ../support/assert.nix { inherit pkgs lib; };

  system = pkgs.stdenv.hostPlatform.system;
  projectInputName = "acme__demo";
  aliasHostName = "alias.hostenv.test";
  nodeName = "node-a";
  deployUser = "shipper";
  trustedSigningKey = "hostenv-provider-test-1:AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  providerCacheUrl = "https://cache.hosting.test";
  providerCachePublicKey = "cache.hosting.test-1:BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB=";
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

  nodeSystems = { "${nodeName}" = system; };

  projectSkeletonDir = pkgs.runCommand "hostenv-nixos-system-project-skeleton" { } ''
    mkdir -p "$out"
    cat > "$out/hostenv.nix" <<'EOF'
    { ... }: {
      defaultEnvironment = "main";
      hostenv = {
        organisation = "acme";
        project = "demo";
        hostenvHostname = "hosting.test";
        root = "/src/demo";
      };
      environments.main = {
        enable = true;
        type = "production";
      };
    }
    EOF
  '';

  projectSkeletonEval = makeHostenv [ (projectSkeletonDir + /hostenv.nix) ] null;
  envName = projectSkeletonEval.config.environments.main.hostenv.userName;
  hostName = projectSkeletonEval.config.environments.main.hostenv.hostname;

  projectDir = pkgs.runCommand "hostenv-nixos-system-project" { } ''
    mkdir -p "$out"
    cat > "$out/hostenv.nix" <<'EOF'
    { ... }: {
      defaultEnvironment = "main";
      hostenv = {
        organisation = "acme";
        project = "demo";
        hostenvHostname = "hosting.test";
        root = "/src/demo";
      };
      environments.main = {
        enable = true;
        type = "production";
        virtualHosts."${hostName}" = {
          enableLetsEncrypt = false;
        };
        virtualHosts."alias.hostenv.test" = {
          enableLetsEncrypt = true;
        };
      };
    }
    EOF
  '';

  projectEval = makeHostenv [ (projectDir + /hostenv.nix) ] null;

  secretsPath = pkgs.writeText "secrets.yaml" ''
    access_tokens: ""
    cache_auth_password: "dummy"
    ${envName}:
      backups_secret: "dummy"
      backups_env: "dummy"
  '';

  statePath = pkgs.writers.writeJSON "state.json" { };
  lockPath = pkgs.writers.writeJSON "flake.lock" {
    nodes.${projectInputName} = {
      original = {
        type = "git";
        url = "https://example.invalid/acme/demo.git";
        ref = "main";
      };
      locked = {
        ref = "main";
        rev = "0000000000000000000000000000000000000000";
        narHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
      };
    };
  };

  mkHostenvStub = system:
    let outPath = ../../modules;
    in {
      inherit outPath;
      modules = outPath;
      makeHostenv.${system} = makeHostenv;
      __toString = self: toString outPath;
    };

  projectInput = {
    outPath = projectDir;
    __toString = self: toString projectDir;
    lib.hostenv.${system} = {
      environments = projectEval.config.environments;
      defaultEnvironment = projectEval.config.defaultEnvironment;
    };
  };

  planInputs = {
    hostenv = mkHostenvStub system;
    ${projectInputName} = projectInput;
  };

  envInput = {
    packages.${system}.main = pkgs.hello;
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

  generatedPlan = lib.importJSON (providerFlake.lib.provider.plan {
    inputs = planInputs;
    system = system;
    inherit lib pkgs;
    hostenvHostname = "hosting.test";
    letsEncrypt = {
      enable = false;
      adminEmail = "ops@example.test";
      acceptTerms = true;
    };
    deployPublicKeys = [ "ssh-ed25519 test" ];
    deployUser = deployUser;
    nixSigning.trustedPublicKeys = [ trustedSigningKey ];
    nodeFor = { default = nodeName; production = nodeName; };
    statePath = statePath;
    planPath = null;
    lockPath = lockPath;
    nodeSystems = nodeSystems;
  }).plan;

  config = lib.recursiveUpdate generatedPlan {
    nodes.${nodeName}.provider.cache = {
      enable = true;
      url = providerCacheUrl;
      publicKey = providerCachePublicKey;
    };
  };

  configMismatch = config // {
    environments = config.environments // {
      "${envName}" = (config.environments.${envName} // {
        hostenv = (config.environments.${envName}.hostenv // { userName = "wrong-user"; });
      });
    };
  };
  configDeployReserved = lib.recursiveUpdate config {
    nodes.${nodeName}.provider.deploy = {
      enable = true;
      providerApiBaseUrl = "https://hosting.test";
      nodeAuthTokenFile = "/run/secrets/hostenv/provider_node_token";
      nodeName = nodeName;
    };
  };
  configServiceResolutionReserved = lib.recursiveUpdate config {
    nodes.${nodeName}.provider.serviceResolution = {
      organisation = "acme";
      project = "demo";
      environmentName = "main";
    };
  };
  configCacheMissing = config // {
    nodes = config.nodes // {
      ${nodeName} = config.nodes.${nodeName} // {
        provider = config.nodes.${nodeName}.provider // {
          cache = {
            enable = true;
          };
        };
      };
    };
  };
  configPreRendered = lib.recursiveUpdate config {
    nodes.${nodeName}.services.nginx.virtualHosts.${aliasHostName} = {
      extraConfig = "add_header X-Node-Only yes;";
    };
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
  systemDeployReserved = nixosSystem {
    config = configDeployReserved;
    inherit nodeSystems nodesPath secretsPath;
    node = nodeName;
    inputs = inputsForSystem;
    nixpkgs = inputs.nixpkgs;
    pkgs = pkgsBySystem;
    localSystem = system;
  };
  systemServiceResolutionReserved = nixosSystem {
    config = configServiceResolutionReserved;
    inherit nodeSystems nodesPath secretsPath;
    node = nodeName;
    inputs = inputsForSystem;
    nixpkgs = inputs.nixpkgs;
    pkgs = pkgsBySystem;
    localSystem = system;
  };
  systemCacheMissing = nixosSystem {
    config = configCacheMissing;
    inherit nodeSystems nodesPath secretsPath;
    node = nodeName;
    inputs = inputsForSystem;
    nixpkgs = inputs.nixpkgs;
    pkgs = pkgsBySystem;
    localSystem = system;
  };
  systemPreRendered = nixosSystem {
    config = configPreRendered;
    inherit nodeSystems nodesPath secretsPath;
    node = nodeName;
    inputs = inputsForSystem;
    nixpkgs = inputs.nixpkgs;
    pkgs = pkgsBySystem;
    localSystem = system;
  };

  nginxOk = systemEval.config.services.nginx.enable == true;
  vhostOk = builtins.hasAttr hostName systemEval.config.services.nginx.virtualHosts;
  primaryVhost = systemEval.config.services.nginx.virtualHosts.${hostName} or { };
  aliasVhost = systemEval.config.services.nginx.virtualHosts.${aliasHostName} or { };
  vhostTlsOk =
    (primaryVhost.enableACME or null) == false
    && (primaryVhost.forceSSL or null) == false
    && (aliasVhost.enableACME or null) == true
    && (aliasVhost.forceSSL or null) == true;
  vhostBoundaryOk =
    !(primaryVhost ? enableLetsEncrypt)
    && !(aliasVhost ? enableLetsEncrypt)
    && !(primaryVhost ? allowIndexing)
    && !(aliasVhost ? allowIndexing)
    && !(primaryVhost ? security)
    && !(aliasVhost ? security)
    && !(primaryVhost ? hsts)
    && !(aliasVhost ? hsts);
  preRenderedAliasVhost = systemPreRendered.config.services.nginx.virtualHosts.${aliasHostName} or { };
  preRenderedNginxSkippedOk =
    lib.strings.hasInfix
      "add_header X-Node-Only yes;"
      (preRenderedAliasVhost.extraConfig or "")
    && !(lib.strings.hasInfix
      "Strict-Transport-Security"
      (preRenderedAliasVhost.extraConfig or ""));
  deployKeysOk =
    lib.elem "ssh-ed25519 test" (systemEval.config.users.users.${deployUser}.openssh.authorizedKeys.keys or [ ]);
  trustedPublicKeysOk =
    lib.elem trustedSigningKey (systemEval.config.nix.settings.trusted-public-keys or [ ]);
  cacheSubstituters = systemEval.config.nix.settings.substituters or [ ];
  providerCacheSubstitutersOk =
    cacheSubstituters != [ ]
    && builtins.head cacheSubstituters == providerCacheUrl
    && builtins.any
      (substituter: substituter == "https://cache.nixos.org/" || substituter == "https://cache.nixos.org")
      cacheSubstituters;
  accessTokensSecret = systemEval.config.sops.secrets.access_tokens or null;
  privateRepoAuthOk =
    accessTokensSecret != null
    && (accessTokensSecret.group or null) == (systemEval.config.users.groups.keys.name or "keys")
    && (accessTokensSecret.mode or null) == "0440"
    && lib.strings.hasInfix
      "!include ${accessTokensSecret.path}"
      (systemEval.config.nix.extraOptions or "");
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
  deployEnvSshUserOk = (deployProfiles.${envName}.sshUser or null) == envName;
  deployEnvProfileUserOk = (deployProfiles.${envName}.user or null) == envName;
  firewallPorts = systemEval.config.networking.firewall.allowedTCPPorts or [ ];
  firewallPortsOk = lib.all (port: lib.elem port firewallPorts) [ 22 80 443 ];
  hasAssertionMessage = messageNeedle: systemConfig:
    builtins.any
      (assertion:
        assertion.assertion == false
        && lib.strings.hasInfix messageNeedle assertion.message
      )
      (systemConfig.config.assertions or [ ]);
  reservedProviderDeployOk =
    hasAssertionMessage
      "provider.deploy is reserved for provider-service node agent wiring"
      systemDeployReserved;
  reservedServiceResolutionOk =
    hasAssertionMessage
      "provider.serviceResolution is reserved for provider-service secret wiring"
      systemServiceResolutionReserved;
  missingCacheSettingsOk =
    hasAssertionMessage
      "provider.cache.url must be configured when provider.cache.enable is true"
      systemCacheMissing
    && hasAssertionMessage
      "provider.cache.publicKey must be configured when provider.cache.enable is true"
      systemCacheMissing;
in
{
  provider-nixos-system-eval =
    asserts.assertTrue "provider-nixos-system-eval"
      (nginxOk && vhostOk && vhostTlsOk && vhostBoundaryOk && preRenderedNginxSkippedOk && deployKeysOk && trustedPublicKeysOk && secretsOk && deploySystemSshUserOk && deployEnvSshUserOk && deployEnvProfileUserOk && firewallPortsOk && ! systemMismatch.success)
      "provider nixosSystem should enforce env key/userName alignment";
  provider-nixos-system-wheel-sudo =
    asserts.assertTrue "provider-nixos-system-wheel-sudo"
      (wheelGroupExists && wheelPasswordless)
      "provider nixosSystem should keep wheel group and passwordless sudo";
  provider-nixos-system-session-vars =
    asserts.assertTrue "provider-nixos-system-session-vars"
      xdgVarsOk
      "provider nixosSystem should set XDG session variables";
  provider-nixos-system-private-repo-auth =
    asserts.assertTrue "provider-nixos-system-private-repo-auth"
      privateRepoAuthOk
      "provider nixosSystem should keep the access token include for private flake fetches";
  provider-nixos-system-provider-cache-substituters =
    asserts.assertTrue "provider-nixos-system-provider-cache-substituters"
      providerCacheSubstitutersOk
      "provider cache should be first substituter without dropping default substituters";
  provider-nixos-system-reserved-provider-service-options =
    # Temporary: remove these reserved-option assertions when provider-service
    # deploy/secret wiring is implemented and the options become real node
    # configuration instead of rejected placeholders.
    asserts.assertTrue "provider-nixos-system-reserved-provider-service-options"
      (reservedProviderDeployOk && reservedServiceResolutionOk)
      "provider-common should reject reserved provider-service options while keeping node cache usable";
  provider-nixos-system-cache-missing-settings =
    asserts.assertTrue "provider-nixos-system-cache-missing-settings"
      missingCacheSettingsOk
      "provider cache should report guarded validation messages when enabled without required settings";
}
