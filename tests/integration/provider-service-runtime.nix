{ pkgs, inputs }:
let
  system = pkgs.stdenv.hostPlatform.system;
  envName = "acme__provider-main";
  previewEnvName = "acme__provider-preview";
  hostName = "provider-main.hostenv.test";
  previewHostName = "provider-preview.hostenv.test";
  nodeName = "node-a";
  trustedSigningKey = "hostenv-provider-test-1:AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
  cacheSigningKey = "hostenv-cache-test-1:BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB=";
  ageRecipient = "age1s4c2vcrfg4sx6knkmgyayfyvhr3plyehsaut9qpp5n9zhln56sqs8c40ap";
  ageSecretKey = "AGE-SECRET-KEY-1T98SVZ66YW95TD5JEVXAA9NGY06EZU397XXWGXA79S584NXVU8ASGQMDJD";

  nodesPath = pkgs.runCommand "provider-service-runtime-nodes-stub" { } ''
    mkdir -p "$out/${nodeName}"
    cat > "$out/${nodeName}/configuration.nix" <<'EOF'
    { ... }: {
      system.stateVersion = "24.11";
      networking.hostName = "node-a";
    }
    EOF
  '';

  secretsFixture = pkgs.runCommand "provider-service-runtime-secrets" { nativeBuildInputs = [ pkgs.sops ]; } ''
    set -euo pipefail
    mkdir -p "$out"

    cat > "$out/plain.yaml" <<'EOF'
    ${envName}:
      backups_secret: "provider-service-backups-secret"
      backups_env: "RESTIC_PASSWORD=provider-service-backups-env"
      env_only: "provider-service-env-only"
    ${previewEnvName}:
      backups_secret: "provider-preview-backups-secret"
      backups_env: "RESTIC_PASSWORD=provider-preview-backups-env"
      env_only: "provider-preview-env-only"
    provider_node_tokens:
      ${nodeName}: "node-token"
    provider_node_tokens_yaml: "${nodeName}: \"node-token\""
    cache_signing_key: "${cacheSigningKey}"
    cache_auth_password: "cache-password"
    EOF

    ${pkgs.sops}/bin/sops \
      --encrypt \
      --input-type yaml \
      --output-type yaml \
      --age "${ageRecipient}" \
      "$out/plain.yaml" \
      > "$out/secrets.yaml"
  '';

  secretsPath = secretsFixture + "/secrets.yaml";

  envInput = {
    packages.${system}.main = pkgs.hello;
  };

  previewEnvInput = {
    packages.${system}.main = pkgs.hello;
  };

  nodeSystems = { "${nodeName}" = system; };

  config = {
    nixSigning.trustedPublicKeys = [ trustedSigningKey ];
    nodes = {
      "${nodeName}" = {
        users.users = {
          "${envName}" = { };
          "${previewEnvName}" = { };
        };
        provider = {
          nixSigning.trustedPublicKeys = [ trustedSigningKey ];
          serviceResolution = {
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
          keys = [ "env_only" ];
        };
        hostenv = {
          organisation = "acme";
          project = "demo";
          environmentName = "main";
          userName = envName;
          hostname = hostName;
          hostenvHostname = "hosting.test";
          upstreamRuntimeDir = "/run/hostenv/nginx/${envName}";
          root = "/src/provider";
        };
        services.hostenv-provider = {
          enable = true;
          deploy.enable = true;
        };
      };
      "${previewEnvName}" = {
        enable = true;
        type = "testing";
        uid = 1002;
        node = nodeName;
        secrets = {
          enable = true;
          keys = [ "env_only" ];
        };
        hostenv = {
          organisation = "acme";
          project = "demo";
          environmentName = "preview";
          userName = previewEnvName;
          hostname = previewHostName;
          hostenvHostname = "hosting.test";
          upstreamRuntimeDir = "/run/hostenv/nginx/${previewEnvName}";
          root = "/src/provider-preview";
        };
      };
    };
    defaultEnvironment = "main";
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
      serviceResolution = {
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

  pkgsBySystem = pkgs.lib.genAttrs [ system ] (_: pkgs);
  inputsForSystem = inputs // {
    "${envName}" = envInput;
    "${previewEnvName}" = previewEnvInput;
    parent = providerFlake;
  };

  systemEval = providerFlake.lib.provider.nixosSystem {
    inherit config nodeSystems nodesPath secretsPath;
    node = nodeName;
    inputs = inputsForSystem;
    nixpkgs = inputs.nixpkgs;
    pkgs = pkgsBySystem;
    localSystem = system;
  };

in
pkgs.testers.runNixOSTest ({ ... }: {
  name = "provider-service-runtime";

  nodes.machine = {
    imports = systemEval._module.args.modules ++ [
      ({ lib, ... }: {
        sops.age.keyFile = "/etc/sops/age/keys.txt";
        environment.etc."sops/age/keys.txt".text = "${ageSecretKey}\n";
        environment.etc."sops/age/keys.txt".mode = "0400";
        security.acme.acceptTerms = true;
        security.acme.defaults.email = "test@example.invalid";
        services.nginx.virtualHosts.${hostName} = {
          enableACME = lib.mkForce false;
          forceSSL = lib.mkForce false;
        };
        services.nginx.virtualHosts.${previewHostName} = {
          enableACME = lib.mkForce false;
          forceSSL = lib.mkForce false;
        };
        virtualisation.memorySize = 2048;
      })
    ];
  };

  testScript = ''
    machine.wait_for_unit("multi-user.target")
    machine.wait_for_unit("hostenv-deploy-agent.service")
    machine.wait_for_unit("hostenv-provider-cache-netrc.service")

    machine.succeed("systemctl cat hostenv-deploy-agent.service | grep '^ExecStart=' | grep -F '/bin/hostenv-deploy-agent --config '")
    machine.succeed("config_path=$(systemctl cat hostenv-deploy-agent.service | grep '^ExecStart=' | grep -o '/nix/store/[^ ]*hostenv-deploy-agent-config.json'); test -n \"$config_path\"; grep -Eq '\"providerApiBaseUrl\"[[:space:]]*:[[:space:]]*\"https://hosting.test\"' \"$config_path\"; grep -Eq '\"nodeAuthTokenFile\"[[:space:]]*:[[:space:]]*\"/run/secrets/hostenv/provider_node_token\"' \"$config_path\"; grep -Eq '\"nodeName\"[[:space:]]*:[[:space:]]*\"node-a\"' \"$config_path\"; grep -Eq '\"stateFile\"[[:space:]]*:[[:space:]]*\"/var/lib/hostenv-deploy-agent/state.json\"' \"$config_path\"; grep -Eq '\"actionTimeoutSeconds\"[[:space:]]*:[[:space:]]*1800' \"$config_path\"; grep -Eq '\"reconnectSeconds\"[[:space:]]*:[[:space:]]*5' \"$config_path\"")

    machine.wait_until_succeeds("test -f /run/secrets/${envName}/env_only", timeout=180)
    machine.wait_until_succeeds("test -f /run/secrets/${previewEnvName}/env_only", timeout=180)
    machine.wait_until_succeeds("test -f /run/secrets/${envName}/provider_node_tokens.yaml", timeout=180)
    machine.wait_until_succeeds("test -f /run/secrets/${envName}/cache_signing_key", timeout=180)
    machine.wait_until_succeeds("test -f /run/secrets/${envName}/cache_auth_password", timeout=180)
    machine.wait_until_succeeds("test -f /run/secrets/hostenv/provider_node_token", timeout=180)
    machine.wait_until_succeeds("test -f /run/secrets/hostenv/cache_auth_password", timeout=180)
    machine.wait_until_succeeds("test -f /run/secrets/hostenv/cache_netrc", timeout=180)

    machine.succeed("test \"$(cat /run/secrets/${envName}/env_only)\" = provider-service-env-only")
    machine.succeed("test \"$(cat /run/secrets/${previewEnvName}/env_only)\" = provider-preview-env-only")
    machine.succeed("grep -qx 'node-a: \"node-token\"' /run/secrets/${envName}/provider_node_tokens.yaml")
    machine.succeed("test \"$(cat /run/secrets/${envName}/cache_signing_key)\" = ${cacheSigningKey}")
    machine.succeed("test \"$(cat /run/secrets/${envName}/cache_auth_password)\" = cache-password")
    machine.succeed("test \"$(cat /run/secrets/hostenv/provider_node_token)\" = node-token")
    machine.succeed("test \"$(cat /run/secrets/hostenv/cache_auth_password)\" = cache-password")
    machine.succeed("test ! -e /run/secrets/${previewEnvName}/provider_node_tokens.yaml")
    machine.succeed("test ! -e /run/secrets/${previewEnvName}/cache_signing_key")
    machine.succeed("test ! -e /run/secrets/${previewEnvName}/cache_auth_password")

    machine.succeed("test \"$(stat -Lc '%U:%G %a' /run/secrets/${envName}/env_only)\" = '${envName}:${envName} 400'")
    machine.succeed("test \"$(stat -Lc '%U:%G %a' /run/secrets/${previewEnvName}/env_only)\" = '${previewEnvName}:${previewEnvName} 400'")
    machine.succeed("test \"$(stat -Lc '%U:%G %a' /run/secrets/${envName}/provider_node_tokens.yaml)\" = '${envName}:${envName} 400'")
    machine.succeed("test \"$(stat -Lc '%U:%G %a' /run/secrets/${envName}/cache_signing_key)\" = '${envName}:${envName} 400'")
    machine.succeed("test \"$(stat -Lc '%U:%G %a' /run/secrets/${envName}/cache_auth_password)\" = '${envName}:${envName} 400'")
    machine.succeed("test \"$(stat -Lc '%U:%G %a' /run/secrets/hostenv/provider_node_token)\" = 'root:root 400'")
    machine.succeed("test \"$(stat -Lc '%U:%G %a' /run/secrets/hostenv/cache_auth_password)\" = 'root:root 400'")
    machine.succeed("test \"$(stat -Lc '%U:%G %a' /run/secrets/hostenv/cache_netrc)\" = 'root:root 400'")

    machine.succeed("grep -qx 'machine hosting.test' /run/secrets/hostenv/cache_netrc")
    machine.succeed("grep -qx 'login cache' /run/secrets/hostenv/cache_netrc")
    machine.succeed("grep -qx 'password cache-password' /run/secrets/hostenv/cache_netrc")

    machine.succeed("id -gn ${envName} | grep -qx ${envName}")
    machine.succeed("loginctl show-user ${envName} -p Linger | grep -qx 'Linger=yes'")

    machine.succeed("nix show-config | grep -Eq '^allowed-users = .*${envName}( |$)'")
    machine.succeed("nix show-config | grep -qx 'netrc-file = /run/secrets/hostenv/cache_netrc'")
    machine.succeed("nix show-config | grep -qx 'require-sigs = true'")
    machine.succeed("nix show-config | grep -Eq '^substituters = https://hosting\\.test/cache( |$)'")
    machine.succeed("nix show-config | grep -Eq '^trusted-public-keys = .*${trustedSigningKey}.*${cacheSigningKey}.*$'")
  '';
})
