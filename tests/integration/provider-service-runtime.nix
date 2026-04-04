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
      networking.hostName = "${nodeName}";
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
            providerApiBaseUrl = "http://hosting.test:18080";
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
        providerApiBaseUrl = "http://hosting.test:18080";
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

  nodes."${hostName}" = {
    imports = systemEval._module.args.modules ++ [
      ({ lib, ... }: {
        sops.age.keyFile = "/etc/sops/age/keys.txt";
        environment.etc."sops/age/keys.txt".text = "${ageSecretKey}\n";
        environment.etc."sops/age/keys.txt".mode = "0400";
        environment.systemPackages = [ pkgs.jq ];
        security.acme.acceptTerms = true;
        security.acme.defaults.email = "test@example.invalid";
        networking.extraHosts = ''
          127.0.0.1 hosting.test
        '';
        systemd.services.mock-hostenv-provider = {
          description = "Mock hostenv deploy websocket endpoint";
          wantedBy = [ "multi-user.target" ];
          before = [ "hostenv-deploy-agent.service" ];
          serviceConfig = {
            ExecStart =
              let
                python = pkgs.python3.withPackages (ps: [ ps.websockets ]);
                script = pkgs.writeText "mock-hostenv-provider.py" ''
                  import asyncio
                  import json
                  from pathlib import Path
                  import websockets

                  LOG_DIR = Path("/run/mock-hostenv-provider")
                  LOG_DIR.mkdir(parents=True, exist_ok=True)

                  next_connection_id = 0

                  def auth_ok(connection_id):
                      return json.dumps({
                          "version": 1,
                          "kind": "auth_ok",
                          "messageId": f"msg-auth-ok-{connection_id}",
                          "timestamp": "2026-04-03T13:00:00Z",
                          "node": "node-a",
                          "payload": {},
                      })

                  async def handler(websocket):
                      global next_connection_id
                      next_connection_id += 1
                      connection_id = next_connection_id

                      auth_payload = await websocket.recv()
                      (LOG_DIR / f"auth-{connection_id}.json").write_text(auth_payload)
                      await websocket.send(auth_ok(connection_id))

                      resume_payload = await websocket.recv()
                      (LOG_DIR / f"resume-{connection_id}.json").write_text(resume_payload)
                      await websocket.wait_closed()

                  async def main():
                      async with websockets.serve(handler, "127.0.0.1", 18080):
                          await asyncio.Future()

                  asyncio.run(main())
                '';
              in
              "${python}/bin/python ${script}";
            Restart = "always";
            RestartSec = "1s";
            DynamicUser = false;
            User = "root";
          };
        };
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
    machine.wait_for_unit("mock-hostenv-provider.service")
    machine.wait_for_unit("hostenv-deploy-agent.service")
    machine.wait_for_unit("hostenv-provider-cache-netrc.service")
    machine.wait_until_succeeds("test -f /run/mock-hostenv-provider/auth-1.json && test -f /run/mock-hostenv-provider/resume-1.json", timeout=180)

    machine.succeed("systemctl cat hostenv-deploy-agent.service | grep '^ExecStart=' | grep -F '/bin/hostenv-deploy-agent --config '")
    machine.succeed("config_path=$(systemctl cat hostenv-deploy-agent.service | grep '^ExecStart=' | grep -o '/nix/store/[^ ]*hostenv-deploy-agent-config.json'); test -n \"$config_path\"; grep -Eq '\"providerApiBaseUrl\"[[:space:]]*:[[:space:]]*\"http://hosting.test:18080\"' \"$config_path\"; grep -Eq '\"nodeAuthTokenFile\"[[:space:]]*:[[:space:]]*\"/run/secrets/hostenv/provider_node_token\"' \"$config_path\"; grep -Eq '\"nodeName\"[[:space:]]*:[[:space:]]*\"node-a\"' \"$config_path\"; grep -Eq '\"stateFile\"[[:space:]]*:[[:space:]]*\"/var/lib/hostenv-deploy-agent/state.json\"' \"$config_path\"; grep -Eq '\"actionTimeoutSeconds\"[[:space:]]*:[[:space:]]*1800' \"$config_path\"; grep -Eq '\"reconnectSeconds\"[[:space:]]*:[[:space:]]*5' \"$config_path\"")
    machine.succeed("jq -e '.kind == \"auth\" and .version == 1 and .node == \"node-a\" and .payload.token == \"node-token\"' /run/mock-hostenv-provider/auth-1.json >/dev/null")
    machine.succeed("jq -e '.kind == \"resume\" and .version == 1 and .node == \"node-a\" and .payload.journalVersion == 1 and .payload.current == null' /run/mock-hostenv-provider/resume-1.json >/dev/null")

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
