{ pkgs, inputs, makeHostenv }:
let
  system = pkgs.stdenv.hostPlatform.system;
  targetEnvName = "acme__demo-main";
  targetEnvUid = 1002;
  controlPlaneNodeName = "control-plane";
  controlUserUid = 1001;
  providerNodeName = "node-a";
  controlProject = "control";
  controlEnvName = "main";
  controlHost = "control-plane";
  nodeToken = "node-token";
  cacheSigningKey = "hostenv-cache-test-1:ORzELhyTE/tAP9ArGwd69bhu2qJZ88SYdUYzJLRuEoJ8piUp+ImhwIwv+ZE9nooXRfVzfTOykSNI24PPgUufSw==";
  cachePublicKey = "hostenv-cache-test-1:fKYlKfiJocCML/mRPZ6KF0X1c30zspEjSNuDz4FLn0s=";
  ageRecipient = "age1s4c2vcrfg4sx6knkmgyayfyvhr3plyehsaut9qpp5n9zhln56sqs8c40ap";
  ageSecretKey = "AGE-SECRET-KEY-1T98SVZ66YW95TD5JEVXAA9NGY06EZU397XXWGXA79S584NXVU8ASGQMDJD";
  jobId = "test-job-0001";
  commitSha = "deadbeefcafebabe";
  providerServiceSrc = builtins.path {
    path = ../../modules/services/hostenv-provider-service;
    name = "hostenv-provider-service-src";
  };
  providerHaskellPackages = pkgs.haskell.packages.ghc912.override {
    overrides = self: super: {
      addressable-content = self.callCabal2nix "addressable-content" inputs."addressable-content".outPath { };
    };
  };
  compiledProviderServicePackage = providerHaskellPackages.callCabal2nix "hostenv-provider-service" providerServiceSrc { };

  activateProfile = pkgs.runCommand "hostenv-deploy-agent-test-profile" { } ''
    mkdir -p "$out/bin"
    cat > "$out/bin/activate" <<'EOF'
    #!${pkgs.bash}/bin/bash
    set -euo pipefail
    touch "/home/${targetEnvName}/stuff"
    exit 0
    EOF
    chmod +x "$out/bin/activate"
  '';

  controlModules = [
    ({ ... }: {
      hostenv = {
        organisation = "acme";
        project = controlProject;
        hostenvHostname = controlHost;
        root = ./.;
      };
      provider.serviceResolution = {
        organisation = "acme";
        project = controlProject;
        environmentName = controlEnvName;
      };
      services.hostenv-provider = {
        enable = true;
        package = compiledProviderServicePackage;
        deploy.enable = true;
        webhookHost = controlHost;
        uiHost = controlHost;
        uiScheme = "http";
      };
      environments.${controlEnvName} = {
        enable = true;
        type = "production";
      };
    })
  ];

  controlEnv = makeHostenv controlModules controlEnvName;
  controlUserName = controlEnv.config.hostenv.userName;
  controlUserHome = "/home/${controlUserName}";
  controlRuntimeDir = controlEnv.config.hostenv.runtimeDir;
  controlUserRuntimeDir = "/run/user/${toString controlUserUid}";
  controlPostgresDataDir = controlEnv.config.services.postgresql.dataDir;
  controlActivatePackage = controlEnv.config.activatePackage;
  controlDbConn = "host=${controlRuntimeDir} dbname=hostenv-provider user=${controlUserName}";

  deployIntentJson = builtins.toJSON {
    schemaVersion = 1;
    actions = [
      {
        actionId = "${jobId}:${providerNodeName}:0";
        user = targetEnvName;
        op = "activate";
        storePath = toString activateProfile;
      }
    ];
  };

  deployActionJson = builtins.toJSON {
    actionId = "${jobId}:${providerNodeName}:0";
    user = targetEnvName;
    op = "activate";
    storePath = toString activateProfile;
  };

  seedSql = pkgs.writeText "hostenv-deploy-agent-multivm-seed.sql" ''
    INSERT INTO jobs (id, kind, status, payload, created_at, started_at, waiting_at)
    VALUES ('${jobId}', 'deploy', 'waiting', '{}'::jsonb, now(), now(), now());

    INSERT INTO deploy_intents (job_id, commit_sha, node, intent, created_at)
    VALUES ('${jobId}', '${commitSha}', '${providerNodeName}', $$${deployIntentJson}$$::jsonb, now());

    INSERT INTO deploy_actions (job_id, node, action_idx, op, user_name, action, status, message, started_at, finished_at, created_at, updated_at)
    VALUES ('${jobId}', '${providerNodeName}', 0, 'activate', '${targetEnvName}', $$${deployActionJson}$$::jsonb, 'queued', 'Queued action activate for ${targetEnvName}', NULL, NULL, now(), now());
  '';

  nodesPath = pkgs.runCommand "hostenv-deploy-agent-multivm-nodes-stub" { } ''
    mkdir -p "$out/${providerNodeName}" "$out/${controlPlaneNodeName}"
    cat > "$out/${providerNodeName}/configuration.nix" <<'EOF'
    { ... }: {
      system.stateVersion = "25.11";
      networking.hostName = "node-a";
    }
    EOF
    cat > "$out/${controlPlaneNodeName}/configuration.nix" <<'EOF'
    { ... }: {
      system.stateVersion = "25.11";
      networking.hostName = "control-plane";
    }
    EOF
  '';

  secretsFixture = pkgs.runCommand "hostenv-deploy-agent-multivm-secrets" { nativeBuildInputs = [ pkgs.sops ]; } ''
    set -euo pipefail
    mkdir -p "$out"
    cat > "$out/plain.yaml" <<'EOF'
    ${controlUserName}:
      backups_secret: "control-backups-secret"
      backups_env: "RESTIC_PASSWORD=control-backups-env"
    ${targetEnvName}:
      backups_secret: "demo-backups-secret"
      backups_env: "RESTIC_PASSWORD=demo-backups-env"
    provider_node_tokens:
      ${providerNodeName}: "${nodeToken}"
    provider_node_tokens_yaml: "${providerNodeName}: \"${nodeToken}\""
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

  nodeSystems = {
    "${controlPlaneNodeName}" = system;
    "${providerNodeName}" = system;
  };

  config = {
    nodes = {
      "${controlPlaneNodeName}" = {
        users.users."${controlUserName}" = { };
        provider = {
          deploy.enable = false;
          serviceResolution = {
            organisation = "acme";
            project = controlProject;
            environmentName = controlEnvName;
          };
          cache = {
            enable = true;
            url = "http://${controlHost}/cache";
            publicKey = cachePublicKey;
            netrcFile = "/run/secrets/hostenv/cache_netrc";
          };
        };
      };
      "${providerNodeName}" = {
        users.users."${targetEnvName}" = { };
        provider = {
          deploy = {
            enable = true;
            providerApiBaseUrl = "http://${controlHost}";
            nodeName = providerNodeName;
            nodeAuthTokenFile = "/run/secrets/hostenv/provider_node_token";
            reconnectSeconds = 1;
          };
          cache = {
            enable = true;
            url = "http://${controlHost}/cache";
            publicKey = cachePublicKey;
            netrcFile = "/run/secrets/hostenv/cache_netrc";
          };
        };
      };
    };
    environments = {
      "${controlUserName}" = {
        enable = true;
        type = "production";
        uid = 1001;
        node = controlPlaneNodeName;
        hostenv = {
          organisation = "acme";
          project = controlProject;
          environmentName = controlEnvName;
          userName = controlUserName;
          hostname = controlHost;
          hostenvHostname = controlHost;
          upstreamRuntimeDir = "/run/hostenv/nginx/${controlUserName}";
          root = "/src/control";
        };
        services.hostenv-provider = {
          enable = true;
          deploy.enable = true;
        };
      };
      "${targetEnvName}" = {
        enable = true;
        type = "production";
        uid = targetEnvUid;
        node = providerNodeName;
        hostenv = {
          organisation = "acme";
          project = "demo";
          environmentName = "main";
          userName = targetEnvName;
          hostname = "demo-main.hostenv.test";
          hostenvHostname = controlHost;
          upstreamRuntimeDir = "/run/hostenv/nginx/${targetEnvName}";
          root = "/src/demo";
        };
      };
    };
    defaultEnvironment = controlEnvName;
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
      hostenvHostname = controlHost;
      nodeFor = { default = providerNodeName; };
      nodeSystems = nodeSystems;
    };
  };

  pkgsBySystem = pkgs.lib.genAttrs [ system ] (_: pkgs);

  controlPlaneEval = providerFlake.lib.provider.nixosSystem {
    inherit config nodeSystems nodesPath secretsPath;
    node = controlPlaneNodeName;
    inputs = inputs // { parent = providerFlake; };
    nixpkgs = inputs.nixpkgs;
    pkgs = pkgsBySystem;
    localSystem = system;
  };

  providerNodeEval = providerFlake.lib.provider.nixosSystem {
    inherit config nodeSystems nodesPath secretsPath;
    node = providerNodeName;
    inputs = inputs // { parent = providerFlake; };
    nixpkgs = inputs.nixpkgs;
    pkgs = pkgsBySystem;
    localSystem = system;
  };

in
pkgs.testers.runNixOSTest ({ ... }: {
  name = "provider-deploy-multivm";

  nodes.controlPlane = {
    imports = controlPlaneEval._module.args.modules ++ [
      ({ lib, ... }: {
        sops.age.keyFile = "/etc/sops/age/keys.txt";
        environment.etc."sops/age/keys.txt".text = "${ageSecretKey}\n";
        environment.etc."sops/age/keys.txt".mode = "0400";
        environment.systemPackages = [ pkgs.postgresql pkgs.websocat ];
        system.extraDependencies = [ activateProfile ];
        networking.firewall.allowedTCPPorts = [ 80 ];
        security.acme.acceptTerms = true;
        security.acme.defaults.email = "test@example.invalid";
        security.pam.services.runuser.setEnvironment = lib.mkForce true;
        services.nginx.virtualHosts."${controlHost}" = {
          enableACME = lib.mkForce false;
          forceSSL = lib.mkForce false;
        };
        systemd.services.control-plane-activate = {
          description = "Activate control-plane hostenv user environment";
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          wants = [ "user@${toString controlUserUid}.service" ];
          path = [ pkgs.bash pkgs.coreutils pkgs.systemd pkgs.util-linux ];
          script = ''
            set -euo pipefail

            export HOME="${controlUserHome}"
            export XDG_RUNTIME_DIR="${controlUserRuntimeDir}"
            export DBUS_SESSION_BUS_ADDRESS="unix:path=${controlUserRuntimeDir}/bus"
            export XDG_CACHE_HOME="$HOME/.cache"
            export XDG_CONFIG_HOME="$HOME/.config"
            export XDG_DATA_HOME="$HOME/.local/share"
            export XDG_STATE_HOME="$HOME/.local/state"

            rm -rf "${controlPostgresDataDir}"

            systemctl start "user@${toString controlUserUid}.service"
            for _ in $(seq 1 120); do
              if [ -S "${controlUserRuntimeDir}/systemd/private" ] && [ -S "${controlUserRuntimeDir}/bus" ]; then
                break
              fi
              sleep 1
            done

            if [ ! -S "${controlUserRuntimeDir}/systemd/private" ] || [ ! -S "${controlUserRuntimeDir}/bus" ]; then
              echo "control-plane-activate: user systemd sockets did not appear for ${controlUserName}" >&2
              exit 1
            fi

            runuser -u "${controlUserName}" -- env \
              HOME="$HOME" \
              XDG_RUNTIME_DIR="$XDG_RUNTIME_DIR" \
              DBUS_SESSION_BUS_ADDRESS="$DBUS_SESSION_BUS_ADDRESS" \
              XDG_CACHE_HOME="$XDG_CACHE_HOME" \
              XDG_CONFIG_HOME="$XDG_CONFIG_HOME" \
              XDG_DATA_HOME="$XDG_DATA_HOME" \
              XDG_STATE_HOME="$XDG_STATE_HOME" \
              "${controlActivatePackage}/bin/activate" || true

            runuser -u "${controlUserName}" -- env \
              HOME="$HOME" \
              XDG_RUNTIME_DIR="$XDG_RUNTIME_DIR" \
              DBUS_SESSION_BUS_ADDRESS="$DBUS_SESSION_BUS_ADDRESS" \
              XDG_CACHE_HOME="$XDG_CACHE_HOME" \
              XDG_CONFIG_HOME="$XDG_CONFIG_HOME" \
              XDG_DATA_HOME="$XDG_DATA_HOME" \
              XDG_STATE_HOME="$XDG_STATE_HOME" \
              systemctl --user daemon-reload

            runuser -u "${controlUserName}" -- env \
              HOME="$HOME" \
              XDG_RUNTIME_DIR="$XDG_RUNTIME_DIR" \
              DBUS_SESSION_BUS_ADDRESS="$DBUS_SESSION_BUS_ADDRESS" \
              XDG_CACHE_HOME="$XDG_CACHE_HOME" \
              XDG_CONFIG_HOME="$XDG_CONFIG_HOME" \
              XDG_DATA_HOME="$XDG_DATA_HOME" \
              XDG_STATE_HOME="$XDG_STATE_HOME" \
              systemctl --user start postgresql.service hostenv-provider-cache-auth.service nginx.service harmonia.service hostenv-provider.service
          '';
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
          };
        };
      })
    ];
  };

  nodes.providerNode = {
    imports = providerNodeEval._module.args.modules ++ [
      ({ lib, ... }: {
        sops.age.keyFile = "/etc/sops/age/keys.txt";
        environment.etc."sops/age/keys.txt".text = "${ageSecretKey}\n";
        environment.etc."sops/age/keys.txt".mode = "0400";
        environment.systemPackages = [ pkgs.jq pkgs.websocat ];
        security.acme.acceptTerms = true;
        security.acme.defaults.email = "test@example.invalid";
        services.nginx.virtualHosts."demo-main.hostenv.test" = {
          enableACME = lib.mkForce false;
          forceSSL = lib.mkForce false;
        };
      })
    ];
  };

  testScript = ''
    controlPlane, providerNode = machines

    controlPlane.wait_for_unit("multi-user.target")
    controlPlane.wait_for_unit("control-plane-activate.service")
    controlPlane.wait_until_succeeds("test -f /run/secrets/${controlUserName}/provider_node_tokens.yaml", timeout=180)
    controlPlane.wait_for_unit("nginx.service")
    controlPlane.wait_until_succeeds("runuser -u ${controlUserName} -- env HOME=${controlUserHome} XDG_RUNTIME_DIR=${controlUserRuntimeDir} DBUS_SESSION_BUS_ADDRESS=unix:path=${controlUserRuntimeDir}/bus XDG_CACHE_HOME=${controlUserHome}/.cache XDG_CONFIG_HOME=${controlUserHome}/.config XDG_DATA_HOME=${controlUserHome}/.local/share XDG_STATE_HOME=${controlUserHome}/.local/state systemctl --user is-active --quiet postgresql.service hostenv-provider-cache-auth.service hostenv-provider.service", timeout=300)
    controlPlane.wait_until_succeeds("runuser -u ${controlUserName} -- psql '${controlDbConn}' -Atc 'select 1' | grep -qx 1", timeout=60)
    controlPlane.wait_until_succeeds("printf '%s\\n' '{\"version\":1,\"kind\":\"auth\",\"messageId\":\"msg-auth-smoke\",\"timestamp\":\"2026-04-03T13:00:00Z\",\"node\":\"${providerNodeName}\",\"payload\":{\"token\":\"${nodeToken}\"}}' | websocat -q -n -t -1 'ws://${controlHost}/api/deploy-jobs/ws?node=${providerNodeName}' | grep -F '\"kind\":\"auth_ok\"' | grep -F '\"node\":\"${providerNodeName}\"'", timeout=180)
    controlPlane.succeed("runuser -u ${controlUserName} -- psql '${controlDbConn}' -f ${seedSql}")

    providerNode.wait_for_unit("multi-user.target")
    providerNode.wait_for_unit("hostenv-deploy-agent.service")
    providerNode.wait_until_succeeds("test -f /run/secrets/hostenv/provider_node_token", timeout=180)
    providerNode.wait_until_succeeds("test -f /run/secrets/hostenv/cache_netrc", timeout=180)
    providerNode.wait_until_succeeds("nix show-config | grep -qx 'netrc-file = /run/secrets/hostenv/cache_netrc'", timeout=180)
    providerNode.wait_until_succeeds("nix show-config | grep -Eq '^substituters = http://${controlHost}/cache( |$)'", timeout=180)
    providerNode.wait_until_succeeds("curl -sf --netrc-file /run/secrets/hostenv/cache_netrc http://${controlHost}/cache/nix-cache-info >/dev/null", timeout=180)
    providerNode.wait_until_succeeds("token=\"$(tr -d '\\n\\r' < /run/secrets/hostenv/provider_node_token)\"; printf '{\"version\":1,\"kind\":\"auth\",\"messageId\":\"msg-auth-node\",\"timestamp\":\"2026-04-03T13:00:00Z\",\"node\":\"${providerNodeName}\",\"payload\":{\"token\":\"%s\"}}\\n' \"$token\" | websocat -q -n -t -1 'ws://${controlHost}/api/deploy-jobs/ws?node=${providerNodeName}' | grep -F '\"kind\":\"auth_ok\"' | grep -F '\"node\":\"${providerNodeName}\"'", timeout=180)
    controlPlane.wait_until_succeeds("runuser -u ${controlUserName} -- psql '${controlDbConn}' -Atc \"select status from jobs where id='${jobId}'\" | grep -qx succeeded", timeout=180)
    controlPlane.wait_until_succeeds("runuser -u ${controlUserName} -- psql '${controlDbConn}' -Atc \"select count(*) from deploy_node_events where job_id='${jobId}' and node='${providerNodeName}' and phase='activate' and status='success'\" | grep -Ev '^0$'", timeout=180)
    controlPlane.wait_until_succeeds("runuser -u ${controlUserName} -- psql '${controlDbConn}' -Atc \"select count(*) from deploy_node_events where job_id='${jobId}' and node='${providerNodeName}' and phase='intent' and status='success'\" | grep -Ev '^0$'", timeout=180)
    providerNode.wait_until_succeeds("jq -e '.journalVersion == 1 and .node == \"${providerNodeName}\" and .current == null and .actions == {} and .users[\"${targetEnvName}\"].storePath == \"${activateProfile}\" and (.users[\"${targetEnvName}\"].updatedAt // \"\") != \"\" and (.updatedAt // \"\") != \"\"' /var/lib/hostenv-deploy-agent/state.json >/dev/null", timeout=180)
  '';
})
