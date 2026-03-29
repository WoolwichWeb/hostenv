{ pkgs, inputs, makeHostenv }:
let
  system = pkgs.stdenv.hostPlatform.system;
  targetEnvName = "acme__demo-main";
  controlPlaneNodeName = "control-plane";
  providerNodeName = "node-a";
  controlProject = "control";
  controlEnvName = "main";
  controlHost = "control-plane";
  nodeToken = "node-token";
  cacheSigningKey = "hostenv-cache-test-1:BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB=";
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

  activateProfile = pkgs.runCommand "provider-deploy-test-profile" { } ''
    mkdir -p "$out/bin"
    cat > "$out/bin/activate" <<'EOF'
    #!${pkgs.bash}/bin/bash
    set -euo pipefail
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
  controlRuntimeDir = controlEnv.config.hostenv.runtimeDir;
  controlListenSocket = controlEnv.config.services.hostenv-provider.listenSocket;
  controlUpstreamSocket = "${controlEnv.config.hostenv.upstreamRuntimeDir}/in.sock";
  controlDbConn = "host=${controlRuntimeDir} dbname=hostenv-provider user=${controlUserName}";
  controlProviderUnit = controlEnv.config.systemd.services.hostenv-provider;
  controlPostgresUnit = controlEnv.config.systemd.services.postgresql;
  controlProviderExecStart = controlProviderUnit.serviceConfig.ExecStart;

  deployIntentJson = builtins.toJSON {
    schemaVersion = 1;
    actions = [
      {
        user = targetEnvName;
        op = "activate";
        storePath = toString activateProfile;
      }
    ];
  };

  deployActionJson = builtins.toJSON {
    user = targetEnvName;
    op = "activate";
    storePath = toString activateProfile;
  };

  seedSql = pkgs.writeText "provider-deploy-multivm-seed.sql" ''
    INSERT INTO jobs (id, kind, status, payload, created_at, started_at, waiting_at)
    VALUES ('${jobId}', 'deploy', 'waiting', '{}'::jsonb, now(), now(), now());

    INSERT INTO deploy_intents (job_id, commit_sha, node, intent, created_at)
    VALUES ('${jobId}', '${commitSha}', '${providerNodeName}', $$${deployIntentJson}$$::jsonb, now());

    INSERT INTO deploy_actions (job_id, node, action_idx, op, user_name, action, status, message, started_at, finished_at, created_at, updated_at)
    VALUES ('${jobId}', '${providerNodeName}', 0, 'activate', '${targetEnvName}', $$${deployActionJson}$$::jsonb, 'queued', 'Queued action activate for ${targetEnvName}', NULL, NULL, now(), now());
  '';

  nodesPath = pkgs.runCommand "provider-deploy-multivm-nodes-stub" { } ''
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

  secretsFixture = pkgs.runCommand "provider-deploy-multivm-secrets" { nativeBuildInputs = [ pkgs.sops ]; } ''
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
            publicKey = cacheSigningKey;
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
          cache.enable = false;
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
        uid = 1002;
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
        networking.firewall.allowedTCPPorts = [ 80 ];
        security.acme.acceptTerms = true;
        security.acme.defaults.email = "test@example.invalid";
        security.pam.services.runuser.setEnvironment = lib.mkForce true;
        services.nginx.virtualHosts."${controlHost}" = {
          enableACME = lib.mkForce false;
          forceSSL = lib.mkForce false;
          locations."~ ^/api/" = {
            recommendedProxySettings = true;
            proxyPass = lib.mkForce "http://unix:${controlUpstreamSocket}:";
            extraConfig = lib.mkForce ''
              proxy_connect_timeout 120s;
              proxy_send_timeout 120s;
              proxy_read_timeout 120s;
              proxy_http_version 1.1;
              proxy_set_header Upgrade $http_upgrade;
              proxy_set_header Connection "upgrade";
            '';
          };
        };
        systemd.services.control-plane-postgresql = {
          inherit (controlPostgresUnit) description path preStart script postStart restartTriggers;
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          serviceConfig = (removeAttrs controlPostgresUnit.serviceConfig [ "ExecStart" ]) // {
            User = controlUserName;
          };
        };
        systemd.services.control-plane-provider = {
          inherit (controlProviderUnit) description path restartIfChanged;
          wantedBy = [ "multi-user.target" ];
          wants = [ "control-plane-postgresql.service" ];
          after = [ "network.target" "control-plane-postgresql.service" ];
          serviceConfig = controlProviderUnit.serviceConfig // {
            Environment = "HOME=/home/${controlUserName}";
            ExecStart = controlProviderExecStart;
            User = controlUserName;
            WorkingDirectory = "/home/${controlUserName}";
          };
        };
        systemd.services.control-plane-provider-cache-auth = {
          inherit (controlEnv.config.systemd.services.hostenv-provider-cache-auth) description script;
          wantedBy = [ "multi-user.target" ];
          before = [ "control-plane-provider.service" ];
          serviceConfig = {
            User = controlUserName;
            Group = "users";
            Type = "oneshot";
            RemainAfterExit = true;
          };
        };
        systemd.services.control-plane-provider-upstream = {
          description = "Expose provider-service socket to nginx upstream runtime";
          wantedBy = [ "multi-user.target" ];
          after = [ "control-plane-provider.service" ];
          wants = [ "control-plane-provider.service" ];
          serviceConfig = {
            ExecStartPre = "${pkgs.coreutils}/bin/rm -f ${controlUpstreamSocket}";
            ExecStart = "${pkgs.socat}/bin/socat UNIX-LISTEN:${controlUpstreamSocket},fork,mode=0660,user=${controlUserName},group=nginx UNIX-CONNECT:${controlListenSocket}";
            Restart = "always";
            RestartSec = "1s";
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
        environment.systemPackages = [ pkgs.websocat ];
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
    controlPlane.wait_for_unit("control-plane-postgresql.service")
    controlPlane.wait_until_succeeds("test -f /run/secrets/${controlUserName}/provider_node_tokens.yaml", timeout=180)
    controlPlane.wait_for_unit("control-plane-provider.service")
    controlPlane.wait_for_unit("control-plane-provider-upstream.service")
    controlPlane.wait_for_unit("nginx.service")
    controlPlane.wait_until_succeeds("runuser -u ${controlUserName} -- psql '${controlDbConn}' -Atc 'select 1' | grep -qx 1", timeout=60)
    controlPlane.wait_until_succeeds("printf '%s\\n' '{\"type\":\"auth\",\"node\":\"${providerNodeName}\",\"token\":\"${nodeToken}\"}' | websocat -q -n -t -1 'ws://${controlHost}/api/deploy-jobs/ws?node=${providerNodeName}' | grep -F '\"type\":\"deploy_hint\"' | grep -F '\"node\":\"${providerNodeName}\"'", timeout=180)
    controlPlane.succeed("runuser -u ${controlUserName} -- psql '${controlDbConn}' -f ${seedSql}")

    providerNode.wait_for_unit("multi-user.target")
    providerNode.wait_for_unit("provider-deploy.service")
    providerNode.wait_until_succeeds("test -f /run/secrets/hostenv/provider_node_token", timeout=180)
    providerNode.wait_until_succeeds("token=\"$(tr -d '\\n\\r' < /run/secrets/hostenv/provider_node_token)\"; printf '{\"type\":\"auth\",\"node\":\"${providerNodeName}\",\"token\":\"%s\"}\\n' \"$token\" | websocat -q -n -t -1 'ws://${controlHost}/api/deploy-jobs/ws?node=${providerNodeName}' | grep -F '\"type\":\"deploy_hint\"' | grep -F '\"node\":\"${providerNodeName}\"'", timeout=180)
    providerNode.wait_until_succeeds("test -s /var/lib/provider-deploy/state.json", timeout=180)

    controlPlane.wait_until_succeeds("runuser -u ${controlUserName} -- psql '${controlDbConn}' -Atc \"select status from jobs where id='${jobId}'\" | grep -qx succeeded", timeout=180)
    controlPlane.wait_until_succeeds("runuser -u ${controlUserName} -- psql '${controlDbConn}' -Atc \"select count(*) from deploy_node_events where job_id='${jobId}' and node='${providerNodeName}' and phase='activate' and status='success'\" | grep -Ev '^0$'", timeout=180)
    controlPlane.wait_until_succeeds("runuser -u ${controlUserName} -- psql '${controlDbConn}' -Atc \"select count(*) from deploy_node_events where job_id='${jobId}' and node='${providerNodeName}' and phase='intent' and status='success'\" | grep -Ev '^0$'", timeout=180)
  '';
})
