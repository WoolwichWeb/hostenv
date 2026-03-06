{ inputs, config, ... }:
let
  cfgTop = config;
  hostenvInputs = cfgTop.flake.lib.hostenvInputs;
  addressableContentInput =
    hostenvInputs.requireInput {
      inherit inputs;
      name = "addressable-content";
      context = "hostenv-provider-service";
    };
in
{
  flake.modules.hostenv.hostenv-provider-service =
    { lib, config, pkgs, ... }:
    let
      cfg = config.services.hostenv-provider;
      enabledEnvironments = lib.filterAttrs (_: env: env.enable or true) (config.hostenv.publicEnvironments or { });
      enabledEnvironmentNames = lib.sort builtins.lessThan (builtins.attrNames enabledEnvironments);
      providerServiceSelection =
        if config ? provider then config.provider.service else null;
      selectedServiceEnv =
        if providerServiceSelection == null then
          null
        else
          let
            matches = lib.filterAttrs
              (_: env:
                env.hostenv.organisation == providerServiceSelection.organisation
                && env.hostenv.project == providerServiceSelection.project
                && env.hostenv.environmentName == providerServiceSelection.environmentName)
              enabledEnvironments;
            values = builtins.attrValues matches;
          in
          if values == [ ] then null else builtins.head values;
      selectedServiceUser =
        if selectedServiceEnv == null then null else selectedServiceEnv.hostenv.userName;
      currentUserName = config.hostenv.userName;
      isSelectedService = selectedServiceUser != null && currentUserName == selectedServiceUser;
      deployTokenMapPath = "/run/secrets/${currentUserName}/provider_node_tokens.yaml";
      uniqueSorted = xs: lib.sort builtins.lessThan (lib.unique xs);
      normalizeGitlabUsername = username:
        if username == null || username == ""
        then null
        else lib.strings.toLower username;
      userEntriesFor = userName:
        lib.filter
          (entry: entry != null)
          (map
            (envName:
              let
                users = enabledEnvironments.${envName}.users or { };
              in
              if builtins.hasAttr userName users then
                {
                  inherit envName;
                  user = users.${userName};
                }
              else
                null)
            enabledEnvironmentNames);
      allUsernames =
        uniqueSorted
          (builtins.concatLists
            (map (envName: builtins.attrNames (enabledEnvironments.${envName}.users or { })) enabledEnvironmentNames));
      gitlabUsernamesFor = userName:
        uniqueSorted
          (lib.filter
            (value: value != null)
            (map
              (entry: normalizeGitlabUsername (entry.user.gitlabUsername or null))
              (userEntriesFor userName)));
      providerHosts =
        uniqueSorted
          (lib.filter
            (host: host != "")
            (map lib.strings.toLower (if cfg.gitlab.hosts == [ ] then [ "gitlab.com" ] else cfg.gitlab.hosts)));
      conflictingUsers = lib.filter (userName: (lib.length (gitlabUsernamesFor userName)) > 1) allUsernames;
      seedUsers =
        map
          (userName:
            let
              entries = userEntriesFor userName;
              emailValues =
                lib.filter
                  (value: value != null && value != "")
                  (map (entry: entry.user.email or null) entries);
              gitlabValues = gitlabUsernamesFor userName;
              gitlabUsername = if gitlabValues == [ ] then null else builtins.head gitlabValues;
              providerAccounts =
                if gitlabUsername == null then
                  [ ]
                else
                  map
                    (host: {
                      provider = "gitlab";
                      host = host;
                      username = gitlabUsername;
                      userId = null;
                    })
                    providerHosts;
            in
            {
              configUsername = userName;
              email = if emailValues == [ ] then null else builtins.head emailValues;
              role = "admin";
              inherit providerAccounts;
            })
          allUsernames;
      providerService = cfgTop.flake.lib.provider.service;
      serviceSrc = cfg.source;
      haskellDeps = cfg.haskellDeps;
      providerHaskellPackages = pkgs.haskell.packages.ghc912.override {
        overrides = self: super: {
          addressable-content = self.callCabal2nix "addressable-content" addressableContentInput.outPath { };
        };
      };
      ghc = providerHaskellPackages.ghcWithPackages (p: map (name: p.${name}) haskellDeps);
      serviceBin = pkgs.writeShellScriptBin "hostenv-provider-service" ''
        exec ${ghc}/bin/runghc -i${serviceSrc} ${serviceSrc}/Main.hs "$@"
      '';
      serviceStart = pkgs.writeShellScript "hostenv-provider-service-start" ''
        set -euo pipefail
        exec ${cfg.package}/bin/hostenv-provider-service --config ${configFile}
      '';
      # Keep proxy_pass target without a URI part so it is valid in regex
      # locations (e.g. ~ ^/webhook/) and preserves the incoming request path.
      proxySocket = "http://unix:${cfg.listenSocket}:";
      providerProxyTimeout = "120s";
      proxyTimeoutConfig = ''
        proxy_connect_timeout ${providerProxyTimeout};
        proxy_send_timeout ${providerProxyTimeout};
        proxy_read_timeout ${providerProxyTimeout};
      '';
      configFile = pkgs.writeText "hostenv-provider-config.json" (builtins.toJSON {
        dataDir = cfg.dataDir;
        flakeRoot = cfg.flakeRoot;
        listenSocket = cfg.listenSocket;
        webhookSecretFile = cfg.webhookSecretFile;
        webhookSecretsDir = cfg.webhookSecretsDir;
        webhookHost = cfg.webhookHost;
        uiBasePath = cfg.uiBasePath;
        uiBaseUrl = "${cfg.uiScheme}://${cfg.uiHost}";
        dbUri = cfg.dbUri;
        gitlab = cfg.gitlab;
        seedUsers = seedUsers;
        gitCredentialsFile = cfg.gitCredentialsFile;
        gitConfigFile = cfg.gitConfigFile;
        flakeTemplate = cfg.flakeTemplate;
        jobs = cfg.jobs;
        deploy = cfg.deploy;
      });

    in
    {
      options.services.hostenv-provider = {
        enable = lib.mkEnableOption "Hostenv provider service (webhooks + admin UI)";

        source = lib.mkOption {
          type = lib.types.path;
          default = providerService.src;
          description = "Path to the hostenv-provider-service source tree.";
        };

        haskellDeps = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = providerService.haskellDeps;
          description = "Haskell package names required by hostenv-provider-service.";
        };

        package = lib.mkOption {
          type = lib.types.package;
          default = serviceBin;
          defaultText = lib.literalExpression ''
            pkgs.writeShellScriptBin "hostenv-provider-service" '''
              exec ''${pkgs.haskell.packages.ghc912.ghcWithPackages [ ... ]}/bin/runghc -i''${config.services.hostenv-provider.source} ''${config.services.hostenv-provider.source}/Main.hs
            ''';
          '';
          description = "Package providing the hostenv-provider-service executable.";
        };

        dataDir = lib.mkOption {
          type = lib.types.str;
          default = "${config.hostenv.dataDir}/hostenv-provider";
          description = "Working directory for the provider service (uses XDG_DATA_HOME/hostenv-provider).";
        };

        listenSocket = lib.mkOption {
          type = lib.types.str;
          default = "${config.hostenv.runtimeDir}/hostenv-provider.sock";
          description = "Unix socket path the webhook service should bind to.";
        };

        webhookSecretFile = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          readOnly = true;
          description = "Path to a global webhook secret token (GitHub HMAC or GitLab token).";
        };

        webhookSecretsDir = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          readOnly = true;
          description = "Directory containing per-project webhook secrets, named by hash or org__project.";
        };

        webhookHost = lib.mkOption {
          type = lib.types.str;
          default = config.hostenv.hostname;
          description = "Hostname whose nginx vhost should proxy /webhook/<hash> to the service socket.";
        };

        uiBasePath = lib.mkOption {
          type = lib.types.str;
          default = "/dashboard";
          description = "Base path for the admin UI.";
        };

        uiHost = lib.mkOption {
          type = lib.types.str;
          default = cfg.webhookHost;
          description = "Hostname used to construct OAuth redirect URLs for the admin UI.";
        };

        uiScheme = lib.mkOption {
          type = lib.types.str;
          default = "https";
          description = "Scheme used to construct OAuth redirect URLs for the admin UI.";
        };

        dbUri = lib.mkOption {
          type = lib.types.str;
          default = "host=${config.hostenv.runtimeDir} dbname=hostenv-provider user=${config.hostenv.userName}";
          description = "PostgreSQL connection string for the provider UI.";
        };

        gitlab = {
          enable = lib.mkEnableOption "Gitlab OAuth support";

          oAuthSecretsFile = lib.mkOption {
            type = lib.types.str;
            default = "/run/secrets/${config.hostenv.userName}/gitlab_oauth";
            readOnly = true;
            description = "Path to a secrets file containing GitLab OAuth client_id/client_secret.";
          };

          hosts = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [ "gitlab.com" ];
            description = "Allowed GitLab hosts for OAuth.";
          };

          tokenEncryptionKeyFile = lib.mkOption {
            type = lib.types.str;
            default = "/run/secrets/${config.hostenv.userName}/gitlab_token_key";
            readOnly = true;
            description = "Path to a key file used to encrypt persisted GitLab OAuth tokens.";
          };

          deployTokenTtlMinutes = lib.mkOption {
            type = lib.types.int;
            default = 60;
            description = "Requested lifetime (in minutes) for per-deploy GitLab project access tokens.";
          };
        };

        gitCredentialsFile = lib.mkOption {
          type = lib.types.str;
          default = "${cfg.dataDir}/git-credentials";
          description = "Path to the git credential store file (generated).";
        };

        gitConfigFile = lib.mkOption {
          type = lib.types.str;
          default = "${cfg.dataDir}/gitconfig";
          description = "Path to the git config file used to point at the credential store.";
        };

        flakeTemplate = lib.mkOption {
          type = lib.types.str;
          default = "flake.template.nix";
          description = "Path to the flake template (relative to flakeRoot if not absolute).";
        };

        cacheAuthPasswordFile = lib.mkOption {
          type = lib.types.str;
          default = "/run/secrets/${config.hostenv.userName}/cache_auth_password";
          readOnly = true;
          description = "Path to cache auth password secret used to render runtime htpasswd/netrc files.";
        };

        flakeRoot = lib.mkOption {
          type = lib.types.str;
          default = "work/provider";
          description = "Subdirectory within dataDir that contains flake.nix.";
        };

        jobs = {
          retentionDays = lib.mkOption {
            type = lib.types.int;
            default = 30;
            description = "How many days of completed provider job logs are retained.";
          };

          cleanupIntervalMins = lib.mkOption {
            type = lib.types.int;
            default = 1440;
            description = "How often completed provider job logs are cleaned up.";
          };

          waitTimeoutMins = lib.mkOption {
            type = lib.types.int;
            default = 120;
            description = "How long a job may remain in waiting before it is marked failed.";
          };

          waitInterval = lib.mkOption {
            type = lib.types.int;
            default = 60;
            description = "How often (in seconds) waiting jobs are scanned for timeout expiry.";
          };
        };

        deploy = {
          enable = lib.mkEnableOption "provider-deploy metadata and node callback APIs";

          nodeAuthTokensFile = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Path to JSON/YAML object mapping node name -> bearer token for deploy callback API auth.";
          };
        };
      };

      config = lib.mkMerge [
        {
          assertions =
            let
              anyUsers = builtins.any (userName: gitlabUsernamesFor userName != [ ]) allUsernames;
            in
            [
              {
                assertion = !(anyUsers && (!cfg.enable || !cfg.gitlab.enable));
                message = ''
                  users.<name>.gitlabUsername is configured in enabled environments,
                  but services.hostenv-provider.enable and services.hostenv-provider.gitlab.enable are not both true.
                '';
              }
              {
                assertion = !(cfg.deploy.enable && providerServiceSelection == null);
                message = ''
                  services.hostenv-provider.deploy.enable requires provider.service to be configured.
                '';
              }
              {
                assertion = !(cfg.deploy.enable && !isSelectedService);
                message = ''
                  services.hostenv-provider.deploy.enable is only supported on the provider.service environment.
                '';
              }
              {
                assertion = !(cfg.deploy.enable && isSelectedService && cfg.deploy.nodeAuthTokensFile == null);
                message = ''
                  services.hostenv-provider.deploy.nodeAuthTokensFile must be configured when services.hostenv-provider.deploy.enable is true.
                '';
              }
            ]
            ++ map
              (userName:
                let
                  assignments =
                    lib.concatStringsSep ", "
                      (map
                        (entry:
                          let
                            value = normalizeGitlabUsername (entry.user.gitlabUsername or null);
                          in
                          "${entry.envName}=${if value == null then "<unset>" else value}")
                        (userEntriesFor userName));
                in
                {
                  assertion = false;
                  message = ''
                    user '${userName}' has conflicting gitlabUsername values across enabled environments: ${assignments}
                  '';
                })
              conflictingUsers;
        }
        (lib.mkIf cfg.enable {
          services.hostenv-provider.deploy.nodeAuthTokensFile =
            if isSelectedService then deployTokenMapPath else null;

          services.postgresql = {
            enable = lib.mkDefault true;
            user = lib.mkDefault config.hostenv.userName;
            dataDir = lib.mkDefault "${config.hostenv.dataDir}/postgresql";
            runtimeDir = lib.mkDefault config.hostenv.runtimeDir;
            ensureDatabases = lib.mkDefault [ "hostenv-provider" ];
            ensureUsers = lib.mkDefault [
              {
                name = config.hostenv.userName;
                ensurePermissions = {
                  "hostenv-provider" = "ALL PRIVILEGES";
                };
              }
            ];
          };

          services.nginx.enable = lib.mkDefault true;
          services.nginx.virtualHosts = {
            "${cfg.webhookHost}" = {
              locations = lib.mkMerge [
                {
                  "~ ^/webhook/" = {
                    recommendedProxySettings = true;
                    proxyPass = proxySocket;
                    extraConfig = proxyTimeoutConfig;
                  };
                  "~ ^/api/" = {
                    recommendedProxySettings = true;
                    proxyPass = proxySocket;
                    extraConfig = ''
                      ${proxyTimeoutConfig}
                      proxy_http_version 1.1;
                      proxy_set_header Upgrade $http_upgrade;
                      proxy_set_header Connection "upgrade";
                    '';
                  };
                }
                {
                  "${cfg.uiBasePath}" = {
                    recommendedProxySettings = true;
                    proxyPass = proxySocket;
                    extraConfig = proxyTimeoutConfig;
                  };
                }
              ];
              serverName = lib.mkDefault cfg.webhookHost;
            };
          };

          systemd.services.hostenv-provider = {
            description = "Hostenv provider webhook service";
            wantedBy = [ "default.target" ];
            wants = lib.optional config.services.postgresql.enable "postgresql.service";
            after = [ "network.target" ] ++ lib.optional config.services.postgresql.enable "postgresql.service";
            restartIfChanged = false;
            path = [
              pkgs.coreutils
              pkgs.curl
              pkgs.git
              pkgs.bind
              pkgs.nix
              pkgs.openssh
              ghc
            ];
            serviceConfig = {
              ExecStart = "${serviceStart}";
              Restart = "on-failure";
              RestartSec = "5s";
            };
          };

          systemd.services.hostenv-provider-cache-auth = {
            description = "Render provider cache auth files from cache_auth_password";
            wantedBy = [ "default.target" ];
            before = [ "harmonia.service" "nginx.service" ];
            script = ''
              set -euo pipefail
              umask 077
              password="$(tr -d '\n\r' < "${cfg.cacheAuthPasswordFile}")"
              hash="$(${pkgs.openssl}/bin/openssl passwd -apr1 "$password")"
              cat > "${config.hostenv.runtimeDir}/cache_htpasswd" <<EOF
cache:$hash
EOF
              chmod 0400 "${config.hostenv.runtimeDir}/cache_htpasswd"
            '';
            serviceConfig = {
              Type = "oneshot";
              RemainAfterExit = true;
            };
          };

          profile = [ cfg.package ];
        })
      ];
    }
  ;
}
