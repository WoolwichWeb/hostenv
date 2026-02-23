{ config, ... }:
let
  cfgTop = config;
in
{
  flake.modules.hostenv.hostenv-provider-service =
    { lib, config, pkgs, ... }:
    let
      cfg = config.services.hostenv-provider;
      enabledEnvironments = lib.filterAttrs (_: env: env.enable or true) (config.hostenv.publicEnvironments or { });
      enabledEnvironmentNames = lib.sort builtins.lessThan (builtins.attrNames enabledEnvironments);
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
      ghc = pkgs.haskell.packages.ghc912.ghcWithPackages (p: map (name: p.${name}) haskellDeps);
      serviceBin = pkgs.writeShellScriptBin "hostenv-provider-service" ''
        exec ${ghc}/bin/runghc -i${serviceSrc} ${serviceSrc}/Main.hs "$@"
      '';
      serviceStart = pkgs.writeShellScript "hostenv-provider-service-start" ''
        set -euo pipefail
        mkdir -p "${cfg.dataDir}"
        exec ${cfg.package}/bin/hostenv-provider-service --config ${configFile}
      '';
      # Keep proxy_pass target without a URI part so it is valid in regex
      # locations (e.g. ~ ^/webhook/) and preserves the incoming request path.
      proxySocket = "http://unix:${cfg.listenSocket}:";
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

        flakeRoot = lib.mkOption {
          type = lib.types.str;
          default = ".";
          description = "Subdirectory within dataDir that contains flake.nix.";
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
                  };
                }
                {
                  "${cfg.uiBasePath}" = {
                    recommendedProxySettings = true;
                    proxyPass = proxySocket;
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
          profile = [ cfg.package ];
        })
      ];
    }
  ;
}
