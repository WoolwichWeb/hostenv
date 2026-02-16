{ config, ... }:
let
  cfgTop = config;
in
{
  flake.modules.hostenv.hostenv-provider-service =
    { lib, config, pkgs, ... }:
    let
      cfg = config.services.hostenv-provider;
      providerService = cfgTop.flake.lib.provider.service;
      serviceSrc = cfg.source;
      haskellDeps = cfg.haskellDeps;
      ghc = pkgs.haskellPackages.ghcWithPackages (p: map (name: p.${name}) haskellDeps);
      serviceBin = pkgs.writeShellScriptBin "hostenv-provider-service" ''
        exec ${ghc}/bin/runghc -i${serviceSrc} ${serviceSrc}/Main.hs
      '';
      proxySocket = "http://unix:${cfg.listenSocket}:/";

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
              exec ''${pkgs.haskellPackages.ghcWithPackages [ ... ]}/bin/runghc -i''${config.services.hostenv-provider.source} ''${config.services.hostenv-provider.source}/Main.hs
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
          description = "Path to a global webhook secret token (GitHub HMAC or GitLab token).";
        };

        webhookSecretsDir = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = "Directory containing per-project webhook secrets, named by hash or org__project.";
        };

        webhookHost = lib.mkOption {
          type = lib.types.str;
          default = config.hostenv.hostname;
          description = "Hostname whose nginx vhost should proxy /webhook/<hash> to the service socket.";
        };

        uiBasePath = lib.mkOption {
          type = lib.types.str;
          default = "/ui";
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

        gitlabOAuthSecretsFile = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = "Path to a secrets file containing GitLab OAuth client_id/client_secret.";
        };

        gitlabHosts = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ "gitlab.com" ];
          description = "Allowed GitLab hosts for OAuth.";
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

        repoSource = lib.mkOption {
          type = lib.types.oneOf [ lib.types.path lib.types.str ];
          default = config.hostenv.root;
          description = "Path to the provider repo to copy into the dataDir if missing.";
        };

        flakeRoot = lib.mkOption {
          type = lib.types.str;
          default = ".";
          description = "Subdirectory within repoSource that contains flake.nix.";
        };
      };

      config = lib.mkIf cfg.enable {
        services.nginx.enable = lib.mkDefault true;
        services.nginx.virtualHosts."${cfg.webhookHost}".locations = lib.mkMerge [
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
        services.nginx.virtualHosts."${cfg.webhookHost}".serverName = lib.mkDefault cfg.webhookHost;

        systemd.services.hostenv-provider = {
          description = "Hostenv provider webhook service";
          wantedBy = [ "default.target" ];
          after = [ "network.target" ];
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
          environment =
            {
              XDG_DATA_HOME = config.hostenv.dataDir;
              HOSTENV_PROVIDER_DATA_DIR = cfg.dataDir;
              HOSTENV_PROVIDER_REPO_SOURCE = toString cfg.repoSource;
              HOSTENV_PROVIDER_FLAKE_ROOT = cfg.flakeRoot;
              HOSTENV_PROVIDER_LISTEN_SOCKET = cfg.listenSocket;
              HOSTENV_PROVIDER_WEBHOOK_HOST = cfg.webhookHost;
              HOSTENV_PROVIDER_UI_BASE_PATH = cfg.uiBasePath;
              HOSTENV_PROVIDER_UI_BASE_URL = "${cfg.uiScheme}://${cfg.uiHost}";
              HOSTENV_PROVIDER_DB_URI = cfg.dbUri;
              HOSTENV_PROVIDER_GITLAB_HOSTS = lib.concatStringsSep "," cfg.gitlabHosts;
              HOSTENV_PROVIDER_GIT_CREDENTIALS_FILE = cfg.gitCredentialsFile;
              HOSTENV_PROVIDER_GIT_CONFIG_FILE = cfg.gitConfigFile;
              HOSTENV_PROVIDER_FLAKE_TEMPLATE = cfg.flakeTemplate;
            }
            // lib.optionalAttrs (cfg.webhookSecretFile != null) {
              HOSTENV_PROVIDER_WEBHOOK_SECRET_FILE = cfg.webhookSecretFile;
            }
            // lib.optionalAttrs (cfg.webhookSecretsDir != null) {
              HOSTENV_PROVIDER_WEBHOOK_SECRETS_DIR = cfg.webhookSecretsDir;
            }
            // lib.optionalAttrs (cfg.gitlabOAuthSecretsFile != null) {
              HOSTENV_PROVIDER_GITLAB_SECRETS_FILE = cfg.gitlabOAuthSecretsFile;
            };
          serviceConfig = {
            ExecStart = "${cfg.package}/bin/hostenv-provider-service";
            WorkingDirectory = cfg.dataDir;
            Restart = "on-failure";
            RestartSec = "5s";
          };
        };
        profile = [ cfg.package ];
      };
    }
  ;
}
