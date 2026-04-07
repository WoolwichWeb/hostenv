{ ... }:
{
  flake.modules.hostenv.harmonia =
    { lib, config, pkgs, ... }:
    let
      cfg = config.services.harmonia;
      format = pkgs.formats.toml { };
      configFile = format.generate "harmonia.toml" cfg.settings;
      vhostName =
        if config.services.hostenv-provider.enable
        then config.services.hostenv-provider.webhookHost
        else config.hostenv.hostname;
      proxySocket = "http://unix:${cfg.socketPath}:";
      authConfig =
        if cfg.htpasswdFile == null || cfg.htpasswdFile == ""
        then ""
        else ''
          auth_basic "Hostenv cache";
          auth_basic_user_file ${cfg.htpasswdFile};
        '';
    in
    {
      options.services.harmonia = {
        enable = lib.mkEnableOption "harmonia binary cache service";

        package = lib.mkOption {
          type = lib.types.package;
          default = pkgs.harmonia;
          description = "Harmonia package to execute.";
        };

        socketPath = lib.mkOption {
          type = lib.types.str;
          default = "${config.hostenv.runtimeDir}/harmonia.sock";
          description = "Unix socket path for Harmonia HTTP traffic.";
        };

        signKeyPaths = lib.mkOption {
          type = lib.types.listOf lib.types.path;
          default = [ "/run/secrets/${config.hostenv.userName}/cache_signing_key" ];
          description = "Paths to the signing keys to use for signing the cache";
        };

        htpasswdFile = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = "${config.hostenv.runtimeDir}/cache_htpasswd";
          description = "Optional htpasswd file path used by nginx for /cache auth.";
        };


        settings = lib.mkOption {
          type = lib.types.submodule { freeformType = format.type; };
          description = "Settings to merge with the default configuration";
        };
      };

      config = lib.mkIf (cfg.enable || config.services.hostenv-provider.enable) {
        services.nginx.enable = lib.mkDefault true;
        services.nginx.virtualHosts.${vhostName}.locations."/cache/" = {
          recommendedProxySettings = true;
          proxyPass = proxySocket;
          extraConfig = ''
            rewrite ^/cache/?(.*)$ /$1 break;
            ${authConfig}
          '';
        };

        services.harmonia.settings = builtins.mapAttrs (_: v: lib.mkDefault v) ({
          bind = "unix:${cfg.socketPath}";
          workers = 4;
          max_connection_rate = 256;
          priority = 50;
        });

        systemd.services.harmonia = {
          description = "Harmonia binary cache service";
          wantedBy = [ "default.target" ];
          after = [ "network.target" ];
          environment = {
            NIX_REMOTE = "daemon";
            LIBEV_FLAGS = "4"; # go ahead and mandate epoll(2)
            CONFIG_FILE = lib.mkIf (configFile != null) configFile;
            SIGN_KEY_PATHS = lib.concatStringsSep " " cfg.signKeyPaths;
            RUST_LOG = "actix_web=debug";
            RUST_BACKTRACE = "1";
          };
          serviceConfig = {
            ExecStart = "${cfg.package}/bin/harmonia";
            Restart = "on-failure";
            RestartSec = "5s";
          };
        };
      };
    }
  ;
}
