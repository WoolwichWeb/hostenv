{ ... }:
{
  flake.modules.hostenv.harmonia =
    { lib, config, pkgs, ... }:
    let
      cfg = config.services.harmonia;
      providerServiceEnabled = config.services.hostenv-provider.enable or false;
      enabled = cfg.enable || providerServiceEnabled;
      vhostName =
        if providerServiceEnabled
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

        signingKeyFile = lib.mkOption {
          type = lib.types.str;
          default = "/run/secrets/${config.hostenv.userName}/cache_signing_key";
          description = "Path to Harmonia private signing key.";
        };

        htpasswdFile = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = "${config.hostenv.runtimeDir}/cache_htpasswd";
          description = "Optional htpasswd file path used by nginx for /cache auth.";
        };

        extraArgs = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ ];
          description = "Extra command-line arguments passed to Harmonia.";
        };

        fcgiwrapSocketPath = lib.mkOption {
          type = lib.types.str;
          default = "${config.hostenv.runtimeDir}/fcgiwrap.sock";
          description = "Unix socket path for fcgiwrap helper service.";
        };
      };

      config = lib.mkIf enabled {
        services.nginx.enable = lib.mkDefault true;
        services.nginx.virtualHosts.${vhostName}.locations."/cache/" = {
          recommendedProxySettings = true;
          proxyPass = proxySocket;
          extraConfig = ''
            rewrite ^/cache/?(.*)$ /$1 break;
            ${authConfig}
          '';
        };

        systemd.services.harmonia = {
          description = "Harmonia binary cache service";
          wantedBy = [ "default.target" ];
          after = [ "network.target" ];
          serviceConfig = {
            ExecStart =
              "${cfg.package}/bin/harmonia --bind unix:${cfg.socketPath} --sign-key-path ${cfg.signingKeyFile}"
              + lib.optionalString (cfg.extraArgs != [ ])
              (" " + lib.escapeShellArgs cfg.extraArgs);
            Restart = "on-failure";
            RestartSec = "5s";
          };
        };

        systemd.sockets.fcgiwrap = {
          wantedBy = [ "sockets.target" ];
          socketConfig = {
            ListenStream = cfg.fcgiwrapSocketPath;
            SocketMode = "0600";
          };
        };

        systemd.services.fcgiwrap = {
          description = "fcgiwrap helper";
          after = [ "fcgiwrap.socket" ];
          requires = [ "fcgiwrap.socket" ];
          serviceConfig = {
            ExecStart = "${pkgs.fcgiwrap}/bin/fcgiwrap -f -s unix:${cfg.fcgiwrapSocketPath}";
            Restart = "on-failure";
            RestartSec = "5s";
          };
        };
      };
    }
  ;
}
