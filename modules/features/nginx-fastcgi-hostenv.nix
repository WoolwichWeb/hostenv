{ ... }:
{
  flake.modules.hostenv.nginx-fastcgi-hostenv =
    { lib, config, ... }:
    let
      cfg = config.hostenv.nginxFastcgi;
      socketFor = pool: "unix:${config.hostenv.runtimeDir}/${pool}.sock";
      mkVhost = name: pool: {
        ${name} = {
          serverName = name;
          locations."~ \\.(php|phtml)$" = {
            recommendedProxySettings = true;
            extraConfig = ''
              include ${config.services.nginx.package}/conf/fastcgi_params;
              fastcgi_pass ${socketFor pool};
              fastcgi_param SCRIPT_FILENAME $document_root$fastcgi_script_name;
            '';
          };
        };
      };
      overlays =
        if cfg.enable && cfg.pool != null && cfg.vhosts != [ ] then
          lib.foldl' lib.recursiveUpdate { }
            (map (v: mkVhost v cfg.pool) cfg.vhosts)
        else { };
    in
    {
      options.hostenv.nginxFastcgi = {
        enable = lib.mkEnableOption "Add a default PHP fastcgi location for selected virtual hosts" // { default = true; };
        pool = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = if (config.services.phpfpm.pools or { }) == { } then null else builtins.head (builtins.attrNames config.services.phpfpm.pools);
          description = "PHP-FPM pool name to route default fastcgi requests to when auto-fastcgi is enabled.";
        };
        vhosts = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ config.hostenv.hostname ];
          description = "Virtual host names to which the default PHP fastcgi location will be added.";
        };
      };

      config = lib.mkIf (cfg.enable && cfg.pool != null && cfg.vhosts != [ ]) {
        services.nginx.virtualHosts = overlays;
      };
    }
  ;
}
