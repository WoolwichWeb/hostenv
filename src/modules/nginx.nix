{ lib, config, pkgs, ... }:
let
  cfg = config.services.nginx;
  # @todo: this includes some unsupported options, such as acme certificates
  # for Let's Encrypt; maybe find a way to override or fork this module.
  vhostOptions = import (pkgs.path + "/nixos/modules/services/web-servers/nginx/vhost-options.nix") { inherit config lib; };
  vhostValues = pkgs.callPackage ./vhost.nix { inherit (cfg) virtualHosts; };

  configFile =
    (
      if cfg.validateConfig
      then pkgs.writers.writeNginxConfig
      else pkgs.writeText
    ) "nginx.conf" ''
      pid ${config.hostenv.runtimeDir}/nginx.pid;
      error_log syslog:server=unix:/dev/log debug;
      daemon off;

      events {
        ${cfg.eventsConfig}
      }

      http {
        ${cfg.commonHttpConfig}

        ${lib.optionalString cfg.recommendedOptimisation ''
        # Recommended optimisation.
        sendfile on;
        tcp_nopush on;
        tcp_nodelay on;
        keepalive_timeout 65;
        ''}

        ${lib.optionalString cfg.recommendedCompression ''
        # Recommended Brotli and gzip settings.
        # https://github.com/NixOS/nixpkgs/blob/nixos-24.11/nixos/modules/services/web-servers/nginx/default.nix#L202
        brotli on;
        brotli_static on;
        brotli_comp_level 5;
        brotli_window 512k;
        brotli_min_length 256;
        brotli_types ${pkgs.lib.concatStringsSep " " compressMimeTypes};

        gzip on;
        gzip_static on;
        gzip_vary on;
        gzip_comp_level 5;
        gzip_min_length 256;
        gzip_proxied expired no-cache no-store private auth;
        gzip_types ${pkgs.lib.concatStringsSep " " compressMimeTypes};
        ''}

        ${vhostValues}
      }
    '';

  # Mime.types values are taken from brotli sample configuration - https://github.com/google/ngx_brotli
  # and Nginx Server Configs - https://github.com/h5bp/server-configs-nginx
  # "text/html" is implicitly included in {brotli,gzip,zstd}_types
  compressMimeTypes = [
    "application/atom+xml"
    "application/geo+json"
    "application/javascript" # Deprecated by IETF RFC 9239, but still widely used
    "application/json"
    "application/ld+json"
    "application/manifest+json"
    "application/rdf+xml"
    "application/vnd.ms-fontobject"
    "application/wasm"
    "application/x-rss+xml"
    "application/x-web-app-manifest+json"
    "application/xhtml+xml"
    "application/xliff+xml"
    "application/xml"
    "font/collection"
    "font/otf"
    "font/ttf"
    "image/bmp"
    "image/svg+xml"
    "image/vnd.microsoft.icon"
    "text/cache-manifest"
    "text/calendar"
    "text/css"
    "text/csv"
    "text/javascript"
    "text/markdown"
    "text/plain"
    "text/vcard"
    "text/vnd.rim.location.xloc"
    "text/vtt"
    "text/x-component"
    "text/xml"
  ];

  execCommand = "${cfg.package}/bin/nginx -e stderr -c ${configFile}";
in
{
  options.services.nginx = {
    enable = lib.mkEnableOption "nginx Web server" // { default = cfg.virtualHosts != null; };

    package = lib.mkOption {
      default = pkgs.nginx;
      defaultText = lib.literalExpression "pkgs.nginx";
      type = lib.types.package;
      apply = p: p.override {
        modules = lib.unique (p.modules ++ cfg.additionalModules);
      };
    };

    additionalModules = lib.mkOption {
      default = [ pkgs.nginxModules.brotli ];
      defaultText = lib.literalExpression "[ pkgs.nginxModules.brotli ]";
      type = lib.types.listOf (lib.types.attrsOf lib.types.anything);
      example = lib.literalExpression "[ pkgs.nginxModules.echo ]";
      description = ''
        Additional [third-party nginx modules](https://www.nginx.com/resources/wiki/modules/)
        to install. Packaged modules are available in `pkgs.nginxModules`.
      '';
    };

    virtualHosts = lib.mkOption {
      type = lib.types.nullOr (lib.types.attrsOf (lib.types.submodule vhostOptions));
      description = "Optional virtual host configuration. Enabling a framework provides a sensible default.";
      default = null;
    };

    defaultMimeTypes = lib.mkOption {
      type = lib.types.path;
      default = "${pkgs.mailcap}/etc/nginx/mime.types";
      defaultText = lib.literalExpression "\${pkgs.mailcap}/etc/nginx/mime.types";
      example = lib.literalExpression "\${pkgs.nginx}/conf/mime.types";
      description = ''
        Default MIME types for NGINX, as MIME types definitions from NGINX are very incomplete,
        we use by default the ones bundled in the mailcap package, used by most of the other
        Linux distributions.
      '';
    };

    recommendedOptimisation = lib.mkOption {
      default = false;
      type = lib.types.bool;
      description = ''
        Enable recommended optimisation settings.
      '';
    };

    recommendedCompression = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Enable recommended compression settings.
        Learn more about compression in Brotli format [here](https://github.com/google/ngx_brotli/).

        This adds `pkgs.nginxModules.brotli` to `services.nginx.additionalModules`.
      '';
    };

    serverTokens = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Show nginx version in headers and error pages.";
    };

    clientMaxBodySize = lib.mkOption {
      type = lib.types.str;
      default = "100m";
      description = "Set nginx global client_max_body_size.";
    };

    eventsConfig = lib.mkOption {
      type = lib.types.lines;
      default = ''
        worker_connections 1024;
      '';
      description = "The nginx events configuration.";
    };

    commonHttpConfig = lib.mkOption {
      type = lib.types.lines;
      default = ''
        # Per-workspace temp paths, if this not set per-workspace every nginx
        # instance tries to write to the same directory, with permissions
        # errors being the result for all but one of them.
        client_body_temp_path ${config.hostenv.stateDir}/nginx/;
        proxy_temp_path ${config.hostenv.stateDir}/nginx/;
        fastcgi_temp_path ${config.hostenv.stateDir}/nginx/;
        scgi_temp_path ${config.hostenv.stateDir}/nginx/;
        uwsgi_temp_path ${config.hostenv.stateDir}/nginx/;

        # Load mime types and configure maximum size of the types hash tables.
        include ${cfg.defaultMimeTypes};
        # The following is tailoured to work with default mime types above.
        types_hash_max_size 2688;

        include ${cfg.package}/conf/fastcgi.conf;
        include ${cfg.package}/conf/uwsgi_params;

        fastcgi_buffers 16 16k;
        fastcgi_buffer_size 32k;
        client_max_body_size ${cfg.clientMaxBodySize};

        default_type application/octet-stream;

        # log_format scripts '$document_root$fastcgi_script_name > $request';
        access_log syslog:server=unix:/dev/log combined;
      '';
      example = ''
        resolver 127.0.0.1 valid=5s;

        log_format myformat '$remote_addr - $remote_user [$time_local] '
                            '"$request" $status $body_bytes_sent '
                            '"$http_referer" "$http_user_agent"';
      '';
      description = ''
        With nginx you must provide common http context definitions before
        they are used, e.g. log_format, resolver, etc. inside of server
        or location contexts. Use this attribute to set these definitions
        at the appropriate location.
      '';
    };

    validateConfig = lib.mkEnableOption "validation of the nginx config." // { default = true; };
  };

  config = lib.mkIf (cfg.enable && cfg.virtualHosts != null) {
    systemd.services.nginx = {
      description = "Nginx Web Server for ${config.hostenv.userName}";
      wantedBy = [ "default.target" ];
      after = [ "network.target" ];
      reloadIfChanged = true;
      preStart = ''
        ${execCommand} -t
      '';
      startLimitIntervalSec = 60;
      serviceConfig = {
        ExecStart = execCommand;
        ExecReload = [
          "${execCommand} -t"
          "${pkgs.coreutils}/bin/kill -HUP $MAINPID"
        ];
        Restart = "always";
        RestartSec = "10s";
      };
    };
  };
}
