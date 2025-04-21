{ lib, config, pkgs, ... }:
let
  cfg = config.services.php-app;

  phpPackage = (cfg.phpPackage.buildEnv {
    extensions = ({ enabled, all }: enabled ++ (with all; [
      apcu
      pdo
      pdo_mysql
      redis
    ]));
    extraConfig = ''
      apc.enable_cli = 1
    '';
  });
in
{
  options.services.php-app = {
    enable = lib.mkEnableOption ''support for a simple PHP application + db.
      Enabling this will bring up an nginx, PHP-FPM, and MySQL.
    '';

    frontController = lib.mkOption {
      type = lib.types.bool;
      description = ''
        Does the PHP application use a front-controller pattern?

        Requests will be matched against files first, then if a match is not
        found, passed to `index.php` in query parameters. For example:
        `index.php?some/page/path`.
      '';
      default = true;
    };

    phpPackage = lib.mkPackageOption pkgs "php" { };

    codebase = {
      name = lib.mkOption {
        type = lib.types.str;
        description = ''
          Name of the codebase. Used to identify the service in various
          configuration, e.g. nginx, systemd services.
          May be any alphanumeric name, just ensure it only contains letters
          and numbers to avoid confusing errors when starting services.
        '';
        default = config.hostenv.userName + "-application";
        defaultText = lib.literalExpression ''
          config.hostenv.userName + "-application"
        '';
      };

      version = lib.mkOption {
        type = lib.types.str;
        default = "1.0.0-dev";
        description = "Optional. Define a version number, will be used when generating directory names and such.";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    services.nginx.virtualHosts = {
      "${cfg.codebase.name}" = lib.mkDefault
        {
          serverName = "_";
          default = true;
          forceSSL = false;
          root = config.hostenv.root;

          # Set up nginx for Drupal.
          locations."~ '\.php$'" = {
            extraConfig = ''
              fastcgi_pass unix:${config.hostenv.runtimeDir}/${cfg.codebase.name}.sock;
              fastcgi_index index.php;
 
              fastcgi_split_path_info ^(.+?\.php)(|/.*)$;
              # Ensure the php file exists. Mitigates CVE-2019-11043
              try_files $fastcgi_script_name =404;
              fastcgi_intercept_errors on;
            '';

            fastcgiParams = {
              # Block httpoxy attacks. See https://httpoxy.org/.
              HTTP_PROXY = "";
              SCRIPT_FILENAME = "$document_root$fastcgi_script_name";
              PATH_INFO = "$fastcgi_path_info";
              QUERY_STRING = "$query_string";
            };
          };
          locations."= /favicon.ico" = {
            extraConfig = ''
              log_not_found off;
              access_log off;
            '';
          };
          locations."= /robots.txt" = {
            extraConfig = ''
              allow all;
              log_not_found off;
              access_log off;
            '';
          };
          locations."~ \..*/.*\.php$" = {
            return = 403;
          };
          locations."~ /vendor/.*\.php$" = {
            return = 404;
            extraConfig = ''
              deny all;
            '';
          };
          locations."~* \.(js|css|png|jpg|jpeg|gif|ico|svg|avif|wasm)$" = {
            extraConfig = ''
              ${ if cfg.frontController then ''
              try_files $uri @rewrite;
              '' else '''' }
              expires max;
              log_not_found off;
            '';
          };

          listen = [{ addr = "unix:${config.hostenv.upstreamRuntimeDir}/in.sock"; }];
        } // (if cfg.frontController then {
        locations."/" = {
          extraConfig = ''
            try_files $uri /index.php?$query_string;
          '';
        };
        locations."@rewrite" = {
          extraConfig = ''
            rewrite ^ /index.php;
          '';
        };
      } else { });
    };

    services.phpfpm.pools."${cfg.codebase.name}" = {
      inherit phpPackage;

      phpOptions = lib.mkDefault ''
        upload_max_filesize = 1G
        post_max_size = 1G
        memory_limit = 512M
        error_log = syslog
        syslog.ident = php
        syslog.facility = user
      '';

      settings = {
        "pm" = lib.mkDefault "dynamic";
        "pm.max_children" = lib.mkDefault 16;
        "pm.min_spare_servers" = lib.mkDefault 4;
        "pm.max_spare_servers" = lib.mkDefault 8;
        "pm.start_servers" = lib.mkDefault 4;
        "pm.max_requests" = lib.mkDefault 500;
        "php_admin_value[error_log]" = "stderr";
        "php_admin_flag[log_errors]" = true;
        "catch_workers_output" = true;
      };
    };

    # @todo: add support for PostgreSQL and/or sqlite.
    services.mysql = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.mariadb;
      user = config.hostenv.userName;
      dataDir = "${config.hostenv.dataDir}/mysql";

      initialDatabases = [
        { name = "drupal"; }
      ];

      ensureUsers = [
        {
          name = config.hostenv.userName;
          ensurePermissions = {
            "drupal.*" = "ALL PRIVILEGES";
          };
        }
        {
          name = "backup";
          ensurePermissions = {
            "*.*" = "SELECT, LOCK TABLES";
          };
        }
      ];

      # Replication enables the binary logging, which gets us
      # point-in-time restore in case anything goes wrong. However,
      # replication to another host is not setup yet.
      replication = lib.mkDefault {
        role = "master";
        masterUser = "replication_primary";
        # While we skip-networking and only listen on a socket,
        # this isn't a concern.
        masterPassword = "";
        # @todo:
        slaveHost = "localhost";
      };

      settings = {
        mysqld = {
          max_connections = lib.mkDefault 1000;
          table_cache = lib.mkDefault 800;
          skip-networking = lib.mkDefault true;
          max_allowed_packet = lib.mkDefault "256M";
          tmp_table_size = lib.mkDefault "32M";
          max_heap_table_size = lib.mkDefault "32M";
          query_cache_size = lib.mkDefault "128M";
          query_cache_limit = lib.mkDefault "8M";
          # This is recommended by Drupal, so make it read-only (unless
          # or until there's a good reason to allow users to change it).
          transaction_isolation = "READ-COMMITTED";

          innodb_buffer_pool_size = lib.mkDefault "10G";
          innodb_log_buffer_size = lib.mkDefault "16M";
          innodb_log_file_size = lib.mkDefault "128M";
          innodb_flush_method = lib.mkDefault "O_DIRECT";
          innodb_stats_on_metadata = lib.mkDefault false;
        };
        mysqldump = {
          quick = true;
          max_allowed_packet = lib.mkDefault "128M";
        };
      };

    };
  };
}
