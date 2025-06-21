{ lib, config, pkgs, ... }:
let
  cfg = config.services.drupal;

  # Note on PHP packaging: the PHP version or package is chosen by the
  # user through the `services.drupal.phpVersion/phpPackage` options, this
  # is fed to the Drupal PHP pool options.
  #
  # The `phpfpm` service builds the PHP package for the Drupal pool (building
  # the package is tricky, as the desired PHP package may be set using a
  # version string) and the let binding below gets that package.
  #
  # This final PHP package is then used in various places in the code, so
  # the same version of PHP is used everywhere.
  drupalPhpPool = config.services.phpfpm.pools."${cfg.codebase.name}";

  utils = import (pkgs.path + "/nixos/lib/utils.nix") { inherit pkgs lib config; };
  # Type for a valid systemd unit option. Needed for correctly passing "timerConfig" to "systemd.timers"
  inherit (utils.systemdUtils.unitOptions) unitOption;

  hostenvSettingsFile = pkgs.writeText "settings.hostenv.php" ''
    <?php

    if (!empty(getenv('XDG_CONFIG_HOME'))) {
      $configDir = getenv('XDG_CONFIG_HOME');
    } else if (!empty(getenv('HOME'))) {
      $configDir = getenv('HOME') . '/.config';
    } else {
      throw new Exception('Could not find config dir using $XDG_CONFIG_DIR or $HOME');
    }

    $settings['file_private_path'] = "${cfg.privateFilesDir}";
    $settings['trusted_host_patterns'] = array_merge(
      $settings['trusted_host_patterns'] ?? [],
      [ '^.+\.hostenv\.sh$' ]
    );

    // For secrets and other things that should not be world-readable in the
    // Nix store.
    if (file_exists($configDir . '/drupal/settings.php')) {
      include $configDir . '/drupal/settings.php';
    }

    if (file_exists('${config.hostenv.runtimeDir}/mysql.sock')) {
      $dbSocket = '${config.hostenv.runtimeDir}/mysql.sock';
    } else {
      throw new Exception('Socket file at \'${config.hostenv.runtimeDir}/mysql.sock\' does not exist');
    }

    // Socket authentication is used here, so there is no password.
    $databases['default']['default'] = [
      'database' => 'drupal',
      'username' => '${config.hostenv.userName}',
      'password' => ''',
      'unix_socket' => $dbSocket,
      'driver' => 'mysql',
      'prefix' => ''',
    ];
  '';

  composerPackage = pkgs.stdenvNoCC.mkDerivation {
    name = cfg.codebase.name + "-scaffold";
    version = cfg.codebase.version;
    dontPatchShebangs = true;
    buildInputs = [ drupalPhpPool.phpPackage.packages.composer ];

    src = drupalPhpPool.phpPackage.buildComposerProject2 (finalAttrs: {
      pname = cfg.codebase.name;
      version = cfg.codebase.version;
      src = lib.cleanSourceWith {
        src = config.hostenv.root;
        filter = path: type: baseNameOf path == "composer.json"
          || baseNameOf path == "composer.lock";
      };
      composerLock = config.hostenv.root + /composer.lock;
      vendorHash = cfg.composer.dependencyHash;
      composerNoPlugins = !cfg.composer.enablePlugins;
      composerNoScripts = !cfg.composer.enableScripts;
      composerNoDev = !cfg.composer.enableDev;
    });

    buildPhase = ''
      pushd share/php/${cfg.codebase.name}
      # Scaffold Drupal files, like 'web/{index.php,autoload.php}' etc.
      composer drupal:scaffold
      popd
    '';

    installPhase = ''
      mkdir -p $out
      cp -r . $out/
    '';
  };

  project = pkgs.stdenvNoCC.mkDerivation
    {
      pname = cfg.codebase.name;
      version = cfg.codebase.version;
      src = config.hostenv.root;
      dontPatchShebangs = true;

      buildPhase =
        let
          settingsPhp = ''
            if (isset($app_root) && isset($site_path)) {
              $sitesDir = $app_root . '/' . $site_path;
            } else {
              $sitesDir = __DIR__;
            }

            if (file_exists('${hostenvSettingsFile}')) {
              include '${hostenvSettingsFile}';
            } else {
              echo('Could not find settings file');
              throw new Exception('Could not find settings file: "${hostenvSettingsFile}"');
            }
          '';
        in
        ''
          if [ -d web ]; then
            export WEBROOT="web/"
          else
            export WEBROOT=""
          fi

          cat >> "$WEBROOT"sites/default/settings.php <<'EOF'
          ${settingsPhp}
          EOF

          ln -s "${cfg.filesDir}" "$WEBROOT"sites/default/files
          ln -s "${hostenvSettingsFile}" "$WEBROOT"sites/default/hostenv.settings.php
          ${lib.optionalString cfg.composer.enable ''
          cp -r ${composerPackage}/share/php/${cfg.codebase.name}/. .
          ''}
        '';

      installPhase = ''
        # This standardises on web accessible files being in `/web`, allowing
        # unofficial support for Drupal 7.
        if [ -d web ]; then
          mkdir -p $out/share/php/${cfg.codebase.name}
          cp -r . $out/share/php/${cfg.codebase.name}/
        else
          mkdir -p $out/share/php/${cfg.codebase.name}/web
          cp -r . $out/share/php/${cfg.codebase.name}/web/
        fi
      '';

    };

  drush = pkgs.writeShellScriptBin "drush" ''
    ${toString project}/share/php/${cfg.codebase.name}/vendor/bin/drush --root=${project} $@
  '';
in
{
  options.services.drupal = {
    enable = lib.mkEnableOption ''support for Drupal in this project.
      Enabling this will bring up all the services required for a Drupal 10+
      project, with some opinionated decisions about PHP setup and which database
      to use (MariaDB).
    '';

    composer = {

      enable = lib.mkEnableOption ''composer integration in this Drupal project.

        If enabled, `dump-autoload` and `drupal:scaffold` will be run during
        the build phase.

        By default, this will be enabled if a 'composer.json' file is found in
        the project root, and that file is in version control.
      '' // { default = lib.pathExists (config.hostenv.root + /composer.lock); };

      dependencyHash = lib.mkOption {
        type = lib.types.str;
        description = "Hash of composer dependencies. Update this when Nix complains about a hash mismatch.";
        default = lib.fakeHash;
      };

      enablePlugins = lib.mkEnableOption "composer plugins when running commands like `composer install`" // { default = true; };
      enableScripts = lib.mkEnableOption "composer scripts when running commands like `composer install`" // { default = true; };
      enableDev = lib.mkEnableOption "dev dependencies in composer";
    };

    maxRequestSize = lib.mkOption {
      type = lib.types.str;
      description = ''
        Maximum allowed size of a client request made to Drupal.
        
        Used for PHP and nginx settings: `upload_max_filesize` and
        `post_max_size` (PHP) and `client_max_body_size` (nginx).

        Note: while these settings mean different things, it is still very
        convenient to set them all at once. Just pad this value enough to
        account for client body size always being slightly larger than file
        upload size, as the former includes headers while the latter does not.
      '';
      default = "1G";
    };

    phpPackage = lib.mkOption {
      type = lib.types.package;
      default = pkgs.php;
      defaultText = "pkgs.php";
    };

    phpVersion = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "The PHP version to use. If not set, will default to the latest version.";
    };

    phpExtensions = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [
        "apcu"
        "pdo"
        "pdo_mysql"
        "redis"
      ];
      description = ''
        PHP extensions to enable for Drupal.
      '';
    };

    phpDisableExtensions = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = ''
        PHP extensions to disable for Drupal.
      '';
    };

    phpOptions = lib.mkOption {
      type = lib.types.lines;
      default = ''
        upload_max_filesize = ${cfg.maxRequestSize}
        post_max_size = ${cfg.maxRequestSize}
        memory_limit = 1024M
        max_execution_time = 300
        error_log = syslog
        syslog.ident = php
        syslog.facility = user
      '';
      example =
        ''
          date.timezone = "CET"
        '';
      description = ''
        Options appended to the PHP configuration file {file}`php.ini`.
      '';
    };

    codebase = {
      name = lib.mkOption {
        type = lib.types.str;
        description = ''
          Name of the codebase. Used to identify the service in various
          configuration, e.g. nginx, systemd services.
          May be any alphanumeric name, just ensure it only contains letters
          and numbers to avoid confusing errors when starting services.
        '';
        default = config.hostenv.project;
      };

      version = lib.mkOption {
        type = lib.types.str;
        default =
          let
            shortRef =
              if config.buildReference != null then
                if builtins.stringLength config.buildReference >= 7 then
                  "#" + builtins.substring 0 7 config.buildReference
                else
                  "#" + config.buildReference
              else
                "";
          in
          "dev-${config.hostenv.safeEnvironmentName}${shortRef}";
        description = "Optional. Define a version. Must be a valid version according to composer.";
        defaultText = lib.literalExpression ''
          "dev-" + config.safeEnvironmentName + "#" + (builtins.subString 0 7 config.buildReference)
        '';
      };
    };

    filesDir = lib.mkOption {
      type = lib.types.str;
      description = "Location of Drupal files directory.";
      default = config.hostenv.dataDir + "/files";
      defaultText = lib.literalExpression ''
        config.hostenv.dataDir + "/files"
      '';
    };

    privateFilesDir = lib.mkOption {
      type = lib.types.str;
      description = "Location of Drupal private files directory (must be an absolute path).";
      default = config.hostenv.dataDir + "/private_files";
      defaultText = lib.literalExpression ''
        config.hostenv.dataDir + "private_files"
      '';
    };

    backups = {
      enable = lib.mkEnableOption "default Drupal backups stored in Restic on S3.";
      restic = {
        environmentFile = lib.mkOption {
          type = lib.types.str;
          default = "/run/secrets/${config.hostenv.userName}/backups_env";
          description = ''
            Location of file containing environment variables for Restic.

            Contains sensitive credentials that shouldn't be in the Nix store. For example, backups stored on Amazon S3 require an access key and secret access key.
          '';
        };
      };
    };

    cron = {
      enable = (lib.mkEnableOption "running Drupal's cron.") // { default = true; };
      timerConfig = lib.mkOption {
        type = lib.types.attrsOf unitOption;
        default = {
          OnCalendar = "hourly";
          RandomizedDelaySec = "120";
          Persistent = true;
        };
        description = ''
          When to run cron. See {manpage}`systemd.timer(5)` for details.
          If `services.drupal.cron.enable` is false, this has
          no effect.
        '';
        example = {
          OnCalendar = "*:0/15";
          RandomizedDelaySec = "60";
          Persistent = true;
        };
      };
    };
  };

  config = lib.mkIf cfg.enable {
    services.restic.backups = lib.mkIf cfg.backups.enable {
      drupal = {
        backupPrepareCommand = ''
          [ -z "$XDG_STATE_HOME" ] && exit 1

          mkdir -p "$XDG_STATE_HOME/mariabackup"
          [ -d "$XDG_STATE_HOME/mariabackup/full" ] && rm -r "$XDG_STATE_HOME/mariabackup/full"

          ${pkgs.mariadb}/bin/mariabackup \
            -S "${config.hostenv.runtimeDir}/mysql.sock" \
            --backup \
            --target-dir=$XDG_STATE_HOME/mariabackup/full
        '';
        paths = [
          "/home/${config.hostenv.userName}/.local/state/mariabackup"
          "/home/${config.hostenv.userName}/.local/share/files"
          "/home/${config.hostenv.userName}/.local/share/private_files"
        ];
        passwordFile = config.hostenv.backupsSecretFile;
        environmentFile = cfg.backups.restic.environmentFile;
        initialize = true;
        wantsUnits = [ "mysql.service" ];
      };
    };

    systemd.services = lib.mkIf cfg.cron.enable {
      "cron-${cfg.codebase.name}" = {
        wants = lib.mkDefault [ "network-online.target" ];
        after = lib.mkDefault [ "network-online.target" ];
        restartIfChanged = lib.mkDefault false;
        serviceConfig = lib.mkDefault {
          Type = "oneshot";
          ExecStart = "${drush}/bin/drush core:cron";
        };
      };
    };
    systemd.timers = lib.mkIf cfg.cron.enable {
      "cron-${cfg.codebase.name}" = {
        wantedBy = [ "timers.target" ];
        timerConfig = cfg.cron.timerConfig;
      };
    };

    services.nginx.clientMaxBodySize = cfg.maxRequestSize;
    services.nginx.virtualHosts = {
      "${cfg.codebase.name}" = lib.mkDefault {
        serverName = "_";
        default = true;
        forceSSL = false;
        root = "${project}/share/php/${cfg.codebase.name}/web";

        # Set up nginx for Drupal.
        locations."~ '\.php$|^/update.php'" = {
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
        locations."~ ^/sites/.*/private/" = {
          return = 403;
        };
        locations."~ ^/sites/[^/]+/files/.*\.php$" = {
          extraConfig = ''
            deny all;
          '';
        };
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
        locations."~ /vendor/.*\.php$" = {
          return = 404;
          extraConfig = ''
            deny all;
          '';
        };
        locations."~* \.(js|css|png|jpg|jpeg|gif|ico|svg|avif|wasm)$" = {
          extraConfig = ''
            try_files $uri @rewrite;
            expires max;
            log_not_found off;
          '';
        };
        locations."~ ^/sites/.*/files/styles/" = {
          extraConfig = ''
            try_files $uri @rewrite;
          '';
        };
        locations."~ ^(/[a-z\-]+)?/system/files/" = {
          extraConfig = ''
            try_files $uri /index.php?$query_string;
          '';
        };
        locations."= /build-ref.txt" = lib.mkIf (config.buildReference != null) {
          return = "200 '${config.buildReference}'";
        };

        listen = [{ addr = "unix:${config.hostenv.upstreamRuntimeDir}/in.sock"; }];
      };
    };

    services.phpfpm.phpPackage = lib.mkDefault drupalPhpPool.phpPackage;
    services.phpfpm.phpOptions = lib.mkDefault ''
      memory_limit = -1
    '';
    services.phpfpm.extensions = lib.mkDefault cfg.phpExtensions;
    services.phpfpm.disableExtensions = lib.mkDefault cfg.phpDisableExtensions;

    services.phpfpm.pools."${cfg.codebase.name}" = {
      phpPackage = cfg.phpPackage;
      phpVersion = cfg.phpVersion;

      phpOptions = cfg.phpOptions;
      extensions = cfg.phpExtensions;
      disableExtensions = cfg.phpDisableExtensions;

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

      # @todo: sensible defaults that scale according to available resources.
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

    activate = ''
      # Activate the Drupal application
      mkdir -p "${cfg.filesDir}"
      mkdir -p "${cfg.privateFilesDir}"
    '';

    profile =
      let
        composer = pkgs.buildEnv {
          name = "composer";
          paths = [ drupalPhpPool.phpPackage.packages.composer ];
          pathsToLink = [ "/bin" ];
        };
      in
      [ project drush composer ];
  };
}
