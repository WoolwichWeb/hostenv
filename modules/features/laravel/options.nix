{ ... }:
{
  flake.modules.hostenv.laravelOptions =
    {
      lib,
      config,
      pkgs,
      ...
    }:
    let
      cfg = config.services.laravel;
      utils = import (pkgs.path + "/nixos/lib/utils.nix") { inherit pkgs lib config; };
      inherit (utils.systemdUtils.unitOptions) unitOption;

      queueWorkerOptions = { ... }: {
        options = {
          connection = lib.mkOption {
            type = lib.types.nullOr (lib.types.strMatching "[A-Za-z0-9_.-]+");
            default = null;
            example = "redis";
            description = "Queue connection passed to `artisan queue:work`. Null uses Laravel's default connection.";
          };

          queues = lib.mkOption {
            type = lib.types.listOf (lib.types.strMatching "[A-Za-z0-9_.-]+");
            default = [ ];
            example = [
              "high"
              "default"
            ];
            description = "Ordered queue names passed to `artisan queue:work --queue`.";
          };

          processes = lib.mkOption {
            type = lib.types.ints.positive;
            default = 1;
            description = "Number of independently supervised worker processes.";
          };

          sleep = lib.mkOption {
            type = lib.types.ints.unsigned;
            default = 3;
            description = "Seconds a worker sleeps when no jobs are available.";
          };

          timeout = lib.mkOption {
            type = lib.types.ints.positive;
            default = 60;
            description = "Maximum seconds a job may run before the worker exits.";
          };

          maxLifetime = lib.mkOption {
            type = lib.types.ints.positive;
            default = 3600;
            description = "Maximum worker lifetime in seconds before systemd restarts it.";
          };

          tries = lib.mkOption {
            type = lib.types.nullOr lib.types.ints.positive;
            default = null;
            description = "Optional maximum number of attempts for a job.";
          };

          stopTimeout = lib.mkOption {
            type = lib.types.ints.positive;
            default = 90;
            description = "Seconds systemd allows a worker to finish its current job after SIGTERM.";
          };
        };
      };
    in
    {
      options.services.laravel = {
        enable = lib.mkEnableOption ''
          support for a Laravel application.

          Enabling this configures Composer packaging, PHP-FPM, nginx, a
          socket-only MariaDB instance, persistent application data, Artisan,
          scheduling, migrations, and optional backups and queue workers.
        '';

        databaseName = lib.mkOption {
          type = lib.types.strMatching "[A-Za-z0-9_]+";
          default = "laravel";
          description = "MariaDB database used by Laravel.";
        };

        maxRequestSize = lib.mkOption {
          type = lib.types.str;
          default = "1G";
          description = ''
            Maximum request size used for nginx's `client_max_body_size` and
            PHP's `upload_max_filesize` and `post_max_size`.
          '';
        };

        codebase = {
          name = lib.mkOption {
            type = lib.types.strMatching "[A-Za-z0-9][A-Za-z0-9_-]*";
            default = config.hostenv.project;
            description = "Application name used for package, PHP-FPM, nginx, and systemd identifiers.";
          };

          version =
            let
              lockFile = config.hostenv.root + /composer.lock;
              calculatedVersion =
                if builtins.pathExists lockFile then
                  let
                    lockJson = builtins.fromJSON (builtins.readFile lockFile);
                    lockFileHash = builtins.hashFile "sha256" lockFile;
                  in
                  builtins.substring 0 7 (lockJson.content-hash or lockFileHash)
                else
                  config.hostenv.safeEnvironmentName;
            in
            lib.mkOption {
              type = lib.types.str;
              default = "dev-${calculatedVersion}";
              description = "Composer-compatible application version.";
            };
        };

        composer = {
          enable = (lib.mkEnableOption "Composer dependency installation") // {
            default = lib.pathExists (config.hostenv.root + /composer.lock);
          };

          dependencyHash = lib.mkOption {
            type = lib.types.str;
            default = lib.fakeHash;
            description = "Hash of Composer dependencies. Update it when Nix reports a fixed-output hash mismatch.";
          };

          enablePlugins = (lib.mkEnableOption "Composer plugins") // {
            default = true;
          };
          enableScripts = (lib.mkEnableOption "Composer scripts") // {
            default = true;
          };
          enableDev = lib.mkEnableOption "Composer development dependencies";
        };

        phpPackage = lib.mkOption {
          type = lib.types.package;
          default = pkgs.php;
          defaultText = "pkgs.php";
          description = "Base PHP package.";
        };

        phpVersion = lib.mkOption {
          type = lib.types.str;
          default = "";
          description = "PHP version. An empty value uses the current nixpkgs PHP.";
        };

        phpExtensions = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [
            "bcmath"
            "curl"
            "fileinfo"
            "mbstring"
            "openssl"
            "pdo"
            "pdo_mysql"
            "redis"
            "tokenizer"
          ];
          description = "PHP extensions enabled for Laravel.";
        };

        phpDisableExtensions = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ ];
          description = "PHP extensions removed from the Laravel PHP package.";
        };

        phpOptions = lib.mkOption {
          type = lib.types.lines;
          default = ''
            upload_max_filesize = ${cfg.maxRequestSize}
            post_max_size = ${cfg.maxRequestSize}
          '';
          description = "Options appended to the Laravel PHP-FPM pool's `php.ini`.";
        };

        storageDir = lib.mkOption {
          type = lib.types.str;
          default = config.hostenv.dataDir + "/laravel/storage";
          defaultText = lib.literalExpression ''config.hostenv.dataDir + "/laravel/storage"'';
          description = "Writable persistent directory linked to the application's `storage` path.";
        };

        bootstrapCacheDir = lib.mkOption {
          type = lib.types.str;
          default = config.hostenv.cacheDir + "/laravel/bootstrap-cache";
          defaultText = lib.literalExpression ''config.hostenv.cacheDir + "/laravel/bootstrap-cache"'';
          description = "Writable persistent directory linked to the application's `bootstrap/cache` path.";
        };

        environmentVariables = lib.mkOption {
          type = lib.types.attrsOf lib.types.singleLineStr;
          default = { };
          example = {
            MAIL_MAILER = "smtp";
            SESSION_DRIVER = "database";
          };
          description = ''
            Additional values written to Laravel's `.env` file.

            NOTE: should not be used for passwords and other credentials.
            This is written to the Nix store.
          '';
        };

        redis.enable = lib.mkEnableOption "a local Redis-compatible Valkey service over a Unix socket";

        scheduler = {
          enable = (lib.mkEnableOption "Laravel's scheduler") // {
            default = true;
          };
          timerConfig = lib.mkOption {
            type = lib.types.attrsOf unitOption;
            default = {
              OnCalendar = "minutely";
              Persistent = true;
            };
            description = "Timer settings for `artisan schedule:run`.";
          };
        };

        migrations.enable = (lib.mkEnableOption "running `artisan migrate --force` during activation") // {
          default = true;
        };

        optimize.enable = lib.mkEnableOption "running `artisan optimize` during activation";

        healthCheckPath = lib.mkOption {
          type = lib.types.strMatching "/.*";
          default = "/";
          description = "HTTP path checked after deployment.";
        };

        backups = {
          enable = lib.mkEnableOption "default Laravel database and storage backups in Restic";
          restic.environmentFile = lib.mkOption {
            type = lib.types.str;
            default = "/run/secrets/${config.hostenv.userName}/backups_env";
            description = "Environment file containing credentials and optional repository settings for Restic.";
          };
        };

        queue.workers = lib.mkOption {
          type = lib.types.attrsOf (lib.types.submodule queueWorkerOptions);
          default = { };
          example = {
            default = {
              connection = "database";
              queues = [
                "high"
                "default"
              ];
              processes = 2;
            };
          };
          description = "Named groups of supervised `artisan queue:work` processes.";
        };
      };
    };
}
