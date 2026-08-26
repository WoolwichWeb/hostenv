# Laravel projects

Hostenv supports Laravel 10, 11, and 12 with one version-independent service
module. Composer decides whether the selected framework release, PHP version,
and extension set are compatible.

## Basic project configuration

Enable Laravel in the project's `.hostenv/hostenv.nix`:

```nix
{ pkgs, ... }:
{
  services.laravel = {
    enable = true;

    # Set this to the hash Nix reports for the committed composer.lock.
    composer.dependencyHash = "sha256-...";

    # Optional PHP selection. An empty phpVersion uses the current nixpkgs PHP.
    phpVersion = "8.3";
    phpExtensions = [
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

    # These values are not secret. They are written to the generated .env.
    environmentVariables = {
      MAIL_MAILER = "smtp";
      SESSION_DRIVER = "database";
    };
  };

  environments.main = {
    enable = true;
    type = "production";
  };
}
```

The packaged application is immutable. nginx serves only its `public/`
directory and sends `/index.php` to PHP-FPM through the environment's Unix
socket. The project root, dotfiles, and other PHP files are not web-accessible.
MariaDB also accepts local Unix-socket connections only. Laravel's default
PHP extension set includes PhpRedis so the application can use either an
external Redis-compatible service or Hostenv's optional local Valkey service.

Hostenv writes non-secret defaults for `APP_ENV`, `APP_DEBUG`, `APP_URL`,
`LOG_CHANNEL`, and the `DB_*` socket connection into the packaged `.env`.
The provider's `laravel_env` file is loaded into the process environment, so
its values take precedence over that file. Hostenv does not parse, whitelist,
or validate the keys in `laravel_env`; Laravel and any configured external
services own their meaning.

## Application data and activation

`storage` and `bootstrap/cache` are links to writable Hostenv-managed paths.
Their defaults are:

- `services.laravel.storageDir = <environment data directory>/laravel/storage`
- `services.laravel.bootstrapCacheDir = <environment cache directory>/laravel/bootstrap-cache`

Contents committed in the project's initial `storage` directory are copied to
the persistent storage directory on its first activation. Activation stops
immediately, before changing services or the database, if the provider has not
created a readable `laravel_env` file.

`services.laravel.migrations.enable` defaults to `true`, so every activation
runs `artisan migrate --force` after MariaDB is ready. This applies to both new
and existing databases. `services.laravel.optimize.enable` defaults to `false`;
enable it to run `artisan optimize` after migrations.

The scheduler is enabled by default and runs `artisan schedule:run` every
minute. Its systemd timer can be changed with
`services.laravel.scheduler.timerConfig`, or the scheduler can be disabled with
`services.laravel.scheduler.enable = false`.

The default deployment check requests `/`. Set
`services.laravel.healthCheckPath` to use an application-specific health route.

## Redis / Valkey

Redis is optional. Enable a local Redis-compatible Valkey service with:

```nix
{
  services.laravel.redis.enable = true;
}
```

This enables Hostenv's generic `services.redis` service, whose package defaults
to `pkgs.valkey`. Plain TCP, TLS TCP, and RDMA listeners and cluster mode are
disabled, so the server listens only on
`<environment runtime directory>/redis.sock`.
Hostenv adds these non-secret Laravel defaults to the generated `.env`:

```dotenv
REDIS_CLIENT=phpredis
REDIS_HOST=/run/hostenv/user/<environment-user>/redis.sock
REDIS_PORT=0
```

Laravel's cache, session, and queue drivers are not changed automatically. For
Laravel 11 and 12, set `CACHE_STORE=redis`; Laravel 10 uses
`CACHE_DRIVER=redis`. Sessions and queues use `SESSION_DRIVER=redis` and
`QUEUE_CONNECTION=redis` across the supported versions. Put these values in
`services.laravel.environmentVariables` or the provider's `laravel_env` when
the application should use Redis for those responsibilities.

The underlying service can also be enabled or configured directly with
`services.redis`. Its data is persisted under
`<environment data directory>/redis` by default, with append-only persistence
enabled. Laravel's Restic backup does not currently include Valkey data, so do
not treat the local Redis service as a disaster-recovery copy of queued or
otherwise irreplaceable data.

## Queue workers

No queue workers run unless they are declared. Each configured process becomes
a separate supervised user service:

```nix
{
  services.laravel.queue.workers = {
    priority = {
      connection = "database";
      queues = [ "high" "default" ]; # order is preserved
      processes = 2;
      sleep = 2;
      timeout = 75;
      maxLifetime = 1800;
      tries = 5;       # optional
      stopTimeout = 120;
    };

    external = {
      connection = "sqs";
      processes = 1;
    };
  };
}
```

Workers receive SIGTERM and are given `stopTimeout` seconds to finish the
current job. systemd restarts workers that fail or exit after `maxLifetime`.
Worker units change with the immutable application path, so deployments
restart them onto the new release.

## Backups and migration restores

Set `services.laravel.backups.enable = true` to create the normal `laravel`
Restic job and the on-demand `laravel-migrate` snapshot. Both contain the
MariaDB backup and persistent storage. Migration restores use the same restore
plan, tags, markers, service ordering, and plan cleanup as Hostenv's Drupal
restore flow.

Laravel support does not provision Horizon, Octane, Reverb, PostgreSQL, or
SQLite. Queue workers may use the local Valkey service or any external backend
configured through `laravel_env`.

## Artisan

Run Artisan against an environment with the typed passthrough command:

```console
hostenv artisan about --env main
hostenv artisan migrate:status --env main
hostenv artisan queue:restart --env main --force
```

The persistent Hostenv `--force` flag adds Artisan's `--no-interaction` option.
On the server, the `artisan` wrapper loads the same `laravel_env` file used by
PHP-FPM, the scheduler, migrations, optimization, and queue workers.
