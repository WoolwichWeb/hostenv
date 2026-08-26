{ pkgs, envs }:
let
  lib = pkgs.lib;
  asserts = (import ../../support { inherit pkgs lib; }).asserts;

  frameworkVersions = {
    "10" = "10.50.3";
    "11" = "11.56.1";
    "12" = "12.68.0";
  };

  profileCheck = major: env:
    let
      serviceName = "laravel${major}";
      cfg = env.config.services.laravel;
      user = env.config.hostenv.userName;
    in
    {
      "laravel-${major}-profile" = asserts.assertRun {
        name = "laravel-${major}-profile";
        inherit env;
        buildInputs = [ pkgs.jq pkgs.nginx pkgs.rsync ];
        script = ''
          fail() {
            printf 'Laravel ${major} fixture failed: %s\n' "$1" >&2
            exit 1
          }

          app="$profile/share/php/${serviceName}"
          project_app=$(readlink -f "$app")
          nginx_conf="$profile/etc/nginx/nginx.conf"
          units="$profile/systemd/user"
          fpm_conf="$profile/etc/php-fpm.d/${serviceName}.conf"

          test -f "$app/public/index.php" || fail "public/index.php was not packaged"
          test -f "$app/vendor/autoload.php" || fail "Composer vendor tree was not packaged"
          test -x "$profile/bin/artisan" || fail "server-side Artisan wrapper is missing"
          test -x "$profile/bin/composer" || fail "Composer is missing from the profile"

          test "$(readlink "$app/storage")" = ${lib.escapeShellArg cfg.storageDir} \
            || fail "storage is not linked to persistent Hostenv data"
          test "$(readlink "$app/bootstrap/cache")" = ${lib.escapeShellArg cfg.bootstrapCacheDir} \
            || fail "bootstrap/cache is not linked to persistent Hostenv cache"
          grep -Fq '${serviceName} initial storage' "$app/.hostenv-initial-storage/hostenv-fixture.txt" \
            || fail "initial storage contents were not retained for activation"

          grep -Fq 'APP_ENV="production"' "$app/.env" || fail "production APP_ENV default is missing"
          grep -Fq 'APP_DEBUG="false"' "$app/.env" || fail "production APP_DEBUG default is missing"
          grep -Fq 'APP_URL="https://${env.config.hostenv.hostname}"' "$app/.env" || fail "canonical APP_URL is missing"
          grep -Fq 'LOG_CHANNEL="stderr"' "$app/.env" || fail "stderr logging default is missing"
          grep -Fq 'DB_SOCKET="${env.config.hostenv.runtimeDir}/mysql.sock"' "$app/.env" \
            || fail "MariaDB socket default is missing"
          grep -Fq 'HOSTENV_FIXTURE="non-secret"' "$app/.env" \
            || fail "additional non-secret environment value is missing"
          if grep -Fq 'APP_KEY=' "$app/.env"; then
            fail "APP_KEY must not be written to the immutable generated .env"
          fi

          grep -Fq "root $project_app/public;" "$nginx_conf" || fail "nginx does not expose only public/"
          grep -Fq 'location = /index.php' "$nginx_conf" || fail "front controller location is missing"
          grep -Fq 'location ~ /\.(?!well-known).*' "$nginx_conf" || fail "hidden-file protection is missing"
          grep -Fq 'fastcgi_pass unix:${env.config.hostenv.runtimeDir}/${serviceName}.sock;' "$nginx_conf" \
            || fail "FastCGI Unix socket is missing"
          if grep -Fq "root $project_app;" "$nginx_conf"; then
            fail "nginx must never serve the Laravel project root"
          fi

          tmpdir=$(mktemp -d)
          mkdir -p "$tmpdir/logs" "$tmpdir/run"
          nginx_output=$("$profile/bin/nginx" -e "$tmpdir/error.log" -t -c "$nginx_conf" -p "$tmpdir" 2>&1 || true)
          printf '%s\n' "$nginx_output" | grep -Fq 'syntax is ok' || fail "nginx syntax check failed"

          grep -Fq 'clear_env = no' "$fpm_conf" || fail "PHP-FPM still clears inherited variables"
          for extension in bcmath curl fileinfo mbstring openssl pdo_mysql tokenizer xml; do
            "$profile/bin/php@${serviceName}" -m | grep -i -x -q "$extension" \
              || fail "PHP extension $extension is missing"
          done

          grep -R -Fq 'EnvironmentFile=/run/secrets/${user}/laravel_env' "$units" \
            || fail "systemd services do not receive laravel_env"
          grep -R -Fq 'schedule:run --no-interaction' "$units" \
            || fail "scheduler service does not run Artisan every minute"
          grep -R -Fq 'OnCalendar=minutely' "$units" \
            || fail "scheduler timer is not configured for every minute"

          app_copy="$tmpdir/app"
          mkdir -p "$app_copy"
          rsync -a --exclude=/storage --exclude=/bootstrap/cache "$app/" "$app_copy/"
          chmod -R u+w "$app_copy"
          mkdir -p \
            "$app_copy/storage/framework/cache" \
            "$app_copy/storage/framework/sessions" \
            "$app_copy/storage/framework/views" \
            "$app_copy/storage/logs" \
            "$app_copy/bootstrap/cache"

          version_output=$("$profile/bin/php@${serviceName}" "$app_copy/artisan" --version)
          printf '%s\n' "$version_output" | grep -Fq 'Laravel Framework ${frameworkVersions.${major}}' \
            || fail "Artisan did not run the pinned Laravel ${major} fixture"
          precedence_output=$(APP_ENV=provider-secret-value \
            "$profile/bin/php@${serviceName}" "$app_copy/artisan" env)
          printf '%s\n' "$precedence_output" | grep -Fq 'provider-secret-value' \
            || fail "an inherited provider value did not override generated .env defaults"

          activation_output=$(
            XDG_CONFIG_HOME="$tmpdir/config" \
            XDG_STATE_HOME="$tmpdir/state" \
            XDG_CACHE_HOME="$tmpdir/cache" \
            "$profile/bin/activate" 2>&1 && exit 99 || true
          )
          printf '%s\n' "$activation_output" | grep -Fq 'Laravel activation requires a readable secret file' \
            || fail "activation did not fail with the provider configuration message"
          test ! -e "$tmpdir/config" || fail "activation changed services before checking laravel_env"
        '';
      };
    };

  cfg10 = envs.laravel10.config;
  cfg12 = envs.laravel12.config;
  cfgDev = envs.laravelDev.config;
  activation12 = cfg12.activate;
  backups12 = cfg12.services.restic.backups;
  queueServiceNames = builtins.filter
    (name: lib.hasPrefix "laravel-queue-" name)
    (builtins.attrNames cfg12.systemd.services);
  priorityUnit = cfg12.systemd.services."laravel-queue-priority-1";
  queueContract =
    builtins.length queueServiceNames == 3
    && lib.hasInfix "queue:work" priorityUnit.serviceConfig.ExecStart
    && lib.hasInfix "database" priorityUnit.serviceConfig.ExecStart
    && lib.hasInfix "--queue=high,default" priorityUnit.serviceConfig.ExecStart
    && lib.hasInfix "--sleep=2" priorityUnit.serviceConfig.ExecStart
    && lib.hasInfix "--timeout=75" priorityUnit.serviceConfig.ExecStart
    && lib.hasInfix "--max-time=1800" priorityUnit.serviceConfig.ExecStart
    && lib.hasInfix "--tries=5" priorityUnit.serviceConfig.ExecStart
    && priorityUnit.serviceConfig.Restart == "always"
    && priorityUnit.serviceConfig.KillSignal == "SIGTERM"
    && priorityUnit.serviceConfig.TimeoutStopSec == "120";
in
profileCheck "10" envs.laravel10
// profileCheck "11" envs.laravel11
// profileCheck "12" envs.laravel12
  // {
  laravel-contracts = asserts.assertTrue "laravel-contracts"
    (
      cfg10.environments.main.requiredSecretFiles == [ "laravel_env" ]
      && cfg10.services.laravel.queue.workers == { }
      && !(builtins.any (name: lib.hasPrefix "laravel-queue-" name) (builtins.attrNames cfg10.systemd.services))
      && queueContract
      && cfg12.services.mysql.settings.mysqld.skip-networking == true
      && cfg12.services.mysql.ensureDatabases == [ "laravel" ]
      && backups12 ? laravel
      && backups12 ? "laravel-migrate"
      && lib.elem "laravel-migrate" backups12."laravel-migrate".tags
      && lib.elem cfg12.services.laravel.storageDir backups12.laravel.paths
      && lib.hasPrefix "laravel_secret_file=" activation12
      && lib.hasInfix "HOSTENV_RESTORE_LARAVEL_BEGIN" activation12
      && lib.hasInfix "restore_key=\"laravel-migrate\"" activation12
      && lib.hasInfix "del(.snapshots" activation12
      && lib.hasInfix "artisan migrate --force" activation12
      && lib.hasInfix "artisan optimize" activation12
      && !(lib.hasInfix "APP_KEY" activation12)
      && cfg10.environments.main.deploymentVerification.checks != [ ]
      && (builtins.head cfg10.environments.main.deploymentVerification.checks).request.path == "/"
      && cfg12.environments.main.deploymentVerification.checks != [ ]
      && (builtins.head cfg12.environments.main.deploymentVerification.checks).request.path == "/up"
    )
    "Laravel runtime, queue, migration, backup, secret, and deployment-verification contracts should evaluate";

  laravel-development-defaults = asserts.assertRun {
    name = "laravel-development-defaults";
    env = envs.laravelDev;
    script = ''
      app="$profile/share/php/laravel12"
      grep -Fq 'APP_ENV="development"' "$app/.env"
      grep -Fq 'APP_DEBUG="true"' "$app/.env"
    '';
  };

  laravel-artisan-cli = asserts.assertRun {
    name = "laravel-artisan-cli";
    env = envs.laravel12;
    script = ''
      cli=${cfg12.hostenv.cliPackage}
      test -x "$cli/bin/hostenv"
      test -x "$cli/bin/artisan"
      grep -Fq -- '/bin/hostenv artisan -- "$@"' "$cli/bin/artisan"
      "$cli/bin/hostenv" --help > "$TMPDIR/root-help"
      grep -Fq 'Laravel:' "$TMPDIR/root-help"
      "$cli/bin/hostenv" artisan --help > "$TMPDIR/artisan-help"
      grep -Fq 'Arguments passed to Artisan' "$TMPDIR/artisan-help"
      grep -Fq 'remote_artisan+=(--no-interaction)' "$cli/bin/hostenv"
    '';
  };

  laravel-secret-store-exclusion = asserts.assertRun {
    name = "laravel-secret-store-exclusion";
    env = envs.laravelSecretExclusion;
    script = ''
      app=$(readlink -f "$profile/share/php/laravelsecret")
      grep -Fq 'APP_ENV="production"' "$app/.env"
      if find "$app" -type f -exec grep -Fq \
        'hostenv-secret-must-not-enter-project-output' '{}' \; -print -quit | grep -q .; then
        echo "source .env secret leaked into the immutable Laravel project" >&2
        exit 1
      fi
      if find "$app" -type f -exec grep -Fq \
        'hostenv-local-secret-must-not-enter-project-output' '{}' \; -print -quit | grep -q .; then
        echo "source .env.local secret leaked into the immutable Laravel project" >&2
        exit 1
      fi
    '';
  };
}
