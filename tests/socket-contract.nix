{ pkgs, envs }:

let
  lib = import ./lib.nix { inherit pkgs; };

  mkNginxSocketTest = env: prefix:
    let
      expected = "unix:${env.config.hostenv.upstreamRuntimeDir}/in.sock";
    in lib.testWithEnv env "${prefix}-nginx-upstream-socket"
      ''
        nginxConf="$profile/etc/nginx/nginx.conf"
        ln -s "$nginxConf" "$out"/attachments/nginx.conf

        if ! grep -q "${expected}" "$nginxConf"; then
          echo "FAIL: expected nginx to listen on ${expected} (${prefix})"
          exit 1
        fi

        echo "PASS: nginx upstream socket matches ${expected} (${prefix})"
        touch "$out"
      '';

  mkPhpSocketTest = env: prefix:
    let
      expectedDir = env.config.hostenv.runtimeDir;
    in lib.testWithEnv env "${prefix}-phpfpm-socket-paths"
      ''
        phpDir="$profile/etc/php-fpm.d"
        ln -s "$phpDir" "$out"/attachments/php-fpm.d

        test -d "$phpDir" || { echo "FAIL: missing $phpDir"; exit 1; }

        if ! grep -R "listen = ${expectedDir}/" "$phpDir"/*.conf; then
          echo "FAIL: no PHP-FPM pool listens under ${expectedDir} (${prefix})"
          exit 1
        fi

        echo "PASS: PHP-FPM sockets live under ${expectedDir} (${prefix})"
        touch "$out"
      '';
in
  mkNginxSocketTest envs.drupalDev "drupal-dev"
  // mkNginxSocketTest envs.drupalProduction "drupal-prod"
  // mkPhpSocketTest envs.drupalDev "drupal-dev"
  // mkPhpSocketTest envs.drupalProduction "drupal-prod"
