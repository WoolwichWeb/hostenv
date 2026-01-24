# Drupal 7 specific tests (assert-based).
{ pkgs, envs }:

let
  lib = pkgs.lib;
  support = import ../../support { inherit pkgs lib; };
  asserts = support.asserts;

  profileStructure = env: prefix: {
    "${prefix}-profile-structure" = asserts.assertRun {
      name = "${prefix}-profile-structure";
      inherit env;
      script = ''
        test -d "$profile" || { echo "missing $profile"; exit 1; }
        test -d "$profile/systemd/user" || { echo "missing systemd/user"; exit 1; }
        test -f "$profile/bin/activate" || { echo "missing bin/activate"; exit 1; }
        test -f "$profile/etc/nginx/nginx.conf" || { echo "missing nginx.conf"; exit 1; }
      '';
    };
  };

  nginxSyntax = env: prefix: {
    "${prefix}-test-nginx-config-valid" = asserts.assertRun {
      name = "${prefix}-test-nginx-config-valid";
      inherit env;
      buildInputs = [ pkgs.nginx ];
      script = ''
        conf="$profile/etc/nginx/nginx.conf"
        test -f "$conf" || { echo "missing nginx.conf"; exit 1; }
        tmpdir=$(mktemp -d)
        mkdir -p "$tmpdir"/{logs,run}
        output=$("$profile"/bin/nginx -e "$tmpdir/error.log" -t -c "$conf" -p "$tmpdir" 2>&1 || true)
        echo "$output" | grep -q "syntax is ok" || { echo "$output"; exit 1; }
      '';
    };
  };

  phpConfig = env: prefix: {
    "${prefix}-php-config-file" = asserts.assertRun {
      name = "${prefix}-php-config-file";
      inherit env;
      script = ''
        phpConf="$profile/etc/php-fpm.d"
        test -d "$phpConf" || { echo "missing $phpConf"; exit 1; }
        compgen -G "$phpConf/*.ini" >/dev/null || { echo "no ini files in $phpConf"; exit 1; }
      '';
    };
  };

  phpVersion74 = env: prefix: {
    "${prefix}-test-php-version" = asserts.assertRun {
      name = "${prefix}-test-php-version";
      inherit env;
      script = ''
        phpBin="$profile/bin/php-fpm"
        test -x "$phpBin" || { echo "php-fpm not found"; exit 1; }
        ver="$("$phpBin" --version | head -n1)"
        echo "$ver" | grep -q "PHP 7.4" || {
          echo "expected PHP 7.4.*, got: $ver"
          exit 1
        }
      '';
    };
  };

  nginxSocketListen = env: prefix: {
    "${prefix}-unix-socket-listen" = asserts.assertRun {
      name = "${prefix}-unix-socket-listen";
      inherit env;
      script = ''
        runtimeDir="${env.config.hostenv.upstreamRuntimeDir}"
        test -n "$runtimeDir" || { echo "upstreamRuntimeDir unset"; exit 1; }
      '';
    };
  };

in
  profileStructure envs.drupal7 "drupal7"
  // nginxSyntax envs.drupal7 "drupal7"
  // phpConfig envs.drupal7 "drupal7"
  // nginxSocketListen envs.drupal7 "drupal7"
  // phpVersion74 envs.drupal7 "drupal7"
