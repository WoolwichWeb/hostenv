# Drupal 6 compatibility tests (assert-based).
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
        test -f "$profile/bin/drush" || { echo "missing bin/drush"; exit 1; }
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

  nginxExtraConfig = env: prefix: {
    "${prefix}-test-nginx-extra-config" = asserts.assertRun {
      name = "${prefix}-test-nginx-extra-config";
      inherit env;
      script = ''
        conf="$profile/etc/nginx/nginx.conf"
        test -f "$conf" || { echo "missing nginx.conf"; exit 1; }
        if ! awk '
          prev && $0 ~ /^[[:space:]]*try_files \$uri @rewrite;[[:space:]]*$/ {
            found = 1
            exit 0
          }

          {
            prev = ($0 ~ /^[[:space:]]*# Location mkBefore test marker[[:space:]]*$/)
          }

          END {
            exit found ? 0 : 1
          }
        ' "$conf"; then
          echo "Expected '# Location mkBefore test marker' to be followed by 'try_files \$uri @rewrite;'"
          echo
          echo "Actual config can be found at:"
          echo "$conf"
          exit 1
        fi
      '';
    };
  };

  drupalStructure = env: prefix: {
    "${prefix}-test-drupal6-structure" = asserts.assertRun {
      name = "${prefix}-test-drupal6-structure";
      inherit env;
      script = ''
        compgen -G "$profile/share/php/*/web" >/dev/null || { echo "missing web directory"; exit 1; }
        compgen -G "$profile/share/php/*/web/index.php" >/dev/null || { echo "missing web/index.php"; exit 1; }
        compgen -G "$profile/share/php/*/web/sites/default/settings.php" >/dev/null || { echo "missing settings.php"; exit 1; }
        compgen -G "$profile/share/php/*/web/sites/default/hostenv.settings.php" >/dev/null || { echo "missing hostenv.settings.php"; exit 1; }
      '';
    };
  };

  settingsContract = env: prefix: {
    "${prefix}-test-drupal6-settings-contract" = asserts.assertRun {
      name = "${prefix}-test-drupal6-settings-contract";
      inherit env;
      script = ''
        settings=$(readlink -f "$profile"/share/php/*/web/sites/default/hostenv.settings.php)
        test -f "$settings" || { echo "missing resolved hostenv settings file"; exit 1; }

        grep -q '\$db_url' "$settings" || { echo "missing Drupal 6 db_url"; exit 1; }
        grep -q '\$db_prefix' "$settings" || { echo "missing Drupal 6 db_prefix"; exit 1; }
        grep -q "\$conf\['file_directory_path'\]" "$settings" || { echo "missing Drupal 6 file_directory_path"; exit 1; }
        grep -q "\$conf\['file_temporary_path'\]" "$settings" || { echo "missing Drupal 6 file_temporary_path"; exit 1; }

        if grep -q '\$databases' "$settings"; then
          echo "modern Drupal databases array leaked into Drupal 6 settings"
          exit 1
        fi
        if grep -q 'trusted_host_patterns' "$settings"; then
          echo "modern trusted_host_patterns leaked into Drupal 6 settings"
          exit 1
        fi
        if grep -q '\$settings' "$settings"; then
          echo "modern settings array leaked into Drupal 6 settings"
          exit 1
        fi
      '';
    };
  };

  drushWrapper = env: prefix: {
    "${prefix}-test-drupal6-drush-wrapper" = asserts.assertRun {
      name = "${prefix}-test-drupal6-drush-wrapper";
      inherit env;
      script = ''
        drush="$profile/bin/drush"
        test -x "$drush" || { echo "missing drush wrapper"; exit 1; }
        grep -q -- '--root=.*web' "$drush" || { echo "drush wrapper missing web root"; exit 1; }
        grep -q -- '--uri=' "$drush" || { echo "drush wrapper missing default uri"; exit 1; }
        grep -q 'exec -a drush' "$drush" || { echo "drush wrapper does not exec as drush"; exit 1; }
        grep -q '/bin/php' "$drush" || { echo "drush wrapper does not route PHP scripts through pool PHP"; exit 1; }
        if grep -q 'vendor/bin/drush' "$drush"; then
          echo "composer vendor drush leaked into Drupal 6 wrapper"
          exit 1
        fi
      '';
    };
  };

  activationContract = env: prefix: {
    "${prefix}-test-drupal6-activation-contract" = asserts.assertRun {
      name = "${prefix}-test-drupal6-activation-contract";
      inherit env;
      script = ''
        activate="$profile/bin/activate"
        test -f "$activate" || { echo "missing activate script"; exit 1; }
        grep -q "HOSTENV_RESTORE_DRUPAL_BEGIN" "$activate" || { echo "restore marker missing"; exit 1; }
        grep -q "table_name='system'" "$activate" || { echo "Drupal 6 system table probe missing"; exit 1; }
        grep -q 'drush updatedb -y' "$activate" || { echo "Drupal 6 updatedb command missing"; exit 1; }
        if grep -q "table_name='key_value'" "$activate"; then
          echo "modern key_value probe leaked into Drupal 6 activation"
          exit 1
        fi
        if grep -q -- '--cache-clear' "$activate"; then
          echo "modern updatedb --cache-clear leaked into Drupal 6 activation"
          exit 1
        fi
      '';
    };
  };

  cronContract = env: prefix: {
    "${prefix}-test-drupal6-cron-contract" = asserts.assertRun {
      name = "${prefix}-test-drupal6-cron-contract";
      inherit env;
      script = ''
        systemdUser="$profile/systemd/user"
        test -d "$systemdUser" || { echo "missing systemd user directory"; exit 1; }
        grep -R -q 'drush cron -y' "$systemdUser" || { echo "Drupal 6 cron command missing"; exit 1; }
        if grep -R -q 'core:cron' "$systemdUser"; then
          echo "modern core:cron leaked into Drupal 6 systemd units"
          exit 1
        fi
      '';
    };
  };

  nginxLegacyDenyRules = env: prefix: {
    "${prefix}-test-drupal6-nginx-legacy-denies" = asserts.assertRun {
      name = "${prefix}-test-drupal6-nginx-legacy-denies";
      inherit env;
      script = ''
        conf="$profile/etc/nginx/nginx.conf"
        test -f "$conf" || { echo "missing nginx.conf"; exit 1; }
        grep -q 'settings\\.php' "$conf" || { echo "settings.php deny rule missing"; exit 1; }
        grep -q '\^/scripts/' "$conf" || { echo "scripts deny rule missing"; exit 1; }
        grep -q 'engine|inc|info|install|make|module|profile|po|sh|sql|theme' "$conf" || {
          echo "legacy Drupal source extension deny rule missing"
          exit 1
        }
      '';
    };
  };

  phpSocketIni = env: prefix: {
    "${prefix}-test-drupal6-php-socket-ini" = asserts.assertRun {
      name = "${prefix}-test-drupal6-php-socket-ini";
      inherit env;
      script = ''
        phpConf="$profile/etc/php-fpm.d"
        test -d "$phpConf" || { echo "missing $phpConf"; exit 1; }
        grep -R -q 'mysqli.default_socket' "$phpConf" || { echo "mysqli socket ini missing"; exit 1; }
        grep -R -q 'pdo_mysql.default_socket' "$phpConf" || { echo "pdo_mysql socket ini missing"; exit 1; }
      '';
    };
  };

in
profileStructure envs.drupal6 "drupal6"
// nginxSyntax envs.drupal6 "drupal6"
// nginxExtraConfig envs.drupal6 "drupal6"
// drupalStructure envs.drupal6 "drupal6"
// settingsContract envs.drupal6 "drupal6"
// drushWrapper envs.drupal6 "drupal6"
// activationContract envs.drupal6 "drupal6"
// cronContract envs.drupal6 "drupal6"
// nginxLegacyDenyRules envs.drupal6 "drupal6"
  // phpSocketIni envs.drupal6 "drupal6"
