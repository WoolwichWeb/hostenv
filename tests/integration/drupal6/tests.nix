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
        test -x "$profile/bin/hello" || { echo "missing project runtime package"; exit 1; }
        test -f "$profile/etc/nginx/nginx.conf" || { echo "missing nginx.conf"; exit 1; }
        cronUnit="$profile/systemd/user/cron-${env.config.services.drupal.codebase.name}.service"
        test -f "$cronUnit" || { echo "missing Drupal 6 cron unit"; exit 1; }
        grep -Fq '${pkgs.hello}/bin' "$cronUnit" || {
          echo "project runtime package missing from Drupal 6 cron PATH"
          exit 1
        }
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

  projectFilesContract = { env, prefix, relativePath, seedFile }: {
    "${prefix}-test-drupal6-project-files-contract" = asserts.assertRun {
      name = "${prefix}-test-drupal6-project-files-contract";
      inherit env;
      script = ''
        activate="$profile/bin/activate"
        test -f "$activate" || { echo "missing activate script"; exit 1; }

        projectRoot="$(readlink -f "$profile"/share/php/*)"
        projectFiles="$projectRoot/${relativePath}"
        test -f "$projectFiles/${seedFile}" || {
          echo "missing seeded project file at $projectFiles/${seedFile}"
          exit 1
        }
        grep -F -q "$projectFiles" "$activate" || {
          echo "activation script does not reference $projectFiles"
          exit 1
        }
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
        lineOf() {
          pattern=$1
          label=$2
          line=$(grep -nF "$pattern" "$conf" | head -n1 | cut -d: -f1 || true)
          test -n "$line" || { echo "$label missing"; exit 1; }
          printf '%s\n' "$line"
        }

        hiddenPhpDenyLine=$(lineOf 'location ~ \..*/.*\.php$ {' 'hidden PHP deny rule')
        privateDenyLine=$(lineOf 'location ~ ^/sites/.*/private/ {' 'private files deny rule')
        filesPhpDenyLine=$(lineOf 'location ~ ^/sites/[^/]+/files/.*\.php$ {' 'uploaded PHP deny rule')
        vendorPhpDenyLine=$(lineOf 'location ~ /vendor/.*\.php$ {' 'vendor PHP deny rule')
        phpHandlerLine=$(lineOf 'location ~ \.php$|^/update.php {' 'PHP handler')

        for denyLine in "$hiddenPhpDenyLine" "$privateDenyLine" "$filesPhpDenyLine" "$vendorPhpDenyLine"; do
          if [ "$denyLine" -ge "$phpHandlerLine" ]; then
            echo "PHP deny rules must render before the catch-all PHP handler"
            exit 1
          fi
        done
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

  php8Compatibility = env: prefix: {
    "${prefix}-test-php8-compatibility" = asserts.assertRun {
      name = "${prefix}-test-php8-compatibility";
      inherit env;
      script = ''
        phpBin="$profile/bin/php"
        test -x "$phpBin" || { echo "PHP CLI not found"; exit 1; }

        version="$($phpBin -r 'echo PHP_VERSION;')"
        case "$version" in
          8.*) ;;
          *) echo "expected PHP 8.*, got: $version"; exit 1;;
        esac

        webRoot="$(readlink -f "$profile"/share/php/*/web)"
        test -d "$webRoot" || { echo "Drupal web root not found"; exit 1; }

        while IFS= read -r -d $'\0' file; do
          output="$($phpBin -d error_reporting=E_ALL -l "$file" 2>&1)"
          case "$output" in
            "No syntax errors detected in "*) ;;
            *) echo "$output"; exit 1;;
          esac
        done < <(
          find "$webRoot" -type f \
            \( -name '*.php' -o -name '*.inc' -o -name '*.module' \
               -o -name '*.install' -o -name '*.theme' -o -name '*.engine' \
               -o -name '*.profile' \) \
            -print0
        )
      '';
    };
  };

  mysqlWrapperArgumentContract = env: prefix: {
    "${prefix}-test-mysql-wrapper-arguments" = asserts.assertRun {
      name = "${prefix}-test-mysql-wrapper-arguments";
      inherit env;
      script = ''
        for wrapper in mysql mysqldump; do
          script="$profile/bin/$wrapper"
          test -x "$script" || { echo "missing $wrapper wrapper"; exit 1; }
          grep -F -q '"$@"' "$script" || {
            echo "$wrapper wrapper does not preserve argument boundaries"
            exit 1
          }
        done
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
// projectFilesContract {
  env = envs.drupal6;
  prefix = "drupal6";
  relativePath = "web/project_files";
  seedFile = "root-layout-seed.txt";
}
// projectFilesContract {
  env = envs.drupal6WebRoot;
  prefix = "drupal6-web-root";
  relativePath = "project_files";
  seedFile = "web-root-layout-seed.txt";
}
// cronContract envs.drupal6 "drupal6"
// nginxLegacyDenyRules envs.drupal6 "drupal6"
// phpSocketIni envs.drupal6 "drupal6"
// php8Compatibility envs.drupal6 "drupal6"
  // mysqlWrapperArgumentContract envs.drupal6 "drupal6"
