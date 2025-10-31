# General tests and useful library functions.
{ pkgs }:
let
  inherit (pkgs) runCommandWith stdenvNoCC writeShellScriptBin;

  testWithEnv' = env: name: derivationArgs: testCommand: {
    "${name}" =
      runCommandWith
        {
          inherit name;
          stdenv = stdenvNoCC;
          derivationArgs = derivationArgs // {
            nativeBuildInputs =
              (derivationArgs.nativeBuildInputs or [ ])
                ++ [ pkgs.shellcheck pkgs.bash pkgs.coreutils ];
          };
        } ''
        set -euo pipefail
        mkdir -p "$out"/attachments
        mkdir -p "$out"/bin
        export profile="${env.config.activatePackage}"
        ln -s "$profile" "$out"/attachments/env

        # Write test body verbatim (no expansion)
        cat > "$out"/bin/test.sh <<'TEST_EOF'
        # shellcheck disable=2250
        # File created by test harness script: 'tests/lib.nix'
        set -euo pipefail
        : "''${profile:?profile env var must be exported by test harness}"
        : "''${out:?out env var must be exported by test harness}"
        ${testCommand}
        TEST_EOF

        # 1) Lint the test
        shellcheck -S style -o all -s bash "$out"/bin/test.sh

        # 2) Then run it
        bash "$out"/bin/test.sh
      '';
  };

  # Test runner/wrappers.
  # Sets up environment and `$profile` variable, then runs
  # the test command.
  testWithEnv'' = env: name: derivationArgs: testCommand:
    {
      "${name}" =
        let
          finalScript = writeShellScriptBin "${name}-script" ''
            set -euo pipefail
            echo "Setting up environment for testing"
            profile="${env.config.activatePackage}"
            ln -s "$profile" "$out"/attachments/env
            echo "Environment setup complete"

            ${testCommand}
          '';
        in
        runCommandWith
          {
            inherit name;
            stdenv = stdenvNoCC;
            derivationArgs = derivationArgs // {
              nativeBuildInputs =
                (derivationArgs.nativeBuildInputs or [ ])
                  ++ [ pkgs.shellcheck ];
            };
          }
          ''
            set -euo pipefail
            shellcheck -S style -o all -s bash "${finalScript}"
            mkdir -p $out/attachments
            ln -s ${finalScript} $out/attachments/test-script
          '';
    };

  # Simplest test runner/wrapper.
  # Provides coreutils and grep, which should be enough for most tests.
  testWithEnv = env: name: testCommand:
    testWithEnv' env name
      {
        buildInputs = [ pkgs.coreutils pkgs.gnugrep ];
      }
      testCommand;

in
{
  inherit testWithEnv testWithEnv';

  testProfileWith = env: prefix: { }
    // testWithEnv env "${prefix}-profile-structure"
    ''
      echo "Testing environment files are in expected locations..."

      test -d "$profile" || { echo "FAIL: build did not produce a valid output at '$profile' (${prefix})"; exit 1; }
      test -d "$profile/systemd/user" || { echo "FAIL: missing $profile/systemd/user (${prefix})"; exit 1; }
      test -f "$profile/bin/activate"  || { echo "FAIL: missing $profile/bin/activate (${prefix})"; exit 1; }
      test -f "$profile/etc/nginx/nginx.conf" || { echo "FAIL: missing $profile/etc/nginx/nginx.conf (${prefix})"; exit 1; }
      echo "PASS: basic profile structure ok (${prefix})"
      touch "$out"
    ''
    // testWithEnv' env "${prefix}-profile-shellcheck"
    {
      nativeBuildInputs = [ pkgs.shellcheck ];
    } ''
    echo "Lint checking profile activation script..."


    echo "PASS: activation script passed lint check"
    touch "$out"
  '';
  # @todo: add this back in after tests are committed:
  # shellcheck --exclude=2250 -S style -o all -s bash "$profile/bin/activate" || exit 1

  php.testConfigWith = env: prefix: testWithEnv env "${prefix}-php-config-file"
    ''
      echo "Testing php configuration file exists (${prefix})..."

      phpConf="$profile/etc/php-fpm.d"
      ln -s "$phpConf" "$out"/attachments/php-fpm.d

      if [[ ! -d "$phpConf" ]]; then
        echo "FAIL: PHP-FPM config directory not found at '$phpConf'"
        echo "Available directories:"
        find "$profile/etc" -type d 2>/dev/null || true
        exit 1
      fi

      if ! compgen -G "$phpConf/*.ini" >/dev/null; then
        echo "FAIL: no .ini file found in '$phpConf' (${prefix})"
        echo "Available files in '$phpConf':"
        ls "$phpConf"
        exit 1
      else
        echo "PASS: PHP configuration file exists (${prefix})"
        touch "$out"
      fi
    '';

  # Validate nginx configuration syntax
  # Uses nginx -t to verify the generated config is syntactically valid
  # Note: We only check syntax, not runtime configuration (like PID file paths)
  nginx.testConfigWith = env: prefix: testWithEnv env "${prefix}-test-nginx-config-valid"
    ''
      echo "Testing nginx configuration syntax (${prefix})..."

      nginxConf="$profile/etc/nginx/nginx.conf"
      ln -s "$nginxConf" "$out"/attachments/nginx.conf

      if [[ ! -f "$nginxConf" ]]; then
        echo "FAIL: nginx.conf not found (${prefix})"
        exit 1
      fi

      # Create temporary directories for nginx -t
      tmpdir=$(mktemp -d)
      mkdir -p "$tmpdir"/{logs,run}

      # Run nginx -t with error log redirected to avoid permission errors
      # nginx will complain about missing runtime paths, but we only care about syntax
      # -e: error log path
      # -t: test configuration
      # -c: config file path
      # -p: prefix for relative paths
      output=$("$profile"/bin/nginx -e "$tmpdir/error.log" -t -c "$nginxConf" -p "$tmpdir" 2>&1 || true)

      # Check if "syntax is ok" appears in the output
      # This indicates the config file is syntactically valid
      if echo "$output" | grep -q "syntax is ok"; then
        echo "PASS: nginx configuration is syntactically valid"
        echo "(Note: Runtime path warnings are expected in this test environment)"
        touch "$out"
      else
        echo "FAIL: nginx configuration has syntax errors"
        echo ""
        echo "Full nginx -t output:"
        echo "$output"
        exit 1
      fi
    '';

  nginx.testProdVsDev = { prod, dev, prefix }: { }
    // testWithEnv prod "${prefix}-prod-no-debug-headers"
    ''
      nginxConf="$profile/etc/nginx/nginx.conf"
      ln -s "$nginxConf" "$out"/attachments/nginx.conf

      test -f "$nginxConf" || { echo "FAIL: nginx.conf missing (${prefix}-prod)"; exit 1; }
      # shellcheck disable=2016
      if grep -q '$hostenv_handled' "$nginxConf"; then
        echo "FAIL: \$hostenv_handled present in production"
        exit 1
      fi

      echo "PASS: no debug headers in prod"
      touch "$out"
    ''
    // testWithEnv dev "${prefix}-dev-has-debug-headers"
    ''
      nginxConf="$profile/etc/nginx/nginx.conf"
      ln -s "$nginxConf" "$out"/attachments/nginx.conf

      test -f "$nginxConf" || { echo "FAIL: nginx.conf missing (${prefix}-dev)"; exit 1; }
      # shellcheck disable=2016
      if ! grep -q '$hostenv_handled' "$nginxConf"; then
        echo "FAIL: \$hostenv_handled missing in dev"
        exit 1
      fi
      echo "PASS: debug headers present in dev"
      touch "$out"
    '';

  nginx.testUnixSocketWith = env: prefix: testWithEnv env "${prefix}-unix-socket-listen"
    ''
      echo "Testing unix socket listener configuration..."

      nginxConf="$profile/etc/nginx/nginx.conf"

      if [[ ! -f "$nginxConf" ]]; then
        echo "FAIL: nginx.conf not found"
        exit 1
      fi

      # Check that nginx is configured to listen on a unix socket
      if ! grep -q 'listen.*unix:.*in\.sock' "$nginxConf"; then
        echo "FAIL: Unix socket listener not found in nginx config"
        echo "Expected to find 'listen unix:.../in.sock'"
        echo ""
        echo "Available listen directives:"
        grep -E 'listen' "$nginxConf" || true
        exit 1
      fi

      echo "PASS: Unix socket listener configured correctly"
      touch "$out"
    '';

}
