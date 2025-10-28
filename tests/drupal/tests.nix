# Test suite for Drupal hostenv configuration
#
# This file defines all checks for the Drupal test environment.
# Import this in flake.nix with: import ./tests/drupal/tests.nix { inherit pkgs drupalProduction testDrupalDebug; }
#
# Run all tests with: nix flake check
# Run individual test with: nix build .#checks.x86_64-linux.test-nginx-conf-exists

{ pkgs, tests }:

let
  inherit (tests) drupalDev drupalProduction;
in
{
  # Basic structure test
  # Verifies that the drupalProduction package builds and contains the expected directory structure
  test-drupal-builds = pkgs.runCommand "test-drupal-builds" { } ''
    echo "Testing that drupalProduction builds successfully..."

    if [ ! -d "${drupalProduction.config.activatePackage}" ]; then
      echo "FAIL: drupalProduction did not produce a valid output"
      exit 1
    fi

    echo "PASS: drupalProduction builds successfully"
    touch $out
  '';

  # Verify nginx.conf is generated
  # The nginx configuration is a critical part of the hostenv setup
  test-nginx-conf-exists = pkgs.runCommand "test-nginx-conf-exists"
    {
      buildInputs = [ pkgs.coreutils ];
    } ''
    echo "Testing that nginx.conf exists in drupalProduction output..."

    if [ ! -f "${drupalProduction.config.activatePackage}/etc/nginx/nginx.conf" ]; then
      echo "FAIL: nginx.conf not found at expected path"
      echo "Contents of ${drupalProduction.config.activatePackage}:"
      ls -R "${drupalProduction.config.activatePackage}" || true
      exit 1
    fi

    echo "PASS: nginx.conf exists at ${drupalProduction.config.activatePackage}/etc/nginx/nginx.conf"
    touch $out
  '';

  # Verify $hostenv_handled is NOT set in production environment
  # Production environments (type = "production") should NOT have route debugging enabled
  # This is important for performance and security
  test-production-no-debug-headers = pkgs.runCommand "test-production-no-debug-headers"
    {
      buildInputs = [ pkgs.gnugrep ];
    } ''
    echo "Testing that production environment does not emit debug headers..."

    nginxConf="${drupalProduction.config.activatePackage}/etc/nginx/nginx.conf"

    if [ ! -f "$nginxConf" ]; then
      echo "FAIL: nginx.conf not found"
      exit 1
    fi

    # Check that $hostenv_handled is NOT present in the config
    if grep -q '\$hostenv_handled' "$nginxConf"; then
      echo "FAIL: Found \$hostenv_handled in production nginx.conf"
      echo "Production environments should not have route debugging enabled"
      echo ""
      echo "Matches found:"
      grep '\$hostenv_handled' "$nginxConf" || true
      exit 1
    fi

    echo "PASS: No debug headers in production environment"
    touch $out
  '';

  # Verify $hostenv_handled IS set in development environment
  # Non-production environments should have route debugging enabled by default
  test-development-has-debug-headers = pkgs.runCommand "test-development-has-debug-headers"
    {
      buildInputs = [ pkgs.gnugrep ];
    } ''
    echo "Testing that development environment emits debug headers..."

    nginxConf="${drupalDev.config.activatePackage}/etc/nginx/nginx.conf"

    if [ ! -f "$nginxConf" ]; then
      echo "FAIL: nginx.conf not found"
      exit 1
    fi

    # Check that $hostenv_handled IS present in the config
    if ! grep -q '\$hostenv_handled' "$nginxConf"; then
      echo "FAIL: \$hostenv_handled not found in development nginx.conf"
      echo "Development environments should have route debugging enabled"
      exit 1
    fi

    echo "PASS: Debug headers present in development environment"
    touch $out
  '';

  # Validate nginx configuration syntax
  # Uses nginx -t to verify the generated config is syntactically valid
  # Note: We only check syntax, not runtime configuration (like PID file paths)
  test-nginx-config-valid = pkgs.runCommand "test-nginx-config-valid"
    {
      buildInputs = [ pkgs.coreutils pkgs.gnugrep ];
    } ''
    echo "Testing nginx configuration syntax..."

    nginxConf="${drupalProduction.config.activatePackage}/etc/nginx/nginx.conf"

    if [ ! -f "$nginxConf" ]; then
      echo "FAIL: nginx.conf not found"
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
    output=$(${drupalProduction.config.activatePackage}/bin/nginx -e "$tmpdir/error.log" -t -c "$nginxConf" -p "$tmpdir" 2>&1 || true)

    # Check if "syntax is ok" appears in the output
    # This indicates the config file is syntactically valid
    if echo "$output" | grep -q "syntax is ok"; then
      echo "PASS: nginx configuration is syntactically valid"
      echo "(Note: Runtime path warnings are expected in this test environment)"
      touch $out
    else
      echo "FAIL: nginx configuration has syntax errors"
      echo ""
      echo "Full nginx -t output:"
      echo "$output"
      exit 1
    fi
  '';

  # Verify required Drupal paths are present
  # Checks that all essential components of the hostenv profile are generated
  test-drupal-structure = pkgs.runCommand "test-drupal-structure"
    {
      buildInputs = [ pkgs.coreutils ];
    } ''
    echo "Testing Drupal file structure..."

    profile="${drupalProduction.config.activatePackage}"

    # Check for PHP-FPM pool config
    if [ ! -d "$profile/etc/php-fpm.d" ]; then
      echo "FAIL: PHP-FPM config directory not found at $profile/etc/phpfpm"
      echo "Available directories:"
      find "$profile/etc" -type d 2>/dev/null || true
      exit 1
    fi

    # Check for systemd services
    if [ ! -d "$profile/systemd/user" ]; then
      echo "FAIL: systemd user services directory not found at $profile/systemd/user"
      exit 1
    fi

    # Check for activation script
    if [ ! -f "$profile/bin/activate" ]; then
      echo "FAIL: activation script not found at $profile/bin/activate"
      exit 1
    fi

    echo "PASS: Drupal structure is correct"
    touch $out
  '';

  # Verify unix socket configuration
  # Hostenv uses unix sockets for communication between user nginx and upstream proxy
  test-unix-socket-listen = pkgs.runCommand "test-unix-socket-listen"
    {
      buildInputs = [ pkgs.gnugrep ];
    } ''
    echo "Testing unix socket listener configuration..."

    nginxConf="${drupalProduction.config.activatePackage}/etc/nginx/nginx.conf"

    if [ ! -f "$nginxConf" ]; then
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
    touch $out
  '';
}
