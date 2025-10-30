# Test suite for Drupal 7 hostenv configuration
{ pkgs, envs }:

let
  lib = import ../lib.nix { inherit pkgs; };
  inherit (lib) testProfileWith;
in
testProfileWith envs.drupal7 "drupal7"
// lib.nginx.testConfigWith envs.drupal7 "drupal7"
// lib.php.testConfigWith envs.drupal7 "drupal7"
// lib.nginx.testUnixSocketWith envs.drupal7 "drupal7"
  // lib.testWithEnv envs.drupal7 "drupal7-test-php-version"
  ''
    echo "Testing that PHP 7.4 is configured for Drupal 7..."

    phpBin="$profile"/bin/php-fpm
    test -x "$phpBin" || { echo "FAIL: php not found at $phpBin"; exit 1; }

    # Grab the first line of --version (e.g. "PHP 7.4.x (cli) …")
    ver="$("$phpBin" --version | head -n1)"
    echo "Detected: $ver"

    # Accept anything that clearly says 7.4
    if echo "$ver" | grep -qE 'PHP[[:space:]]+7\.4(\.|[[:space:]])'; then
      echo "PASS: PHP 7.4 is configured for Drupal 7"
    else
      echo "FAIL: expected PHP 7.4.*; got: $ver"
      exit 1
    fi

    mkdir -p "$out/attachments"
    printf '%s\n' "$ver" > "$out"/attachments/php-version
    touch "$out"
  ''
