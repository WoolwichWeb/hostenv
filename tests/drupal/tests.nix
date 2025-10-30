# Drupal 10+ specific tests.
{ pkgs, envs }:

let
  lib = import ../lib.nix { inherit pkgs; };
  inherit (lib) testWithEnv;

  testDrupalStructure = env: prefix: testWithEnv env "${prefix}-test-drupal-structure"
    ''
      echo "Testing Drupal file structure..."

      mkdir -p "$out"/attachments/share
      ln -s "$profile"/share/php "$out"/attachments/share/php

      ls -lah "$profile/share/php"
      compgen -G "$profile/share/php/*/web" >/dev/null || { echo "FAIL: missing $profile/share/php/<codebase>/web directory"; exit 1; }
      compgen -G "$profile/share/php/*/web/index.php" >/dev/null || { echo "FAIL: missing $profile/share/php/<codebase>/web/index.php file"; exit 1; }

      echo "PASS: Drupal structure is correct ($profile)"
      touch "$out"
    '';
in
lib.testProfileWith envs.drupalProduction "drupal-prod"
// lib.testProfileWith envs.drupalDev "drupal-dev"
// lib.nginx.testConfigWith envs.drupalProduction "drupal-prod"
// lib.nginx.testConfigWith envs.drupalDev "drupal-dev"
// lib.nginx.testProdVsDev { prod = envs.drupalProduction; dev = envs.drupalDev; prefix = "drupal"; }
// lib.php.testConfigWith envs.drupalDev "drupal-dev"
// lib.php.testConfigWith envs.drupalDev "drupal-prod"
// lib.nginx.testUnixSocketWith envs.drupalProduction "drupal-prod"
// lib.nginx.testUnixSocketWith envs.drupalDev "drupal-dev"
// testDrupalStructure envs.drupalProduction "drupal-prod"
  // testDrupalStructure envs.drupalDev "drupal-dev"
