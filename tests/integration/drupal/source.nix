{ pkgs }:

let
  drupalScaffold = pkgs.fetchzip {
    url = "https://ftp.drupal.org/files/projects/drupal-11.3.1.tar.gz";
    sha256 = "18nfziwihs3dnmfn8wcwaa0d2c3zky1pwch2127vlspgmck9mply";
    stripRoot = true;
  };
in
pkgs.runCommand "drupal11-test-root" { } ''
  mkdir -p "$out"
  install -Dm644 ${./composer.json} "$out"/composer.json
  install -Dm644 ${./composer.lock} "$out"/composer.lock
  mkdir -p "$out"/web
  cp -a ${drupalScaffold}/. "$out"/web/
''
