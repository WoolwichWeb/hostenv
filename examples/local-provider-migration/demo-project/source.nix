{ pkgs }:
let
  drupalScaffold = pkgs.fetchzip {
    url = "https://ftp.drupal.org/files/projects/drupal-11.3.1.tar.gz";
    sha256 = "sha256-nt6aJqvvarqPCAIyfoOffzDRgFKccWRdtW1oGHn8zqI=";
    stripRoot = true;
  };
in
pkgs.runCommand "drupal11-demo-root" { } ''
  mkdir -p "$out"
  install -Dm644 ${./composer.json} "$out"/composer.json
  install -Dm644 ${./composer.lock} "$out"/composer.lock
  mkdir -p "$out"/web
  cp -a ${drupalScaffold}/. "$out"/web/
''
