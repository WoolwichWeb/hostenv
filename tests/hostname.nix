{ pkgs, makeHostenv }:

let
  testHostenvHostname = "cli.test.hostenv";

  env = makeHostenv {
    organisation = "acme";
    project = "demo";
    hostenvHostname = testHostenvHostname;
    root = ./drupal; # any path is fine, not used by this test
    environmentName = "main";
    modules = [
      ({ ... }: {
        environments.main.enable = true;
        environments.main.type = "production";
      })
    ];
  };
in
pkgs.runCommand "hostenv-hostname-test" { } ''
  set -euo pipefail
  got_hostenv="${env.config.hostenv.hostenvHostname}"
  want_hostenv="${testHostenvHostname}"
  if [ "$got_hostenv" != "$want_hostenv" ]; then
    echo "expected hostenvHostname=$want_hostenv, got $got_hostenv" >&2
    exit 1
  fi

  got_hostname="${env.config.hostenv.hostname}"
  case "$got_hostname" in
    *.$want_hostenv) ;;
    *)
      echo "hostname $got_hostname does not end with .$want_hostenv" >&2
      exit 1
      ;;
  esac

  touch "$out"
''
