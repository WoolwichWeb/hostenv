{ pkgs, makeHostenv }:
let
  lib = pkgs.lib;
  support = import ../support { inherit pkgs lib; };
  asserts = support.asserts;

  env = makeHostenv [
    ({ ... }: {
      hostenv = {
        organisation = "acme";
        project = "demo";
        hostenvHostname = "hosting.test";
        root = ./drupal; # any path is fine, not used by this test
      };
      environments.main = {
        enable = true;
        type = "production";
      };
    })
  ] "main";

  cli = env.config.hostenv.cliPackage;
in
asserts.assertRun {
  name = "hostenv-cli-list";
  inherit env;
  script = ''
    export HOME="$TMPDIR/home"
    export XDG_CONFIG_HOME="$HOME/.config"
    mkdir -p "$XDG_CONFIG_HOME"

    "${cli}/bin/hostenv" list --env main >/dev/null
  '';
}
