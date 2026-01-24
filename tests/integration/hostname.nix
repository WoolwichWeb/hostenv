{ pkgs, makeHostenv }:
let
  lib = pkgs.lib;
  support = import ../support { inherit pkgs lib; };
  asserts = support.asserts;

  testHostenvHostname = "cli.test.hostenv";

  env = makeHostenv [
    ({ ... }: {
      hostenv = {
        organisation = "acme";
        project = "demo";
        hostenvHostname = testHostenvHostname;
        root = ./drupal; # any path is fine, not used by this test
      };
      environments.main.enable = true;
      environments.main.type = "production";
    })
  ] "main";

  cfg = env.config.hostenv;

  okHostenv = cfg.hostenvHostname == testHostenvHostname;
  okHostname = lib.hasSuffix ".${testHostenvHostname}" cfg.hostname;
in
asserts.assertTrue "hostenv-hostname-test"
  (okHostenv && okHostname)
  "hostenvHostname and hostname should reflect configured control-plane domain"
