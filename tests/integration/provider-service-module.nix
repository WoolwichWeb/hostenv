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
        root = ./.;
      };
      services.hostenv-provider.enable = true;
      environments.main = {
        enable = true;
        type = "testing";
      };
    })
  ] null;

  vhostNames = builtins.tryEval (builtins.attrNames env.config.services.nginx.virtualHosts);
  expectedHost = env.config.hostenv.hostname;
  ok = vhostNames.success && builtins.elem expectedHost vhostNames.value;
in
asserts.assertTrue "provider-service-module-eval"
  ok
  "services.hostenv-provider should evaluate nginx virtualHosts without dynamic-attribute collisions"
