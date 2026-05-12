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
      services.nginx.virtualHosts."example.test" = { };
      environments.main = {
        enable = true;
        type = "testing";
      };
    })
  ] null;

  vhostNames = builtins.tryEval (builtins.attrNames env.config.services.nginx.virtualHosts);
  fallbackServerName = builtins.tryEval env.config.services.nginx.virtualHosts."example.test".serverName;
  nginxPreStart = builtins.tryEval env.config.systemd.services.nginx.preStart;
  expectedHost = env.config.hostenv.hostname;
  ok =
    vhostNames.success
    && builtins.elem expectedHost vhostNames.value
    && fallbackServerName.success
    && fallbackServerName.value == "example.test"
    && nginxPreStart.success;
in
asserts.assertTrue "provider-service-module-eval"
  ok
  "services.hostenv-provider should evaluate nginx virtualHosts and default serverName from the attr name"
