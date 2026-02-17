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
  hasPostgresService = env.config.systemd.services ? postgresql;
  ensuredDatabases = env.config.services.postgresql.ensureDatabases or [ ];
  ensuresProviderDb = builtins.elem "hostenv-provider" ensuredDatabases;
  configOk =
    vhostNames.success
    && builtins.elem expectedHost vhostNames.value
    && hasPostgresService
    && ensuresProviderDb;
in
if !configOk then
  asserts.assertTrue "provider-service-module-eval"
    false
    "services.hostenv-provider should evaluate cleanly and include nginx + PostgreSQL defaults"
else
  asserts.assertRun {
    name = "provider-service-module-eval";
    inherit env;
    script = ''
      test -f "$profile/systemd/user/hostenv-provider.service" || { echo "missing hostenv-provider.service"; exit 1; }
      test -f "$profile/systemd/user/postgresql.service" || { echo "missing postgresql.service"; exit 1; }
      grep -q -- '^ExecStart=.*hostenv-provider-service --config ' "$profile/systemd/user/hostenv-provider.service" \
        || { echo "hostenv-provider.service must use file-based config"; exit 1; }
      if grep -q -- '^Environment="HOSTENV_PROVIDER_' "$profile/systemd/user/hostenv-provider.service"; then
        echo "hostenv-provider.service should not carry HOSTENV_PROVIDER_* env config"
        exit 1
      fi
    '';
  }
