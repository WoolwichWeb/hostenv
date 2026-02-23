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
      allEnvironments.users.alice = {
        email = "alice@example.com";
        publicKeys = [ ];
        gitlabUsername = "AliceUser";
      };
      services.hostenv-provider.enable = true;
      services.hostenv-provider.gitlab.enable = true;
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
      nginxConf="$profile/etc/nginx/nginx.conf"
      test -f "$nginxConf" || { echo "missing nginx.conf"; exit 1; }
      tmpdir=$(mktemp -d)
      mkdir -p "$tmpdir"/{logs,run}
      output=$("$profile"/bin/nginx -e "$tmpdir/error.log" -t -c "$nginxConf" -p "$tmpdir" 2>&1 || true)
      echo "$output" | grep -q "syntax is ok" || { echo "$output"; exit 1; }
      grep -q -- "proxy_connect_timeout 600s;" "$nginxConf" || { echo "missing provider proxy_connect_timeout"; exit 1; }
      grep -q -- "proxy_send_timeout 600s;" "$nginxConf" || { echo "missing provider proxy_send_timeout"; exit 1; }
      grep -q -- "proxy_read_timeout 600s;" "$nginxConf" || { echo "missing provider proxy_read_timeout"; exit 1; }
      execStart=$(sed -n 's/^ExecStart=//p' "$profile/systemd/user/hostenv-provider.service")
      test -n "$execStart" || { echo "hostenv-provider.service missing ExecStart"; exit 1; }
      test -x "$execStart" || { echo "hostenv-provider ExecStart target is not executable"; exit 1; }
      configPath=$(sed -n 's/^exec .* --config //p' "$execStart" | head -n1)
      test -f "$configPath" || { echo "hostenv-provider config file missing"; exit 1; }
      grep -q -- '"seedUsers":' "$configPath" || { echo "hostenv-provider config missing seedUsers"; exit 1; }
      grep -q -- '"configUsername":"alice"' "$configPath" || { echo "hostenv-provider config missing alice seed"; exit 1; }
      grep -q -- '"provider":"gitlab"' "$configPath" || { echo "hostenv-provider config missing gitlab provider account seed"; exit 1; }
      if grep -q -- '^WorkingDirectory=' "$profile/systemd/user/hostenv-provider.service"; then
        echo "hostenv-provider.service must not depend on a pre-existing WorkingDirectory"
        exit 1
      fi
    '';
  }
