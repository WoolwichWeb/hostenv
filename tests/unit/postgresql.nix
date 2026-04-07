{ pkgs }:
let
  lib = pkgs.lib;
  support = import ../support { inherit pkgs lib; };
  asserts = support.asserts;
  postgresqlModule = (import ../../modules/features/postgresql.nix { }).flake.modules.hostenv.postgresql;

  evalDefault = support.evalWithBase {
    modules = [
      postgresqlModule
      ({ ... }: {
        hostenv = {
          userName = "pgtest";
          runtimeRoot = "/run/hostenv";
        };
        services.postgresql = {
          enable = true;
          user = "pgtest";
        };
      })
    ];
  };

  defaultModeOk = (evalDefault.config.services.postgresql.settings.unix_socket_permissions or null) == 504;

  evalEnsureDatabases = support.evalWithBase {
    modules = [
      postgresqlModule
      ({ ... }: {
        hostenv = {
          userName = "pgtest";
          runtimeRoot = "/run/hostenv";
        };
        services.postgresql = {
          enable = true;
          user = "pgtest";
          ensureDatabases = [ "hostenv-provider" ];
        };
      })
    ];
  };

  postStartScript = evalEnsureDatabases.config.systemd.services.postgresql.postStart or "";
  ensureDbUsesGexec = lib.hasInfix "\\gexec" postStartScript;
  ensureDbAvoidsDoBlock = !(lib.hasInfix "DO $$" postStartScript);
  profilePaths = builtins.map toString (evalEnsureDatabases.config.profile or [ ]);

  profileWrappersConfigured = pkgs.runCommand "postgresql-profile-wrappers-configured" { } ''
    set -euo pipefail

    found_psql=0
    found_pg_dump=0
    found_pg_dumpall=0

    for p in ${lib.escapeShellArgs profilePaths}; do
      if [ -x "$p/bin/psql" ]; then
        found_psql=1
        grep -q 'PGHOST="/run/hostenv/user/pgtest"' "$p/bin/psql"
        grep -q 'PGUSER="pgtest"' "$p/bin/psql"
        grep -q 'PGDATABASE="hostenv-provider"' "$p/bin/psql"
      fi

      if [ -x "$p/bin/pg_dump" ]; then
        found_pg_dump=1
        grep -q 'PGHOST="/run/hostenv/user/pgtest"' "$p/bin/pg_dump"
        grep -q 'PGUSER="pgtest"' "$p/bin/pg_dump"
        grep -q 'PGDATABASE="hostenv-provider"' "$p/bin/pg_dump"
      fi

      if [ -x "$p/bin/pg_dumpall" ]; then
        found_pg_dumpall=1
        grep -q 'PGHOST="/run/hostenv/user/pgtest"' "$p/bin/pg_dumpall"
        grep -q 'PGUSER="pgtest"' "$p/bin/pg_dumpall"
      fi
    done

    [ "$found_psql" -eq 1 ] || { echo "psql wrapper missing"; exit 1; }
    [ "$found_pg_dump" -eq 1 ] || { echo "pg_dump wrapper missing"; exit 1; }
    [ "$found_pg_dumpall" -eq 1 ] || { echo "pg_dumpall wrapper missing"; exit 1; }

    echo ok > "$out"
  '';

  evalInvalid = support.evalWithBase {
    modules = [
      postgresqlModule
      ({ ... }: {
        hostenv = {
          userName = "pgtest";
          runtimeRoot = "/run/hostenv";
        };
        services.postgresql = {
          enable = true;
          user = "pgtest";
          settings.unix_socket_permissions = 770;
        };
      })
    ];
  };

  invalidModeFails =
    lib.any (a: a.assertion == false)
      (evalInvalid.config.assertions or [ ]);
in
{
  postgresql-socket-permissions-default =
    asserts.assertTrue "postgresql-socket-permissions-default" defaultModeOk
      "postgresql unix_socket_permissions default should be 504 (octal 0770)";

  postgresql-socket-permissions-assertion =
    asserts.assertTrue "postgresql-socket-permissions-assertion" invalidModeFails
      "postgresql unix_socket_permissions should fail assertion when outside 0..511";

  postgresql-ensure-database-gexec =
    asserts.assertTrue "postgresql-ensure-database-gexec" ensureDbUsesGexec
      "postgresql ensureDatabases should use psql \\gexec for CREATE DATABASE";

  postgresql-ensure-database-no-do =
    asserts.assertTrue "postgresql-ensure-database-no-do" ensureDbAvoidsDoBlock
      "postgresql ensureDatabases should not emit DO $$ blocks for CREATE DATABASE";

  postgresql-profile-wrappers-configured = profileWrappersConfigured;
}
