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
}
