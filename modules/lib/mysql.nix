{ ... }:
{
  flake.lib.hostenv.mysql = {
    mkPrograms =
      {
        lib,
        pkgs,
        package,
      }:
      let
        isMariaDB = lib.getName package == lib.getName pkgs.mariadb;
        program =
          mysqlName: mariadbName: lib.getExe' package (if isMariaDB then mariadbName else mysqlName);
      in
      {
        inherit isMariaDB;
        client = program "mysql" "mariadb";
        dump = program "mysqldump" "mariadb-dump";
        server = program "mysqld" "mariadbd";
        installDb = if isMariaDB then lib.getExe' package "mariadb-install-db" else null;
      };
  };
}
