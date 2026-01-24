{ pkgs, makeHostenv }:
let
  lib = pkgs.lib;
  support = import ../support { inherit pkgs lib; };
  asserts = support.asserts;

  baseModule = { ... }: {
    hostenv = {
      organisation = "org";
      project = "proj";
      hostenvHostname = "hosting.test";
      root = "/srv/app";
    };
    defaultEnvironment = "main";
    environments.main = {
      enable = true;
      type = "production";
      hostenv.userName = "org-proj-main";
      hostenv.hostname = "org-proj-main.hosting.test";
      virtualHosts = { };
    };
    services.mysql = {
      enable = true;
      user = "org-proj-main";
      package = pkgs.mariadb;
      backups.enable = false;
    };
  };

  drupalEnv = makeHostenv [
    baseModule
    ({ ... }: {
      services.drupal = {
        enable = true;
        backups.enable = true;
      };
    })
  ] null;
  drupalFail = lib.any (a: a.assertion == false) drupalEnv.config.assertions;

  phpAppEnv = makeHostenv [
    baseModule
    ({ ... }: {
      services.php-app = {
        enable = true;
        backups.enable = true;
      };
    })
  ] null;
  phpAppFail = lib.any (a: a.assertion == false) phpAppEnv.config.assertions;

in
{
  drupal-backups-require-mysql-backups =
    asserts.assertTrue "drupal-backups-require-mysql-backups" drupalFail
      "drupal backups should require mysql backups to be enabled";

  php-app-backups-require-mysql-backups =
    asserts.assertTrue "php-app-backups-require-mysql-backups" phpAppFail
      "php-app backups should require mysql backups to be enabled";
}
