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
      backups.enable = true;
    };
  };

  drupalEnv = makeHostenv [
    baseModule
    ({ lib, ... }: {
      services.drupal = {
        enable = true;
        backups.enable = true;
      };
      services.restic.backups."drupal-migrate".tags = lib.mkForce [ "daily" ];
    })
  ] null;
  drupalFail = lib.any (a: a.assertion == false) drupalEnv.config.assertions;

  phpAppEnv = makeHostenv [
    baseModule
    ({ lib, ... }: {
      services.php-app = {
        enable = true;
        backups.enable = true;
      };
      services.restic.backups."php-app-migrate".tags = lib.mkForce [ "daily" ];
    })
  ] null;
  phpAppFail = lib.any (a: a.assertion == false) phpAppEnv.config.assertions;

in
{
  drupal-migrate-tags-assert =
    asserts.assertTrue "drupal-migrate-tags-assert" drupalFail
      "drupal migrate backups should require the backup name tag";

  php-app-migrate-tags-assert =
    asserts.assertTrue "php-app-migrate-tags-assert" phpAppFail
      "php-app migrate backups should require the backup name tag";
}
