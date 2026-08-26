{ pkgs, makeHostenv }:
let
  lib = pkgs.lib;
  asserts = (import ../support { inherit pkgs lib; }).asserts;

  base = { ... }: {
    hostenv = {
      organisation = "acme";
      project = "secret-files";
      hostenvHostname = "hosting.test";
      root = ./.;
    };
    environments.main = {
      enable = true;
      type = "production";
    };
  };

  merged = makeHostenv [
    base
    { environments.main.requiredSecretFiles = [ "FIRST_SECRET" ]; }
    { environments.main.requiredSecretFiles = [ "second_secret" ]; }
  ] "main";

  invalid = builtins.tryEval (builtins.deepSeq
    (makeHostenv [
      base
      { environments.main.requiredSecretFiles = [ "../not-a-secret-name" ]; }
    ] "main").config.environments.main.requiredSecretFiles
    true);

  laravel = makeHostenv [
    base
    { services.laravel = { enable = true; composer.enable = false; }; }
  ] "main";

  drupal = makeHostenv [
    base
    { services.drupal = { enable = true; composer.enable = false; }; }
  ] "main";

  phpOnly = makeHostenv [
    base
    { services.php-app.enable = true; }
  ] "main";
in
{
  required-secret-files-merge-and-validation =
    asserts.assertTrue "required-secret-files-merge-and-validation"
      (
        merged.config.environments.main.requiredSecretFiles == [ "FIRST_SECRET" "second_secret" ]
        && !invalid.success
      )
      "requiredSecretFiles should merge valid restrictive names and reject project-controlled paths";

  required-secret-files-framework-boundary =
    asserts.assertTrue "required-secret-files-framework-boundary"
      (
        laravel.config.environments.main.requiredSecretFiles == [ "laravel_env" ]
        && drupal.config.environments.main.requiredSecretFiles == [ ]
        && phpOnly.config.environments.main.requiredSecretFiles == [ ]
      )
      "Laravel alone should request laravel_env; Drupal and generic PHP environments should remain unchanged";
}
