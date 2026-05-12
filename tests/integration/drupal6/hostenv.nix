{ pkgs, config, lib, ... }: {

  services.drupal.enable = true;
  services.drupal.majorVersion = 6;
  services.drupal.backups.enable = true;

  # Use a stub Drush for structural evaluation. The Drupal 6 module still
  # wraps the configured package with the pool's PHP CLI package at runtime.
  services.drupal.drupal6.drushPackage = pkgs.writeShellScriptBin "drush" ''
    echo "stub drush $@"
  '';

  services.drupal.cron.timerConfig.OnCalendar = "*:0/5";

  environments.main = {
    enable = true;
    type = "production";
    virtualHosts = {
      "drupal6.example.com" = {
        globalRedirect = "www.drupal6.example.com";
      };
      "www.drupal6.example.com" = { };
    };
  };

  environments.test = {
    enable = true;
    type = "testing";
  };

  services.nginx.virtualHosts."${config.services.drupal.codebase.name}".locations."/" = {
    extraConfig = lib.mkBefore ''
      # Location mkBefore test marker
    '';
  };
}
