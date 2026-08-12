{ pkgs, config, lib, ... }: {

  services.drupal.enable = true;
  services.drupal.majorVersion = 6;
  services.drupal.backups.enable = true;
  services.drupal.codebase.version = "6.x-test";

  # Use a stub Drush for structural evaluation. The Drupal 6 module still
  # wraps the configured package with the pool's PHP CLI package at runtime.
  services.drupal.drupal6.drushPackage = pkgs.writeShellScriptBin "drush" ''
    echo "stub drush $@"
  '';

  # Keep the manually invoked VM test small enough to run on a development
  # machine. The production default is intentionally much larger.
  services.mysql.settings.mysqld.innodb_buffer_pool_size = lib.mkForce "128M";

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
