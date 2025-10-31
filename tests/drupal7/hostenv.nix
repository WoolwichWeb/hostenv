{ pkgs, config, ... }: {

  services.drupal.enable = true;
  services.drupal.backups.enable = true;

  # Use PHP 7.4 for Drupal 7 compatibility
  services.drupal.phpVersion = "7.4";

  # Drupal 7 doesn't use Composer in the same way as Drupal 8+
  # Disable composer since we're using the standard Drupal 7 tarball
  services.drupal.composer.enable = false;

  services.drupal.cron.timerConfig.OnCalendar = "*:0/5";

  environments.main = {
    enable = true;
    type = "production";
    virtualHosts = {
      "example.com" = {
        globalRedirect = "www.example.com";
      };
      "www.example.com" = { };
    };
  };

  environments.test = {
    enable = true;
    type = "testing";
  };
}
