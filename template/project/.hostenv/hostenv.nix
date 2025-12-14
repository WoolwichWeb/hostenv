{ pkgs, config, ... }: {

  services.drupal.enable = true;
  # services.drupal.backups.enable = true;
  # services.drupal.composer.dependencyHash = "REPLACE THIS WITH THE HASH FROM HOSTENV/NIX";
  # services.drupal.phpVersion = "8.4";

  # Run cron every fifteen minutes (this is the default for Drupal, so no need
  # to set it here, this is just an example).
  # services.drupal.cron.timerConfig.OnCalendar = "*:0/15";

  # @todo: instructions for adding an SSH key.

  environments.main = {
    enable = true;
  };

  # Example production branch:
  # environments.prod = {
  #   enable = true;
  #
  #   # There is only one "production" environment, designated by
  #   # `type = "production"`
  #   # Note: the production environment can be called anything, and point to
  #   # any git branch or tag.
  #   type = "production";
  #
  #   # When you're ready to launch your site, add production domains here.
  #   # Note: domains must be registered and pointing at the correct
  #   # hostenv environment.
  #   virtualHosts = {
  #     # Example redirect for naked TLD → www subdomain.
  #     "your-site.com" = {
  #       globalRedirect = "www.your-site.com";
  #     };
  #     "www.your-site.com" = {
  #       # Example redirect for a URL within the site:
  #       locations."/" = {
  #         return = "301 /town-events";
  #       };
  #     };
  #   };
  # };

  # Example user testing branch:
  # environments.user-testing = {
  #   enable = true;
  #   type = "testing";
  # };

  # Another branch example:
  # environments."feature/main/add-exciting-feature" = {
  #   enable = true;
  #   # Note: if "type" is not added here, it defaults to "development".
  # };

}
