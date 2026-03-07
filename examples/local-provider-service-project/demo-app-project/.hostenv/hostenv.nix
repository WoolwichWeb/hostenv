{ ... }:
{
  services.drupal.enable = true;
  services.drupal.backups.enable = true;
  services.drupal.composer.dependencyHash = "sha256-WQu4NjUork7SygsUUpzZTTpUnp2wPHylgSZUf7Edb38=";
  services.drupal.cron.timerConfig.OnCalendar = "*:0/5";
  services.drupal.settings.extraSettings = ''
    $settings['hash_salt'] = 'hostenv-local-demo-salt';
  '';

  # Keep the DB footprint small for the local demo VMs.
  services.mysql.settings.mysqld.innodb_buffer_pool_size = "256M";

  environments.main = {
    enable = true;
    type = "production";
    virtualHosts."demo.hostenv.local" = {
      enableLetsEncrypt = false;
      globalRedirect = null;
    };

    users.demo = {
      email = "demo@example.test";
      publicKeys = [
        "SSH_PUBLIC_KEY_PLACEHOLDER"
      ];
    };
  };
}
