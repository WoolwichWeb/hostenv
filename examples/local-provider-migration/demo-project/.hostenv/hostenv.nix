{ pkgs, config, ... }:
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

  # PostgreSQL for hostenv-provider service (ephemeral, workdir-relative)
  services.postgresql = {
    enable = true;
    user = config.hostenv.userName;
    dataDir = "${config.hostenv.dataDir}/postgresql";
    runtimeDir = config.hostenv.runtimeDir;
    ensureDatabases = [ "hostenv-provider" ];
    ensureUsers = [
      {
        name = config.hostenv.userName;
        ensurePermissions = { "hostenv-provider" = "ALL PRIVILEGES"; };
      }
    ];
  };

  # Provider-service configuration (no GitLab OAuth for demo simplicity)
  services.hostenv-provider = {
    enable = true;
    webhookHost = "demo.hostenv.test";
    uiBasePath = "/dashboard";
    # GitLab OAuth disabled for demo - uses dev mode
  };

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
