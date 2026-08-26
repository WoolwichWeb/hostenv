{ config, ... }:
{
  services.laravel = {
    enable = true;
    backups.enable = true;
    redis.enable = true;
    scheduler.timerConfig.OnCalendar = "minutely";
    environmentVariables.HOSTENV_FIXTURE = "non-secret";
  };

  environments.main = {
    enable = true;
    type = "production";
    virtualHosts = {
      "laravel.example.com".globalRedirect = "www.laravel.example.com";
      "www.laravel.example.com" = { };
    };
  };

  environments.dev = {
    enable = true;
    type = "development";
  };

  services.nginx.virtualHosts.${config.services.laravel.codebase.name}.locations."/".extraConfig = ''
    # Laravel fixture location merge marker
  '';
}
