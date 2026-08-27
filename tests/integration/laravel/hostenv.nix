{ config, pkgs, ... }:
{
  packages = [ pkgs.hello ];

  services.laravel = {
    enable = true;
    assets.package = pkgs.runCommand "laravel-test-public-assets" { } ''
      mkdir -p "$out/css" "$out/js"
      printf '%s\n' 'hostenv generated stylesheet' > "$out/css/hostenv.css"
      printf '%s\n' 'hostenv generated script' > "$out/js/hostenv.js"
    '';
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
