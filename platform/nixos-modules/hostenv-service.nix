{ config, lib, pkgs, ... }:
{
  # nginx proxy to systemd socket
  # take data from request, like branch, or tag, repo name.
  # generate hostenv username
  # use that to call deploy-rs
  # if there is no data directory (or something), restore from restic backup (unless the Drupal module should be responsible for that, maybe an 'on deploy' script)
  # update Cloudflare DNS
  # hostenv.nix enables hostenv and provides list of servers
  options.services.hostenv = {
    enable = lib.mkEnableOption ''hostenv deployment services
        Note: you probably don't need to enable this, unless you are self-
        hosting hostenv.
        '';
  };
}
