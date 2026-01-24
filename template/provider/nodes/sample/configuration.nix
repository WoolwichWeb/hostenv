{ config, pkgs, ... }:
{
  imports = [ ./hardware-configuration.nix ];
  networking.hostName = "sample-node";
  system.stateVersion = "24.11";
}
