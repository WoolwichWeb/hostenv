{ ... }:
{
  config.flake.lib.provider = {
    nixosSystem = ./nixos-system.nix;
  };

  config.flake.flakeModules.provider = ./flake-module.nix;
}
