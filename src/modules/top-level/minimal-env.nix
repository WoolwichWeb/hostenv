# Minimal hostenv configuration, meant to quickly build the hostenv CLI
# (and the environment metadata it requires).
{ lib, ... }:
{
  options.hostenv = lib.mkOption {
    type = lib.types.submodule (import ../hostenv.nix);
    description = "Hostenv configuration for the current environment.";
  };

  imports = [
    ../environments.nix
    ../hostenv.nix
    ../hostenv-cli.nix
  ];
}
