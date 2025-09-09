# Minimal hostenv configuration, meant to quickly build the hostenv CLI
# (and the environment metadata it requires).
{ lib, ... }:
{
  options.hostenv = lib.mkOption {
    type = lib.types.submodule ../hostenv.nix;
    description = "Hostenv configuration for the current environment.";
  };

  # # Fake options
  # Allow merging user hostenv.nix modules, while ignoring the bits we
  # have no interest in.
  options.systemd = lib.mkOption {
    type = lib.types.attr;
    description = "In a minimal hostenv environment this option is ignored.";
  };
  options.services = lib.mkOption {
    type = lib.types.attr;
    description = "In a minimal hostenv environment this option is ignored.";
  };

  imports = [
    ../environments.nix
    ../hostenv.nix
    ../hostenv-cli.nix
  ];
}
