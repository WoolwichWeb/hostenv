{ ... }:
{
  config.flake = {
    modules = ../.;
    cliModule = ../cli-flake-module.nix;

    hostenvModules = {
      full-env = ../core/full-env.nix;
      environment = ../core/environment.nix;
      environments = ../core/environments.nix;
      hostenv = ../core/hostenv.nix;
      cli = ../core/cli.nix;
    };

    nixosModules = {
      plan-bridge = ../nixos/plan-bridge.nix;
      backups-hostenv = ../nixos/backups-hostenv.nix;
      nginx-hostenv = ../nixos/nginx-hostenv.nix;
      nginx-tuning-hostenv = ../nixos/nginx-tuning-hostenv.nix;
      hostenv-service = ../nixos/hostenv-service.nix;
      monitoring-hostenv = ../nixos/monitoring-hostenv.nix;
      users-slices = ../nixos/users-slices.nix;
      top-level = ../nixos/top-level.nix;
    };
  };
}
