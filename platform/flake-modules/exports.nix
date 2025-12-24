{ ... }:
{
  config.flake = {
    # Re-usable flake-parts modules.
    flakeModules = {
      cli = ./cli.nix;
      environmentRegistry = ./environment-registry.nix;
      projectOutputs = ./project-outputs.nix;
      provider = ../../provider/flake-module.nix;
      hostenvProviderService = ./hostenv-provider-service.nix;
    };

    # Modules consumed by evalModules.
    hostenvModules = {
      fullEnv = ../hostenv-modules/full-env.nix;
      environment = ../hostenv-modules/environment.nix;
      environments = ../hostenv-modules/environments.nix;
      hostenv = ../hostenv-modules/hostenv.nix;
      projectTools = ../hostenv-modules/project-tools.nix;
    };

    # Modules consumed by nixosSystem.
    nixosModules = {
      backupsHostenv = ../nixos-modules/backups-hostenv.nix;
      nginxHostenv = ../nixos-modules/nginx-hostenv.nix;
      nginxTuningHostenv = ../nixos-modules/nginx-tuning-hostenv.nix;
      hostenvService = ../nixos-modules/hostenv-service.nix;
      monitoringHostenv = ../nixos-modules/monitoring-hostenv.nix;
      usersSlices = ../nixos-modules/users-slices.nix;
      topLevel = ../nixos-modules/top-level.nix;
    };

    lib.provider = {
      nixosSystem = ../provider/nixos-system.nix;
    };
  };
}
