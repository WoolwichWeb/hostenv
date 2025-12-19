{ ... }:
{
  config.flake = {
    flakeModules = {
      cli = ./cli.nix;
      hostenvOutputs = ./hostenv-outputs.nix;
      provider = ../../provider/flake-module.nix;
    };

    hostenvModules = {
      fullEnv = ../hostenv-modules/full-env.nix;
      environment = ../hostenv-modules/environment.nix;
      environments = ../hostenv-modules/environments.nix;
      hostenv = ../hostenv-modules/hostenv.nix;
      projectTools = ../hostenv-modules/project-tools.nix;
    };

    nixosModules = {
      planBridge = ../nixos-modules/plan-bridge.nix;
      backupsHostenv = ../nixos-modules/backups-hostenv.nix;
      nginxHostenv = ../nixos-modules/nginx-hostenv.nix;
      nginxTuningHostenv = ../nixos-modules/nginx-tuning-hostenv.nix;
      hostenvService = ../nixos-modules/hostenv-service.nix;
      monitoringHostenv = ../nixos-modules/monitoring-hostenv.nix;
      usersSlices = ../nixos-modules/users-slices.nix;
      topLevel = ../nixos-modules/top-level.nix;
    };
  };
}
