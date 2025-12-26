{ lib, ... }:
let
  types = lib.types;
in
{
  options.flake = {
    lib = lib.mkOption {
      type = types.submodule {
        freeformType = types.attrsOf types.unspecified;
        options = {
          hostenvModules = lib.mkOption {
            type = types.attrsOf types.path;
            default = { };
            description = "Hostenv evalModules modules (merged across modules, exported under lib.hostenvModules).";
          };
        };
      };
      default = { };
      description = "Library outputs merged under flake.lib.";
    };

    flakeModules = lib.mkOption {
      type = types.attrsOf types.path;
      default = { };
      description = "Exported flake-parts modules (merged across modules).";
    };

    # Note: flake-parts already defines `flake.nixosModules`, so we do not
    # declare it here to avoid option redefinition errors.
  };

  config.flake = {
    # Re-usable flake-parts modules.
    flakeModules = {
      exports = ./exports.nix;
      cli = ./cli.nix;
      environmentRegistry = ./environment-registry.nix;
      projectOutputs = ./project-outputs.nix;
      hostenvProviderService = ./hostenv-provider-service.nix;
    };

    # Modules consumed by evalModules.
    lib.hostenvModules = {
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
  };
}
