{
  description = "Hostenv";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    phps = {
      url = "github:fossar/nix-phps";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        utils.follows = "flake-utils";
      };
    };
    hostenv.url = "gitlab:woolwichweb/hostenv?dir=src/modules";
  };

  outputs = { self, nixpkgs, flake-utils, ... } @ inputs: flake-utils.lib.eachDefaultSystem
    (system:
      let
        pkgs = import nixpkgs { inherit system; };

        organisation = "yourcompany";
        project = "projectname";

        projectEnv = pkgs.callPackage ./hostenv.nix { };
        minimalHostenv = pkgs.lib.evalModules {
          specialArgs = inputs // { inherit inputs pkgs; };
          modules = [
            (inputs.hostenv.modules + /top-level/minimal-env.nix)
            # The project environments.
            { environments = projectEnv.environments; }
            ({ config, ... }: {
              hostenv.organisation = organisation;
              hostenv.project = project;
              hostenv.environmentName = config.defaultEnvironment;
              hostenv.root = ../.;
            })
          ];
        };

        # Given an environment name, creates a hostenv environment.
        # Usually environment names will correspond with git branches or
        # tags, but this is not enforced (mostly because Flakes don't seem
        # to support retrieving git metadata from the current environment,
        # beyond the current commit ref).
        makeHostenv = environmentName: pkgs.lib.evalModules {
          specialArgs = inputs // { inherit inputs pkgs; };
          modules = [
            (inputs.hostenv.modules + /top-level/full-env.nix)
            {
              hostenv.organisation = organisation;
              hostenv.project = project;
              hostenv.environmentName = environmentName;
              hostenv.root = ../.;
              buildReference = self.rev or null;
            }
            # systemd stuff from nixpkgs.
            {
              # From:
              # https://github.com/NixOS/nixpkgs/blob/release-24.11/nixos/modules/config/locale.nix#L87
              # This way services are restarted when tzdata changes.
              systemd.globalEnvironment.TZDIR = "${pkgs.tzdata}/share/zoneinfo";
            }
            ../hostenv.nix
          ];
        };

      in
      {

        packages = with pkgs.lib; mapAttrs
          (environmentName: environment:
            let hostenv = makeHostenv environmentName;
            in hostenv.config.activatePackage
          )
          (filterAttrs (n: v: v.enable) minimalHostenv.config.environments)
        // {
          default =
            let defaultHostenv = makeHostenv minimalHostenv.config.defaultEnvironment;
            in defaultHostenv.config.activatePackage;
        };

        hostenv.environments = minimalHostenv.config.environments;

        apps = minimalHostenv.config.hostenv.apps;

        devShells = minimalHostenv.config.hostenv.devShells;

      });
}
