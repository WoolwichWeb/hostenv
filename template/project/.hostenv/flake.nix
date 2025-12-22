#
# This file contains boilerplate for Nix. To configure your hostenv, edit
# the `hostenv.nix` file in this directory.
#
{
  description = "Hostenv project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-parts.url = "github:hercules-ci/flake-parts";
    phps = {
      url = "github:fossar/nix-phps";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        utils.follows = "flake-utils";
      };
    };
    hostenv = {
      url = "gitlab:woolwichweb/hostenv?dir=platform";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        flake-parts.follows = "flake-parts";
      };
    };
  };

  outputs = inputs@{ flake-parts, nixpkgs, hostenv, ... }:
    let
      systems = nixpkgs.lib.systems.flakeExposed;

      organisation = "yourorganisation";
      project = "yourproject";
      hostenvHostname = "your.hostenv.hostname"; # e.g. hostenv.example.com
      root = ../.;
      backupsRepoHost = "your.backup.provider"; # Restic format, for example "s3:https://s3.amazonaws.com/"

    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      inherit systems;
      imports = [
        hostenv.flakeModules.environmentRegistry
        hostenv.flakeModules.cli
        hostenv.flakeModules.projectOutputs
      ];

      perSystem = { system, ... }: {
        hostenvProject = {
          modules = [
            ./hostenv.nix
            ({ ... }: {
              buildReference = inputs.self.rev or null;
              hostenv = {
                inherit organisation project hostenvHostname root backupsRepoHost;
              };
            })
          ];
          environmentName = null;
        };
      };

    };
}
