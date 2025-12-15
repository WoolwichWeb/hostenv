#
# This file contains boilerplate for Nix. To configure your hostenv, edit
# the `hostenv.nix` file in this directory.
#
{
  description = "Hostenv project";

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
    hostenv = {
      url = "gitlab:woolwichweb/hostenv?dir=modules";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
  };

  outputs = inputs@{ flake-parts, nixpkgs, hostenv, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = inputs.nixpkgs.lib.systems.flakeExposed;

      imports = [ hostenv.lib.cliModule ];

      perSystem = { system, pkgs, config, ... }:
        let
          organisation = "yourorganisation";
          project = "yourproject";
          hostenvHostname = "your.hostenv.hostname"; # e.g. hostenv.example.com
          root = ../.;
          backupsRepoHost = "your.backup.provider"; # Restic format, for example "s3:https://s3.amazonaws.com/"

          baseModules = [
            ./hostenv.nix
            ({ ... }: {
              buildReference = inputs.self.rev or null;
              hostenv = { inherit organisation project hostenvHostname root backupsRepoHost; };
            })
          ];

          makeHostenv = hostenv.makeHostenv.${system};
          defaultHostenv = makeHostenv baseModules null;
          envs = defaultHostenv.config.environments;
        in
        {
          # Wire CLI via reusable module
          hostenvCli = {
            makeHostenv = hostenv.makeHostenv;
            modules = baseModules;
            environmentName = null; # use defaultEnvironment
          };

          packages = pkgs.lib.mapAttrs
            (environmentName: environment:
              let hostenvEval = makeHostenv baseModules environmentName;
              in hostenvEval.config.activatePackage
            )
            (pkgs.lib.filterAttrs (n: v: v.enable) envs)
          // {
            default = defaultHostenv.config.activatePackage;
          };

          apps = defaultHostenv.config.hostenv.apps;

          devShells = defaultHostenv.config.hostenv.devShells;
        };
    };
}
