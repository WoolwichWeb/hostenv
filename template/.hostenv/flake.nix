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
      url = "gitlab:woolwichweb/hostenv?dir=src/modules";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
  };

  outputs = { self, nixpkgs, flake-utils, hostenv, ... } @ inputs: flake-utils.lib.eachDefaultSystem
    (system:
      let
        pkgs = import nixpkgs { inherit system; };

        organisation = "yourorganisation";
        project = "yourproject";
        hostenvHostname = "your.hostenv.hostname"; # e.g. hostenv.example.com
        root = ../.;
        backupsRepoHost = "your.backup.provider"; # Restic format, for example "s3:https://s3.amazonaws.com/"

        # Uses hostenv from inputs to build an environment's config.
        makeHostenv =
          let
            # Put together the configuration hostenv needs to build an
            # environment.
            modules = [
              ./hostenv.nix
              ({ ... }: {
                buildReference = self.rev or null;
                hostenv = { inherit organisation project hostenvHostname root backupsRepoHost; };
              })
            ];
          in
          # Call the function that builds the environment.
            # Note the use of `${system}`, this is because Flake outputs are
            # separated by CPU / platform architecture.
          hostenv.makeHostenv.${system} modules;

        # This default hostenv sans environmentName is for bootstrapping
        # purposes.
        defaultHostenv = makeHostenv null;

        inherit (pkgs.lib) mapAttrs filterAttrs;

      in
      {
        # Helpful for debugging.
        inherit makeHostenv;

        packages = mapAttrs
          (environmentName: environment:
            let hostenv = makeHostenv environmentName;
            in hostenv.config.activatePackage
          )
          (filterAttrs (n: v: v.enable) defaultHostenv.config.environments)
        // {
          default = defaultHostenv.config.activatePackage;
        };

        hostenv = defaultHostenv.config;

        apps = defaultHostenv.config.hostenv.apps;

        devShells = defaultHostenv.config.hostenv.devShells;

      });
}
