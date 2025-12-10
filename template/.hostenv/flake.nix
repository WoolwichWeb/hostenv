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

        # Uses hostenv from inputs to build an environment's config.
        makeHostenv = environmentName:
          let

            # Added to the config if an environmentName is set,
            # if it's not this tells hostenv to use the default.
            # As for why it does this, it's for bootstrapping purposes.
            # So the CLI and other tooling can find which environments
            # are available, without having to pick one.
            addendum =
              if environmentName == null
              then { }
              else { inherit environmentName; };

            # Put together the configuration hostenv needs to build an
            # environment.
            envConfig =
              {
                inherit organisation project;
                root = ../.;
                buildReference = self.rev or null;
                modules = [
                  ({ ... }: { hostenv.hostenvHostname = hostenvHostname; })
                  ./hostenv.nix
                ];
              } // addendum;

          in
          # Call the function that builds the environment.
            # Note the use of `${system}`, this is because Flake outputs are
            # often separated by CPU / platform architecture.
          hostenv.makeHostenv.${system} envConfig;

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
