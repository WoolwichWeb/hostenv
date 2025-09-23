{
  description = "Hostenv upstream";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    pog = {
      url = "github:jpetrucciani/pog";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, pog, ... } @ inputs: {
    modules = ./.;
  } // flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ pog.overlays.${system}.default ];
      };
    in
    {
      inherit inputs;

      # Create a hostenv environment. 
      #
      # Usually environment names will correspond with git branches or 
      # tags, but this is not enforced (mostly because Flakes don't seem 
      # to support retrieving git metadata from the current environment, 
      # beyond the current commit ref).
      makeHostenv =
        { organisation
        , project
        , root
        , modules
        , buildReference ? null
        , environmentName ? null
        }:

        pkgs.lib.evalModules {
          specialArgs = inputs // { inherit inputs pkgs; };
          modules = [
            (self.modules + /top-level/full-env.nix)
            ({ config, ... }: {
              inherit buildReference;
              hostenv = {
                inherit organisation project root;
                environmentName =
                  if environmentName == null
                  then config.defaultEnvironment
                  else environmentName;
              };
            })
            # systemd stuff from nixpkgs.
            {
              # From:
              # https://github.com/NixOS/nixpkgs/blob/release-24.11/nixos/modules/config/locale.nix#L87
              # This way services are restarted when tzdata changes.
              systemd.globalEnvironment.TZDIR = "${pkgs.tzdata}/share/zoneinfo";
            }
          ] ++ modules;
        };
    });

}
