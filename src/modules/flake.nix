{
  description = "Hostenv client";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    pog = {
      url = "github:jpetrucciani/pog";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    phps = {
      url = "github:fossar/nix-phps?ref=update_flake_lock_action";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        utils.follows = "flake-utils";
      };
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
      # FYI: Usually environment names will correspond with git branches or 
      # tags, but this is not enforced (mostly because Flakes don't seem 
      # to support retrieving git metadata from the current environment, 
      # beyond the current commit ref).
      makeHostenv = modules: environmentName: pkgs.lib.evalModules {
        specialArgs = inputs // { inherit inputs pkgs; };
        modules = [
          (self.modules + /top-level/full-env.nix)
          ({ config, ... }: {
            # Added to the config if an environmentName is set,
            # if it's not this tells hostenv to use the default.
            # As for why it does this, it's for bootstrapping purposes.
            # So the CLI and other tooling can find which environments
            # are available, without having to pick one.
            hostenv.environmentName =
              if environmentName == null
              then config.defaultEnvironment
              else environmentName;
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
