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
      # Usage:
      #   makeHostenv {
      #     organisation = "acme";
      #     project = "example";
      #     hostenvHostname = "hosting.example";
      #     root = ./.;                 # project root
      #     environmentName = "main";  # optional, defaults to config.defaultEnvironment
      #     modules = [ ./hostenv.nix ]; # required project modules
      #   }
      #
      # Rationale: callers nearly always need to set these knobs, so we take a
      # single attrset instead of the previous curried form (modules -> envName).
      makeHostenv = {
        organisation,
        project,
        hostenvHostname,
        root,
        environmentName ? null,
        modules ? [ ],
        buildReference ? null,
      }: pkgs.lib.evalModules {
        specialArgs = inputs // { inherit inputs pkgs; };
        modules = [
          (self.modules + /top-level/full-env.nix)
          ({ config, ... }: {
            hostenv = {
              inherit organisation project hostenvHostname root;
              environmentName =
                if environmentName == null
                then config.defaultEnvironment
                else environmentName;
            };
            inherit buildReference;
          })
          # systemd plumbing from nixpkgs so services restart when tzdata changes.
          {
            systemd.globalEnvironment.TZDIR = "${pkgs.tzdata}/share/zoneinfo";
          }
        ] ++ modules;
      };
    });

}
