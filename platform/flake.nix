{
  description = "Hostenv client";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-parts.url = "github:hercules-ci/flake-parts";
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

  outputs = inputs@{ flake-parts, nixpkgs, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;

      imports = [
        ./flake-modules/make-hostenv.nix
        ./flake-modules/cli.nix
        ./flake-modules/exports.nix
      ];
    };
}
