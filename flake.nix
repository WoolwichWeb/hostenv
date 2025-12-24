{
  description = "Hostenv: the PaaS you control";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    # phps = {
    #   url = "github:fossar/nix-phps";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };
    search = {
      url = "github:NuschtOS/search";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hostenv-platform = {
      url = ./platform;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
      # inputs.phps.follows = "phps";
    };
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = inputs.nixpkgs.lib.systems.flakeExposed;
      imports = [
        ./provider/flake-module.nix
        inputs.hostenv-platform.flakeModules.exports
        inputs.hostenv-platform.flakeModules.environmentRegistry
        inputs.hostenv-platform.flakeModules.cli
        inputs.hostenv-platform.flakeModules.hostenvProviderService
        ./flake-parts/docs.nix
        ./flake-parts/tests.nix
        ./flake-parts/devshells.nix
        ./flake-parts/templates.nix
      ];
    };
}
