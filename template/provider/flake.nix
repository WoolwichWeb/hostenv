{
  description = "Hostenv provider template";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    hostenv = {
      url = "gitlab:woolwichweb/hostenv";
      inputs.hostenv-platform.follows = "hostenv-platform";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hostenv-platform = {
      url = "gitlab:woolwichweb/hostenv?dir=platform";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
    };
    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ flake-parts, hostenv, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" ];

      imports = [ hostenv.flakeModules.provider ];

      provider = {
        hostenvHostname = "hosting.example.com";
        deployPublicKey = "ssh-ed25519 AAAA..."; # replace me
        nodeSystems = { default = "x86_64-linux"; };
        nodeFor = { default = "node-a"; production = "node-a"; testing = "node-a"; development = "node-a"; };
        planSource = "eval";
      };
    };
}
