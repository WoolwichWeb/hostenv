{
  description = "Hostenv provider template";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    hostenv = {
      url = "gitlab:woolwichweb/hostenv";
      inputs.hostenv-platform.follows = "hostenv-platform";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
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

    # Enter each project as a separate input using this template:
    # organisation__project = {
    #   url = "git:some-git-host/repo?dir=.hostenv&ref=main";
    #   # ^ If hostenv's flake.nix is in a different directory, or the root,
    #   #   change 'dir' accordingly.
    #   #   To change which branch contains the specification of a project's
    #   #   environments update 'ref'.

    #   inputs.nixpkgs.follows = "nixpkgs";
    #   inputs.hostenv.follows = "hostenv-platform";
    #   inputs.flake-parts.follows = "flake-parts";
    # };
    # Note: hostenv uses the double-underscore (__) to determine which inputs
    # are hostenv projects and which are just standard inputs, like 'nixpkgs'.

  };

  outputs = inputs@{ flake-parts, hostenv, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" ];

      imports = [ hostenv.flakeModules.provider ];

      provider = {
        hostenvHostname = "hosting.example.com";
        deployPublicKeys = [ "ssh-ed25519 AAAA..." ]; # replace me
        nodeSystems = { default = "x86_64-linux"; };
        nodeFor = { default = "node-a"; production = "node-a"; testing = "node-a"; development = "node-a"; };
        planSource = "eval";
      };
    };
}
