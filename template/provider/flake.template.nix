{
  description = "Hostenv provider template (managed)";

  inputs = {
    hostenv = {
      url = "gitlab:woolwichweb/hostenv";
    };
    nixpkgs.follows = "hostenv/nixpkgs";
    flake-parts.follows = "hostenv/flake-parts";
    phps.follows = "hostenv/phps";
    deploy-rs.follows = "hostenv/deploy-rs";
    sops-nix.follows = "hostenv/sops-nix";

    # Hostenv provider service injects client project inputs here.
    {{HOSTENV_PROJECT_INPUTS}}
  };

  outputs = inputs@{ flake-parts, hostenv, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" ];

      imports = [
        hostenv.flakeModules.provider
      ];

      provider = {
        hostenvHostname = "hosting.example.com";
        deployPublicKeys = [ "ssh-ed25519 AAAA..." ]; # replace me
        nodeSystems.node-a = "x86_64-linux";
        nodeFor.default = "node-a";
      };
    };
}
