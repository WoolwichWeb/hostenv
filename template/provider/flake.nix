{
  description = "Hostenv provider template";

  inputs = {
    hostenv.url = "gitlab:woolwichweb/hostenv";
    nixpkgs.follows = "hostenv/nixpkgs";
    flake-parts.follows = "hostenv/flake-parts";
  };

  outputs = inputs@{ flake-parts, hostenv, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" ];

      imports = [ hostenv.lib.hostenv.providerModule ];

      provider = {
        hostenvHostname = "hosting.example.com";
        deployPublicKey = "ssh-ed25519 AAAA..."; # replace me
        nodeSystems = { default = "x86_64-linux"; };
        nodeFor = { default = "node-a"; production = "node-a"; testing = "node-a"; development = "node-a"; };
        nodesPath = ./nodes;
        secretsPath = ./secrets/secrets.yaml;
        statePath = ./generated/state.json;
        planPath = ./generated/plan.json;
        planSource = "eval";
      };
    };
}
