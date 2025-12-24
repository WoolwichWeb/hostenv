{
  description = "Hostenv provider template (managed)";

  inputs = {
    hostenv.url = "gitlab:woolwichweb/hostenv";
    hostenv-platform = {
      url = "gitlab:woolwichweb/hostenv?dir=platform";
      inputs.nixpkgs.follows = "hostenv/nixpkgs";
      inputs.flake-parts.follows = "hostenv/flake-parts";
    };
    nixpkgs.follows = "hostenv/nixpkgs";
    flake-parts.follows = "hostenv/flake-parts";

    # Hostenv provider service injects client project inputs here.
    {{HOSTENV_PROJECT_INPUTS}}
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
