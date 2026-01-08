{
  description = "Hostenv provider template (managed)";

  inputs = {
    hostenv = {
      url = "gitlab:woolwichweb/hostenv";
    };
    nixpkgs.follows = "hostenv/nixpkgs";
    flake-parts.follows = "hostenv/flake-parts";
    import-tree.follows = "hostenv/import-tree";
    phps.follows = "hostenv/phps";

    # Hostenv provider service injects client project inputs here.
    {{HOSTENV_PROJECT_INPUTS}}
  };

  outputs = inputs@{ flake-parts, hostenv, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" ];

      imports = [
        (inputs.import-tree (hostenv + "/modules"))
        hostenv.flakeModules.provider
      ];

      provider = {
        hostenvHostname = "hosting.example.com";
        deployPublicKeys = [ "ssh-ed25519 AAAA..." ]; # replace me
        nodeSystems = { default = "x86_64-linux"; };
        nodeFor = { default = "node-a"; production = "node-a"; testing = "node-a"; development = "node-a"; };
        planSource = "eval";
      };
    };
}
