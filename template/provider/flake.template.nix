{
  description = "Hostenv provider template (managed)";

  inputs = {
    hostenv = {
      url = "gitlab:woolwichweb/hostenv";
    };
    nixpkgs.follows = "hostenv/nixpkgs";
    flake-parts.follows = "hostenv/flake-parts";
    phps.follows = "hostenv/phps";
    comin.follows = "hostenv/comin";

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
        nodeSystems = { default = "x86_64-linux"; };
        nodeFor = { default = "node-a"; production = "node-a"; testing = "node-a"; development = "node-a"; };
        planSource = "eval";
        # comin = {
        #   enable = true;
        #   remoteUrl = "https://gitlab.com/acme/provider.git";
        #   providerApiBaseUrl = "https://hosting.example.com";
        #   nodeAuthTokenFile = "/run/secrets/hostenv-provider/comin_node_token";
        # };
      };
    };
}
