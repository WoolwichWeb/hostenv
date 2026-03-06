{
  description = "Hostenv provider template (managed)";

  inputs = {
    hostenv = {
      url = "gitlab:woolwichweb/hostenv";
    };
    nixpkgs.follows = "hostenv/nixpkgs";
    flake-parts.follows = "hostenv/flake-parts";
    phps.follows = "hostenv/phps";

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
        # deploy = {
        #   enable = true;
        #   providerApiBaseUrl = "https://hosting.example.com";
        #   nodeAuthTokenFile = "/run/secrets/hostenv/provider_node_token";
        # };
        # Note: cache client configuration is automatic when provider-service
        # is enabled (provider.service points to an environment with
        # services.hostenv-provider.enable = true).
      };
    };
}
