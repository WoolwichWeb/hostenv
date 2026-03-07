{
  description = "Hostenv local VM provider-service control-plane project";

  inputs = {
    hostenv.url = "HOSTENV_INPUT_URL_PLACEHOLDER";
    nixpkgs.follows = "hostenv/nixpkgs";
    flake-parts.follows = "hostenv/flake-parts";
    phps.follows = "hostenv/phps";
  };

  outputs = inputs@{ flake-parts, hostenv, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [ hostenv.flakeModules.project ];

      perSystem = { ... }: {
        hostenvProject = {
          modules = [
            ./hostenv.nix
            ({ ... }: {
              hostenv = {
                organisation = "demo";
                project = "provider-service";
                hostenvHostname = "provider.demo.hostenv.test";
                root = ../.;
              };
            })
          ];
          environmentName = null;
        };
      };
    };
}
