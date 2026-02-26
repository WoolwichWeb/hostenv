{
  description = "Hostenv local VM migration demo project";

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

      perSystem = { pkgs, ... }: {
        hostenvProject = {
          modules = [
            ./hostenv.nix
            ({ ... }: {
              hostenv = {
                organisation = "demo";
                project = "drupal";
                hostenvHostname = "demo.hostenv.test";
                root = import ../source.nix { inherit pkgs; };
                backupsRepoHost = "/mnt/hostenv-shared/backups";
              };
            })
          ];
          environmentName = null;
        };
      };
    };
}
