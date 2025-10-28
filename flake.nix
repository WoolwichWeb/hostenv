{
  description = "Hostenv: the PaaS you control";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    search = {
      url = "github:NuschtOS/search";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
    hostenv-internal = {
      url = ./src/modules;
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, hostenv-internal, search, ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = (import nixpkgs) {
            inherit system;
          };
          makeHostenv = hostenv-internal.makeHostenv.${system};

          docSearch = search.packages.${system}.mkMultiSearch {
            title = "hostenv options search";
            scopes = [
              {
                modules = [
                  ./src/modules/top-level/full-env.nix
                  { _module.args = { inherit pkgs; }; }
                  ({ config, ... }: {
                    hostenv = {
                      organisation = "org";
                      project = "example";
                      root = ./.;
                      environmentName = "main";
                    };
                  })
                  {
                    environments.main = {
                      enable = true;
                      type = "production";
                      virtualHosts = {
                        "www.example.com" = { };
                      };
                    };
                  }
                ];
                urlPrefix = "https://gitlab.com/woolwichweb/hostenv/-/blob/main/";
              }
            ];
          };

          serveDocs = pkgs.writeShellApplication {
            name = "serve-docs";
            runtimeInputs = [ pkgs.http-server ];
            runtimeEnv.server_flags = [
              # Search for available port
              "--port=0"

              # Disable browser cache
              "-c-1"

              # Open using xdg-open
              "-o"
            ];
            text = ''
              http-server ${docSearch} "''${server_flags[@]}"
            '';
          };

          tests = {
            # Test Drupal environment (production)
            # Uses the main environment from tests/drupal/hostenv.nix
            drupalProduction = makeHostenv {
              organisation = "test-drupal";
              project = "test-drupal-project";
              buildReference = self.rev or null;
              environmentName = "main";
              root = ./tests/drupal;
              modules = [ ./tests/drupal/hostenv.nix ];
            };

            # Test Drupal environment (development)
            # Used to verify that debug headers work in non-production environments.
            # Note: hostenv will automatically generate a consistent URL for this environment
            drupalDev = makeHostenv {
              organisation = "test-drupal";
              project = "test-drupal-project";
              buildReference = self.rev or null;
              environmentName = "dev";
              root = ./tests/drupal;
              modules = [
                ./tests/drupal/hostenv.nix
                {
                  environments.dev = {
                    enable = true;
                    type = "development";
                  };
                }
              ];
            };
          };

        in
        {
          inherit tests;
          apps.default = (flake-utils.lib.mkApp { drv = serveDocs; }) // {
            meta.description = "Serve hostenv documentation site";
          };
          packages = {
            # Searchable documentation package.
            inherit docSearch;
            testDrupalProduction = tests.drupalProduction.config.activatePackage;
            testDrupalDev = tests.drupalDev.config.activatePackage;
          };

          # Import test suite from external file
          checks = import ./tests/drupal/tests.nix {
            inherit pkgs tests;
          };
        }) //
    {
      templates = {
        default = {
          path = ./template;
          description = "Hostenv project template";
          welcomeText = ''
            ## Thank you for using Hostenv

            What's next:

             - Configure your project in `.hostenv/hostenv.nix`.
             - Check the project README.md for more instructions:
               https://gitlab.com/woolwichweb/hostenv/-/blob/main/README.md?ref_type=heads
          '';
        };
      };
    };
}
