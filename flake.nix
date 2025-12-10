{
  description = "Hostenv: the PaaS you control";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-parts.url = "github:hercules-ci/flake-parts";
    phps = {
      url = "github:fossar/nix-phps?ref=update_flake_lock_action";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        utils.follows = "flake-utils";
      };
    };
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
      inputs.phps.follows = "phps";
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, search, hostenv-internal, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        ./src/provider/flake-module.nix
        # Minimal provider defaults so hostenv's own flake outputs evaluate; real providers override.
        ({ config.provider = {
             hostenvHostname = "example.invalid";
             deployPublicKey = "";
             letsEncrypt = { adminEmail = "admin@example.invalid"; acceptTerms = true; };
             nodeFor = { default = "devnode"; };
             nodeSystems = { devnode = "x86_64-linux"; };
             hostenvProjectDir = ".hostenv";
           }; })
      ];

      perSystem = { system, pkgs, self', ... }:
        let
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
              "--port=0"
              "-c-1"
              "-o"
            ];
            text = ''
              http-server ${docSearch} "''${server_flags[@]}"
            '';
          };

          envs = import ./tests/environments.nix { inherit pkgs makeHostenv; };
        in
        {
          apps.default = {
            type = "app";
            program = "${serveDocs}/bin/serve-docs";
            meta.description = "Serve hostenv documentation site";
          };

          packages = {
            inherit docSearch;
            default = docSearch;
          };

          checks = import ./tests { inherit pkgs envs makeHostenv; };

          devShells.default = pkgs.mkShell {
            buildInputs = [
              self'.packages.hostenv-provider
              self'.packages.hostenv-provider-plan
              pkgs.sops
              pkgs.age
              pkgs.jq
              pkgs.bind
              pkgs.deploy-rs
              pkgs.git
              pkgs.haskellPackages.haskell-language-server
            ];
            shellHook = ''
              export HOSTENV_PROVIDER_OUT=''${HOSTENV_PROVIDER_OUT:-generated}
            '';
          };
        };

      flake = {
        # Expose provider module under lib to avoid nonstandard top-level outputs.
        lib = {
          hostenv = {
            providerModule = ./src/provider/flake-module.nix;
          };
        };
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
    };
}
