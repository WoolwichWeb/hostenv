{ inputs, ... }:
{
  systems = inputs.nixpkgs.lib.systems.flakeExposed;
  imports = [
    ./../provider/flake-module.nix
  ];

  perSystem = { system, pkgs, self', ... }:
    let
      makeHostenv = inputs.hostenv-internal.makeHostenv.${system};
      docSearch = inputs.search.packages.${system}.mkMultiSearch {
        title = "hostenv options search";
        scopes = [
          {
            modules = [
              ./../modules/core/full-env.nix
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
                  virtualHosts = { "www.example.com" = { }; };
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
        runtimeEnv.server_flags = [ "--port=0" "-c-1" "-o" ];
        text = ''
          http-server ${docSearch} "''${server_flags[@]}"
        '';
      };

      envs = import ./../../tests/environments.nix { inherit pkgs makeHostenv; };
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

      checks = import ./../../tests { inherit pkgs envs makeHostenv; };

      devShells.default = pkgs.mkShell {
        buildInputs = [
          self'.packages.hostenv-provider
          self'.packages.hostenv-provider-plan
          pkgs.sops pkgs.age pkgs.jq pkgs.bind pkgs.deploy-rs pkgs.git
          pkgs.haskellPackages.haskell-language-server
        ];
        shellHook = ''
          export HOSTENV_PROVIDER_OUT=''${HOSTENV_PROVIDER_OUT:-generated}
        '';
      };
    };

  flake = {
    lib.hostenv.providerModule = ./../provider/flake-module.nix;
    templates.default = {
      path = ./../../template;
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
}
