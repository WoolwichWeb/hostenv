{ inputs, ... }:
{
  systems = inputs.nixpkgs.lib.systems.flakeExposed;
  imports = [
    ../provider/flake-module.nix
    inputs.hostenv-platform.flakeModules.hostenvOutputs
    inputs.hostenv-platform.flakeModules.cli
  ];
  provider = {
    planSource = "disk";
    planPath = ./../tests/support/provider/plan-empty.json;
    statePath = ./../tests/support/provider/state-empty.json;
    nodesPath = ./../template/provider/nodes;
    secretsPath = ./../template/provider/secrets/secrets.yaml.example;
  };

  perSystem = { system, pkgs, self', ... }:
    let
      makeHostenv = inputs.hostenv-platform.makeHostenv.${system};
      docSearch = pkgs.writeTextDir "index.html" "<html><body>docs disabled</body></html>";

      serveDocs = pkgs.writeShellApplication {
        name = "serve-docs";
        runtimeInputs = [ pkgs.http-server ];
        runtimeEnv.server_flags = [ "--port=0" "-c-1" "-o" ];
        text = ''
          http-server ${docSearch} "''${server_flags[@]}"
        '';
      };

      envs = import ./../tests/environments.nix { inherit pkgs makeHostenv; };
    in
    {
      apps.default = {
        type = "app";
        program = "${serveDocs}/bin/serve-docs";
        meta.description = "Serve hostenv documentation site";
      };
      packages = {
        inherit docSearch;
        default = self'.packages.hostenv-provider;
      };
      hostenvProject = {
        makeHostenv = makeHostenv;
        modules = [
          ./../tests/integration/drupal/hostenv.nix
          ({ ... }: {
            hostenv = {
              organisation = "test";
              project = "testproject";
              hostenvHostname = "hosting.test";
              root = ./../tests/integration/drupal;
            };
          })
        ];
        environmentName = "main";
      };

      checks = import ./../tests { inherit pkgs envs makeHostenv inputs; };

      devShells.default = pkgs.mkShell {
        buildInputs = let hq = pkgs.haskellPackages; in [
          self'.packages.hostenv-provider
          self'.packages.hostenv-provider-plan
          pkgs.sops
          pkgs.age
          pkgs.jq
          pkgs.bind
          pkgs.deploy-rs
          pkgs.git
          (pkgs.ghc.withPackages (x: [ x.turtle x.aeson x.text-conversions ]))
          hq.haskell-language-server
          hq.aeson
          hq.turtle
          pkgs.kittysay
        ];
        shellHook = ''
          export HOSTENV_PROVIDER_OUT=''${HOSTENV_PROVIDER_OUT:-generated}
        '';
      };
    };

  flake = {
    flakeModules.provider = ./../provider/flake-module.nix;
    templates.default = {
      path = ./../template/project;
      description = "Hostenv project template";
      welcomeText = ''
        ## Thank you for using Hostenv

        What's next:

         - Configure your project in `.hostenv/hostenv.nix`.
         - Check the project README.md for more instructions:
           https://gitlab.com/woolwichweb/hostenv/-/blob/main/README.md?ref_type=heads
      '';
    };
    templates.provider = {
      path = ./../template/provider;
      description = "Hostenv provider template";
      welcomeText = ''
        ## Hostenv provider flake

        - Set `provider.*` options in flake.nix (hostname, deploy key, nodes).
        - Add node configs under nodes/<name>/configuration.nix.
        - Generate plan/state: nix run .#hostenv-provider-plan
        - Deploy using generated/flake.nix (e.g. via deploy-rs).
      '';
    };
  };
}
