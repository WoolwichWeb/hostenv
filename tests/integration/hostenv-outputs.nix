{ pkgs, makeHostenv, inputs }:
let
  lib = pkgs.lib;
  support = import ../support { inherit pkgs lib; };
  asserts = support.asserts;

  system = pkgs.stdenv.hostPlatform.system;
  flake = inputs.flake-parts.lib.mkFlake { inherit inputs; } {
    systems = [ system ];
    imports = [ (inputs.import-tree ../../modules) ];

    project.enable = true;

    perSystem = { ... }: {
      hostenvProject = {
        makeHostenv = makeHostenv;
        modules = [
          ({ ... }: {
            hostenv = {
              organisation = "acme";
              project = "demo";
              hostenvHostname = "hosting.test";
              root = ./.;
            };
            environments.main = {
              enable = true;
              type = "production";
            };
          })
        ];
        environmentName = "main";
      };
    };
  };

  outputs = flake.lib.hostenv.${system};
  ok = outputs ? environments && outputs ? defaultEnvironment;
in
asserts.assertTrue "hostenv-outputs-eval"
  ok
  "lib.hostenv outputs should include environments/defaultEnvironment for each system"
