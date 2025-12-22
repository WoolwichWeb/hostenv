{ pkgs, makeHostenv, inputs }:
let
  lib = pkgs.lib;
  support = import ../support { inherit pkgs lib; };
  asserts = support.asserts;

  system = pkgs.stdenv.hostPlatform.system;
  flake = inputs.flake-parts.lib.mkFlake { inherit inputs; } {
    systems = [ system ];
    imports = [ ../../platform/flake-modules/environment-registry.nix ];

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

  outputs = flake.hostenv.${system};
  ok = outputs ? environments && outputs ? defaultEnvironment;
in
asserts.assertTrue "hostenv-outputs-eval"
  ok
  "hostenv outputs should include environments/defaultEnvironment for each system"
