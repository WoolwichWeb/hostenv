{ pkgs, makeHostenv, inputs }:
let
  lib = pkgs.lib;
  support = import ../support { inherit pkgs lib; };
  asserts = support.asserts;

  system = pkgs.stdenv.hostPlatform.system;
  modules = inputs.import-tree ../../modules;
  moduleList = if builtins.isList modules then modules else [ modules ];
  flake = inputs.flake-parts.lib.mkFlake { inherit inputs; } {
    systems = [ system ];
    imports = [ inputs.devshell.flakeModule ] ++ moduleList;

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

  shell = flake.devShells.${system}.default;
  ok = lib.isDerivation shell;
in
asserts.assertTrue "devshells-eval"
  ok
  "devShells output should evaluate to a derivation"
