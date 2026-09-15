{
  pkgs,
  makeHostenv,
  inputs,
}:
let
  lib = pkgs.lib;
  system = pkgs.stdenv.hostPlatform.system;
  modules = inputs.import-tree ../../modules;
  moduleList = if builtins.isList modules then modules else [ modules ];
  flake = inputs.flake-parts.lib.mkFlake { inherit inputs; } {
    systems = [ system ];
    imports = [ inputs.devshell.flakeModule ] ++ moduleList;

    project.enable = true;

    perSystem = { ... }: {
      # This check verifies the generated devshell wrappers, not the Hostenv
      # repository's development toolchain.
      devshells.default.devshell.packages = lib.mkForce [ ];

      hostenvProject = {
        makeHostenv = makeHostenv;
        modules = [
          ({ ... }: {
            hostenv = {
              organisation = lib.mkForce "acme";
              project = lib.mkForce "demo";
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

  defaultShell = flake.devShells.${system}.default;
  environmentShell = flake.devShells.${system}.main;
  defaultProfile = defaultShell.config.devshell.package;
  environmentProfile = environmentShell.config.devshell.package;
in
assert lib.isDerivation defaultShell;
assert lib.isDerivation environmentShell;
pkgs.runCommand "devshells-eval" { } ''
  test -x ${defaultProfile}/bin/devshell
  test ! -e ${defaultProfile}/bin/hostenv

  test -x ${environmentProfile}/bin/devshell-main
  test -x ${environmentProfile}/bin/hostenv
  test ! -e ${environmentProfile}/bin/hostenv-main

  touch "$out"
''
