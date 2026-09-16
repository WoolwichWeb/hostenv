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
  devshellFixturePackage = pkgs.writeTextFile {
    name = "devshell-fixture-package";
    destination = "/bin/.devshell-fixture";
    text = "";
  };
  flake = inputs.flake-parts.lib.mkFlake { inherit inputs; } {
    systems = [ system ];
    imports = [ inputs.devshell.flakeModule ] ++ moduleList;

    project.enable = true;

    perSystem = { ... }: {
      documentation.enable = false;

      # This check verifies the generated devshell wrappers, not the Hostenv
      # repository's development toolchain. Keep one tiny package so
      # numtide/devshell has a bin directory in which to install its wrapper.
      devshells.default.devshell.packages = lib.mkForce [ devshellFixturePackage ];

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
  defaultApp = flake.apps.${system}.default;
  hostenvApp = flake.apps.${system}.hostenv;
  defaultPackage = flake.packages.${system}.default;
  environmentPackage = flake.packages.${system}.main;
in
assert lib.isDerivation defaultShell;
assert lib.isDerivation environmentShell;
assert defaultApp.program == hostenvApp.program;
assert defaultPackage.outPath == environmentPackage.outPath;
pkgs.runCommand "devshells-eval" { } ''
  test -x ${defaultProfile}/bin/devshell
  test ! -e ${defaultProfile}/bin/hostenv

  test -x ${environmentProfile}/bin/devshell-main
  test -x ${environmentProfile}/bin/hostenv
  test ! -e ${environmentProfile}/bin/hostenv-main

  touch "$out"
''
