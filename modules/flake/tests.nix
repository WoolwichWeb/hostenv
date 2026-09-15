{ inputs, lib, config, ... }:
{
  perSystem = { system, pkgs, config, ... }:
    lib.mkIf (!(inputs ? hostenv))
      (
        let
          makeHostenv = config.flake.makeHostenv.${system};
          envs = import ../../tests/environments.nix { inherit pkgs makeHostenv; };
          drupalRoot = import ../../tests/integration/drupal/source.nix { inherit pkgs; };
        in
        {
          hostenvProject = {
            inherit makeHostenv;
            modules = [
              ../../tests/integration/drupal/hostenv.nix
              ({ ... }: {
                hostenv = {
                  organisation = "test";
                  project = "testproject";
                  hostenvHostname = "hosting.test";
                  root = drupalRoot;
                };
              })
            ];
            environmentName = "main";
          };

          checks = import ../../tests {
            inherit pkgs envs makeHostenv inputs;
            documentationEnabled = config.documentation.checks.enable;
          };

          packages = lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
            drupal6-vm-test = import ../../tests/integration/drupal6/vm-test.nix {
              inherit pkgs;
              env = envs.drupal6;
              hostenvNixosModule = config.flake.modules.nixos.hostenv-top-level;
            };
          };
        }
      );
}
