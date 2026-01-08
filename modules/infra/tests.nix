{ inputs, lib, config, ... }:
{
  perSystem = { system, pkgs, ... }:
    lib.mkIf (!(inputs ? hostenv))
      (let
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

        checks = import ../../tests { inherit pkgs envs makeHostenv inputs; };
      });
}
