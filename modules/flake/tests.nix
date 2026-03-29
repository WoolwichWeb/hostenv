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
        packages = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
          provider-deploy-multivm-test = import ../../tests/integration/provider-deploy-multivm.nix { inherit pkgs inputs makeHostenv; };
          provider-nixos-runtime-test = import ../../tests/integration/provider-nixos-runtime.nix { inherit pkgs inputs; };
          provider-service-runtime-test = import ../../tests/integration/provider-service-runtime.nix { inherit pkgs inputs; };
          provider-secrets-runtime-test = import ../../tests/integration/provider-secrets-runtime.nix { inherit pkgs inputs; };
        };
      });
}
