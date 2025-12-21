{ inputs, ... }:
{
  provider = {
    deployPublicKey = "ssh-ed25519 test";
    planSource = "disk";
    planPath = ./../tests/support/provider/plan-empty.json;
    statePath = ./../tests/support/provider/state-empty.json;
    nodesPath = ./../template/provider/nodes;
    # Stub for flake checks only; real providers must set their own secretsPath.
    secretsPath = builtins.toFile "secrets.yaml" "{}\n";
  };

  perSystem = { system, pkgs, ... }:
    let
      makeHostenv = inputs.hostenv-platform.makeHostenv.${system};
      envs = import ./../tests/environments.nix { inherit pkgs makeHostenv; };
    in
    {
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
    };
}
