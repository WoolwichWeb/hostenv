{ inputs, ... }:
{
  provider = {
    deployPublicKey = "ssh-ed25519 test";
    planSource = "disk";
    planPath = ./../tests/support/provider/plan-empty.json;
    statePath = ./../tests/support/provider/state-empty.json;
  };

  perSystem = { system, pkgs, ... }:
    let
      makeHostenv = inputs.hostenv-platform.makeHostenv.${system};
      envs = import ./../tests/environments.nix { inherit pkgs makeHostenv; };
    in
    {
      hostenvProject = {
        inherit makeHostenv;
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
