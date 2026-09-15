{ pkgs, inputs }:
let
  lib = pkgs.lib;
  asserts = import ../support/assert.nix { inherit pkgs lib; };

  expectedPlan = pkgs.writeText "provider-tooling-plan.json" "{}";
  expectedState = pkgs.writeText "provider-tooling-state.json" "{}";
  expectedFlake = pkgs.writeText "provider-tooling-flake.nix" "{}";

  toolingModule = import ../../modules/entrypoints/provider/tooling.nix {
    inherit inputs lib;
    config = {
      provider = {
        enable = true;
        secretsFile = "secrets/secrets.yaml";
      };
      flake.lib = {
        provider.plan =
          args:
          assert
            args.sopsSecretKeys == {
              "provider-test" = [ "backups_secret" ];
            };
          {
            plan = expectedPlan;
            state = expectedState;
            flake = expectedFlake;
          };
        hostenvInputs.requireInput = _: {
          outPath = ../..;
        };
        hostenv.readYaml = _pkgs: _path: {
          "provider-test" = {
            backups_secret = "";
          };
        };
      };
    };
  };

  perSystem = toolingModule.config.content.perSystem {
    system = pkgs.stdenv.hostPlatform.system;
    inherit pkgs;
    config.provider.haskellDevPackages = [ ];
  };
in
asserts.assertTrue "provider-tooling-plan-paths-eval" (
  perSystem.provider.planPaths.plan == expectedPlan
) "provider tooling should build plan paths using the outer flake config"
