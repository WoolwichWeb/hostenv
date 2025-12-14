{ pkgs, makeHostenv }:
let
  lib = pkgs.lib;
  asserts = (import ./support { inherit pkgs lib; }).asserts;

  # Attempt to force the activation script for a config with two production envs.
  # This should fail the "only one production" invariant.
  evalResult = builtins.tryEval (
    (makeHostenv [
      ({ ... }: {
        hostenv = {
          organisation = "org";
          project = "proj";
          hostenvHostname = "host.test";
          root = ./.;
        };
        environments = {
          prodA = { enable = true; type = "production"; };
          prodB = { enable = true; type = "production"; };
        };
      })
    ] "prodA").config.activate
  );
in
{
  hostenv-production-singleton =
    asserts.assertTrue "hostenv-production-singleton"
      (!evalResult.success)
      "Evaluation must fail when more than one production environment is enabled";
}
