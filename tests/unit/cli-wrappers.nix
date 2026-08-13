{ pkgs, makeHostenv }:
let
  lib = pkgs.lib;
  asserts = (import ../support { inherit pkgs lib; }).asserts;

  # Force the CLI package far enough to evaluate wrapper-name validation.
  wrapperNameEvaluation =
    executable:
    builtins.tryEval (
      (makeHostenv [
        ({ ... }: {
          hostenv = {
            organisation = "acme";
            project = "wrapper-name-test";
            hostenvHostname = "hosting.test";
            root = ./.;
          };
          environments.main = {
            enable = true;
            type = "production";
          };
          hostenv.cli.commands.collision = {
            inherit executable;
            script = "true";
          };
        })
      ] "main").config.hostenv.cliPackage.outPath
    );
in
{
  hostenv-cli-wrapper-allows-unreserved-name =
    asserts.assertTrue "hostenv-cli-wrapper-allows-unreserved-name"
      (wrapperNameEvaluation "safe-wrapper").success
      "The reserved-name fixture must accept an ordinary standalone wrapper name";

  hostenv-cli-wrapper-reserves-main-program =
    asserts.assertTrue "hostenv-cli-wrapper-reserves-main-program"
      (!(wrapperNameEvaluation "hostenv").success)
      "A standalone wrapper named 'hostenv' must fail evaluation instead of silently replacing the main CLI";

  hostenv-cli-wrapper-reserves-completion-program =
    asserts.assertTrue "hostenv-cli-wrapper-reserves-completion-program"
      (!(wrapperNameEvaluation "_hostenv_complete").success)
      "A standalone wrapper named '_hostenv_complete' must fail evaluation instead of silently replacing Pog's completion program";
}
