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
      (wrapperNameEvaluation "safe.wrapper_+-1").success
      "The wrapper-name fixture must accept documented standalone program names";

  hostenv-cli-wrapper-rejects-relative-path =
    asserts.assertTrue "hostenv-cli-wrapper-rejects-relative-path"
      (!(wrapperNameEvaluation "./hostenv").success)
      "A standalone wrapper must be a program name, not a relative path";

  hostenv-cli-wrapper-rejects-nested-path =
    asserts.assertTrue "hostenv-cli-wrapper-rejects-nested-path"
      (!(wrapperNameEvaluation "bin/tool").success)
      "A standalone wrapper must not contain a path separator";

  hostenv-cli-wrapper-rejects-dot-directory =
    asserts.assertTrue "hostenv-cli-wrapper-rejects-dot-directory"
      (!(wrapperNameEvaluation ".").success)
      "A standalone wrapper must not use the current-directory name";

  hostenv-cli-wrapper-rejects-parent-directory =
    asserts.assertTrue "hostenv-cli-wrapper-rejects-parent-directory"
      (!(wrapperNameEvaluation "..").success)
      "A standalone wrapper must not use the parent-directory name";

  hostenv-cli-wrapper-rejects-whitespace =
    asserts.assertTrue "hostenv-cli-wrapper-rejects-whitespace"
      (!(wrapperNameEvaluation "hostenv tool").success)
      "A standalone wrapper must use conventional program-name characters";

  hostenv-cli-wrapper-reserves-main-program =
    asserts.assertTrue "hostenv-cli-wrapper-reserves-main-program"
      (!(wrapperNameEvaluation "hostenv").success)
      "A standalone wrapper named 'hostenv' must fail evaluation instead of silently replacing the main CLI";

  hostenv-cli-wrapper-reserves-completion-program =
    asserts.assertTrue "hostenv-cli-wrapper-reserves-completion-program"
      (!(wrapperNameEvaluation "_hostenv_complete").success)
      "A standalone wrapper named '_hostenv_complete' must fail evaluation instead of silently replacing Pog's completion program";
}
