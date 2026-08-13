{ pkgs, makeHostenv }:
let
  lib = pkgs.lib;
  asserts = (import ../support { inherit pkgs lib; }).asserts;

  # Force the CLI package far enough to run Pog command-name validation.
  commandNameEvaluation =
    commandDefinitions:
    builtins.tryEval (
      (makeHostenv [
        ({ ... }: {
          hostenv = {
            organisation = "acme";
            project = "command-name-test";
            hostenvHostname = "hosting.test";
            root = ./.;
          };
          environments.main = {
            enable = true;
            type = "production";
          };
          hostenv.cli.commands = commandDefinitions;
        })
      ] "main").config.hostenv.cliPackage.outPath
    );
in
{
  hostenv-cli-command-names-allow-distinct-paths =
    asserts.assertTrue "hostenv-cli-command-names-allow-distinct-paths"
      (commandNameEvaluation {
        some-command.script = "true";
        some_other_command.script = "true";
      }).success
      "Commands that produce different Pog function names must remain valid";

  hostenv-cli-command-names-reject-hyphen-underscore-collision =
    asserts.assertTrue "hostenv-cli-command-names-reject-hyphen-underscore-collision"
      (
        !(commandNameEvaluation {
          some-command.script = "true";
          some_command.script = "true";
        }).success
      )
      "A hyphenated command must not silently collide with an underscored command";

  hostenv-cli-command-names-reject-path-separator-collision =
    asserts.assertTrue "hostenv-cli-command-names-reject-path-separator-collision"
      (
        !(commandNameEvaluation {
          path__leaf.script = "true";
          path.commands.leaf.script = "true";
        }).success
      )
      "A flat command must not silently collide with Pog's nested-path function name";
}
