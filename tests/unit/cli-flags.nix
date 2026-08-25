{ pkgs, makeHostenv }:
let
  lib = pkgs.lib;
  asserts = (import ../support { inherit pkgs lib; }).asserts;

  # Force the CLI package far enough to evaluate Hostenv's flag validation.
  commandEvaluation =
    commandDefinitions:
    builtins.tryEval (
      (makeHostenv [
        ({ ... }: {
          hostenv = {
            organisation = "acme";
            project = "flag-test";
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

  shortFlagEvaluation =
    short:
    commandEvaluation {
      probe = {
        flags = [{
          name = "example";
          inherit short;
        }];
        script = "true";
      };
    };
in
{
  hostenv-cli-flags-allow-distinct-sibling-variables =
    asserts.assertTrue "hostenv-cli-flags-allow-distinct-sibling-variables"
      (commandEvaluation {
        left = {
          flags = [{ name = "some-flag"; short = ""; }];
          script = "true";
        };
        right = {
          flags = [{ name = "some_flag"; short = ""; }];
          script = "true";
        };
      }).success
      "Flags on separate command paths do not share Bash variables";

  hostenv-cli-flags-reject-root-persistent-variable-collision =
    asserts.assertTrue "hostenv-cli-flags-reject-root-persistent-variable-collision"
      (
        !(commandEvaluation {
          probe = {
            flags = [{ name = "tty_mode"; short = ""; }];
            script = "true";
          };
        }).success
      )
      "A local flag must not collide with a normalized root persistent flag";

  hostenv-cli-flags-reject-inherited-variable-collision =
    asserts.assertTrue "hostenv-cli-flags-reject-inherited-variable-collision"
      (
        !(commandEvaluation {
          parent = {
            persistentFlags = [{ name = "some-flag"; short = "s"; }];
            commands.child = {
              flags = [{ name = "some_flag"; short = "c"; }];
              script = "true";
            };
          };
        }).success
      )
      "A child flag must not collide with a normalized inherited persistent flag";

  hostenv-cli-flags-allow-empty-short-name =
    asserts.assertTrue "hostenv-cli-flags-allow-empty-short-name"
      (shortFlagEvaluation "").success
      "An empty short flag name must remain the supported way to disable the short option";

  hostenv-cli-flags-allow-single-short-name =
    asserts.assertTrue "hostenv-cli-flags-allow-single-short-name"
      (shortFlagEvaluation "z").success
      "A single alphanumeric short flag name must remain valid";

  hostenv-cli-flags-reject-multiple-short-name =
    asserts.assertTrue "hostenv-cli-flags-reject-multiple-short-name"
      (!(shortFlagEvaluation "zz").success)
      "A short flag name must contain at most one character";

  hostenv-cli-flags-reject-shell-unsafe-short-name =
    asserts.assertTrue "hostenv-cli-flags-reject-shell-unsafe-short-name"
      (!(shortFlagEvaluation ":").success)
      "A short flag name must be safe for GNU getopt and Pog's generated shell";
}
