{ pkgs, makeHostenv }:
let
  lib = pkgs.lib;
  support = import ../support { inherit pkgs lib; };
  asserts = support.asserts;

  env = makeHostenv [
    ({ ... }: {
      hostenv = {
        organisation = "acme";
        project = "legacy-completion-test";
        hostenvHostname = "hosting.test";
        root = ./drupal;
      };

      environments.main = {
        enable = true;
        type = "production";
      };

      hostenv.cli.commands = {
        visible-probe = {
          description = "Visible command used by the compatibility test.";
          script = "true";
        };

        hidden-probe = {
          description = "Hidden command used by the compatibility test.";
          hidden = true;
          script = "true";
        };

        disabled-probe = lib.mkIf false {
          description = "Disabled command used by the compatibility test.";
          script = "true";
        };
      };
    })
  ] "main";

  cli = env.config.hostenv.cliPackage;
in
asserts.assertRun {
  name = "hostenv-cli-legacy-completion";
  inherit env;
  script = ''
    export HOME="$TMPDIR/home"
    export XDG_CONFIG_HOME="$HOME/.config"
    mkdir -p "$XDG_CONFIG_HOME/direnv"
    printf '[global]\nhide_env_diff = true\n' > "$XDG_CONFIG_HOME/direnv/direnv.toml"

    "${cli}/bin/hostenv" --env main __complete-subcommands > "$TMPDIR/commands"

    grep -Fxq 'visible-probe' "$TMPDIR/commands" \
      || { cat "$TMPDIR/commands" >&2; exit 1; }
    grep -Fxq 'environment' "$TMPDIR/commands" \
      || { cat "$TMPDIR/commands" >&2; exit 1; }

    if grep -Fxq 'hidden-probe' "$TMPDIR/commands"; then
      printf 'hidden project command leaked into legacy completion\n' >&2
      exit 1
    fi
    if grep -Fxq 'banner' "$TMPDIR/commands"; then
      printf 'hidden built-in command leaked into legacy completion\n' >&2
      exit 1
    fi
    if grep -Fxq '__complete-subcommands' "$TMPDIR/commands"; then
      printf 'compatibility command completed itself\n' >&2
      exit 1
    fi
    if grep -Fxq 'disabled-probe' "$TMPDIR/commands"; then
      printf 'disabled command leaked into legacy completion\n' >&2
      exit 1
    fi

    "${cli}/bin/hostenv" --help > "$TMPDIR/help"
    if grep -Fq '__complete-subcommands' "$TMPDIR/help"; then
      printf 'compatibility command should remain hidden from normal help\n' >&2
      exit 1
    fi
  '';
}
