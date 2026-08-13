# Hostenv CLI command API

Project and feature modules can add commands at `hostenv.cli.commands.<name>`.

For example:

```nix
hostenv.cli.commands.mysql = {
  script = helpers: ''
    exec ssh $SSH_TTY "$user@$host" -- mysql "$@"
  '';
  description = "Run mysql remotely.";
  group = "Database";
  runtimeInputs = [ pkgs.openssh ];
  executable = "mysql";
  parsing = "passthrough";
  arguments = [{
    name = "arguments";
    variadic = true;
    completion = [ ];
  }];
};
```

`executable` creates a standalone program that dispatches through the full
command path. For example, `executable = "mysql"` on the `mysql` command runs
`hostenv mysql -- "$@"`. Arguments after `--` are kept for the wrapped tool,
including values that look like Hostenv flags.

Note: Hostenv uses [Pog](https://github.com/jpetrucciani/pog) to generate its
CLI.

## Recursive commands

Commands can nest to any depth:

```nix
hostenv.cli.commands.remote = {
  description = "Manage remote environments.";
  commands.add = {
    aliases = [ "new" ];
    description = "Add a remote environment.";
    flags = [{
      name = "url";
      argument = "URL";
      required = true;
    }];
    arguments = [{
      name = "name";
      description = "Remote name";
    }];
    script = ''
      printf 'adding %s at %s\n' "$1" "$url"
    '';
  };
};
```

Each command supports Pog's command fields: `script`, `description`,
`aliases`, `group`, `hidden`, `default`, `parsing`, `flags`,
`persistentFlags`, `exclusiveFlags`, `arguments`, `argumentCompletion`,
`beforeExit`, and nested `commands`. Hostenv adds `runtimeInputs`, which are
collected into the root CLI package, and `executable` for standalone wrappers.

`parsing` accepts `"interspersed"`, `"non-interspersed"`, `"passthrough"`, or
`"disabled"`. Use `passthrough` plus a final variadic argument for commands
that wrap another CLI. Known Hostenv flags are consumed; unknown options and
positional values remain in order. Use `--` when a wrapped option has the same
name as a Hostenv persistent flag.

Flags and arguments accept static completion values or Pog's shell-neutral
completion constructors:

```nix
{ pkgs, ... }: {
  hostenv.cli.commands.inspect.arguments = [{
    name = "target";
    completion = pkgs.pog.pog.completions.values [
      { value = "main"; description = "production environment"; }
      { value = "testing"; description = "testing environment"; }
    ];
  }];
}
```

The normal Hostenv CLI package contains Pog's Carapace specification,
`_hostenv_complete` query command, and generated adapters for Bash, Fish, Zsh,
Nushell, and Pog's other supported shells. Hostenv does not expose Pog's Arx,
AppImage, or host-script outputs because its CLI depends on Nix packages and
the project dev shell.
