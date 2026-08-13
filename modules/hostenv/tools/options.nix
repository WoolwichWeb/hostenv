{ ... }:
{
  flake.modules.hostenv.tools-options =
    { lib, ... }:
    let
      inherit (lib) mkOption types;

      appType = types.submodule (
        { ... }: {
          options = {
            type = mkOption {
              type = types.enum [ "app" ];
              default = "app";
              description = "Nix flake app type.";
            };
            program = mkOption {
              type = types.str;
              description = "Executable or derivation path to run.";
            };
          };
        }
      );

      completionType = types.unspecified;

      flagType = types.submodule (
        { config, ... }: {
          options = {
            name = mkOption {
              type = types.singleLineStr;
              description = "Long flag name, without leading dashes.";
            };
            short = mkOption {
              type = types.singleLineStr;
              default = lib.substring 0 1 config.name;
              description = "Short flag name, without a leading dash. Use an empty string to disable it.";
            };
            default = mkOption {
              type = types.str;
              default = "";
              description = "Default flag value.";
            };
            bool = mkOption {
              type = types.bool;
              default = false;
              description = "Whether this is a boolean flag.";
            };
            optionalValue = mkOption {
              type = types.bool;
              default = false;
              description = "Whether the flag value is optional.";
            };
            repeatable = mkOption {
              type = types.bool;
              default = false;
              description = "Whether the flag may be repeated.";
            };
            hidden = mkOption {
              type = types.bool;
              default = false;
              description = "Whether to omit the flag from help and completion.";
            };
            description = mkOption {
              type = types.str;
              default = "a flag";
              description = "Flag help text.";
            };
            argument = mkOption {
              type = types.singleLineStr;
              default = "VAR";
              description = "Value name shown in help.";
            };
            envVar = mkOption {
              type = types.nullOr types.singleLineStr;
              default = null;
              description = "Environment variable used as a flag value fallback. Pog's default is used when null.";
            };
            required = mkOption {
              type = types.bool;
              default = false;
              description = "Whether a value must be supplied.";
            };
            prompt = mkOption {
              type = types.nullOr types.lines;
              default = null;
              description = "Shell expression used to prompt for a required value. Pog's default is used when null.";
            };
            promptError = mkOption {
              type = types.nullOr types.str;
              default = null;
              description = "Error shown when prompting does not produce a value.";
            };
            promptErrorExitCode = mkOption {
              type = types.int;
              default = 3;
              description = "Exit status used when a required flag is missing.";
            };
            completion = mkOption {
              type = types.nullOr completionType;
              default = null;
              description = "Shell-neutral completion values or a value from `pkgs.pog.pog.completions`.";
            };
          };
        }
      );

      argumentType = types.either types.singleLineStr (
        types.submodule (
          { ... }: {
            options = {
              name = mkOption {
                type = types.singleLineStr;
                description = "Argument name shown in help.";
              };
              description = mkOption {
                type = types.str;
                default = "";
                description = "Argument help text.";
              };
              variadic = mkOption {
                type = types.bool;
                default = false;
                description = "Whether this final argument accepts every remaining positional value.";
              };
              completion = mkOption {
                type = types.nullOr completionType;
                default = null;
                description = "Shell-neutral completion values or a value from `pkgs.pog.pog.completions`.";
              };
            };
          }
        )
      );

      commandType = types.submodule (
        { ... }: {
          options = {
            script = mkOption {
              type = types.either types.lines (types.functionTo types.lines);
              default = "";
              description = "Shell code to run for this command, or a function from Pog helpers that returns shell code to run.";
            };
            description = mkOption {
              type = types.str;
              default = "";
              description = "Command help text.";
            };
            aliases = mkOption {
              type = types.listOf types.singleLineStr;
              default = [ ];
              description = "Alternate names for this command.";
            };
            group = mkOption {
              type = types.singleLineStr;
              default = "";
              description = "Group heading used in help and completion.";
            };
            hidden = mkOption {
              type = types.bool;
              default = false;
              description = "Whether this command is show in help and completion";
            };
            default = mkOption {
              type = types.bool;
              default = false;
              description = "If true, this command will be run when none is specified.";
            };
            parsing = mkOption {
              type = types.nullOr (
                types.enum [
                  "interspersed"
                  "non-interspersed"
                  "passthrough"
                  "disabled"
                ]
              );
              default = null;
              description = "How Pog parses known and unknown options for this command.";
            };
            flags = mkOption {
              type = types.listOf flagType;
              default = [ ];
              description = "Flags owned by this command.";
            };
            persistentFlags = mkOption {
              type = types.listOf flagType;
              default = [ ];
              description = "Flags accepted by this command and all descendants.";
            };
            exclusiveFlags = mkOption {
              type = types.listOf (types.listOf types.singleLineStr);
              default = [ ];
              description = "Groups of mutually exclusive long flag names.";
            };
            arguments = mkOption {
              type = types.listOf argumentType;
              default = [ ];
              description = "Positional arguments accepted by this command.";
            };
            argumentCompletion = mkOption {
              type = types.nullOr completionType;
              default = null;
              description = "Completion for positional arguments.";
            };
            beforeExit = mkOption {
              type = types.lines;
              default = "";
              description = "Shell code run when this command path exits.";
            };
            commands = mkOption {
              type = types.lazyAttrsOf commandType;
              default = { };
              description = "Nested command definitions.";
            };

            runtimeInputs = mkOption {
              type = types.listOf types.package;
              default = [ ];
              example = "[ pkgs.boxes pkgs.mysql ]";
              description = "Packages added to the root Hostenv CLI's PATH for this command tree.";
            };
            executable = mkOption {
              type = types.nullOr types.singleLineStr;
              default = null;
              example = "mysql";
              description = "Optional standalone wrapper program that runs this command.";
            };
          };
        }
      );
    in
    {
      options.hostenv.cliPackage = mkOption {
        type = types.package;
        description = "The hostenv CLI package for this project.";
      };

      options.hostenv.projectUploadPackage = mkOption {
        type = types.package;
        readOnly = true;
        internal = true;
        description = "The internal executable that uploads project files during deployment.";
      };

      options.hostenv.devShells = mkOption {
        type = types.attrsOf types.deferredModule;
        description = "Devshell specifications for hostenv project environments.";
      };

      options.hostenv.apps = mkOption {
        type = types.attrsOf appType;
        description = "Apps for working with hostenv projects.";
      };

      options.hostenv.cli.commands = mkOption {
        type = types.lazyAttrsOf commandType;
        default = { };
        description = ''
          Commands for the `hostenv` CLI application.

          Uses Pog. See: https://github.com/jpetrucciani/pog
        '';
      };
    };
}
