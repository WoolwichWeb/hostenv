{ ... }:
{
  flake.modules.hostenv.tools-options =
    { lib, ... }:
    let
      appType = lib.types.submodule ({ ... }: {
        options = {
          type = lib.mkOption {
            type = lib.types.enum [ "app" ];
            default = "app";
            description = "Nix flake app type.";
          };
          program = lib.mkOption {
            type = lib.types.str;
            description = "Executable or derivation path to run.";
          };
        };
      });

      scriptType = lib.types.submodule ({ ... }: {
        options = {
          exec = lib.mkOption {
            type = lib.types.functionTo lib.types.lines;
            description = "Shell code to execute when the hostenv sub-command is run.";
          };
          description = lib.mkOption {
            type = lib.types.str;
            description = "Description of the script.";
            default = "";
          };
          runtimeInputs = lib.mkOption {
            type = lib.types.listOf lib.types.package;
            description = "Packages to be available in PATH when the sub-command is run.";
            default = [ ];
            example = "[ pkgs.boxes pkgs.mysql ]";
          };
          makeScript = lib.mkOption {
            type = lib.types.bool;
            description = "Whether to make this sub-command a top-level script (e.g. hostenv mysql -> mysql).";
            default = false;
          };
          internal = lib.mkOption {
            type = lib.types.bool;
            description = "Hides this sub-command from 'hostenv list'.";
            default = false;
          };
        };
      });

      devshellEntryType = lib.types.submodule ({ ... }: {
        options = {
          text = lib.mkOption {
            type = lib.types.lines;
            description = "Shell code to execute at startup.";
          };
          deps = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [ ];
            description = "Startup step dependencies.";
          };
        };
      });

      devshellEnvType = lib.types.submodule ({ ... }: {
        options = {
          name = lib.mkOption {
            type = lib.types.str;
            description = "Environment variable name.";
          };
          value = lib.mkOption {
            type = lib.types.nullOr (lib.types.oneOf [
              lib.types.str
              lib.types.int
              lib.types.bool
              lib.types.package
            ]);
            default = null;
            description = "Shell-escaped value to set.";
          };
          eval = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Raw shell value (not escaped).";
          };
          prefix = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Prefix PATH-like variables with a relative path.";
          };
          unset = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Unset this variable.";
          };
        };
      });

      devshellSpecType = lib.types.submodule ({ ... }: {
        options = {
          devshell = {
            name = lib.mkOption {
              type = lib.types.nullOr lib.types.str;
              default = null;
              description = "Human-readable name for the dev shell.";
            };
            packages = lib.mkOption {
              type = lib.types.listOf lib.types.package;
              default = [ ];
              description = "Packages available in the dev shell.";
            };
            packagesFrom = lib.mkOption {
              type = lib.types.listOf lib.types.package;
              default = [ ];
              description = "Derivations to pull build inputs from.";
            };
            startup = lib.mkOption {
              type = lib.types.attrsOf devshellEntryType;
              default = { };
              description = "Shell snippets executed on startup.";
            };
            interactive = lib.mkOption {
              type = lib.types.attrsOf devshellEntryType;
              default = { };
              description = "Shell snippets executed for interactive shells.";
            };
            motd = lib.mkOption {
              type = lib.types.nullOr lib.types.str;
              default = null;
              description = "Message-of-the-day text.";
            };
          };
          env = lib.mkOption {
            type = lib.types.listOf devshellEnvType;
            default = [ ];
            description = "Environment variables for the dev shell.";
          };
        };
        freeformType = lib.types.attrsOf lib.types.unspecified;
      });
    in
    {
      options.hostenv.cliPackage = lib.mkOption {
        type = lib.types.package;
        description = "The hostenv CLI package for this project.";
      };

      options.hostenv.devShells = lib.mkOption {
        type = lib.types.attrsOf devshellSpecType;
        description = "Devshell specifications for hostenv project environments.";
      };

      options.hostenv.apps = lib.mkOption {
        type = lib.types.attrsOf appType;
        description = "Apps for working with hostenv projects.";
      };

      options.hostenv.subCommands = lib.mkOption {
        type = lib.types.attrsOf scriptType;
        default = { };
        description = "Sub-commands available when running the `hostenv` CLI application.";
      };
    };
}
