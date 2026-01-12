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

      devshellSpecType = lib.types.deferredModule;
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
