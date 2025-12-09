{ lib, pkgs, ... }:
let
  types = lib.types;
in
{
  options.hostenv = {
    environments = lib.mkOption {
      type = types.attrsOf (types.submodule ({ name, ... }: {
        options = {
          enable = lib.mkEnableOption "environment ${name}";
          type = lib.mkOption {
            type = types.enum [ "production" "testing" "development" ];
            default = "development";
          };
          user = lib.mkOption {
            type = types.str;
            description = "Unix user for this environment (user = environment).";
          };
          hostname = lib.mkOption {
            type = types.str;
            description = "Primary hostname for this environment.";
          };
          virtualHosts = lib.mkOption {
            type = types.attrsOf types.attrs;
            default = { };
            description = "Additional vhosts and settings (nginx-facing attrset).";
          };
          phpVersion = lib.mkOption {
            type = types.nullOr types.str;
            default = null;
            description = "Desired PHP version if applicable.";
          };
          dbName = lib.mkOption {
            type = types.nullOr types.str;
            default = null;
            description = "Database name if applicable.";
          };
          extras = lib.mkOption {
            type = types.attrs;
            default = { };
            description = "Freeform bag for feature modules to read.";
          };
        };
      }));
      default = { };
      description = "Top-level environment definitions; trunk for dendritic modules.";
    };

    defaultEnvironment = lib.mkOption {
      type = types.str;
      default = lib.mkDefault "main";
      description = "Default environment name.";
    };
  };
}
