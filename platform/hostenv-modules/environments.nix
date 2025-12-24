# Core hostenv environment trunk: schema, defaults, and bridge to hostenv.*
{ config, lib, ... }:
let
  cfg = config.environments;
  forceNull = "__HOSTENV_INTERNAL_DO_NOT_CHANGE_SEMAPHORE__";
  topLevel = config.hostenv or { };
  types = lib.types;
  publicEnvs = import ./public-environments.nix { inherit lib; };
in
{
  options.allEnvironments = lib.mkOption {
    type = types.submodule {
      options = {
        users = lib.mkOption {
          type = types.attrsOf (types.submodule {
            options = {
              email = lib.mkOption { type = types.str; description = "A valid email address for the user."; };
              publicKeys = lib.mkOption { type = types.listOf types.singleLineStr; description = "OpenSSH public keys for this user."; };
            };
          });
          default = { };
        };
      };
    };
    default = { users = { }; };
    description = "Settings applied across all environments.";
  };

  # User-facing env definitions; typed against the core environment schema.
  options.environments = lib.mkOption {
    type = types.attrsOf (types.submoduleWith {
      modules = [ ./environment.nix ];
      specialArgs = {
        allUsers = config.allEnvironments.users;
        topLevel = topLevel;
        forceNull = forceNull;
      };
    });
    default = { };
  };

  # Default environment name (user facing) – bridged into hostenv.defaultEnvironment for consumers.
  options.defaultEnvironment = lib.mkOption {
    type = types.str;
    description = "Environment built when the default is not specified by the user.";
    example = "production";
    default = "main";
  };

  options.hostenv.publicEnvironments = lib.mkOption {
    type = types.attrs;
    default = { };
    internal = true;
    description = "Sanitized, user-facing view of environments for JSON output.";
  };

  # Bridge to the hostenv.* trunk used by feature modules so there is one canonical view.
  config = {
    hostenv.environments = lib.mkDefault cfg;
    hostenv.defaultEnvironment = lib.mkDefault config.defaultEnvironment;
    hostenv.publicEnvironments = lib.mkDefault (publicEnvs cfg);
  };
}
