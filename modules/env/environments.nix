# Server environments, their routes, and the users that may access them.
{ config, lib, ... }:
let
  cfg = config.environments;
  defaultPriority = 1000;

  # Flag for when the user has set the value of an option to
  # `lib.mkForce null` and post-processing should respect that.
  forceNull = "__HOSTENV_INTERNAL_DO_NOT_CHANGE_SEMAPHORE__";

  # `config.hostenv` is not mandatory, as this module may be evaluated as
  # part of a full hostenv build, or separately.
  topLevel = config.hostenv or { };

in
{

  options.allEnvironments = with lib.types; lib.mkOption {
    type = submodule {
      options = {
        users = lib.mkOption {
          type = attrsOf (submodule {
            options = {
              email = lib.mkOption {
                type = str;
                description = "A valid email address for the user.";
              };
              publicKeys = lib.mkOption {
                type = listOf singleLineStr;
                description = "OpenSSH public keys for this user.";
              };
            };
          });
          default = { };
        };
      };
    };
    default = { users = { }; };
    description = "Settings applied across all environments.";
  };

  options.environments = with lib.types; lib.mkOption {
    type = attrsOf (submoduleWith {
      modules = [ ./environment.nix ];
      specialArgs = {
        allUsers = config.allEnvironments.users;
        topLevel = topLevel;
        defaultPriority = defaultPriority;
        forceNull = forceNull;
      };
    });
    default = { };
  };

  options.defaultEnvironment = with lib.types; lib.mkOption {
    type = str;
    description = "Environment built when the default is not specified by the user.";
    example = "production";
    default = "main";
  };

  # Bridge to the hostenv.* trunk used by feature modules so there is one canonical
  # view of environments, and reflect defaultEnvironment both ways.
  config = {
    hostenv.environments = lib.mkDefault cfg;
    hostenv.defaultEnvironment = lib.mkDefault config.defaultEnvironment;
  };
}
