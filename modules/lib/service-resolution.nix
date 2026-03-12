{ ... }:
{
  flake.lib.hostenv.mkServiceResolutionOption = { lib }: lib.mkOption {
    # @todo: not sure if this should be nullable or use an `enable` option.
    type = lib.types.nullOr (lib.types.submodule {
      options = {
        organisation = lib.mkOption {
          type = lib.types.str;
          description = "Organisation that owns the provider-service environment.";
        };
        project = lib.mkOption {
          type = lib.types.str;
          description = "Project that owns the provider-service environment.";
        };
        environmentName = lib.mkOption {
          type = lib.types.str;
          description = "Environment name that runs the provider service and receives provider secrets.";
        };
      };
    });

    default = null;
    description = "Provider environment selector used for provider-service secrets.";
  };
}
