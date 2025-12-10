{ lib, pkgs, config, ... }:
let
  allEnvs = config.hostenv.environments or { };
  envs = lib.filterAttrs (_: env: env.enable or true) allEnvs;
in
{
  options.hostenv.monitoring.enable = lib.mkOption {
    type = lib.types.bool;
    default = false;
    description = "Enable basic monitoring labels and exporters per environment.";
  };

  config = lib.mkIf (envs != { } && config.hostenv.monitoring.enable) {
    services.prometheus = {
      exporters.node.enable = lib.mkDefault true;
      scrapeConfigs = lib.mkDefault [];
    };

    # Example: label injection for future log/metrics shipping
    environment.variables = lib.mapAttrs (name: env: {
      "HOSTENV_ENV" = name;
      "HOSTENV_PROJECT" = env.extras.project or env.user or name;
    }) envs;
  };
}
