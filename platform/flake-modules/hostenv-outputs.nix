{ inputs, lib, config, ... }:
let
  types = lib.types;
  fp = inputs.flake-parts.lib;
in
{
  options.perSystem = fp.mkPerSystemOption ({ system, config, ... }: {
    options.hostenvProject = {
      makeHostenv = lib.mkOption {
        type = types.functionTo types.unspecified; # modules -> envName -> eval
        default = inputs.self.makeHostenv.${system};
        description = "Function to evaluate hostenv modules for this system.";
      };
      modules = lib.mkOption {
        type = types.listOf types.deferredModule;
        default = [ ];
        description = "Module list to pass to makeHostenv.";
      };
      environmentName = lib.mkOption {
        type = types.nullOr types.str;
        default = null; # null => defaultEnvironment, enables discovery
        description = "Environment to evaluate; null uses defaultEnvironment.";
      };
    };

    config = let
      mk = config.hostenvProject.makeHostenv;
      eval = mk config.hostenvProject.modules config.hostenvProject.environmentName;
    in {
      flake.hostenv.${system} = {
        inherit (eval.config) environments defaultEnvironment;
      };
    };
  });
}
