{ inputs, lib, config, ... }:
let
  types = lib.types;
  fp = inputs.flake-parts.lib;
in
{
  options.perSystem = fp.mkPerSystemOption ({ system, config, ... }: {
    options.hostenvProject = {
      makeHostenv = lib.mkOption {
        type = types.functionTo (types.functionTo types.unspecified);
        default = inputs.self.makeHostenv.${system};
      };
      modules = lib.mkOption {
        type = types.listOf types.deferredModule;
        default = [ ];
      };
      environmentName = lib.mkOption {
        type = types.nullOr types.str;
        default = null;
      };
    };

    config.hostenvProject.eval =
      let
        mk = config.hostenvProject.makeHostenv;
      in
      mk config.hostenvProject.modules config.hostenvProject.environmentName;

    config.hostenvProject.outputs = {
      inherit (config.hostenvProject.eval.config) environments defaultEnvironment;
    };
  });

  # Lift perSystem outputs into flake outputs
  config.flake.hostenv =
    lib.genAttrs config.systems (system:
      config.perSystem.${system}.hostenvProject.outputs);
}
