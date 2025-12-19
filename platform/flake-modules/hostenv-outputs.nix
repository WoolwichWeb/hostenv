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
        default =
          if inputs ? hostenv && inputs.hostenv ? makeHostenv
          then inputs.hostenv.makeHostenv.${system}
          else inputs.self.makeHostenv.${system};
      };

      modules = lib.mkOption {
        type = types.listOf types.deferredModule;
        default = [ ];
      };

      environmentName = lib.mkOption {
        type = types.nullOr types.str;
        default = null;
      };

      eval = lib.mkOption {
        type = types.unspecified;
        readOnly = true;
        internal = true;
        description = "Internal: full evalModules result for this system.";
      };

      outputs = lib.mkOption {
        type = types.unspecified;
        readOnly = true;
        description = "Export-ready hostenv discovery outputs.";
      };
    };

    config.hostenvProject.eval =
      let mk = config.hostenvProject.makeHostenv;
      in mk config.hostenvProject.modules config.hostenvProject.environmentName;

    config.hostenvProject.outputs = {
      inherit (config.hostenvProject.eval.config) environments defaultEnvironment;
    };
  });

  # Lift perSystem outputs into flake outputs
  config.flake.hostenv =
    lib.genAttrs config.systems (system:
      config.allSystems.${system}.hostenvProject.outputs);
}
