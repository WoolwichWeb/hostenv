{ inputs, lib, config, self, ... }:
let
  fp = inputs.flake-parts.lib;
  types = lib.types;
  cfg = config.project;
in
{
  options.project.enable = lib.mkEnableOption "Enable project outputs and hostenv project evaluation.";

  options.perSystem = fp.mkPerSystemOption ({ system, config, ... }: {
    options.hostenvProject = {
      makeHostenv = lib.mkOption {
        type = types.functionTo (types.functionTo types.unspecified);
        default =
          if inputs ? hostenv && inputs.hostenv ? makeHostenv
          then inputs.hostenv.makeHostenv.${system}
          else self.makeHostenv.${system};
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
  });

  config.perSystem = { system, config, pkgs, ... }:
    lib.mkIf cfg.enable
      (let
        makeHostenv = config.hostenvProject.makeHostenv;
        baseModules = config.hostenvProject.modules;
        defaultEval = makeHostenv baseModules config.hostenvProject.environmentName;

        envs = defaultEval.config.environments;
        enabledEnvs = pkgs.lib.filterAttrs (_: v: v.enable) envs;

        envPackages =
          pkgs.lib.mapAttrs
            (environmentName: _environment:
              let e = makeHostenv baseModules environmentName;
              in e.config.activatePackage
            )
            enabledEnvs;

        outputs = {
          inherit (defaultEval.config) environments defaultEnvironment;
        };
      in
      {
        hostenvProject.eval = defaultEval;
        hostenvProject.outputs = outputs;

        packages = envPackages // { default = defaultEval.config.activatePackage; };
        apps = defaultEval.config.hostenv.apps;
        devShells = defaultEval.config.hostenv.devShells;
      });

  # Lift perSystem outputs into flake outputs
  config.flake.lib.hostenv = lib.mkIf cfg.enable
    (lib.genAttrs config.systems (system:
      config.allSystems.${system}.hostenvProject.outputs));
}
