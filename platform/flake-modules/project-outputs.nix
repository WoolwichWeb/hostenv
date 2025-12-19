{ inputs, lib, ... }:
let
  fp = inputs.flake-parts.lib;
in
{
  options.perSystem = fp.mkPerSystemOption ({ system, config, pkgs, ... }: {
    config =
      let
        makeHostenv = config.hostenvProject.makeHostenv;
        baseModules = config.hostenvProject.modules;
        defaultEval = config.hostenvProject.eval;

        envs = defaultEval.config.environments;
        enabledEnvs = pkgs.lib.filterAttrs (_: v: v.enable) envs;

        envPackages =
          pkgs.lib.mapAttrs
            (environmentName: _environment:
              let e = makeHostenv baseModules environmentName;
              in e.config.activatePackage
            )
            enabledEnvs;
      in
      {
        packages =
          envPackages
          // { default = defaultEval.config.activatePackage; };

        apps = defaultEval.config.hostenv.apps;
        devShells = defaultEval.config.hostenv.devShells;
      };
  });
}
