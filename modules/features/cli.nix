{ inputs, lib, config, ... }:
let
  fp = inputs.flake-parts.lib;
  projectEnabled = config.project.enable or false;
in
{
  options.perSystem = fp.mkPerSystemOption ({ ... }: { });

  config.perSystem = { config, ... }:
    lib.mkIf projectEnabled
      (let
        eval = config.hostenvProject.eval;
        cliPkg = eval.config.hostenv.cliPackage;
      in
      {
        packages.hostenv-cli = cliPkg;
        apps.hostenv = {
          type = "app";
          program = "${cliPkg}/bin/hostenv";
          meta.description = "Hostenv CLI";
        };
      });
}
