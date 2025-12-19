{ inputs, ... }:
let
  fp = inputs.flake-parts.lib;
in
{
  options.perSystem = fp.mkPerSystemOption ({ config, system, ... }: {

    config =
      let
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
      };
  });
}
