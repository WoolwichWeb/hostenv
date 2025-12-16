{ inputs, lib, ... }:
let
  types = lib.types;
  fp = inputs.flake-parts.lib;
in
{
  options.perSystem = fp.mkPerSystemOption ({ config, system, ... }: {
    options.hostenvCli = {
      makeHostenv = lib.mkOption {
        type = types.functionTo types.unspecified;
        default = inputs.self.makeHostenv;
        description = "makeHostenv function (modules -> environmentName -> eval) from hostenv modules flake.";
      };
      modules = lib.mkOption {
        type = types.listOf types.deferredModule;
        default = [ ];
        description = "Module list to pass to makeHostenv for building the CLI package.";
      };
      environmentName = lib.mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Environment to use when building the hostenv CLI (null means defaultEnvironment).";
      };
    };

    config = let
      mk = config.hostenvCli.makeHostenv;
      envEval = mk config.hostenvCli.modules config.hostenvCli.environmentName;
      cliPkg = envEval.config.hostenv.cliPackage;
    in {
      packages.hostenv-cli = cliPkg;
      apps.hostenv = {
        type = "app";
        program = "${cliPkg}/bin/hostenv";
        meta.description = "Hostenv CLI";
      };
    };
  });
}
