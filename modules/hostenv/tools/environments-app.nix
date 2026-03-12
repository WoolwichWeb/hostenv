{ ... }:
{
  flake.modules.hostenv.tools-environments-app =
    { lib, config, pkgs, ... }:
    let
      envJson = builtins.toJSON config.exportedEnvironments;

      envs = pkgs.writeShellScriptBin "hostenv-environments" ''
        echo '${envJson}' | ${pkgs.jq}/bin/jq
      '';
    in
    {
      # Used by template `.envrc` to discover envs
      config.hostenv.apps.hostenv-environments = lib.mkDefault {
        type = "app";
        program = "${envs}/bin/hostenv-environments";
      };
    };
}
