{ lib, config, pkgs, ... }:
let
  envJsonEval = builtins.tryEval (builtins.toJSON config.environments);
  envJson =
    assert envJsonEval.success
      || builtins.throw ''
      hostenv: config.environments must be JSON-serializable.

      Non-JSON data should be stored elsewhere (e.g. config.hostenv.*).
    '';
    envJsonEval.value;

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
}
