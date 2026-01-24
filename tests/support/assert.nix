{ pkgs, lib }:
let
  assertTrue = name: condition: message:
    if condition then
      pkgs.runCommand name { } ''echo ok > $out''
    else
      pkgs.runCommand name { } ''
        echo ${lib.strings.escapeShellArg message} >&2
        exit 1
      '';

  # Run a shell snippet against a realised profile path, failing with a clear message.
  # `env` must expose `config.activatePackage`.
  assertRun = { name, env, script, buildInputs ? [ ] }:
    pkgs.runCommand name
      {
        buildInputs =
          [ pkgs.bash pkgs.coreutils pkgs.findutils pkgs.gnugrep pkgs.gnused pkgs.gawk ]
          ++ buildInputs;
      }
      ''
        set -euo pipefail
        export profile="${env.config.activatePackage}"
        : "profile=$profile"
        ${pkgs.bash}/bin/bash -euo pipefail -c ${lib.strings.escapeShellArg script}
        echo ok > "$out"
      '';
in {
  inherit assertTrue assertRun;
}
