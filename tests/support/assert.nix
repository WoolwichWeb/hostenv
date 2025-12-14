{ pkgs, lib }:
let
  mkJson = name: value: pkgs.writeText name (builtins.toJSON value);
  assertTrue = name: condition: message:
    if condition then
      pkgs.runCommand name { } ''echo ok > $out''
    else
      pkgs.runCommand name { } ''
        echo ${lib.strings.escapeShellArg message} >&2
        exit 1
      '';
in {
  inherit mkJson assertTrue;
}
