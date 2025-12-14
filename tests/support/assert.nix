{ pkgs, lib }:
let
  mkJson = name: value: pkgs.writeText name (builtins.toJSON value);
  jqAssert = name: expr: value:
    pkgs.runCommand name { buildInputs = [ pkgs.jq pkgs.coreutils ]; } ''
      cat ${mkJson "${name}.json" value} > $out
      jq -e '${expr}' "$out" >/dev/null || {
        echo "jq assertion failed: ${expr}" >&2
        cat "$out" >&2
        exit 1
      }
    '';
  assertTrue = name: condition: message:
    if condition then
      pkgs.runCommand name { } ''echo ok > $out''
    else
      pkgs.runCommand name { } ''
        echo ${lib.strings.escapeShellArg message} >&2
        exit 1
      '';
in {
  inherit mkJson jqAssert assertTrue;
}
