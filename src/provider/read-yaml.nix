{ pkgs, ... }:

path:

let
  jsonOutputDrv =
    pkgs.runCommand
      "from-yaml"
      { nativeBuildInputs = [ pkgs.remarshal ]; }
      "remarshal --stringify -if yaml -i \"${path}\" -of json -o \"$out\"";
in
builtins.fromJSON (builtins.readFile jsonOutputDrv)
