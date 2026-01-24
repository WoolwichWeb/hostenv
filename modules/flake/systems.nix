{ inputs, lib, ... }:
{
  systems = lib.mkDefault inputs.nixpkgs.lib.systems.flakeExposed;
}
