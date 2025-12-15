{ pkgs, lib }:
let
  stubBase = import ./stubs/base.nix { inherit lib pkgs; };
in
{
  stubs = {
    base = stubBase;
  };
  asserts = import ./assert.nix { inherit pkgs lib; };
  providerView = import ./provider-view.nix { inherit lib; };
  samples = import ./sample-envs.nix { inherit lib; };
  # Convenience wrapper to run lib.evalModules with the common root stub.
  evalWithBase = { modules, specialArgs ? { }, ... }@args:
    lib.evalModules (args // {
      specialArgs = { inherit pkgs; } // specialArgs;
      modules = [ stubBase ] ++ modules;
    });
}
