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
  evalWithBase = { modules, ... }@args:
    let
      args' = builtins.removeAttrs args [ "modules" ];
    in
    lib.evalModules (args' // {
      modules = [
        { _module.args = { inherit pkgs; }; }
        stubBase
      ] ++ modules;
    });
}
