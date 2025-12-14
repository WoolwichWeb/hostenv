{ pkgs, lib }:
{
  stubs = {
    base = import ./stubs/base.nix { inherit lib pkgs; };
  };
  asserts = import ./assert.nix { inherit pkgs lib; };
  providerView = import ./provider-view.nix { inherit lib; };
}
