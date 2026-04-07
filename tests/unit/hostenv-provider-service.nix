{ pkgs, inputs }:
let
  src = ../../modules/services/hostenv-provider-service;
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      addressable-content = self.callCabal2nix "addressable-content" inputs.addressable-content.outPath { };
    };
  };
  servicePkg = haskellPackages.callCabal2nix "hostenv-provider-service" src { };

in
{
  hostenv-provider-service-build = servicePkg;
  hostenv-provider-service-tests = pkgs.haskell.lib.doCheck servicePkg;
}
