{ pkgs }:
let
  src = ../../modules/services/hostenv-provider-service;
  servicePkg = pkgs.haskellPackages.callCabal2nix "hostenv-provider-service" src { };

in
{
  hostenv-provider-service-build = servicePkg;
  hostenv-provider-service-tests = pkgs.haskell.lib.doCheck servicePkg;
}
