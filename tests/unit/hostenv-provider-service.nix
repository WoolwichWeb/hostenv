{ pkgs, documentation }:
let
  src = ../../modules/services/hostenv-provider-service;
  rawServicePkg = pkgs.haskellPackages.callCabal2nix "hostenv-provider-service" src { };
  servicePkg =
    if documentation.haskell.haddock.enable then
      rawServicePkg
    else
      pkgs.haskell.lib.dontHaddock rawServicePkg;
  ghc =
    (pkgs.haskellPackages.ghcWithPackages.override {
      installDocumentation = documentation.haskell.dependencies.enable;
    })
      (p: [
        p.aeson
        p.bytestring
        p.cryptonite
        p.memory
        p.text
      ]);

in
{
  hostenv-provider-service-build = servicePkg;
  hostenv-provider-service-tests = pkgs.runCommand "hostenv-provider-service-tests" { } ''
    set -euo pipefail
    ${ghc}/bin/runghc -i${src} ${src}/Test.hs
    echo ok > $out
  '';
}
