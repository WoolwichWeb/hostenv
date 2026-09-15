{ pkgs }:
let
  src = ../../modules/services/hostenv-provider-service;
  servicePkg = pkgs.haskell.lib.dontHaddock (
    pkgs.haskellPackages.callCabal2nix "hostenv-provider-service" src { }
  );
  ghc =
    (pkgs.haskellPackages.ghcWithPackages.override {
      installDocumentation = false;
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
