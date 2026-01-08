{ pkgs }:
let
  src = ../../modules/services/hostenv-provider-service;
  ghc = pkgs.haskellPackages.ghcWithPackages (p: [
    p.aeson
    p.bytestring
    p.cryptonite
    p.memory
    p.text
  ]);

in
{
  hostenv-provider-service-tests = pkgs.runCommand "hostenv-provider-service-tests" { } ''
    set -euo pipefail
    ${ghc}/bin/runghc -i${src} ${src}/Test.hs
    echo ok > $out
  '';
}
