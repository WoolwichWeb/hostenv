{ pkgs }:
let
  src = ../../provider;
  cliPkg = pkgs.haskellPackages.callCabal2nix "hostenv-provider-cli" src { };
  ghc = pkgs.haskellPackages.ghcWithPackages (p: [
    p.text
  ]);
in
{
  provider-cli-typecheck = cliPkg;
  provider-cli-signing-targets = pkgs.runCommand "provider-cli-signing-targets" { } ''
    set -euo pipefail
    ${ghc}/bin/runghc -i${src} ${src}/TestSigningTargets.hs
    echo ok > "$out"
  '';
}
