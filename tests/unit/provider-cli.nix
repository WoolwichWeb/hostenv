{ pkgs }:
let
  cliPkg = pkgs.haskellPackages.callCabal2nix "hostenv-provider-cli" ../../provider { };
in
{
  provider-cli-typecheck = cliPkg;
}
