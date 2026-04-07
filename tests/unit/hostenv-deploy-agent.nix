{ pkgs, ... }:
let
  src = ../../modules/services/hostenv-deploy-agent;
  agentPkg = pkgs.haskellPackages.callCabal2nix "hostenv-deploy-agent" src { };
in
{
  hostenv-deploy-agent-build = agentPkg;
  hostenv-deploy-agent-tests = pkgs.haskell.lib.doCheck agentPkg;
}
