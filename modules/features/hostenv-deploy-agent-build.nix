{ inputs, lib, config, ... }:
let
  fp = inputs.flake-parts.lib;
  cfgTop = config;
  providerEnabled = config.provider.enable or false;
in
{
  options.perSystem = fp.mkPerSystemOption ({ pkgs, ... }:
    let
      deployAgent = cfgTop.flake.lib.provider.deployAgent;
      packageNames = deployAgent.haskellDeps;
      agentDrv = pkgs.haskell.packages.ghc912.callCabal2nix "hostenv-deploy-agent" deployAgent.src { };
      runtimeInputs = with pkgs; [
        bash
        coreutils
        nix
        shadow
        systemd
        util-linux
      ];
      agentPkg = pkgs.symlinkJoin {
        name = "hostenv-deploy-agent";
        paths = [ agentDrv ];
        nativeBuildInputs = [ pkgs.makeWrapper ];
        postBuild = ''
          wrapProgram "$out/bin/hostenv-deploy-agent" \
            --prefix PATH : ${lib.makeBinPath runtimeInputs}
        '';
      };
    in
    {
      config = lib.mkIf providerEnabled {
        packages.hostenv-deploy-agent = agentPkg;
        apps.hostenv-deploy-agent = {
          type = "app";
          program = "${agentPkg}/bin/hostenv-deploy-agent";
          meta.description = "Hostenv provider deploy node agent app";
        };
        hostenv.haskell.devPackages = packageNames;
        devshells.default.devshell.packages = [ agentPkg ];
      };
    });
}
