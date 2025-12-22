{ inputs, lib, ... }:
let
  fp = inputs.flake-parts.lib;
in
{
  options.perSystem = fp.mkPerSystemOption ({ config, pkgs, self', ... }:
    let
      ghcPackageNames = lib.unique config.hostenv.haskell.devPackages;
      devGhc = pkgs.haskellPackages.ghcWithPackages (p: map (name: p.${name}) ghcPackageNames);
    in
    {
      options.hostenv.devShell = {
        packages = lib.mkOption { type = lib.types.listOf lib.types.package; default = [ ]; };
        inputsFrom = lib.mkOption { type = lib.types.listOf lib.types.package; default = [ ]; };
        shellHook = lib.mkOption { type = lib.types.lines; default = ""; };
      };
      options.hostenv.haskell.devPackages = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Haskell package names contributed by micro-projects for the default dev shell GHC environment.";
      };

      config = {
        hostenv.haskell.devPackages =
          [ "haskell-language-server" ]
          ++ (lib.attrByPath [ "provider" "haskellDevPackages" ] [ ] config);
        devShells.default = pkgs.mkShell {
          inputsFrom = config.hostenv.devShell.inputsFrom;

          buildInputs =
            [
              config.packages.hostenv-provider
              config.packages.hostenv-provider-plan
              pkgs.sops
              pkgs.age
              pkgs.jq
              pkgs.bind
              pkgs.deploy-rs
              pkgs.git
              pkgs.postgresql
              pkgs.socat
              devGhc
            ]
            ++ config.hostenv.devShell.packages;

          shellHook = ''
            export HOSTENV_PROVIDER_OUT=''${HOSTENV_PROVIDER_OUT:-generated}
          '' + "\n" + config.hostenv.devShell.shellHook;
        };
      };
    });
}
