{ ... }:
{
  perSystem = { pkgs, self', ... }: {
    devShells.default = pkgs.mkShell {
      buildInputs = let hq = pkgs.haskellPackages; in [
        self'.packages.hostenv-provider
        self'.packages.hostenv-provider-plan
        pkgs.sops
        pkgs.age
        pkgs.jq
        pkgs.bind
        pkgs.deploy-rs
        pkgs.git
        (pkgs.ghc.withPackages (x: [ x.turtle x.aeson x.text-conversions ]))
        hq.haskell-language-server
        hq.aeson
        hq.turtle
        pkgs.kittysay
      ];
      shellHook = ''
        export HOSTENV_PROVIDER_OUT=''${HOSTENV_PROVIDER_OUT:-generated}
      '';
    };
  };
}
