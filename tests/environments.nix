{ pkgs, makeHostenv }:

let
  baseModules = [
    ({ ... }: {
      hostenv = {
        organisation = "test";
        project = "test-project";
        root = ./drupal;
        hostenvHostname = "hosting.test";
      };
    })
    ./drupal/hostenv.nix
  ];
in {
  drupalDev = makeHostenv baseModules "dev";

  drupalProduction = makeHostenv baseModules "main";

  drupal7 =
    let
      drupal7src = pkgs.stdenv.mkDerivation {
        pname = "drupal7-test-source";
        version = "7.103";

        src = pkgs.fetchurl {
          url = "https://ftp.drupal.org/files/projects/drupal-7.103.tar.gz";
          sha256 = "sha256-PYoM7tJzsHhyLbXYOX461xEl2dQ4+Y9RWX3EYmrOSfU=";
        };

        installPhase = ''
          mkdir -p $out
          cp -r . $out/
        '';
      };
    in
    makeHostenv [
      ({ ... }: {
        hostenv = {
          organisation = "test";
          project = "test-project";
          root = drupal7src;
          hostenvHostname = "hosting.test";
        };
      })
      ./drupal7/hostenv.nix
    ] "main";
}
