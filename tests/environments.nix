{ pkgs, makeHostenv }:

let
  drupalRoot = import ./integration/drupal/source.nix { inherit pkgs; };

  baseModules = [
    ({ ... }: {
      hostenv = {
        organisation = "test";
        project = "test-project";
        root = drupalRoot;
        hostenvHostname = "hosting.test";
      };
    })
    ./integration/drupal/hostenv.nix
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
      ./integration/drupal7/hostenv.nix
    ] "main";

  drupal6 =
    let
      drupal6src = pkgs.stdenv.mkDerivation {
        pname = "drupal6-test-source";
        version = "6.38";

        src = pkgs.fetchurl {
          url = "https://ftp.drupal.org/files/projects/drupal-6.38.tar.gz";
          sha256 = "sha256-RqbX7BcOdPPIWxH98PrnTOBpHUJguEi/X6/x8PXzHUs=";
        };

        installPhase = ''
          mkdir -p $out
          cp -r . $out/
          mkdir -p $out/sites/default/files
          printf '%s\n' root-layout-seed > $out/sites/default/files/root-layout-seed.txt
        '';
      };
    in
    makeHostenv [
      ({ ... }: {
        hostenv = {
          organisation = "test";
          project = "test-project";
          root = drupal6src;
          hostenvHostname = "hosting.test";
        };
      })
      ./integration/drupal6/hostenv.nix
    ] "main";

  drupal6WebRoot =
    let
      drupal6src = pkgs.stdenv.mkDerivation {
        pname = "drupal6-web-root-test-source";
        version = "6.38";

        src = pkgs.fetchurl {
          url = "https://ftp.drupal.org/files/projects/drupal-6.38.tar.gz";
          sha256 = "sha256-RqbX7BcOdPPIWxH98PrnTOBpHUJguEi/X6/x8PXzHUs=";
        };

        installPhase = ''
          mkdir -p $out/web
          cp -r . $out/web/
          mkdir -p $out/web/sites/default/files
          printf '%s\n' web-root-layout-seed > $out/web/sites/default/files/web-root-layout-seed.txt
        '';
      };
    in
    makeHostenv [
      ({ ... }: {
        hostenv = {
          organisation = "test";
          project = "test-project";
          root = drupal6src;
          hostenvHostname = "hosting.test";
        };
      })
      ./integration/drupal6/hostenv.nix
    ] "main";
}
