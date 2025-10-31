{ pkgs, makeHostenv }:

let
  hostenvArgs = {
    organisation = "test";
    project = "test-project";
    root = ./drupal;
    environmentName = "dev";
    modules = [ ./drupal/hostenv.nix ];
  };
in
{
  drupalDev = makeHostenv hostenvArgs;

  drupalProduction = makeHostenv (hostenvArgs // {
    environmentName = "main";
  });

  drupal7 = makeHostenv (hostenvArgs // {
    root = pkgs.stdenv.mkDerivation {
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
    modules = [ ./drupal7/hostenv.nix ];
    environmentName = "main";
  });
}
