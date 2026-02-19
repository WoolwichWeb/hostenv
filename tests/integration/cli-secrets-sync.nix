{ pkgs, makeHostenv }:
let
  lib = pkgs.lib;
  support = import ../support { inherit pkgs lib; };
  asserts = support.asserts;

  env = makeHostenv [
    ({ ... }: {
      hostenv = {
        organisation = "acme";
        project = "demo";
        hostenvHostname = "hosting.test";
        root = ./drupal;
      };

      secrets = {
        enable = true;
        keys = [ "project_token" ];
        providerPublicKeys = [ "age1sfa74n3v8ljdh02k7262u3epz7wdj0xk90ve0yru654pcaqt34sqq66ex9" ];
      };

      allEnvironments.users.alice = {
        email = "alice@example.test";
        publicKeys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ7jiIqEDu1TAI2OL8cI575ufkhPJ1fxqC6qmJPaj5s0 alice"
        ];
      };

      environments.main = {
        enable = true;
        type = "production";
        secrets = {
          enable = true;
          keys = [ "env_token" ];
        };
      };
    })
  ] "main";

  cli = env.config.hostenv.cliPackage;
in
asserts.assertRun {
  name = "hostenv-cli-secrets-sync";
  inherit env;
  script = ''
    export HOME="$TMPDIR/home"
    export XDG_CONFIG_HOME="$HOME/.config"
    mkdir -p "$XDG_CONFIG_HOME"

    work="$TMPDIR/work"
    mkdir -p "$work/.hostenv"
    : >"$work/.hostenv/hostenv.nix"
    cd "$work"

    "${cli}/bin/hostenv" list --env main >/dev/null

    test -f ".hostenv/secrets/.sops.yaml"
    test -f ".hostenv/secrets/project.yaml"
    test -f ".hostenv/secrets/main.yaml"

    grep -q '^project_token:' ".hostenv/secrets/project.yaml"
    grep -q '^env_token:' ".hostenv/secrets/main.yaml"
    grep -q '^sops:' ".hostenv/secrets/project.yaml"
    grep -q '^sops:' ".hostenv/secrets/main.yaml"
    grep -q 'project\\.yaml' ".hostenv/secrets/.sops.yaml"
    grep -q 'main\\.yaml' ".hostenv/secrets/.sops.yaml"
  '';
}
