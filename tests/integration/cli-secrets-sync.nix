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

      services.hostenv-provider.gitlab.enable = true;

      secrets = {
        enable = true;
        keys = [ "project_token" ];
        providerPublicKeys = [ "age1586arg8pt7tks24q03y24sfgc6fhr52nsv9mael2xh8u4w9gwgdslqqmaq" ];
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

    age_key_file="$TMPDIR/age.key"
    cat >"$age_key_file" <<'EOF'
    # created: 2026-02-21T19:13:24-05:00
    # public key: age1586arg8pt7tks24q03y24sfgc6fhr52nsv9mael2xh8u4w9gwgdslqqmaq
    AGE-SECRET-KEY-1NPNDRWJS9ACV58EKJ970QQLER58P3J2M88DNUQ0KPED497V6H6FQGDUZZQ
    EOF
    chmod 600 "$age_key_file"
    export SOPS_AGE_KEY_FILE="$age_key_file"

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
    grep -q '^gitlab_token_key:' ".hostenv/secrets/project.yaml"

    token_key_before="$(${pkgs.sops}/bin/sops --decrypt ".hostenv/secrets/project.yaml" | ${pkgs.yq-go}/bin/yq -r '.gitlab_token_key // ""')"
    printf '%s\n' "$token_key_before" | grep -Eq '^key=[0-9a-f]{64}$'

    "${cli}/bin/hostenv" list --env main >/dev/null

    token_key_after="$(${pkgs.sops}/bin/sops --decrypt ".hostenv/secrets/project.yaml" | ${pkgs.yq-go}/bin/yq -r '.gitlab_token_key // ""')"
    [ "$token_key_before" = "$token_key_after" ]
  '';
}
