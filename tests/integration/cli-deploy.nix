{ pkgs, makeHostenv }:
let
  support = import ../support { inherit pkgs; lib = pkgs.lib; };
  asserts = support.asserts;

  env = makeHostenv [
    ({ ... }: {
      hostenv = {
        organisation = "acme";
        project = "demo";
        hostenvHostname = "hosting.test";
        root = ./drupal;
      };
      environments.main = {
        enable = true;
        type = "production";
      };
    })
  ] "main";

  cli = env.config.hostenv.cliPackage;
  projectUpload = env.config.hostenv.projectUploadPackage;
in
asserts.assertRun {
  name = "hostenv-cli-project-upload";
  inherit env;
  script = ''
    cli=${cli}/bin/hostenv

    assert_cli_contains() {
      expected="$1"
      reason="$2"
      if ! grep -F -- "$expected" "$cli" >/dev/null; then
        printf 'hostenv deploy wiring test failed: %s\n' "$reason" >&2
        printf 'Expected the generated CLI to contain:\n  %s\n' "$expected" >&2
        printf 'Generated deploy section:\n' >&2
        grep -n -A10 -B5 'Deploying project code' "$cli" >&2 || true
        exit 1
      fi
    }

    assert_cli_contains '${pkgs.lib.getExe projectUpload}' \
      'hostenv deploy no longer invokes the executable covered by the behavioral test'
    assert_cli_contains 'project_root="$(git rev-parse --show-toplevel)"' \
      'the deployment source is no longer derived from the Git repository root'
    assert_cli_contains '"$project_root/" "$hostenv_user@$hostenv_host:/home/$hostenv_user/code/project/"' \
      'the sync helper is no longer called with the project contents and expected remote destination'
  '';
}
