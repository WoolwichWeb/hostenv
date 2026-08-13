{ pkgs, env }:
let
  lib = pkgs.lib;
  support = import ../support { inherit pkgs lib; };
  asserts = support.asserts;
  cli = env.config.hostenv.cliPackage;
in
asserts.assertRun {
  name = "hostenv-cli-service-wrappers";
  inherit env;
  script = ''
    fail() {
      printf 'hostenv service wrapper assertion failed: %s\n' "$1" >&2
      exit 1
    }

    for executable in drush mysql mysqldump; do
      test -x "${cli}/bin/$executable" \
        || fail "$executable should be installed beside the Hostenv CLI"
      grep -Fq -- "/bin/hostenv $executable -- \"\$@\"" "${cli}/bin/$executable" \
        || fail "$executable should dispatch through its Hostenv command path and preserve argv"
    done

    "${cli}/bin/hostenv" --help > "$TMPDIR/root-help"
    grep -Fq -- "Database:" "$TMPDIR/root-help" \
      || fail "database commands should retain their help group"
    grep -Fq -- "Drupal:" "$TMPDIR/root-help" \
      || fail "Drupal commands should retain their help group"

    "${cli}/bin/hostenv" mysql --help > "$TMPDIR/mysql-help"
    grep -Fq -- "Usage: hostenv mysql" "$TMPDIR/mysql-help" \
      || fail "generated service commands should retain native per-command help"
    grep -Fq -- "ARGUMENTS..." "$TMPDIR/mysql-help" \
      || fail "wrapped service commands should expose their pass-through argument contract"
  '';
}
