{ pkgs, makeHostenv }:
let
  env = makeHostenv [
    ({ ... }: {
      hostenv = {
        organisation = "acme";
        project = "demo";
        hostenvHostname = "hosting.test";
        root = ../integration/drupal;
      };
      environments.main = {
        enable = true;
        type = "production";
      };
    })
  ] "main";

  projectUpload = env.config.hostenv.projectUploadPackage;
in
{
  project-upload-gitignore-filter = pkgs.runCommand "project-upload-gitignore-filter" { } ''
    project_root="$TMPDIR/project"
    destination="$TMPDIR/destination"

    mkdir -p \
      "$project_root/.git/objects/pack" \
      "$project_root/nested" \
      "$destination"

    printf '%s\n' \
      'ignored-root.txt' \
      '*.pack' \
      'objects/' \
      > "$project_root/.gitignore"
    printf '%s\n' 'ignored-nested.txt' > "$project_root/nested/.gitignore"

    printf 'ref: refs/heads/main\n' > "$project_root/.git/HEAD"
    printf 'git pack\n' > "$project_root/.git/objects/pack/repository.pack"
    printf 'tracked\n' > "$project_root/tracked.txt"
    printf 'untracked\n' > "$project_root/untracked.txt"
    printf 'nested keep\n' > "$project_root/nested/kept.txt"
    printf 'ignored root\n' > "$project_root/ignored-root.txt"
    printf 'ignored nested\n' > "$project_root/nested/ignored-nested.txt"
    printf 'stale\n' > "$destination/stale.txt"

    ${pkgs.lib.getExe projectUpload} "$project_root/" "$destination/"

    fail() {
      printf 'deploy-sync test failed: %s\n' "$1" >&2
      printf 'Destination tree:\n' >&2
      find "$destination" -mindepth 1 -printf '  %P\n' | sort >&2
      exit 1
    }

    assert_deployed() {
      relative_path="$1"
      reason="$2"
      [ -e "$destination/$relative_path" ] \
        || fail "expected '$relative_path' to be deployed because $reason"
    }

    assert_excluded() {
      relative_path="$1"
      reason="$2"
      [ ! -e "$destination/$relative_path" ] \
        || fail "expected '$relative_path' to be absent because $reason"
    }

    assert_deployed '.git/HEAD' \
      'Git metadata must bypass project ignore rules so the remote git reset can run'
    assert_deployed '.git/objects/pack/repository.pack' \
      "the /.git/*** include must take precedence over the '*.pack' and 'objects/' ignore rules"
    assert_deployed '.gitignore' \
      'the root filter file is part of the deployed repository'
    assert_deployed 'tracked.txt' \
      'ordinary files that do not match an ignore rule must be transferred'
    assert_deployed 'untracked.txt' \
      'non-ignored untracked files are part of the working tree deployment'
    assert_deployed 'nested/.gitignore' \
      'per-directory filter files remain part of the deployed repository'
    assert_deployed 'nested/kept.txt' \
      'ordinary files below a nested filter must still be transferred'

    assert_excluded 'ignored-root.txt' \
      'rules from the root .gitignore must be applied'
    assert_excluded 'nested/ignored-nested.txt' \
      'rsync must discover and apply nested .gitignore files recursively'
    assert_excluded 'stale.txt' \
      '--delete must remove destination files that are absent from the source'

    touch "$out"
  '';
}
