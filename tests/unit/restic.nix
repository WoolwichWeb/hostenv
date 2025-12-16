{ pkgs }:
let
  lib = pkgs.lib;
  support = import ../support { inherit pkgs lib; };
  asserts = support.asserts;

  restic_exclusive_repo =
    let
      eval = support.evalWithBase {
        specialArgs = { inherit pkgs; };
        modules = [
          ../../platform/services/restic.nix
          ({ ... }: {
            hostenv.cacheDir = "/tmp/hostenv-cache";
            hostenv.userName = "restic-test";
            hostenv.backupsRepoHost = "s3:https://backups.invalid";
          })
          ({ ... }: {
            _module.check = false;
            services.restic.enable = true;
            services.restic.backups.bad = {
              paths = [ "/data" ];
              repository = "s3:https://example.invalid";
              repositoryFile = pkgs.writeText "repo-file" "s3:https://other.invalid";
              environmentFile = null;
              passwordFile = "/tmp/pw";
            };
          })
        ];
      };
      anyFail = lib.any (a: a.assertion == false) eval.config.assertions;
    in
    asserts.assertTrue "restic-exclusive-repo-assert" anyFail
      "Expected restic assertion to fail when repository and repositoryFile are both set";

  restic_repo_envfile_ok =
    let
      eval = support.evalWithBase {
        specialArgs = { inherit pkgs; };
        modules = [
          ../../platform/services/restic.nix
          ({ ... }: {
            hostenv.cacheDir = "/tmp/hostenv-cache";
            hostenv.userName = "restic-test";
            hostenv.backupsRepoHost = "s3:https://backups.invalid";
          })
          ({ ... }: {
            _module.check = false;
            services.restic.enable = true;
            services.restic.backups.ok = {
              paths = [ "/data" ];
              repository = "s3:https://example.invalid";
              repositoryFile = null;
              environmentFile = "/tmp/envfile";
              passwordFile = "/tmp/pw";
            };
          })
        ];
      };
      assertionsOk = ! lib.any (a: a.assertion == false) eval.config.assertions;
      service = eval.config.systemd.services."restic-backups-ok";
      env = service.environment or { };
      repoOk = (env.RESTIC_REPOSITORY or "") == "s3:https://example.invalid";
      repoFileNull = (env.RESTIC_REPOSITORY_FILE or null) == null;
    in
    asserts.assertTrue "restic-repo-envfile-ok"
      (assertionsOk && repoOk && repoFileNull)
      "restic repo+envfile should pass assertions and set repository/env correctly";

in
{
  inherit restic_exclusive_repo restic_repo_envfile_ok;
}
