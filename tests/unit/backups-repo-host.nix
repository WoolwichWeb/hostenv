{ pkgs, makeHostenv }:
let
  lib = pkgs.lib;
  support = import ../support { inherit pkgs lib; };
  asserts = support.asserts;

  env = makeHostenv [
    ({ ... }: {
      hostenv = {
        organisation = "acme";
        project = "camp";
        root = ./.;
        backupsRepoHost = "s3:https://s3.amazonaws.com/";
      };
      environments.main = {
        enable = true;
        type = "production";
      };
      services.restic.backups.drupal = {
        paths = [ "/data" ];
        environmentFile = "/run/secrets/restic-env";
      };
    })
  ] "main";

  cfg = env.config;
  trimmedOk = cfg.hostenv.backupsRepoHost == "s3:https://s3.amazonaws.com";
  expectedRepo =
    "${cfg.hostenv.backupsRepoHost}/${cfg.hostenv.organisation}-${cfg.hostenv.project}/${cfg.hostenv.userName}/restic";
  repoOk = cfg.services.restic.backups.drupal.repository == expectedRepo;
in
{
  backups-repo-host-trim =
    asserts.assertTrue "backups-repo-host-trim"
      (trimmedOk && repoOk)
      "backupsRepoHost should strip a trailing slash and restic defaults should use the trimmed host";
}
