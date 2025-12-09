{ pkgs, makeHostenv }:
let
  envs = makeHostenv {
    organisation = "sampleorg";
    project = "sampleproj";
    root = ./.;
    modules = [
      ({ ... }: {
        hostenv.organisation = "sampleorg";
        hostenv.project = "sampleproj";
        environments.main = {
          enable = true;
          type = "production";
          virtualHosts."sample.hostenv.test" = { };
          hostenv.userName = "sample-main";
          hostenv.hostname = "sample.hostenv.test";
          extras = {
            backups = {
              repo = "s3:https://s3.amazonaws.com/sample-backups";
              passwordFile = "/run/secrets/sample-main/backups_secret";
              envFile = "/run/secrets/sample-main/backups_env";
              timer = { OnCalendar = "hourly"; };
            };
            nginx = {
              aliases = [ "www.sample.hostenv.test" ];
              csp = "default-src 'self'";
            };
          };
        };
        defaultEnvironment = "main";
      })
    ];
  };
in envs
