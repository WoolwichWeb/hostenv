{ lib, pkgs, config, ... }:
let
  allEnvs = config.hostenv.environments or { };
  envs = lib.filterAttrs (_: env: env.enable or true) allEnvs;
in
{
  options.hostenv.backups.enable = lib.mkOption {
    type = lib.types.bool;
    default = false;
    description = "Enable hostenv backups (per-environment, future integration).";
  };

  config = lib.mkIf (envs != { } && config.hostenv.backups.enable) {
    services.restic.backups = lib.mapAttrs
      (name: env:
        let
          repoHost = env.extras.backups.repoHost or config.hostenv.backupsRepoHost;
          repository = env.extras.backups.repo or "${repoHost}/${name}";
        in
        {
          inherit repository;
          passwordFile = env.extras.backups.passwordFile or "/run/secrets/${name}/backups_secret";
          environmentFile = env.extras.backups.envFile or "/run/secrets/${name}/backups_env";
          timerConfig = env.extras.backups.timer or { OnCalendar = "hourly"; };
          paths = [
            env.extras.backups.dataDir or "/home/${env.user or name}/.local/share"
            env.extras.backups.stateDir or "/home/${env.user or name}/.local/state"
          ];
          user = env.user or name;
          group = env.user or name;
          pruneOpts = [
            "--keep-daily 7"
            "--keep-weekly 4"
            "--keep-monthly 6"
            "--keep-yearly 7"
          ];
        }
      )
      (lib.filterAttrs (_: env: env.extras ? backups) envs);

    # Secrets must be present; warn if missing.
    assertions =
      lib.mapAttrsToList
        (name: env: {
          assertion = env.extras ? backups;
          message = "Backups enabled but env.extras.backups missing for ${name}";
        })
        envs
      ++ [
        {
          assertion = config.hostenv.backupsRepoHost != null;
          message = "hostenv.backups.enable is true but hostenv.backupsRepoHost is null; set a repository host.";
        }
      ];
  };
}
