{ lib, pkgs, config, ... }:
let
  envs = config.hostenv.environments or { };
in
{
  options.hostenv.backups.enable = lib.mkOption {
    type = lib.types.bool;
    default = false;
    description = "Enable hostenv backups (per-environment, future integration).";
  };

  config = lib.mkIf (envs != { } && config.hostenv.backups.enable) {
    services.restic.backups = lib.mapAttrs (name: env: {
      repository = env.extras.backups.repo or "s3:https://s3.amazonaws.com/${name}";
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
      ];
    }) (lib.filterAttrs (_: env: env.extras ? backups) envs);

    # Secrets must be present; warn if missing.
    assertions = lib.mapAttrsToList (name: env: {
      assertion = env.extras ? backups;
      message = "Backups enabled but env.extras.backups missing for ${name}";
    }) envs;
  };
}
