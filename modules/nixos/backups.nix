{ ... }:
{
  flake.modules.nixos.backups =
    { lib, pkgs, config, ... }:
    let
      allEnvs = config.hostenv.environments or { };
      envs = lib.filterAttrs (_: env: env.enable) allEnvs;
      repoHostFor = env: env.hostenv.backupsRepoHost or config.hostenv.backupsRepoHost;
      envsWithBackups = lib.filterAttrs (_: env: repoHostFor env != null) envs;
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
              repoHost = repoHostFor env;
              repository = "${repoHost}/${name}";
            in
            {
              inherit repository;
              passwordFile = env.hostenv.backupsSecretFile or "/run/secrets/${name}/backups_secret";
              environmentFile = env.hostenv.backupsEnvFile or "/run/secrets/${name}/backups_env";
              timerConfig = { OnCalendar = "hourly"; };
              paths = [
                env.hostenv.dataDir or "/home/${env.hostenv.userName or name}/.local/share"
                env.hostenv.stateDir or "/home/${env.hostenv.userName or name}/.local/state"
              ];
              user = env.hostenv.userName or name;
              group = env.hostenv.userName or name;
              pruneOpts = [
                "--keep-daily 7"
                "--keep-weekly 4"
                "--keep-monthly 6"
                "--keep-yearly 7"
              ];
            }
          )
          envsWithBackups;
    
        # Secrets must be present; warn if missing.
        assertions = [
          {
            assertion = envsWithBackups != { };
            message = "hostenv.backups.enable is true but no environment has backupsRepoHost set.";
          }
        ];
      };
    }
  ;
}
