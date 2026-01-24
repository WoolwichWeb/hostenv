{ ... }:
{
  flake.modules.nixos.hostenv-top-level =
    { config, pkgs, lib, ... }:
    let
      envs = config.hostenv.environments or { };
      publicEnvs = config.flake.lib.hostenv.publicEnvironments;
      enabledEnvs = lib.filterAttrs (_: env: env.enable or true) envs;
      envUser = name: env: env.hostenv.userName or name;
      envKeys = env:
        lib.foldlAttrs (acc: _n: u: acc ++ (u.publicKeys or [ ])) [ ] (env.users or { });
    in
    {
      options.hostenv = {
        environments = lib.mkOption {
          type = lib.types.attrsOf lib.types.unspecified;
          default = { };
          description = "Hostenv environments (provider plan or project output).";
        };

        defaultEnvironment = lib.mkOption {
          type = lib.types.str;
          default = "main";
          description = "Default environment name for hostenv.";
        };

        publicEnvironments = lib.mkOption {
          type = lib.types.attrs;
          default = { };
          internal = true;
          description = "Sanitized, user-facing view of hostenv environments.";
        };

        runtimeRoot = lib.mkOption {
          type = lib.types.str;
          default = "/run/hostenv";
          description = "Root directory for hostenv runtime sockets and state.";
        };

        logRoot = lib.mkOption {
          type = lib.types.str;
          default = "/var/log/hostenv";
          description = "Root directory for hostenv logs.";
        };
      };

      config = lib.mkIf (enabledEnvs != { }) {
        hostenv.publicEnvironments = lib.mkDefault (publicEnvs envs);

        # Ensure runtime directories exist for user-level services and upstream sockets.
        # Permissions align with the hostenv/nginx contract:
        # - user/<env>: owned by the env user, setgid to keep group 'users', private to others.
        # - nginx/<env>: owned by env user but group 'nginx' so system nginx can reach sockets; setgid keeps group.
        systemd.tmpfiles.rules =
          let
            base = [
              "d ${config.hostenv.runtimeRoot}         0755 root root  -"
              "d ${config.hostenv.runtimeRoot}/nginx   0755 root root  -"
              "d ${config.hostenv.runtimeRoot}/user    0755 root root  -"
            ];
            perEnv = lib.flatten (lib.mapAttrsToList
              (name: env:
                let user = envUser name env;
                in [
                  "d ${config.hostenv.runtimeRoot}/nginx/${user} 2770 ${user} nginx -"
                  "d ${config.hostenv.runtimeRoot}/user/${user}  2700 ${user} users -"
                ])
              enabledEnvs);
            logs = [
              "d ${config.hostenv.logRoot} 0755 root root -"
            ];
          in
          base ++ perEnv ++ logs;

        # Basic users/groups for environments (idempotent; real UID assignment handled in provider plan).
        users =
          let
            userEntries = lib.listToAttrs (lib.mapAttrsToList
              (name: env:
                let user = envUser name env;
                in {
                  name = user;
                  value = {
                    isNormalUser = true;
                    uid = env.uid or null;
                    group = user;
                    createHome = true;
                    openssh.authorizedKeys.keys = envKeys env;
                    linger = true;
                  };
                })
              enabledEnvs);
            groupEntries = lib.listToAttrs (lib.mapAttrsToList
              (name: env:
                let user = envUser name env;
                in { name = user; value = { }; })
              enabledEnvs);
          in
          {
            users = userEntries;
            groups = groupEntries;
          };

        # Expose a small facts file for debugging/ops.
        environment.etc."hostenv/facts".text = ''
          hostenv_environments=${builtins.toString (builtins.length (builtins.attrNames enabledEnvs))}
        '';
      };
    }
  ;
}
