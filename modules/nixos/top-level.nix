{ config, pkgs, lib, ... }:
let
  envs = config.hostenv.environments or { };
  enabledEnvs = lib.filterAttrs (_: env: env.enable or true) envs;
in
{
  options.hostenv = {
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
    environment.etc."hostenv/environments.json".text = pkgs.lib.generators.toJSON { } enabledEnvs;

    # Ensure runtime directories exist for user-level services and upstream sockets.
    systemd.tmpfiles.rules =
      let
        mkRuntime = name: env: [
          "d ${config.hostenv.runtimeRoot}/user/${name} 0755 ${env.user or name} ${env.user or name} -"
          "d ${config.hostenv.runtimeRoot}/nginx/${name} 0755 ${env.user or name} ${env.user or name} -"
        ];
      in lib.flatten (lib.mapAttrsToList mkRuntime enabledEnvs);

    # Minimal logging layout; feature modules can add exporters/forwarders.
    systemd.tmpfiles.rules ++= [
      "d ${config.hostenv.logRoot} 0755 root root -"
    ];

    # Basic users/groups for environments (idempotent; real UID assignment handled in provider plan).
    users = {
      users = lib.mapAttrs (name: env: {
        isNormalUser = true;
        uid = env.extras.uid or null;
        group = name;
        createHome = true;
        openssh.authorizedKeys.keys = env.extras.publicKeys or [ ];
        linger = true;
      }) enabledEnvs;

      groups = lib.mapAttrs (_: _env: { }) enabledEnvs;
    };

    # Expose a small facts file for debugging/ops.
    environment.etc."hostenv/facts".text = ''
      hostenv_environments=${builtins.toString (builtins.length (builtins.attrNames enabledEnvs))}
    '';
  };
}
