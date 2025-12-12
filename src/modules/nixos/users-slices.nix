{ lib, pkgs, config, ... }:
let
  allEnvs = config.hostenv.environments or { };
  envs = lib.filterAttrs (_: env: env.enable or true) allEnvs;
in
{
  options = {
    hostenv.sliceDefaults = lib.mkOption {
      type = lib.types.attrs;
      default = {
        CPUAccounting = "yes";
        CPUQuota = "200%";
        MemoryAccounting = "yes";
        MemoryMax = "12G";
      };
      description = "Default slice limits applied per environment.";
    };
  };

  config = lib.mkIf (envs != { }) {
    users = {
      users = lib.mapAttrs (name: env: {
        isNormalUser = true;
        uid = lib.mkDefault null;
        group = name;
        createHome = true;
        openssh.authorizedKeys.keys = env.extras.publicKeys or [ ];
        linger = true;
      }) envs;

      groups = lib.mapAttrs (_: env: { }) envs;
    };

    systemd = {
      slices = lib.mapAttrs (name: env: {
        description = "${name} slice";
        sliceConfig = config.hostenv.sliceDefaults;
      }) envs // lib.mapAttrs (name: _env: { }) envs;

      services = lib.mapAttrs (name: env: {
        overrideStrategy = "asDropin";
        serviceConfig.Slice = "${name}-${toString (env.extras.uid or 0)}.slice";
      }) envs;
    };
  };
}
