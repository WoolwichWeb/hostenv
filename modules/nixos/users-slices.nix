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
      users = lib.mapAttrs
        (name: env: {
          isNormalUser = true;
          uid = lib.mkDefault null;
          group = name;
          createHome = true;
          openssh.authorizedKeys.keys = env.extras.publicKeys or [ ];
          linger = true;
        })
        envs;

      groups = lib.mapAttrs (_: env: { }) envs;
    };

    # For information about slice hierarchy, see:
    # https://systemd.io/CONTROL_GROUP_INTERFACE/
    #
    # Quote:
    # > Slices may be used to group a number of services and scopes together
    # > in a hierarchial tree. Slices do not contain processes themselves,
    # > but the services and scopes contained in them do. Slices are named in
    # > the style of foobar-waldo.slice, where the path to the location of the
    # > slice in the tree is encoded in the name with “-“ as separator for
    # > the path components (foobar-waldo.slice is hence a subslice of
    # > foobar.slice). There’s one special slices defined, -.slice, which
    # > is the root slice of all slices (foobar.slice is hence subslice of
    # > -.slice). This is similar how in regular file paths, “/” denotes the
    # > root directory.
    systemd = {
      slices = lib.mapAttrs
        (name: env: {
          description = "${name} slice";
          sliceConfig = config.hostenv.sliceDefaults;
        })
        envs;

      services = lib.mapAttrs
        (name: env: {
          overrideStrategy = "asDropin";
          serviceConfig.Slice = "${name}-${toString (env.extras.uid or 0)}.slice";
        })
        envs;
    };
  };
}
