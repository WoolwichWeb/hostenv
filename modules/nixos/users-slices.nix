{ lib, pkgs, config, ... }:
let
  allEnvs = config.hostenv.environments or { };
  envs = lib.filterAttrs (_: env: env.enable or true) allEnvs;
  userOf = name: env: env.user or name;
  uidOf = env: env.extras.uid or null;
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
      users = lib.listToAttrs (lib.mapAttrsToList
        (name: env:
          let user = userOf name env;
          in {
            name = user;
            value = {
              isNormalUser = true;
              uid = lib.mkDefault (uidOf env);
              group = user;
              createHome = true;
              openssh.authorizedKeys.keys = env.extras.publicKeys or [ ];
              linger = true;
            };
          })
        envs);

      groups = lib.listToAttrs (lib.mapAttrsToList
        (name: env:
          let user = userOf name env;
          in { name = user; value = { }; })
        envs);
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
      slices = lib.listToAttrs (lib.mapAttrsToList
        (name: env:
          let user = userOf name env;
          in {
            name = "${user}.slice";
            value = {
              description = "${user} slice";
              sliceConfig = config.hostenv.sliceDefaults;
            };
          })
        envs);

      services = lib.listToAttrs (lib.filter (a: a.value != null) (lib.mapAttrsToList
        (name: env:
          let
            user = userOf name env;
            uid = uidOf env;
          in if uid == null then { name = ""; value = null; } else {
            name = "user@${toString uid}";
            value = {
              overrideStrategy = "asDropin";
              serviceConfig.Slice = "${user}.slice";
            };
          })
        envs));
    };
  };
}
