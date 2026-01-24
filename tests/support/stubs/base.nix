{ lib, pkgs }:
{
  options = {
    assertions = lib.mkOption {
      type = lib.types.listOf (lib.types.submodule {
        options = {
          assertion = lib.mkOption { type = lib.types.bool; };
          message = lib.mkOption { type = lib.types.str; };
        };
      });
      default = [ ];
    };

    hostenv = lib.mkOption {
      type = lib.types.submodule { freeformType = lib.types.attrs; };
      default = { };
    };

    services.nginx = lib.mkOption { type = lib.types.attrs; default = { }; };
    services.restic = lib.mkOption {
      type = lib.types.submodule { freeformType = lib.types.attrs; };
      default = { };
    };

    systemd.services = lib.mkOption { type = lib.types.attrs; default = { }; };
    systemd.timers = lib.mkOption { type = lib.types.attrs; default = { }; };
    systemd.paths = lib.mkOption { type = lib.types.attrs; default = { }; };
    systemd.slices = lib.mkOption { type = lib.types.attrs; default = { }; };

    users.users = lib.mkOption { type = lib.types.attrs; default = { }; };
    users.groups = lib.mkOption { type = lib.types.attrs; default = { }; };

    environment.variables = lib.mkOption { type = lib.types.attrs; default = { }; };
    environment.systemPackages = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [ ];
    };

    programs.ssh.package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.openssh;
    };

    activate = lib.mkOption { type = lib.types.str; default = ""; };
    profile = lib.mkOption { type = lib.types.listOf lib.types.path; default = [ ]; };
  };
}
