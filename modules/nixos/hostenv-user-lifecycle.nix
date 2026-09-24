{ ... }:
{
  flake.modules.nixos.hostenv-user-lifecycle =
    { config, lib, pkgs, ... }:
    let
      retiredUsers = config.provider.retiredUsers;
      cleanupUser = name: user:
        let
          escapedName = lib.escapeShellArg name;
          lingerPath = lib.escapeShellArg "/var/lib/systemd/linger/${name}";
        in
        ''
          echo "retiring Hostenv user ${name}"

          if getent passwd ${escapedName} >/dev/null; then
            ${pkgs.systemd}/bin/loginctl disable-linger ${escapedName} || true
          else
            rm -f ${lingerPath}
          fi

          ${pkgs.systemd}/bin/systemctl stop ${lib.escapeShellArg "user@${toString user.uid}.service"} || true
        '';
    in
    {
      options.provider.retiredUsers = lib.mkOption {
        type = lib.types.attrsOf (lib.types.submodule ({ ... }: {
          options.uid = lib.mkOption {
            type = lib.types.int;
            description = "Reserved UID of a retired Hostenv environment user.";
          };
        }));
        default = { };
        internal = true;
        description = "Retired Hostenv users whose lingering systemd managers must be stopped before account removal.";
      };

      config = lib.mkIf (retiredUsers != { }) {
        system.activationScripts.hostenv-retired-users = {
          text = lib.concatStringsSep "\n" (lib.mapAttrsToList cleanupUser retiredUsers);
        };

        # NixOS removes immutable users in the `users` activation snippet.
        # Stop lingering managers first so switch-to-configuration does not
        # later try to reload a dead /run/user/<uid>/bus.
        system.activationScripts.users.deps = lib.mkAfter [ "hostenv-retired-users" ];
      };
    }
  ;
}
