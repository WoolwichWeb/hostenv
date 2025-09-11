# Liberally cribs from:
# https://github.com/NixOS/nixpkgs/blob/nixos-24.11/nixos/modules/system/boot/systemd.nix
# And removes all the system-level options. Plus:
# https://github.com/NixOS/nixpkgs/blob/nixos-24.11/nixos/modules/system/boot/systemd/user.nix
#
# @todo: add support for restarting/reloading a service when an arbitrary
# derivation changes. As opposed to `reloadIfChanged` and `restartIfChanged`.

{ lib, config, pkgs, ... }:

with lib;

let
  cfg = config.systemd;
  utils = import (pkgs.path + "/nixos/lib/utils.nix") { inherit pkgs lib config; };
  systemdUtils = utils.systemdUtils;

  # Useful functions from upstream, also available:
  # makeUnit, generateUnits, mountToUnit, etc.
  inherit (systemdUtils.lib)
    targetToUnit
    serviceToUnit
    sliceToUnit
    socketToUnit
    timerToUnit
    pathToUnit
    ;

in
{
  options.systemd =
    {

      package = mkPackageOption pkgs "systemd" { };

      enableStrictShellChecks = mkEnableOption "" // { description = "Whether to run shellcheck on the generated scripts for systemd units."; };

      units = mkOption {
        description = "Definition of systemd units; see {manpage}`systemd.unit(5)`.";
        default = { };
        type = systemdUtils.types.units;
      };

      packages = mkOption {
        default = [ ];
        type = types.listOf types.package;
        example = literalExpression "[ pkgs.systemd-cryptsetup-generator ]";
        description = "Packages providing systemd units and hooks.";
      };

      targets = mkOption {
        default = { };
        type = systemdUtils.types.targets;
        description = "Definition of systemd target units; see {manpage}`systemd.target(5)`";
      };

      services = mkOption {
        default = { };
        type = systemdUtils.types.services;
        description = "Definition of systemd service units; see {manpage}`systemd.service(5)`.";
      };

      sockets = mkOption {
        default = { };
        type = systemdUtils.types.sockets;
        description = "Definition of systemd socket units; see {manpage}`systemd.socket(5)`.";
      };

      timers = mkOption {
        default = { };
        type = systemdUtils.types.timers;
        description = "Definition of systemd timer units; see {manpage}`systemd.timer(5)`.";
      };

      paths = mkOption {
        default = { };
        type = systemdUtils.types.paths;
        description = "Definition of systemd path units; see {manpage}`systemd.path(5)`.";
      };

      slices = mkOption {
        default = { };
        type = systemdUtils.types.slices;
        description = "Definition of slice configurations; see {manpage}`systemd.slice(5)`.";
      };

      globalEnvironment = mkOption {
        type = with types; attrsOf (nullOr (oneOf [ str path package ]));
        default = { };
        example = { TZ = "CET"; };
        description = ''
          Environment variables passed to *all* systemd units (for this user).
        '';
      };

    };

  config =
    let
      newUnits = pkgs.buildEnv {
        name = "systemd-user-${config.hostenv.userName}";
        paths = mapAttrsToList writeUnit cfg.units;
        # The paths above are the units, in postBuild we create those units'
        # dependencies using systemd's directory + symlink structure.
        postBuild =
          # Finally, concat the list of Bash scripts together.
          concatStringsSep "\n" (
            # Transform the set of named units into a list of string
            # Bash scripts.
            mapAttrsToList
              (name: unit:
                # Transforms a unit into a script to install that unit.
                concatStringsSep "\n" (
                  map (installDeps "wants" name) unit.wantedBy
                  ++ map (installDeps "requires" name) unit.requiredBy
                  ++ map (installDeps "upholds" name) unit.upheldBy
                ))
              # The units to map over.
              cfg.units
          );
      };

      writeUnit = name: unit: pkgs.writeTextDir "systemd/user/${name}" unit.text;

      # Make a script that will create a systemd dependency.
      # The naming is a bit odd, so think of it this way:
      # unitThat controlVerbs object, or to give a concrete example,
      # app.timer might be a unitThat wants app.service.
      installDeps = controlVerb: objectUnit: unitThat: ''
        mkdir -p $out/systemd/user/${unitThat}.${controlVerb}
        pushd $out/systemd/user/${unitThat}.${controlVerb}
        ln -s ../${objectUnit} ${objectUnit}
        popd
        ls -lah $out
        ls -lah $out/systemd/user/
      '';

    in
    {
      systemd.units =
        mapAttrs' (n: v: nameValuePair "${n}.path" (pathToUnit v)) cfg.paths
        // mapAttrs' (n: v: nameValuePair "${n}.service" (serviceToUnit v)) cfg.services
        // mapAttrs' (n: v: nameValuePair "${n}.slice" (sliceToUnit v)) cfg.slices
        // mapAttrs' (n: v: nameValuePair "${n}.socket" (socketToUnit v)) cfg.sockets
        // mapAttrs' (n: v: nameValuePair "${n}.target" (targetToUnit v)) cfg.targets
        // mapAttrs' (n: v: nameValuePair "${n}.timer" (timerToUnit v)) cfg.timers;

      # Generate timer units for all services that have a ‘startAt’ value.
      systemd.timers = mapAttrs
        (name: service: {
          wantedBy = [ "timers.target" ];
          timerConfig.OnCalendar = service.startAt;
        })
        (filterAttrs (name: service: service.startAt != [ ]) cfg.services);

      # This is simplistic compared to upstream nixpkgs implementation.
      # For example, see upstreams use of `restartTriggers` and `reloadTriggers`:
      # https://github.com/NixOS/nixpkgs/blob/release-24.11/nixos/lib/systemd-lib.nix#L439
      # https://github.com/NixOS/nixpkgs/blob/release-24.11/pkgs/by-name/sw/switch-to-configuration-ng/src/src/main.rs#L388
      activate =
        let
          # Make sure that we have an environment where we are likely to
          # successfully talk with systemd.
          ensureSystemd = ''
            env XDG_RUNTIME_DIR="''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}" \
                PATH="${dirOf cfg.package}/bin:$PATH" \
          '';

          systemctl = "${cfg.package}/bin/systemctl";
        in
        ''
          ## systemd activation script.

          # The following is snagged from Home Manager:
          # https://github.com/nix-community/home-manager/blob/9a9fef316ad191b3086edda465e850af282de4e0/modules/systemd.nix#L340C1-L368C26

          if [ ! -n "$XDG_CONFIG_HOME" ]; then
            echo '$XDG_CONFIG_HOME is not set, cannot continue'
            exit 1
          fi

          # Ensure the config directory exists, if this is a new user there's a good chance it hasn't been created yet.
          mkdir -p "$XDG_CONFIG_HOME"

          systemdStatus=$(${systemctl} --user is-system-running 2>&1 || true)

          if [ "$systemdStatus" == 'running' ] || [ "$systemdStatus" == 'degraded' ]; then
            if [ "$systemdStatus" == 'degraded' ]; then
              echo "⚠️ The user systemd session is degraded:"
              ${systemctl} --user --no-pager --state=failed
              echo "⚠️ Attempting to reload services anyway... ⚠️"
            fi

            mkdir -p "${config.hostenv.stateDir}/hostenv/current-state"
            oldUnitsDir="${config.hostenv.stateDir}/hostenv/current-state/systemd/user"

            if [ ! -e "$oldUnitsDir" ]; then
              oldUnitsDir=
            fi

            if [ -e "$XDG_CONFIG_HOME"/systemd ]; then
              if [ -L "$XDG_CONFIG_HOME"/systemd ]; then
                rm "$XDG_CONFIG_HOME"/systemd
              else
                echo "Couldn't unlink old systemd user directory at '$XDG_CONFIG_HOME/systemd'. Please check and delete it manually, then try again."
                echo "hostenv takes over the user systemd directory, so run 'ls -lah $XDG_CONFIG_HOME/systemd' to see what's in the directory, then 'mv $XDG_CONFIG_HOME/systemd' once you have checked there isn't anything important in that directory."
                exit 1
              fi
            fi

            ln -s "${newUnits}/systemd" "$XDG_CONFIG_HOME"/systemd

            # This compares the existing units with the ones in the new
            # derivation, with help from the excellent `sd-switch`.
            # https://git.sr.ht/~rycee/sd-switch
            ${ensureSystemd} ${lib.getExe pkgs.sd-switch} --user \
              ''${oldUnitsDir:+--old-units "$oldUnitsDir"} \
              --new-units "$XDG_CONFIG_HOME"/systemd/user

            # The new units become the old in the next activation.
            if [ -e "$XDG_STATE_HOME"/hostenv/current-state/systemd ]; then
              if [ -L "$XDG_STATE_HOME"/hostenv/current-state/systemd ]; then
                rm "$XDG_STATE_HOME"/hostenv/current-state/systemd
              else
                echo "Couldn't unlink old systemd user directory at '$XDG_STATE_HOME/hostenv/current-state/systemd'. Please check and delete it manually, then try again."
                exit 1
              fi
            fi

            ln -s "${newUnits}/systemd" "$XDG_STATE_HOME"/hostenv/current-state/systemd

            unset oldUnitsDir newUnits

          else
            echo "⚠️ User systemd daemon not running. The hosting environment cannot be started. ⚠️"
          fi

          unset systemdStatus
        '';

      profile = [ newUnits ];
    };

}
