# devShells for hostenv projects.
#
# Very much a WIP. Major todos:
#
# 1. Move away from string-interpolated Bash script, to Haskell or Rust.
# 2. Allow modules to extend hostenv, e.g. drush and mysql should not be
#    included here, they should be in the Drupal and MySQL modules
#    respectively.
{ lib, config, pkgs, ... }:

let
  hostenvShells = lib.mapAttrs
    (environmentName: environment:
      let
        cfg = environment.hostenv;

        mysql = pkgs.writeShellScriptBin "mysql" ''
          echo
          echo "${emoji}  Running mysql on '${environmentName}'"
          ssh -t ${cfg.userName}@${cfg.hostname} "mysql $@"
        '';

        drush = pkgs.writeShellScriptBin "drush" ''
          echo
          echo "${emoji}  Running drush on '${environmentName}' "
          ssh -t ${cfg.userName}@${cfg.hostname} "drush $@"
        '';

        emojiMap = {
          production = "🚨";
          testing = "🧪";
          development = "🛠️";
        };
        emoji = emojiMap.${environment.type} or "📦";

        hostenvApp =
          let
            cexDestination = "/tmp/hostenv-${cfg.userName}-cex";
          in
          pkgs.writeShellScriptBin "hostenv" ''
            # Help
            Help() {
              echo
              echo "${emoji}  Hostenv - ${environmentName} (${environment.type})" | ${pkgs.boxes}/bin/boxes -d whirly
              echo
              echo "Usage:"
              echo "  hostenv sync-from ENVIRONMENT_NAME"
              echo "  hostenv environment [ENVIRONMENT_NAME]"
              echo "  hostenv [app-log|cex|default-environment|deploy|environments|environment-config|git-log|ssh]"
              echo
            }

            ExitError() {
              if test -n "$1"; then
                echo
                echo "$1"
                echo
              fi
              exit 1
            }

            SyncFrom() {
              ExitError "not implemented yet"

              set -x

              currentBranch=$(git symbolic-ref --short HEAD) || exit 1
              from=$(Environments | ${pkgs.jq}/bin/jq '.["'"$1"'"]') || exit 1
              to=$(Environments | ${pkgs.jq}/bin/jq '.["'"$currentBranch"'"]') || exit 1

              [ $from = null ] || [ $to = null ] && ExitError "error: could not determine environment configuration"

              # Destination file name for the backup source
              # environment file.
              backupsEnvTmp=$(echo $from | ${pkgs.jq}/bin/jq \
                --raw-output \
                '.hostenv | .userName + "_" + (now | tostring) + "_backupsenv"' \
              ) || exit 1

              fromBackupsEnv=$(echo $from | ${pkgs.jq}/bin/jq \
                --raw-output \
                '.hostenv | .userName + "@" + .hostname + ":" + .backupsEnvFile' \
              ) || exit 1

              toBackupsEnv=$(echo $to | ${pkgs.jq}/bin/jq \
                --raw-output \
                '.hostenv | .userName + "@" + .hostname + ":/tmp/'"$backupsEnvTmp"'"' \
              ) || exit 1
                  
              fromSsh=$(echo $from | ${pkgs.jq}/bin/jq \
                --raw-output \
                '.hostenv | .userName + "@" + .hostname' \
              ) || exit 1

              toSsh=$(echo $to | ${pkgs.jq}/bin/jq \
                --raw-output \
                '.hostenv | .userName + "@" + .hostname' \
              ) || exit 1

              fromEnvFile=$(echo $from | ${pkgs.jq}/bin/jq \
                --raw-output \
                '.hostenv | .backupsEnvFile' \
              ) || exit 1
                  
              # Take a backup in the from environment, and find the backup
              # repository URL.
              fromRepo=$(ssh "$fromSsh" bash <<EOF
            systemctl --user start restic-backups-drupal.service
            cat $fromEnvFile | grep 'RESTIC_REPOSITORY' | cut -d '"' -f 2
            EOF
            )

              # @todo: replace this, it's very slow and hard on space.
              ssh "$toSsh" "~/.local/bin/restic-drupal copy --from-repo $fromRepo latest"

              set +x
            }

            Cex() {
              ssh -q ${cfg.userName}@${cfg.hostname} bash <<EOF
            if [ -d ${cexDestination} ]; then
              rm -r ${cexDestination} || exit 1
            fi
            mkdir -p ${cexDestination} || exit 1
            chmod o-rw ${cexDestination} || exit 1
            drush --quiet cex --destination=${cexDestination} $@ || exit 1
            EOF
              if [ $? = 0 ]; then
                rsync -az --delete ${cfg.userName}@${cfg.hostname}:${cexDestination}/ ../config/sync/
              else
                # clean up
                ssh -t -q ${cfg.userName}@${cfg.hostname} 'rm -r ${cexDestination} 2> /dev/null' 2> /dev/null
                ExitError "config export failed"
                exit
              fi
              ssh -q ${cfg.userName}@${cfg.hostname} 'rm -r ${cexDestination}' || exit 1
              echo "🗂️ ⬇️  Config successfully exported from '${environmentName}'"
            }

            Environments() {
              echo '${builtins.toJSON config.environments}' | ${pkgs.jq}/bin/jq
            }

            DefaultEnvironment() {
              echo '${builtins.toJSON config.environments."${config.defaultEnvironment}"}' | ${pkgs.jq}/bin/jq
            }

            Environment() {
              environmentName=$([ "$1" = "" ] && echo "${environmentName}" || echo "$1")
              Environments | ${pkgs.jq}/bin/jq '.["'"$environmentName"'"] | { "'"$environmentName"'": . }'
            }

            HostenvConfig() {
              echo '${builtins.toJSON cfg}' | ${pkgs.jq}/bin/jq
            }

            Ssh() {
              exec ssh ${cfg.userName}@${cfg.hostname} "$@"
            }

            # Hacky remote builds, due to not having found how to
            # run 'result/bin/activate' on a remote build yet.
            Deploy() {
              currentBranch=$(git symbolic-ref --short HEAD) || exit 1

              ssh ${cfg.userName}@${cfg.hostname} 'mkdir -p /home/${cfg.userName}/code/project'
              rsync --delete --exclude-from=../.gitignore --exclude '.hostenv/result' --exclude '.devenv' --exclude '*.sql' --exclude '*.sql.gz' -avz ../ "${cfg.userName}@${cfg.hostname}:/home/${cfg.userName}/code/project/" || exit 1
              Ssh bash <<EOF
            cd /home/${cfg.userName}/code/project/.hostenv
            git reset --hard
            git clean --force
            nix build .#$currentBranch && result/bin/activate || exit 1
            nix profile remove .hostenv >/dev/null 2>&1
            nix profile install .#$currentBranch
            EOF
            }

            GitLog() {
              ssh -t ${cfg.userName}@${cfg.hostname} 'cd ~/code/project && git log'
            }

            AppLog() {
              ssh -t ${cfg.userName}@${cfg.hostname} "resize; journalctl --user -xe $@"
            }

            # Main
            case $1 in
              app-log)
                shift
                AppLog "$@"
                exit;;
              cex)
                shift
                Cex "$@"
                exit;;
              default-environment)
                DefaultEnvironment
                exit;;
              deploy)
                Deploy
                exit;;
              environment)
                Environment "$2"
                exit;;
              environments)
                Environments
                exit;;
              environment-config)
                HostenvConfig
                exit;;
              git-log)
                GitLog
                exit;;
              ssh)
                shift
                Ssh "$@"
                exit;;
              sync-from)
                echo "Syncing"
                SyncFrom "$2"
                exit;;
              help)
                Help
                exit;;
              "")
                Help
                exit;;
              *)
                echo "Invalid parameters"
                Help
                exit;;
            esac
          '';
      in
      pkgs.mkShell {
        buildInputs = [ hostenvApp drush mysql pkgs.restic ];
        shellHook = ''
          currentBranch=$(git symbolic-ref --short HEAD)

          if [ ! "$currentBranch" = "${environmentName}" ]; then
            if git show-ref --quiet --branches "${environmentName}"; then
              git checkout "${environmentName}" || exit 1
            fi
          fi

          echo
          echo "${emoji}  Working in hostenv environment: '"'${environmentName}'"' (${environment.type})" | ${pkgs.boxes}/bin/boxes -d whirly
          echo
        '';
      }
    )
    (lib.filterAttrs (n: v: v.enable) config.environments)
  // {
    default = pkgs.mkShell {
      shellHook = ''
        gitRef=$(git symbolic-ref -q --short HEAD)
        detachedHead="$?"
        [ ! "$detachedHead" = 0 ] \
          && echo "⚠️  Cannot load hostenv - git is in detached head state"
        refInOutputs=$([ ! -z "$gitRef" ] && echo "$envs" | grep -x "$gitRef" | wc -l || exit 0) || exit 1
        [ "$detachedHead" = 0 ] && [ "$refInOutputs" = 0 ] \
          && echo "
        ⚠️  Cannot load hostenv - '$gitRef' is not a hostenv environment

        ℹ️  Add 'environments.$gitRef.enable = true;' to your hostenv.nix
           if you would like to make it a hostenv environment.
        "
      '';
    };
  };

in
{
  options.hostenv.devShells = lib.mkOption {
    type = lib.types.attrsOf lib.types.package;
    description = "Shell environments for working with hostenv projects";
  };

  config.hostenv.devShells = hostenvShells;
}
