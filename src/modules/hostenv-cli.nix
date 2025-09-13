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
  inherit (pkgs) pog;

  # @todo: Put this type in a `lib.types` module.
  appType = lib.types.submodule ({ name, ... }: {
    options = {
      type = lib.mkOption {
        type = lib.types.enum [ "app" ];
        default = "app";
        description = "Nix flake app type.";
      };
      program = lib.mkOption {
        type = lib.types.str;
        description = "Executable or derivation path to run.";
      };
    };
  });

  # JSON blobs we already embed today
  envJson = builtins.toJSON config.environments;
  defaultEnvName = config.defaultEnvironment or "";

  hostenvCli =
    let
      subCommands = [
        "help"
        "environments"
        "environment"
        "default-environment"
        "ssh"
        "deploy"
        "cex"
        "git-log"
        "app-log"
        "sync-from"
      ];
    in
    pog.pog {
      name = "hostenv";
      description = ''Interact with your hosting environments.

  SUBCOMMAND may be one of: ${builtins.concatStringsSep " " subCommands}
      '';
      version = "0.1.0";

      # Flags you actually use
      flags = [
        {
          name = "env";
          short = "e";
          description = "Target environment (defaults to current branch or '${defaultEnvName}')";
          # tab completion from embedded environments
          completion = "jq -r 'keys[]' <<< '${envJson}'";
          argument = "ENV";
        }
        { name = "force"; short = "f"; description = "Skip confirmations"; bool = true; }
      ];

      arguments = [{ name = "subcommand"; }];
      argumentCompletion = ''printf '%s\n' \
        "${builtins.concatStringsSep " " subCommands}"; :'';

      runtimeInputs = with pkgs; [ jq openssh rsync boxes git ];

      bashBible = true;
      strict = true;

      script = h: with h; ''
        set -o pipefail

        # Derives the env name in this order:
        # 1) --env, 2) current branch, 3) defaultEnvironment
        env_name="''${env:-$(
          git symbolic-ref -q --short HEAD 2>/dev/null || true
        )}"
        if [ -z "$env_name" ]; then env_name="${defaultEnvName}"; fi

        # jq helpers over embedded JSON
        env_or_null() { jq -c --arg e "$1" '.[$e] // null' <<< '${envJson}'; }
        env_cfg="$(env_or_null "$env_name")"
        if [ -z "$env_cfg" ] || [ "$env_cfg" == "null" ]; then
          die "Unknown environment: $env_name" 2
        fi

        user="$(jq -r '.hostenv.userName' <<< "$env_cfg")"
        host="$(jq -r '.hostenv.hostname' <<< "$env_cfg")"
        typ="$(jq -r '.type' <<< "$env_cfg")"
        emoji="$(
          case "$typ" in
            production)  echo "🚨" ;;
            testing)     echo "🧪" ;;
            development) echo "🛠️" ;;
            *)           echo "📦" ;;
          esac
        )"

        banner() {
          echo
          echo " $emoji  Hostenv - $env_name ($typ)" | boxes -d whirly
          echo
        }

        sub="''${1:-help}"; shift || true

        case "$sub" in
          environments)
            jq <<< '${envJson}'
            ;;

          environment)
            jq <<< "$env_cfg"
            # jq --arg e "$env_name" '.[$e] | {($e): .}' <<< '${envJson}'
            ;;

          default-environment)
            jq --arg e "${defaultEnvName}" '.[$e]' <<< '${envJson}'
            ;;

          ssh)
            exec ssh "$user"@"$host" "$@"
            ;;

          git-log)
            exec ssh -t "$user"@"$host" 'cd ~/code/project && git log'
            ;;

          app-log)
            exec ssh -t "$user"@"$host" 'resize; journalctl --user -xe '"$*"
            ;;

          cex)
            # remote drush cex with temp dir + rsync back
            dest="/tmp/hostenv-''${user}-cex"
            if ssh -q "$user"@"$host" bash -s -- "$dest" "$@" <<'RS'; then
          set -euo pipefail
          dest="$1"; shift
          [ -d "$dest" ] && rm -rf -- "$dest"
          mkdir -p -- "$dest"
          chmod o-rw -- "$dest"
          drush --quiet cex --destination="$dest" "$@"
        RS
              rsync -az --delete "$user@$host:$dest/" ../config/sync/
              # shellcheck disable=SC2016
              ssh -q "$user"@"$host" "rm -rf -- $(printf %q '$dest')" || true
              green "🗂️  Config exported from '$env_name'"
            else
              # shellcheck disable=SC2016
              ssh -q "$user"@"$host" "rm -rf -- $(printf %q '$dest')" || true
              die "Config export failed" 1
            fi
            ;;

          deploy)
            currentBranch="$(git symbolic-ref --short HEAD)"
            if ${notFlag "force"}; then
              ${confirm { prompt = "Deploy '$currentBranch'?"; exit_code = 69; }}
            fi
            ssh "$user"@"$host" 'mkdir -p /home/'"$user"'/code/project'
            rsync --delete \
              --exclude-from=../.gitignore \
              --exclude '.hostenv/result' --exclude '.devenv' \
              --exclude '*.sql' --exclude '*.sql.gz' \
              -avz ../ "$user@$host:/home/$user/code/project/"

            ${spinner {
              title   = "Building & activating $currentBranch...";
              command = ''
                ssh "$user"@"$host" bash -lc '
                  set -euo pipefail
                  cd /home/'"$user"'/code/project/.hostenv
                  git reset --hard
                  git clean -fdx
                  nix build .#'"$currentBranch"' && result/bin/activate
                '
              '';
            }}
            green "✅ Deploy complete"
            ;;

          sync-from)
            die "not implemented yet" 3
            ;;

          help|"")
            help
            ;;

          *)
            die "Unknown subcommand: $sub" 2
            ;;
        esac
      '';
    };

  hostenvShells = lib.mapAttrs
    (environmentName: environment:
      let
        cfg = environment.hostenv;

        mysql = pkgs.writeShellScriptBin "mysql" ''
          echo
          echo "${emoji}  Running mysql on '${environmentName}'"
          ssh -t ${cfg.userName}@${cfg.hostname} "mysql $@"
        '';

        mysqldump = pkgs.writeShellScriptBin "mysqldump" ''
          echo "${emoji}  Running mysql on '${environmentName}'"
          exec ssh ${cfg.userName}@${cfg.hostname} "mysqldump $@ | gzip" | gunzip
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

      in
      pkgs.mkShell {
        buildInputs = [ hostenvCli drush mysql mysqldump pkgs.restic pkgs.boxes ];
        shellHook = ''
          currentBranch=$(git symbolic-ref --short HEAD)

          if [ ! "$currentBranch" = "${environmentName}" ]; then
            if git show-ref --quiet --branches "${environmentName}"; then
              git checkout "${environmentName}" || exit 1
            fi
          fi

          echo
          cat <<'RS' | ${pkgs.boxes}/bin/boxes -d whirly
          ${emoji}  Working in hostenv environment: "${environmentName}" (${environment.type}) 

          Remote environment commands: drush mysql mysqldump
          RS
          echo
        '';
      }
    )
    (lib.filterAttrs (n: v: v.enable) config.environments)
  // {
    default = pkgs.mkShell {
      shellHook = ''
        ENV_JSON='${builtins.toJSON (lib.filterAttrs (n: v: v.enable) config.environments)}'

        if gitRef=$(git symbolic-ref -q --short HEAD 2>/dev/null); then
          # Is current branch a hostenv environment?
          if jq -e --arg ref "$gitRef" 'has($ref)' <<<"$ENV_JSON" >/dev/null; then
            : # OK
          else
            printf '%s\n\n%s\n' \
        "⚠️  Cannot load hostenv — '$gitRef' is not a hostenv environment" \
        "ℹ️  Add 'environments.$gitRef.enable = true;' to your hostenv.nix if you would like to make it a hostenv environment."
          fi
        else
          if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
            echo "⚠️  Cannot load hostenv — git is in detached HEAD state"
          else
            echo "⚠️  Cannot load hostenv — this is not a git repository"
          fi
        fi
      '';
    };
  };

  hostenvApps =
    let
      envs = pkgs.writeShellScriptBin "hostenv-environments" ''
        echo '${builtins.toJSON config.environments}' | ${pkgs.jq}/bin/jq
      '';
    in
    {
      # This is used in the template `.envrc` file for information about
      # available environments.
      hostenv-environments = {
        type = "app";
        program = "${envs}/bin/hostenv-environments";
      };
    };

in
{
  options.hostenv.devShells = lib.mkOption {
    type = lib.types.attrsOf lib.types.package;
    description = "Shell environments for working with hostenv projects";
  };
  options.hostenv.apps = lib.mkOption {
    type = lib.types.attrsOf appType;
    description = "Apps for working with hostenv projects";
  };

  config.hostenv.devShells = lib.mkDefault hostenvShells;
  config.hostenv.apps = lib.mkDefault hostenvApps;
}
