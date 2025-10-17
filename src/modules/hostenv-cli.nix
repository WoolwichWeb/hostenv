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

  scriptType = lib.types.submodule (
    { config, name, ... }:
    {
      options = {
        exec = lib.mkOption {
          type = lib.types.functionTo lib.types.lines;
          description = "Shell code to execute when the hostenv sub-command is run.";
        };
        description = lib.mkOption {
          type = lib.types.str;
          description = "Description of the script.";
          default = "";
        };
        runtimeInputs = lib.mkOption {
          type = lib.types.listOf lib.types.package;
          description = "Packages to be available in PATH when the sub-command is run.";
          default = [ ];
          example = "[ pkgs.boxes pkgs.mysql ]";
        };
        makeScript = lib.mkOption {
          type = lib.types.bool;
          description = "Whether to make this sub-command a top-level script, e.g. the command `hostenv mysql` might become runnable using only `mysql` if mysql's `makeScript = true;`.";
          default = false;
        };
        internal = lib.mkOption {
          type = lib.types.bool;
          description = "Hides this sub-command from 'hostenv list'.";
          default = false;
        };
      };
    }
  );

  envJson = builtins.toJSON config.environments;
  defaultEnvName = config.defaultEnvironment or "";

  subCommandList = lib.attrsToList config.hostenv.subCommands;
  userFacingSubCommands = builtins.filter (cmd: !cmd.value.internal) subCommandList;
  # List of sub-command names, with internal names filtered out.
  # (so this list is good for user-facing output only)
  subCommandNames = builtins.map (cmd: cmd.name) userFacingSubCommands;
  scripts = builtins.filter (cmd: cmd.value.makeScript) subCommandList;

  hostenvCli = pog.pog {
    name = "hostenv";
    description = ''Interact with your hosting environments.

  SUBCOMMAND may be one of: ${builtins.concatStringsSep " " subCommandNames}
  
  Use 'hostenv list' to see a list of sub-commands and their descriptions
      '';
    version = "0.1.0";

    flags = [
      {
        name = "env";
        short = "e";
        description = "Target environment (defaults to current branch or '${defaultEnvName}')";
        completion = ''"$(command -v hostenv)" environments | jq -r 'keys[]' '';
        argument = "ENV";
      }
      { name = "force"; short = "f"; description = "Skip confirmations"; bool = true; }
      {
        name = "tty_mode";
        description = "TTY mode for remote commands: auto|on|off";
        argument = "MODE";
        default = "auto";
        completion = ''printf "%s\n" auto on off'';
      }
    ];

    arguments = [{ name = "subcommand"; }];
    argumentCompletion = ''"$(command -v hostenv)" __complete-subcommands'';

    runtimeInputs = with pkgs; [ jq openssh rsync boxes git gum ] ++ builtins.concatLists (builtins.map (v: v.value.runtimeInputs) subCommandList);

    bashBible = true;
    strict = true;

    script = helpers: with helpers; ''
      set -o pipefail

      # Derives the env name in this order:
      # 1) --env, 2) current branch, 3) defaultEnvironment
      env_name="''${env:-$(
        git symbolic-ref -q --short HEAD 2>/dev/null || true
      )}"
      if ${var.empty "env_name"}; then env_name="${defaultEnvName}"; fi

      # jq helpers over embedded JSON
      env_or_null() { jq -c --arg e "$1" '.[$e] // null' <<< '${envJson}'; }
      env_cfg="$(env_or_null "$env_name")"
      if ${var.empty "env_cfg"} || [ "$env_cfg" == "null" ]; then
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

      # If the user hasn't already setup a direnv configuration file
      # make one for them with sensible defaults.
      cfg="''${XDG_CONFIG_HOME:-$HOME/.config}/direnv/direnv.toml"

      if [ ! -f "$cfg" ]; then
        mkdir -p "$(dirname "$cfg")"
        cat >"$cfg" <<'EOF'
      [global]
      hide_env_diff = true
      EOF
      bold "Note: Created a direnv config file at: '$cfg'"
      green "You may change the settings there and hostenv will not overwrite them"
      echo
      fi

      banner() {
        echo
        cat <<RS | boxes -d whirly
      $emoji  Working in hostenv environment: "$env_name" ($typ) 

      Commands: ${builtins.concatStringsSep ", " (builtins.map (cmd: cmd.name) scripts ++ [ "hostenv" ])}
      RS
        echo
      }

      sub="''${1:-help}"; shift || true

      case "$sub" in

        ${builtins.concatStringsSep "\n" (builtins.map (cmd: ''
            ${cmd.name})
              ${cmd.value.exec helpers}
              ;;
        '') subCommandList)}

        "")
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
        scriptDerivations = builtins.map
          (cmd: pkgs.writeShellScriptBin cmd.name ''
            exec hostenv ${cmd.name} -- "$@"
          '')
          scripts;
      in
      pkgs.mkShell {

        buildInputs = with pkgs; [
          hostenvCli
          restic
          boxes
          jq
        ] ++ scriptDerivations;

        shellHook = ''
          currentBranch=$(git symbolic-ref --short HEAD)

          if [ ! "$currentBranch" = "${environmentName}" ]; then
            if git show-ref --quiet --branches "${environmentName}"; then
              git checkout "${environmentName}" || exit 1
            fi
          fi

          hostenv banner
        '';

      })
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
  options.hostenv.subCommands = lib.mkOption {
    type = lib.types.attrsOf scriptType;
    default = { };
    description = "Sub-commands available when running the `hostenv` CLI application.";
  };

  config.hostenv.devShells = lib.mkDefault hostenvShells;
  config.hostenv.apps = lib.mkDefault hostenvApps;
  config.hostenv.subCommands = {

    help = {
      exec = helpers: ''
        help
      '';
      description = "Print help for the hostenv command.";
    };

    banner = {
      exec = helpers: ''
        banner
      '';
      internal = true;
    };

    list = {
      exec = helpers: ''
        ${builtins.concatStringsSep "\n" (builtins.map (cmd: ''
          echo
          bold "${cmd.name}"
          ${lib.optionalString (cmd.value.description != "") ''
            cat <<'EOF'
          ${cmd.value.description}
          EOF
          ''}
        '') userFacingSubCommands)
        }
      '';
      description = "List available hostenv sub-commands. Then run one like 'hostenv SUBCOMMAND'.";
    };

    ssh = {
      exec = helpers: ''
        case "$tty_mode" in
          auto|"")
            if [ -t 0 ]; then SSH_TTY="-tt"; else SSH_TTY="-T"; fi
            ;;
          on|force|yes|true|1)
            SSH_TTY="-tt"
            ;;
          off|no|false|0)
            SSH_TTY="-T"
            ;;
          *)
            die "invalid --tty value: '$tty_mode' (use: auto|on|off)" 2
            ;;
        esac

        debug "tty_mode=$tty_mode ssh_flag=$SSH_TTY stdin_is_tty=$([ -t 0 ] && echo yes || echo no)"
        exec ssh $SSH_TTY "$user"@"$host" "$@"
      '';
      description = "Connect to the remote hostenv environment over SSH.";
    };

    app-log = {
      exec = helpers: ''
          case "$tty_mode" in
            auto|"")
              if [ -t 0 ]; then SSH_TTY="-tt"; else SSH_TTY="-T"; fi
              ;;
            on|force|yes|true|1)
              SSH_TTY="-tt"
              ;;
            off|no|false|0)
              SSH_TTY="-T"
              ;;
            *)
              die "invalid --tty value: '$tty_mode' (use: auto|on|off)" 2
              ;;
          esac

          debug "tty_mode=$tty_mode ssh_flag=$SSH_TTY stdin_is_tty=$([ -t 0 ] && echo yes || echo no)"
          exec ssh $SSH_TTY "$user"@"$host" bash -s -- "$@" <<'REMOTE'
        set -euo pipefail
        resize
        exec journalctl --user -xe "$@"
        REMOTE
      '';
      description = "View remote application logs.";
    };

    deploy = {
      exec = helpers: with helpers;
        ''
          currentBranch="$(git symbolic-ref --short HEAD)"
          debug "currentBranch=''${currentBranch}"
          if ${helpers.notFlag "force"}; then
            if [ ! "$currentBranch" = "$env_name" ]; then
              deploy_msg="$emoji  Deploy '$currentBranch' to environment '$env_name'?"
              default="--default=false"
            else
              deploy_msg="$emoji  Deploy '$currentBranch'?"
              default=""
            fi
            gum confirm $default --affirmative="Deploy" --negative="Cancel" "$deploy_msg" || exit 67
            unset deploy_msg
          else
            debug "--force detected, skipping confirmation"
          fi

          case "$tty_mode" in
            auto|"")
              if [ -t 0 ]; then SSH_TTY="-tt"; else SSH_TTY="-T"; fi
              ;;
            on|force|yes|true|1)
              SSH_TTY="-tt"
              ;;
            off|no|false|0)
              SSH_TTY="-T"
              ;;
            *)
              die "invalid --tty value: '$tty_mode' (use: auto|on|off)" 2
              ;;
          esac

          debug "tty_mode=$tty_mode ssh_flag=$SSH_TTY stdin_is_tty=$([ -t 0 ] && echo yes || echo no)"
          debug 'mkdir -p /home/'"$user"'/code/project'

          ${spinner {
            title = "Preparing remote directory for project code...";
            command = ''
              --show-error -- ssh $SSH_TTY "$user"@"$host" 'mkdir -p /home/'"$user"'/code/project'
            '';
          }}

          debug "rsync to $user@$host:/home/$user/code/project/"
          ${spinner {
            title = "Deploying project code...";
            command = ''
              --show-error -- rsync --delete \
                --exclude-from=../.gitignore --exclude-from=.gitignore \
                --exclude '.hostenv/result' --exclude '.devenv' \
                --exclude '*.sql' --exclude '*.sql.gz' --exclude '../web/sites/default/files' \
                -avz ../ "$user@$host:/home/$user/code/project/"
            '';
          }}

          # Remote build (with FOD auto-fix).
          debug "running ssh -T $user@$host bash -s -- $currentBranch $user <<'REMOTE_SCRIPT'"
          debug "ignoring SSH_TTY='$SSH_TTY' while building and activating remote. Using '-T'"
          ${spinner {
            title = "Building & activating $currentBranch...";
            command = ''
          --show-output --show-error -- ssh -T "$user@$host" bash -s -- "$currentBranch" "$user" <<'REMOTE_SCRIPT'
          set -euo pipefail

          branch="$1"
          ruser="$2"

          debug "moving into hostenv dir: /home/$ruser/code/project/.hostenv"
          cd "/home/$ruser/code/project/.hostenv"
          debug "running 'git reset --hard'"
          git reset --hard
          debug "running 'git clean -fdx'"
          git clean -fdx

          tries=0
          while :; do
            tries=$((tries + 1))
            set +e
            debug "running 'nix build .#$branch'
            out="$(nix build ".#$branch" 2>&1)"
            status=$?
            debug "build status was '$status'"
            set -e
            if [ "$status" -eq 0 ]; then
              debug "no FOD errors detected, continuing deployment"
              break
            fi

            # Extract FOD error message from nix build output.
            debug "got a FOD error"
            specified_hash="$(printf '%s' "$out" | sed -n 's/.*specified:[[:space:]]*\(sha256-[A-Za-z0-9+\/=]\+\).*/\1/p' | tail -n1)"
            got_hash="$(printf '%s' "$out" | sed -n 's/.*got:[[:space:]]*\(sha256-[A-Za-z0-9+\/=]\+\).*/\1/p' | tail -n1)"
            debug "FOD mismatch. Specified '$specified_hash' got '$got_hash'"

            if ${var.notEmpty "specified_hash"} && ${var.notEmpty "got_hash"}; then
              target="./hostenv.nix"
              if ${file.exists "target"}; then
                if sed -i "s|\"$specified_hash\"|\"$got_hash\"|" "$target"; then
                  echo "Updated $specified_hash -> $got_hash in $target"
                else
                  echo "Could not update $specified_hash in $target." >&2
                  printf '%s\n' "$out" >&2
                  exit 1
                fi
              else
                echo "ERROR: $target not found." >&2
                exit 1
              fi

              if [ "$tries" -ge 10 ]; then
                echo "ERROR: Too many FOD fix attempts (>=10). Bailing." >&2
                exit 1
              fi
              continue
            fi

            # Not a fixable FOD mismatch; print the original error.
            printf '%s\n' "$out" >&2
            exit 1
          done

          debug "final build"
          nix --quiet --quiet build ".#$branch"
          debug "running result/bin/activate"
          result/bin/activate
          debug "running 'nix profile install .#$branch --priority 4"
          nix profile install .#$branch --priority 4
          debug "remote deploy script finished"
          REMOTE_SCRIPT
          '';
          }}

          debug "rsync $user@$host:/home/$user/code/project/.hostenv/hostenv.nix → hostenv.nix"
          debug "the remote build updates FOD hashes and this downloads them"
          ${spinner {
            title = "Updating local hostenv.nix...";
            command = ''
              --show-error -- rsync -az \
                "$user@$host:/home/$user/code/project/.hostenv/hostenv.nix" \
                hostenv.nix
            '';
          }}

          green "✅ Deploy complete"
        '';
      description = ''
        Deploy your local codebase to the remote hostenv environment.
        Caution: Another developer could clobber your deployment if this is not used with care.'';
    };

    environment = {
      exec = helpers: ''
        jq <<< "$env_cfg"
      '';
      description = "Print hostenv environment information as JSON (defaults to your current environment).";
    };

    environments = {
      exec = helpers: ''
        jq <<< '${envJson}'
      '';
      description = "Print hostenv environment information as JSON (for all environments).";
    };

    files-dump = {
      exec = helpers: with helpers; ''
        debug "running rsync -az $user@$host:/home/$user/.local/share/{files,private_files} files/"
        mkdir -p files
        ${spinner {
          title = "Downloading files...";
          command = ''
            --show-error -- rsync -az \
              "$user@$host:/home/$user/.local/share/"{files,private_files} \
              files/
          '';
        }}
      '';
      description = "Download files from hostenv environment. Downloads to 'files/'. See also: files-up.";
    };

    files-up = {
      exec = helpers: with helpers; ''
        debug "running rsync -az files/{files,private_files} $user@$host:/home/$user/.local/share/"
        mkdir -p files
        ${spinner {
          title = "Uploading files...";
          command = ''
            --show-error -- rsync -az \
              files/{files,private_files} \
              "$user@$host:/home/$user/.local/share/"
          '';
        }}
      '';
      description = "Upload files to hostenv environment from 'files/'. See also: files-dump.";
    };

    __complete-subcommands = {
      exec = helpers: ''
          cat <<'EOF'
        ${builtins.concatStringsSep "\n" subCommandNames}
        EOF
      '';
      internal = true;
      description = "List internal commands for Bash completion.";
    };
  };
}

