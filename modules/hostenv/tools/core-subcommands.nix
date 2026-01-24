{ ... }:
{
  flake.modules.hostenv.tools-core-subcommands =
    { lib, config, ... }:
    let
      envJsonEval = builtins.tryEval (builtins.toJSON config.hostenv.publicEnvironments);
      envJson =
        assert envJsonEval.success
          || builtins.throw ''
          hostenv: config.hostenv.publicEnvironments must be JSON-serializable.

          Non-JSON data should be stored elsewhere (e.g. config.hostenv.*).
        '';
        envJsonEval.value;

      core = {
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
          exec = helpers: with helpers; ''
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
                    debug "ignoring SSH_TTY='$SSH_TTY' while building and activating remote. Using '-T'"
                    ${spinner {
                      title = "Building & activating $currentBranch...";
                      command = ''
                        --show-output --show-error -- ssh -T "$user@$host" bash -s -- "$currentBranch" "$user" <<'REMOTE_SCRIPT'
                        set -euo pipefail

                        branch="$1"
                        ruser="$2"

                        cd "/home/$ruser/code/project/.hostenv"
                        git reset --hard
                        git clean -fdx

                        tries=0
                        while :; do
                          tries=$((tries + 1))
                          set +e
                          out="$(nix build ".#$branch" 2>&1)"
                          status=$?
                          set -e
                          if [ "$status" -eq 0 ]; then
                            break
                          fi

                          specified_hash="$(printf '%s' "$out" | sed -n 's/.*specified:[[:space:]]*\(sha256-[A-Za-z0-9+\/=]\+\).*/\1/p' | tail -n1)"
                          got_hash="$(printf '%s' "$out" | sed -n 's/.*got:[[:space:]]*\(sha256-[A-Za-z0-9+\/=]\+\).*/\1/p' | tail -n1)"

                          if [ -n "$specified_hash" ] && [ -n "$got_hash" ]; then
                            target="./hostenv.nix"
                            if [ -f "$target" ]; then
                              sed -i "s|\"$specified_hash\"|\"$got_hash\"|" "$target"
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

                          printf '%s\n' "$out" >&2
                          exit 1
                        done

                        nix --quiet --quiet build ".#$branch"
                        result/bin/activate
                        nix profile install ".#$branch" --priority 4
            REMOTE_SCRIPT
                      '';
                    }}

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
              title = "Downloading files from '$env_name'...";
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
              title = "Uploading files to '$env_name'...";
              command = ''
                --show-error -- rsync -az \
                  files/{files,private_files} \
                  "$user@$host:/home/$user/.local/share/"
              '';
            }}
          '';
          description = "Upload files to hostenv environment from 'files/'. See also: files-dump.";
        };
      };
    in
    {
      # Use mkDefault so services/projects can override if needed
      config.hostenv.subCommands = lib.mapAttrs (_: v: lib.mkDefault v) core;
    };
}
