# Core hostenv commands.
# Only basic commands are defined here, others are defined by
# services and user projects (see: /modules/features/ for examples).
{ ... }:
{
  flake.modules.hostenv.tools-core-subcommands =
    { lib, config, ... }:
    let
      core = {
        ssh = {
          script = helpers: ''
            exec ssh $SSH_TTY "$user"@"$host" "$@"
          '';
          description = "Connect to the remote hostenv environment over SSH.";
          group = "Remote access";
          parsing = "passthrough";
          arguments = [
            {
              name = "arguments";
              description = "Arguments passed to ssh";
              variadic = true;
              completion = [ ];
            }
          ];
        };

        app-log = {
          script = helpers: ''
            exec ssh $SSH_TTY "$user"@"$host" bash -s -- "$@" <<'REMOTE'
            set -euo pipefail
            resize
            exec journalctl --user -xe "$@"
            REMOTE
          '';
          description = "View remote application logs.";
          group = "Remote access";
          parsing = "passthrough";
          arguments = [
            {
              name = "arguments";
              description = "Arguments passed to journalctl";
              variadic = true;
              completion = [ ];
            }
          ];
        };

        deploy = {
          script =
            helpers: with helpers; ''
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

              debug 'mkdir -p /home/'"$user"'/code/project'

              ${spinner {
                title = "Preparing remote directory for project code...";
                command = ''
                  --show-error -- ssh $SSH_TTY "$user"@"$host" 'mkdir -p /home/'"$user"'/code/project'
                '';
              }}

              project_root="$(git rev-parse --show-toplevel)"
              debug "rsync to $user@$host:/home/$user/code/project/"
              ${spinner {
                title = "Deploying project code...";
                command = ''
                  --show-error -- ${lib.getExe config.hostenv.projectUploadPackage} \
                    "$project_root/" "$user@$host:/home/$user/code/project/"
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
          group = "Deployment";
        };

        environment = {
          script = helpers: ''
            jq <<< "$env_cfg"
          '';
          description = "Print hostenv environment information as JSON (defaults to your current environment).";
          group = "Environment information";
        };

        environments = {
          script = helpers: ''
            jq <<< ${lib.escapeShellArg (builtins.toJSON config.exportedEnvironments)}
          '';
          description = "Print hostenv environment information as JSON (for all environments).";
          group = "Environment information";
        };

        files-dump = {
          script =
            helpers: with helpers; ''
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
          group = "File transfer";
        };

        files-up = {
          script =
            helpers: with helpers; ''
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
          group = "File transfer";
        };
      };
    in
    {
      # Use mkDefault so services/projects can override if needed
      config.hostenv.cli.commands = lib.mapAttrs (_: v: lib.mkDefault v) core;
    };
}
