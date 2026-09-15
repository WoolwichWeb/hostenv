# Core hostenv commands.
# Only basic commands are defined here, others are defined by
# services and user projects (see: /modules/features/ for examples).
{ ... }:
{
  flake.modules.hostenv.tools-core-subcommands =
    { lib, config, ... }:
    let
      # Gum 2.x uses Bubble Tea with input disabled for `gum spin`, while
      # Bubble Tea still probes DEC modes 2026/2027. The terminal replies then
      # leak into the calling shell. Keep interactive `gum confirm`, but use a
      # query-free shell spinner for non-interactive work.
      spinner = import ../../../lib/spinner.nix;

      core = {
        ssh = {
          script = helpers: ''
            exec ssh $hostenv_ssh_tty "$hostenv_user"@"$hostenv_host" "$@"
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
            hostenv_ssh_exec journalctl --user -xe "$@"
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
                if [ ! "$currentBranch" = "$hostenv_env_name" ]; then
                  deploy_msg="$hostenv_emoji  Deploy '$currentBranch' to environment '$hostenv_env_name'?"
                  default="--default=false"
                else
                  deploy_msg="$hostenv_emoji  Deploy '$currentBranch'?"
                  default=""
                fi
                gum confirm $default --affirmative="Deploy" --negative="Cancel" "$deploy_msg" || exit 67
                unset deploy_msg
              else
                debug "--force detected, skipping confirmation"
              fi

              debug 'mkdir -p /home/'"$hostenv_user"'/code/project'

              ${spinner {
                title = "Preparing remote directory for project code...";
                variant = "prepare";
                showError = true;
                command = ''
                  hostenv_ssh_run mkdir -p "/home/$hostenv_user/code/project"
                '';
              }}

              project_root="$(git rev-parse --show-toplevel)"
              debug "rsync to $hostenv_user@$hostenv_host:/home/$hostenv_user/code/project/"
              ${spinner {
                title = "Deploying project code...";
                variant = "upload";
                showError = true;
                command = ''
                  ${lib.getExe config.hostenv.projectUploadPackage} \
                    "$project_root/" "$hostenv_user@$hostenv_host:/home/$hostenv_user/code/project/"
                '';
              }}

              # Remote build (with FOD auto-fix).
              debug "ignoring hostenv_ssh_tty='$hostenv_ssh_tty' while building and activating remote. Using '-T'"
              ${spinner {
                title = "Building $currentBranch...";
                variant = "build";
                activationMarker = "__HOSTENV_NEKO_ACTIVATE__";
                activationTitle = "Activating $currentBranch...";
                showOutput = true;
                showError = true;
                command = ''
                              remote_command="$(hostenv_quote_remote_command bash -s -- "$currentBranch" "$hostenv_user")"
                              ssh -T "$hostenv_user@$hostenv_host" "$remote_command" <<'REMOTE_SCRIPT'
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
                              printf '%s\n' '__HOSTENV_NEKO_ACTIVATE__'
                              result/bin/activate
                              nix profile install ".#$branch" --priority 4
                  REMOTE_SCRIPT
                '';
              }}

              ${spinner {
                title = "Updating local hostenv.nix...";
                variant = "sync";
                finish = "sleep";
                successTitle = "✅ Deploy complete";
                showError = true;
                command = ''
                  rsync -az \
                    "$hostenv_user@$hostenv_host:/home/$hostenv_user/code/project/.hostenv/hostenv.nix" \
                    hostenv.nix
                '';
              }}

            '';
          description = ''
            Deploy your local codebase to the remote hostenv environment.
            Caution: Another developer could clobber your deployment if this is not used with care.'';
          group = "Deployment";
        };

        environment = {
          script = helpers: ''
            jq <<< "$hostenv_environment"
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
              debug "running rsync -az $hostenv_user@$hostenv_host:/home/$hostenv_user/.local/share/{files,private_files} files/"
              mkdir -p files
              ${spinner {
                title = "Downloading files from '$hostenv_env_name'...";
                variant = "download";
                showError = true;
                command = ''
                  rsync -az \
                    "$hostenv_user@$hostenv_host:/home/$hostenv_user/.local/share/"{files,private_files} \
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
              debug "running rsync -az files/{files,private_files} $hostenv_user@$hostenv_host:/home/$hostenv_user/.local/share/"
              mkdir -p files
              ${spinner {
                title = "Uploading files to '$hostenv_env_name'...";
                variant = "upload";
                showError = true;
                command = ''
                  rsync -az \
                    files/{files,private_files} \
                    "$hostenv_user@$hostenv_host:/home/$hostenv_user/.local/share/"
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
