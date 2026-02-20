{ ... }:
{
  flake.modules.hostenv.tools-core-subcommands =
    { lib, config, pkgs, ... }:
    let
      envJsonEval = builtins.tryEval (builtins.toJSON config.hostenv.publicEnvironments);
      envJson =
        assert envJsonEval.success
          || builtins.throw ''
          hostenv: config.hostenv.publicEnvironments must be JSON-serializable.

          Non-JSON data should be stored elsewhere (e.g. config.hostenv.*).
        '';
        envJsonEval.value;
      projectSecrets = config.secrets or { };
      projectSecretsJsonEval = builtins.tryEval (builtins.toJSON projectSecrets);
      projectSecretsJson =
        assert projectSecretsJsonEval.success
          || builtins.throw ''
          hostenv: secrets configuration must be JSON-serializable.
        '';
        projectSecretsJsonEval.value;

      secretsHelpers = ''
        scope_enabled() { jq -r '.enable // false' <<<"$1"; }
        scope_file() { jq -r '.file // ""' <<<"$1"; }
        scope_keys() { jq -r '.keys[]?' <<<"$1"; }
        scope_provider_keys() { jq -r '.providerPublicKeys[]?' <<<"$1"; }
        unique_lines() { awk 'NF && !seen[$0]++'; }

        collect_project_user_ssh_keys() {
          jq -r '.[] | .users // {} | to_entries[] | .value.publicKeys[]?' <<<"$all_envs_json" | unique_lines
        }

        collect_env_user_ssh_keys() {
          env_json="$1"
          jq -r '.users // {} | to_entries[] | .value.publicKeys[]?' <<<"$env_json" | unique_lines
        }

        convert_ssh_to_age() {
          while IFS= read -r ssh_key; do
            [ -n "$ssh_key" ] || continue
            ssh-to-age 2>/dev/null <<<"$ssh_key" || true
          done | unique_lines
        }

        join_csv() {
          paste -sd, -
        }

        detect_project_root() {
          root="''${PRJ_ROOT:-$PWD}"
          if [ ! -f "$root/hostenv.nix" ] && [ -f "$root/.hostenv/hostenv.nix" ]; then
            root="$root/.hostenv"
          fi
          printf '%s\n' "$root"
        }

        scope_file_rel_or_default() {
          scope_json="$1"
          default_rel="$2"
          configured="$(scope_file "$scope_json")"
          if [ -n "$configured" ]; then
            case "$configured" in
              /*)
                die "hostenv secrets: absolute paths are not supported ('$configured'). Use a path relative to .hostenv." 2
                ;;
              *)
                printf '%s\n' "$configured"
                ;;
            esac
          else
            printf '%s\n' "$default_rel"
          fi
        }

        collect_scope_recipients_csv() {
          scope_json="$1"
          user_mode="$2"
          fallback_scope_json="$3"
          env_json="$4"

          recips_tmp="$(mktemp)"
          users_tmp="$(mktemp)"
          age_tmp="$(mktemp)"

          scope_provider_keys "$scope_json" | unique_lines >|"$recips_tmp"
          if [ ! -s "$recips_tmp" ]; then
            scope_provider_keys "$fallback_scope_json" | unique_lines >|"$recips_tmp"
          fi

          case "$user_mode" in
            project) collect_project_user_ssh_keys >|"$users_tmp" ;;
            env) collect_env_user_ssh_keys "$env_json" >|"$users_tmp" ;;
            *) : >|"$users_tmp" ;;
          esac

          convert_ssh_to_age <"$users_tmp" >|"$age_tmp"
          cat "$age_tmp" >>"$recips_tmp"

          recipients_csv="$(unique_lines <"$recips_tmp" | join_csv)"
          rm -f "$recips_tmp" "$users_tmp" "$age_tmp"
          printf '%s\n' "$recipients_csv"
        }

        append_creation_rule() {
          rel_path="$1"
          recipients_csv="$2"
          [ -n "$recipients_csv" ] || return 0
          escaped_rel="$(printf '%s' "$rel_path" | sed -e 's/[][(){}.^$+*?|\\/]/\\&/g')"
          printf "  - path_regex: '^%s$'\n" "$escaped_rel" >>"$tmp_sops_cfg"
          printf '    age: "%s"\n' "$recipients_csv" >>"$tmp_sops_cfg"
        }

        ensure_scope_file() {
          scope_json="$1"
          rel_path="$2"
          recipients_csv="$3"
          abs_path="$project_root/$rel_path"

          mkdir -p "$(dirname "$abs_path")"

          plain_tmp="$(mktemp)"
          if [ -f "$abs_path" ]; then
            if ! sops --decrypt "$abs_path" >|"$plain_tmp" 2>/dev/null; then
              cp "$abs_path" "$plain_tmp"
            fi
          else
            printf '{}\n' >|"$plain_tmp"
          fi

          while IFS= read -r secret_key; do
            [ -n "$secret_key" ] || continue
            yq -i ".\"$secret_key\" = (.\"$secret_key\" // \"\")" "$plain_tmp"
          done < <(scope_keys "$scope_json")

          cp "$plain_tmp" "$abs_path"
          rm -f "$plain_tmp"
          sops --encrypt --age "$recipients_csv" --in-place "$abs_path" >/dev/null
        }

        sync_hostenv_secrets() {
          project_root="$(detect_project_root)"
          all_envs_json='${envJson}'
          project_scope='${projectSecretsJson}'
          project_enabled="$(scope_enabled "$project_scope")"
          any_enabled="$project_enabled"

          mkdir -p "$project_root/secrets"
          tmp_sops_cfg="$(mktemp)"
          printf 'creation_rules:\n' >|"$tmp_sops_cfg"

          if [ "$project_enabled" = "true" ]; then
            project_rel="$(scope_file_rel_or_default "$project_scope" "secrets/project.yaml")"
            project_recipients_csv="$(collect_scope_recipients_csv "$project_scope" "project" "$project_scope" "{}")"
            [ -n "$project_recipients_csv" ] || die "hostenv secrets: project secrets enabled but no recipients available" 2
            append_creation_rule "$project_rel" "$project_recipients_csv"
          fi

          while IFS= read -r iter_env_name; do
            [ -n "$iter_env_name" ] || continue
            iter_env_cfg="$(jq -c --arg e "$iter_env_name" '.[$e] // null' <<<"$all_envs_json")"
            [ "$iter_env_cfg" != "null" ] || continue
            iter_scope="$(jq -c '.secrets // {}' <<<"$iter_env_cfg")"
            iter_enabled="$(scope_enabled "$iter_scope")"
            if [ "$iter_enabled" = "true" ]; then
              any_enabled=true
              iter_safe="$(jq -r '.hostenv.safeEnvironmentName // .hostenv.environmentName // empty' <<<"$iter_env_cfg")"
              [ -n "$iter_safe" ] || iter_safe="$iter_env_name"
              iter_rel="$(scope_file_rel_or_default "$iter_scope" "secrets/$iter_safe.yaml")"
              iter_recipients_csv="$(collect_scope_recipients_csv "$iter_scope" "env" "$project_scope" "$iter_env_cfg")"
              [ -n "$iter_recipients_csv" ] || die "hostenv secrets: environment '$iter_env_name' secrets are enabled but no recipients are available" 2
              append_creation_rule "$iter_rel" "$iter_recipients_csv"
            fi
          done < <(jq -r 'keys[]' <<<"$all_envs_json")

          if [ "$any_enabled" != "true" ]; then
            rm -f "$tmp_sops_cfg"
            return 0
          fi

          cp "$tmp_sops_cfg" "$project_root/secrets/.sops.yaml"
          rm -f "$tmp_sops_cfg"

          if [ "$project_enabled" = "true" ]; then
            project_rel="$(scope_file_rel_or_default "$project_scope" "secrets/project.yaml")"
            project_recipients_csv="$(collect_scope_recipients_csv "$project_scope" "project" "$project_scope" "{}")"
            ensure_scope_file "$project_scope" "$project_rel" "$project_recipients_csv"
          fi

          while IFS= read -r iter_env_name; do
            [ -n "$iter_env_name" ] || continue
            iter_env_cfg="$(jq -c --arg e "$iter_env_name" '.[$e] // null' <<<"$all_envs_json")"
            [ "$iter_env_cfg" != "null" ] || continue
            iter_scope="$(jq -c '.secrets // {}' <<<"$iter_env_cfg")"
            if [ "$(scope_enabled "$iter_scope")" = "true" ]; then
              iter_safe="$(jq -r '.hostenv.safeEnvironmentName // .hostenv.environmentName // empty' <<<"$iter_env_cfg")"
              [ -n "$iter_safe" ] || iter_safe="$iter_env_name"
              iter_rel="$(scope_file_rel_or_default "$iter_scope" "secrets/$iter_safe.yaml")"
              iter_recipients_csv="$(collect_scope_recipients_csv "$iter_scope" "env" "$project_scope" "$iter_env_cfg")"
              ensure_scope_file "$iter_scope" "$iter_rel" "$iter_recipients_csv"
            fi
          done < <(jq -r 'keys[]' <<<"$all_envs_json")
        }
      '';

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

        __sync-secrets = {
          exec = helpers: ''
            ${secretsHelpers}
            sync_hostenv_secrets
          '';
          internal = true;
          runtimeInputs = [ pkgs.sops pkgs.yq-go pkgs.ssh-to-age ];
        };

        secrets = {
          exec = helpers: ''
            ${secretsHelpers}
            sync_hostenv_secrets

            project_root="$(detect_project_root)"
            env_cfg_local="$env_cfg"
            all_envs_json='${envJson}'
            project_scope='${projectSecretsJson}'
            env_scope="$(jq -c '.secrets // {}' <<<"$env_cfg_local")"
            project_enabled="$(scope_enabled "$project_scope")"
            env_enabled="$(scope_enabled "$env_scope")"

            env_safe="$(jq -r '.hostenv.safeEnvironmentName // .hostenv.environmentName // empty' <<<"$env_cfg_local")"
            [ -n "$env_safe" ] || env_safe="$env_name"
            default_project_rel="secrets/project.yaml"
            default_env_rel="secrets/$env_safe.yaml"

            resolve_scope_rel() {
              scope_json="$1"
              default_rel="$2"
              scope_file_rel_or_default "$scope_json" "$default_rel"
            }

            target="''${1:-}"
            chosen_rel=""

            if [ "$target" = "project" ]; then
              [ "$project_enabled" = "true" ] || die "hostenv secrets: project secrets are not enabled" 2
              chosen_rel="$(resolve_scope_rel "$project_scope" "$default_project_rel")"
            elif [ -n "$target" ]; then
              target_env_cfg="$(jq -c --arg e "$target" '.[$e] // null' <<<"$all_envs_json")"
              [ "$target_env_cfg" != "null" ] || die "hostenv secrets: unknown environment '$target'" 2
              target_scope="$(jq -c '.secrets // {}' <<<"$target_env_cfg")"
              target_enabled="$(scope_enabled "$target_scope")"
              [ "$target_enabled" = "true" ] || die "hostenv secrets: environment '$target' secrets are not enabled" 2
              target_safe="$(jq -r '.hostenv.safeEnvironmentName // .hostenv.environmentName // empty' <<<"$target_env_cfg")"
              [ -n "$target_safe" ] || target_safe="$target"
              chosen_rel="$(scope_file_rel_or_default "$target_scope" "secrets/$target_safe.yaml")"
            else
              if [ "$env_enabled" = "true" ]; then
                env_rel="$(resolve_scope_rel "$env_scope" "$default_env_rel")"
                if sops --decrypt "$project_root/$env_rel" >/dev/null 2>&1; then
                  chosen_rel="$env_rel"
                fi
              fi

              if [ -z "$chosen_rel" ] && [ "$project_enabled" = "true" ]; then
                chosen_rel="$(resolve_scope_rel "$project_scope" "$default_project_rel")"
              fi

              if [ -z "$chosen_rel" ] && [ "$env_enabled" = "true" ]; then
                chosen_rel="$(resolve_scope_rel "$env_scope" "$default_env_rel")"
              fi
            fi

            [ -n "$chosen_rel" ] || die "hostenv secrets: no enabled secret scope found for this environment (try 'hostenv secrets project' or enable secrets)" 2

            cd "$project_root"
            exec sops "$chosen_rel"
          '';
          description = "Open project or environment SOPS secrets file (defaults by current branch).";
          runtimeInputs = [ pkgs.sops pkgs.yq-go pkgs.ssh-to-age ];
        };
      };
    in
    {
      # Use mkDefault so services/projects can override if needed
      config.hostenv.subCommands = lib.mapAttrs (_: v: lib.mkDefault v) core;
    };
}
