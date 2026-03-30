{ ... }:
{
  flake.modules.nixos."provider-deploy" =
    { config, lib, pkgs, ... }:
    let
      cfg = config.services.provider-deploy;
      runAsUserScript = pkgs.writeShellApplication {
        name = "provider-deploy-run-as-user";
        runtimeInputs = [ pkgs.coreutils pkgs.shadow pkgs.bash ];
        text = ''
          set -euo pipefail

          user_name="$1"
          shift

          user_uid="$(id -u "$user_name")"
          user_home="$(getent passwd "$user_name" | cut -d: -f6)"

          if [ -z "$user_home" ]; then
            echo "provider-deploy: could not determine home for user $user_name" >&2
            exit 1
          fi

          printf 'provider-deploy: run-as-user prepared user=%s uid=%s home=%s runtime_dir=%s config_dir=%s\n' \
            "$user_name" "$user_uid" "$user_home" "/run/user/$user_uid" "$user_home/.config" >&2

          systemctl start "user@$user_uid.service"
          for _ in $(seq 1 30); do
            if [ -S "/run/user/$user_uid/systemd/private" ] || [ -S "/run/user/$user_uid/bus" ]; then
              break
            fi
            sleep 1
          done

          printf 'provider-deploy: run-as-user sockets user=%s systemd_private=%s dbus_bus=%s\n' \
            "$user_name" \
            "$([ -S "/run/user/$user_uid/systemd/private" ] && printf yes || printf no)" \
            "$([ -S "/run/user/$user_uid/bus" ] && printf yes || printf no)" >&2

          exec runuser -u "$user_name" -- env \
            XDG_RUNTIME_DIR="/run/user/$user_uid" \
            DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$user_uid/bus" \
            "$@"
        '';
      };
      agentScript = pkgs.writeShellApplication {
        name = "provider-deploy-agent";
        runtimeInputs = with pkgs; [
          coreutils
          curl
          jq
          nix
          util-linux
          shadow
          websocat
        ];
        text = ''
          set -euo pipefail

          token_file="${cfg.nodeAuthTokenFile}"
          api_base="${cfg.providerApiBaseUrl}"
          node_name="${cfg.nodeName}"
          state_file="${cfg.stateFile}"
          action_timeout="${toString cfg.actionTimeoutSeconds}"
          reconnect_seconds="${toString cfg.reconnectSeconds}"
          if [ -z "$api_base" ] || [ -z "$node_name" ] || [ -z "$token_file" ]; then
            echo "provider-deploy: missing required configuration" >&2
            exit 1
          fi

          if [ ! -s "$token_file" ]; then
            echo "provider-deploy: token file missing or empty: $token_file" >&2
            exit 1
          fi

          token="$(tr -d '\n\r' < "$token_file")"
          mkdir -p "$(dirname "$state_file")"
          touch "$state_file"
          auth_cfg="$(mktemp)"
          trap 'rm -f "$auth_cfg"' EXIT
          printf 'header = "Authorization: Bearer %s"\n' "$token" > "$auth_cfg"
          event_stderr_max_lines=25
          command_stderr_file=""
          command_stdout_file=""
          command_exit_code=0

          ensure_state_file() {
            if [ ! -s "$state_file" ]; then
              printf '{"lastAppliedJobId":"","lastAppliedSignature":"","lastCommitSha":"","system":{},"users":{},"updatedAt":""}\n' > "$state_file"
            fi
          }

          job_signature() {
            local payload="$1"
            jq -c '{jobId:(.jobId // ""),commitSha:(.commitSha // ""),systemPath:(.intent.systemPath // .intent.systemToplevel // ""),actions:(.intent.actions // [])}' <<<"$payload" | sha256sum | cut -d' ' -f1
          }

          last_applied_signature() {
            if [ -s "$state_file" ]; then
              jq -r '.lastAppliedSignature // empty' "$state_file" 2>/dev/null || true
            fi
          }

          trim_stderr() {
            local path="$1"
            if [ -f "$path" ]; then
              tail -n "$event_stderr_max_lines" "$path" | jq -Rs '.'
            else
              printf '""\n'
            fi
          }

          mark_system_state() {
            local store_path="$1"
            ensure_state_file
            tmp="$(mktemp)"
            jq -c --arg storePath "$store_path" --arg updatedAt "$(date -u +%Y-%m-%dT%H:%M:%SZ)" '.system={storePath:$storePath,updatedAt:$updatedAt} | .updatedAt=$updatedAt' "$state_file" > "$tmp"
            mv "$tmp" "$state_file"
          }

          mark_user_state() {
            local user_name="$1"
            local store_path="$2"
            ensure_state_file
            tmp="$(mktemp)"
            jq -c --arg user "$user_name" --arg storePath "$store_path" --arg updatedAt "$(date -u +%Y-%m-%dT%H:%M:%SZ)" '.users[$user]={storePath:$storePath,updatedAt:$updatedAt} | .updatedAt=$updatedAt' "$state_file" > "$tmp"
            mv "$tmp" "$state_file"
          }

          cleanup_command_files() {
            if [ -n "$command_stdout_file" ]; then rm -f "$command_stdout_file"; fi
            if [ -n "$command_stderr_file" ]; then rm -f "$command_stderr_file"; fi
            command_stdout_file=""
            command_stderr_file=""
          }

          ensure_state_file

          ws_url_for_api() {
            case "$api_base" in
              https://*)
                printf 'wss://%s/api/deploy-jobs/ws?node=%s\n' "''${api_base#https://}" "$node_name"
                ;;
              http://*)
                printf 'ws://%s/api/deploy-jobs/ws?node=%s\n' "''${api_base#http://}" "$node_name"
                ;;
              *)
                return 1
                ;;
            esac
          }

          is_safe_atom() {
            local value="$1"
            case "$value" in
              ""|*[!a-z0-9._:-]*) return 1 ;;
              *) return 0 ;;
            esac
          }

          post_event() {
            local job_id="$1"
            local status="$2"
            local phase="$3"
            local message="$4"
            local payload_json="$5"
            local request_body=""
            local attempt=1
            local max_attempts=6
            local rc=0

            request_body="$(jq -cn --arg node "$node_name" --arg status "$status" --arg phase "$phase" --arg message "$message" --argjson payload "$payload_json" '{node:$node,status:$status,phase:$phase,message:$message,payload:$payload}')"

            while true; do
              if curl -fsS \
                --config "$auth_cfg" \
                --max-time 20 \
                --header "Content-Type: application/json" \
                --request POST \
                --data "$request_body" \
                "$api_base/api/deploy-jobs/$job_id/events" >/dev/null; then
                printf 'provider-deploy: posted event job=%s status=%s phase=%s\n' \
                  "$job_id" "$status" "$phase" >&2
                return 0
              fi

              rc=$?
              if [ "$attempt" -ge "$max_attempts" ]; then
                printf 'provider-deploy: failed to post event job=%s status=%s phase=%s attempts=%s\n' \
                  "$job_id" "$status" "$phase" "$attempt" >&2
                return "$rc"
              fi

              printf 'provider-deploy: retrying event post job=%s status=%s phase=%s attempt=%s/%s\n' \
                "$job_id" "$status" "$phase" "$attempt" "$max_attempts" >&2
              attempt=$((attempt + 1))
              sleep "$reconnect_seconds"
            done
          }

          post_event_with_command() {
            local job_id="$1"
            local status="$2"
            local phase="$3"
            local message="$4"
            local extra_payload="$5"
            local stderr_json
            stderr_json="$(trim_stderr "$command_stderr_file")"
            post_event "$job_id" "$status" "$phase" "$message" "$(jq -cn --argjson extra "$extra_payload" --argjson stderrSummary "$stderr_json" --argjson exitCode "$command_exit_code" '{exitCode:$exitCode,stderrSummary:$stderrSummary} + $extra')"
          }

          run_ws_session() {
            local ws_url
            local auth_message
            if ! ws_url="$(ws_url_for_api)"; then
              echo "provider-deploy: could not derive websocket URL from providerApiBaseUrl=$api_base" >&2
              return 1
            fi
            auth_message="$(jq -cn --arg node "$node_name" --arg token "$token" '{type:"auth",node:$node,token:$token}')"
            # shellcheck disable=SC2016
            if (env HOSTENV_WS_AUTH_MESSAGE="$auth_message" ${pkgs.bash}/bin/sh -c 'printf "%s\n" "$HOSTENV_WS_AUTH_MESSAGE"; tail -f /dev/null' | websocat "$ws_url" 2>/dev/null | while IFS= read -r line; do
              payload="$(jq -c 'select(type=="object" and .type=="deploy_job" and (.jobId // "") != "" and .intent != null) | {jobId,commitSha,node,intent,actions}' <<<"$line" 2>/dev/null || true)"
              if [ -z "$payload" ]; then
                continue
              fi
              current_job_id="$(jq -r '.jobId // empty' <<<"$payload")"
              current_signature="$(job_signature "$payload")"
              previous_signature="$(last_applied_signature)"
              if [ -n "$current_job_id" ] && [ "$current_signature" != "$previous_signature" ]; then
                handle_job "$payload" || true
              fi
            done); then
              return 0
            fi
            return 1
          }

          run_cmd_cancelable() {
            local job_id="$1"
            local _description="$2"
            shift 2
            local child
            command_stdout_file="$(mktemp)"
            command_stderr_file="$(mktemp)"
            command_exit_code=0

            setsid "$@" >"$command_stdout_file" 2>"$command_stderr_file" &
            child=$!

            while kill -0 "$child" >/dev/null 2>&1; do
              sleep 1
            done

            if wait "$child"; then
              command_exit_code=0
            else
              command_exit_code=$?
            fi
            return "$command_exit_code"
          }

          run_profile_action() {
            local job_id="$1"
            local op="$2"
            local user_name="$3"
            local store_path="$4"
            local source_node="$5"
            local to_node="$6"
            local migrations_csv="$7"
            local phase="$op"
            local profile="/nix/var/nix/profiles/per-user/$user_name/profile"
            local exec_path=""

            if ! is_safe_atom "$user_name"; then
              return 3
            fi
            if [ -n "$source_node" ] && ! is_safe_atom "$source_node"; then
              return 3
            fi
            if [ -n "$to_node" ] && ! is_safe_atom "$to_node"; then
              return 3
            fi

            if [ -n "$store_path" ]; then
              if ! run_cmd_cancelable "$job_id" "realise-$op-$user_name" nix-store --realise "$store_path"; then
                return $?
              fi
              if ! run_cmd_cancelable "$job_id" "set-profile-$user_name" ${runAsUserScript}/bin/provider-deploy-run-as-user "$user_name" nix-env -p "$profile" --set "$store_path"; then
                return $?
              fi
            fi

            if [ -x "$profile/bin/$op" ]; then
              exec_path="$profile/bin/$op"
            elif [ -n "$store_path" ] && [ -x "$store_path/bin/$op" ]; then
              exec_path="$store_path/bin/$op"
            else
              return 3
            fi

            printf 'provider-deploy: resolved action executable job=%s op=%s user=%s exec_path=%s profile=%s\n' \
              "$job_id" "$op" "$user_name" "$exec_path" "$profile" >&2

            if [ "$op" = "restore" ] && [ -n "$source_node" ]; then
              local snapshot_tmp
              local snapshot_payload_file
              snapshot_tmp="$(mktemp)"
              local snapshot_code
              snapshot_code="$(curl -sS --config "$auth_cfg" --max-time 20 --output "$snapshot_tmp" --write-out '%{http_code}' "$api_base/api/deploy-jobs/$job_id/backup-snapshot?node=$node_name&source=$source_node&user=$user_name" || true)"
              if [ "$snapshot_code" = "200" ]; then
                snapshot_payload_file="$(mktemp)"
                jq -c '.payload // {}' "$snapshot_tmp" > "$snapshot_payload_file"
                if ! run_cmd_cancelable "$job_id" "$op-$user_name" timeout "$action_timeout" ${runAsUserScript}/bin/provider-deploy-run-as-user "$user_name" env HOSTENV_RESTORE_SNAPSHOT_FILE="$snapshot_payload_file" HOSTENV_MIGRATIONS="$migrations_csv" "$exec_path"; then
                  rc=$?
                  rm -f "$snapshot_payload_file"
                  rm -f "$snapshot_tmp"
                  return "$rc"
                fi
                rm -f "$snapshot_payload_file"
              else
                rm -f "$snapshot_tmp"
                return 4
              fi
              rm -f "$snapshot_tmp"
              return 0
            fi

            if ! run_cmd_cancelable "$job_id" "$op-$user_name" timeout "$action_timeout" ${runAsUserScript}/bin/provider-deploy-run-as-user "$user_name" "$exec_path"; then
              return $?
            fi

            if [ -n "$store_path" ] && [ -d "$store_path/systemd/user" ]; then
              local default_wants_dir="$store_path/systemd/user/default.target.wants"
              local -a default_wanted_units=()
              local unit_path

              if [ -d "$default_wants_dir" ]; then
                for unit_path in "$default_wants_dir"/*; do
                  [ -e "$unit_path" ] || continue
                  default_wanted_units+=("$(basename "$unit_path")")
                done
              fi

              # shellcheck disable=SC2016
              if ! run_cmd_cancelable "$job_id" "verify-$op-$user_name" timeout "$action_timeout" ${runAsUserScript}/bin/provider-deploy-run-as-user "$user_name" env HOSTENV_SYSTEMCTL_BIN="${pkgs.systemd}/bin/systemctl" ${pkgs.bash}/bin/bash -lc '
                set -euo pipefail

                if [ ! -L "$XDG_CONFIG_HOME/systemd" ]; then
                  echo "provider-deploy: expected $XDG_CONFIG_HOME/systemd to be a symlink after activation" >&2
                  exit 1
                fi

                if [ ! -d "$XDG_CONFIG_HOME/systemd/user" ]; then
                  echo "provider-deploy: expected $XDG_CONFIG_HOME/systemd/user to exist after activation" >&2
                  exit 1
                fi

                if [ ! -L "$XDG_STATE_HOME/hostenv/current-state/systemd" ]; then
                  echo "provider-deploy: expected $XDG_STATE_HOME/hostenv/current-state/systemd to be a symlink after activation" >&2
                  exit 1
                fi

                "$HOSTENV_SYSTEMCTL_BIN" --user daemon-reload
                "$HOSTENV_SYSTEMCTL_BIN" --user reset-failed || true

                if [ "$#" -gt 0 ]; then
                  "$HOSTENV_SYSTEMCTL_BIN" --user start "$@"
                fi

                for unit_name in "$@"; do
                  load_state="$("$HOSTENV_SYSTEMCTL_BIN" --user show -p LoadState --value "$unit_name")"
                  if [ "$load_state" != "loaded" ]; then
                    echo "provider-deploy: unit $unit_name has unexpected LoadState=$load_state" >&2
                    exit 1
                  fi
                done
              ' _ "''${default_wanted_units[@]}"; then
                return $?
              fi
            fi

            if [ -n "$store_path" ] && { [ "$op" = "activate" ] || [ "$op" = "reload" ] || [ "$op" = "restore" ] || [ "$op" = "backup" ]; }; then
              mark_user_state "$user_name" "$store_path"
            fi
          }

          handle_job() {
            local job_json="$1"
            local job_id
            local commit_sha
            local system_path
            local action_count
            local top_level_action_count
            local intent_actions_json
            local top_level_actions_json

            job_id="$(jq -r '.jobId // empty' <<<"$job_json")"
            commit_sha="$(jq -r '.commitSha // empty' <<<"$job_json")"
            system_path="$(jq -r '.intent.systemPath // .intent.systemToplevel // empty' <<<"$job_json")"

            [ -n "$job_id" ] || return 1

            local current_signature
            current_signature="$(job_signature "$job_json")"

            action_count="$(jq '.intent.actions | length' <<<"$job_json")"
            top_level_action_count="$(jq '(.actions // []) | length' <<<"$job_json")"
            intent_actions_json="$(jq -c '.intent.actions // []' <<<"$job_json")"
            top_level_actions_json="$(jq -c '.actions // []' <<<"$job_json")"
            printf 'provider-deploy: received job=%s commit=%s system_path=%s intent_action_count=%s top_level_action_count=%s intent_actions=%s top_level_actions=%s\n' \
              "$job_id" "$commit_sha" "$system_path" "$action_count" "$top_level_action_count" "$intent_actions_json" "$top_level_actions_json" >&2
            if [ -z "$system_path" ] && [ "$action_count" -eq 0 ]; then
              ensure_state_file
              tmp="$(mktemp)"
              jq -c --arg signature "$current_signature" --arg updatedAt "$(date -u +%Y-%m-%dT%H:%M:%SZ)" '.lastAppliedSignature=$signature | .updatedAt=$updatedAt' "$state_file" > "$tmp"
              mv "$tmp" "$state_file"
              return 0
            fi

            post_event "$job_id" "running" "intent" "Deploy job started" "{}" || true

            if [ -n "$system_path" ]; then
              post_event "$job_id" "running" "system" "Switching system profile" "{}" || true
              if ! run_cmd_cancelable "$job_id" "system-realise" nix-store --realise "$system_path"; then
                rc=$?
                if [ "$rc" -eq 99 ] || [ "$rc" -eq 98 ]; then
                  post_event_with_command "$job_id" "failed" "intent" "Superseded by newer job" '{"reason":"superseded","step":"system-realise"}' || true
                else
                  post_event_with_command "$job_id" "failed" "system" "Failed to realise system path" '{"step":"system-realise"}' || true
                fi
                cleanup_command_files
                return "$rc"
              fi
              if ! run_cmd_cancelable "$job_id" "system-set" nix-env -p /nix/var/nix/profiles/system --set "$system_path"; then
                rc=$?
                if [ "$rc" -eq 99 ] || [ "$rc" -eq 98 ]; then
                  post_event_with_command "$job_id" "failed" "intent" "Superseded by newer job" '{"reason":"superseded","step":"system-set"}' || true
                else
                  post_event_with_command "$job_id" "failed" "system" "Failed to set system profile" '{"step":"system-set"}' || true
                fi
                cleanup_command_files
                return "$rc"
              fi
              if ! run_cmd_cancelable "$job_id" "system-switch" "$system_path/bin/switch-to-configuration" switch; then
                rc=$?
                if [ "$rc" -eq 99 ] || [ "$rc" -eq 98 ]; then
                  post_event_with_command "$job_id" "failed" "intent" "Superseded by newer job" '{"reason":"superseded","step":"system-switch"}' || true
                else
                  post_event_with_command "$job_id" "failed" "system" "System switch failed" '{"step":"system-switch"}' || true
                fi
                cleanup_command_files
                return "$rc"
              fi
              post_event_with_command "$job_id" "success" "system" "System switch complete" '{"step":"system-switch"}' || true
              printf 'provider-deploy: system switch complete job=%s action_count=%s\n' "$job_id" "$action_count" >&2
              mark_system_state "$system_path"
              cleanup_command_files
            fi

            local idx=0
            while [ "$idx" -lt "$action_count" ]; do
              local action
              local op
              local user_name
              local store_path
              local source_node
              local to_node
              local migrations_csv

              action="$(jq -c ".intent.actions[$idx]" <<<"$job_json")"
              op="$(jq -r '.op // empty' <<<"$action")"
              user_name="$(jq -r '.user // empty' <<<"$action")"
              store_path="$(jq -r '.storePath // .envStorePath // .path // empty' <<<"$action")"
              source_node="$(jq -r '.fromNode // empty' <<<"$action")"
              to_node="$(jq -r '.toNode // empty' <<<"$action")"
              migrations_csv="$(jq -r '(.migrations // []) | map(tostring) | join(",")' <<<"$action")"

              printf 'provider-deploy: preparing action job=%s idx=%s op=%s user=%s store_path=%s source=%s target=%s\n' \
                "$job_id" "$idx" "$op" "$user_name" "$store_path" "$source_node" "$to_node" >&2

              if [ -z "$op" ] || [ -z "$user_name" ]; then
                post_event "$job_id" "failed" "intent" "Malformed action payload" "{}" || true
                printf 'provider-deploy: malformed action payload job=%s idx=%s raw=%s\n' "$job_id" "$idx" "$action" >&2
                return 1
              fi

              post_event "$job_id" "running" "$op" "Running action $op for $user_name" "{}" || true

              if ! run_profile_action "$job_id" "$op" "$user_name" "$store_path" "$source_node" "$to_node" "$migrations_csv"; then
                rc=$?
                if [ "$rc" -eq 99 ] || [ "$rc" -eq 98 ]; then
                  post_event_with_command "$job_id" "failed" "intent" "Superseded by newer job" "$(jq -cn --arg reason superseded --arg op "$op" '{reason:$reason,op:$op}')" || true
                elif [ "$rc" -eq 124 ]; then
                  post_event_with_command "$job_id" "timed_out" "$op" "Action timed out" "$(jq -cn --arg op "$op" '{op:$op}')" || true
                elif [ "$rc" -eq 4 ]; then
                  post_event_with_command "$job_id" "failed" "$op" "Restore snapshot unavailable" "$(jq -cn --arg op "$op" '{op:$op}')" || true
                elif [ "$rc" -eq 3 ]; then
                  post_event_with_command "$job_id" "failed" "$op" "Action executable not found" "$(jq -cn --arg op "$op" '{op:$op}')" || true
                else
                  post_event_with_command "$job_id" "failed" "$op" "Action failed" "$(jq -cn --arg op "$op" '{op:$op}')" || true
                fi
                printf 'provider-deploy: action failed job=%s idx=%s op=%s user=%s rc=%s\n' "$job_id" "$idx" "$op" "$user_name" "$rc" >&2
                cleanup_command_files
                return "$rc"
              fi

              if [ "$op" = "backup" ]; then
                post_event_with_command "$job_id" "success" "$op" "Action complete" "$(jq -cn --arg user "$user_name" --arg op "$op" '{user:$user,op:$op}')" || true
              else
                post_event_with_command "$job_id" "success" "$op" "Action complete" "$(jq -cn --arg user "$user_name" --arg op "$op" '{user:$user,op:$op}')" || true
              fi
              printf 'provider-deploy: action succeeded job=%s idx=%s op=%s user=%s\n' "$job_id" "$idx" "$op" "$user_name" >&2
              cleanup_command_files
              idx=$((idx + 1))
            done

            printf 'provider-deploy: all actions complete job=%s action_count=%s\n' "$job_id" "$action_count" >&2
            post_event "$job_id" "success" "intent" "Deploy job complete" "{}" || true
            ensure_state_file
            tmp="$(mktemp)"
            
            # Write out deployment result to state.json.
            local final_updated_at
            local final_user_states
            final_updated_at="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
            final_user_states="$(jq -c --arg updatedAt "$final_updated_at" '(.intent.actions // []) | map(select(((.op // "") == "activate") or ((.op // "") == "reload") or ((.op // "") == "restore") or ((.op // "") == "backup")) | { key: (.user // ""), value: { storePath: (.storePath // .envStorePath // .path // ""), updatedAt: $updatedAt } } | select(.key != "" and .value.storePath != "")) | from_entries' <<<"$job_json")"
            jq -c --arg jobId "$job_id" --arg signature "$current_signature" --arg commitSha "$commit_sha" --arg updatedAt "$final_updated_at" --argjson userStates "$final_user_states" '.lastAppliedJobId=$jobId | .lastAppliedSignature=$signature | .lastCommitSha=$commitSha | .users = ((.users // {}) + $userStates) | .updatedAt=$updatedAt' "$state_file" > "$tmp"
            mv "$tmp" "$state_file"
          }

          # Continuously re-connect to socket after short delay.
          while true; do
            if ! run_ws_session; then
              sleep "$reconnect_seconds"
            fi
          done
        '';
      };
    in
    {
      options.services.provider-deploy = {
        enable = lib.mkEnableOption "hostenv provider deploy node agent";

        providerApiBaseUrl = lib.mkOption {
          type = lib.types.str;
          default = "";
          description = "Base URL for provider deploy APIs.";
        };

        nodeAuthTokenFile = lib.mkOption {
          type = lib.types.str;
          default = "/run/secrets/hostenv/provider_node_token";
          description = "Path to bearer token used by provider-deploy.";
        };

        nodeName = lib.mkOption {
          type = lib.types.str;
          default = "";
          description = "Logical node name used by provider-deploy.";
        };

        reconnectSeconds = lib.mkOption {
          type = lib.types.int;
          default = 5;
          description = "Reconnect/backoff delay in seconds.";
        };

        stateFile = lib.mkOption {
          type = lib.types.str;
          default = "/var/lib/provider-deploy/state.json";
          description = "Local provider-deploy state file path.";
        };

        actionTimeoutSeconds = lib.mkOption {
          type = lib.types.int;
          default = 1800;
          description = "Maximum seconds allowed per deploy action.";
        };
      };

      config = lib.mkIf cfg.enable {
        security.pam.services.runuser.setEnvironment = lib.mkForce true;

        assertions = [
          {
            assertion = cfg.providerApiBaseUrl != "";
            message = "services.provider-deploy.providerApiBaseUrl must be set when services.provider-deploy.enable is true.";
          }
          {
            assertion = cfg.nodeName != "";
            message = "services.provider-deploy.nodeName must be set when services.provider-deploy.enable is true.";
          }
          {
            assertion = cfg.reconnectSeconds > 0;
            message = "services.provider-deploy.reconnectSeconds must be greater than zero.";
          }
          {
            assertion = cfg.actionTimeoutSeconds > 0;
            message = "services.provider-deploy.actionTimeoutSeconds must be greater than zero.";
          }
        ];

        systemd.services.provider-deploy = {
          description = "Hostenv provider deploy node agent";
          wantedBy = [ "multi-user.target" ];
          after = [ "network-online.target" ];
          wants = [ "network-online.target" ];
          serviceConfig = {
            ExecStart = "${agentScript}/bin/provider-deploy-agent";
            Restart = "always";
            RestartSec = "5s";
            DynamicUser = false;
            User = "root";
            StateDirectory = "provider-deploy";
          };
        };
      };
    }
  ;
}
