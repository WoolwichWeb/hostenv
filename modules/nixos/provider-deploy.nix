{ ... }:
{
  flake.modules.nixos."provider-deploy" =
    { config, lib, pkgs, ... }:
    let
      cfg = config.services.provider-deploy;
      agentScript = pkgs.writeShellApplication {
        name = "provider-deploy-agent";
        runtimeInputs = [ pkgs.coreutils pkgs.curl pkgs.jq pkgs.nix pkgs.util-linux pkgs.shadow pkgs.websocat ];
        text = ''
          set -euo pipefail

          token_file="${cfg.nodeAuthTokenFile}"
          api_base="${cfg.providerApiBaseUrl}"
          node_name="${cfg.nodeName}"
          state_file="${cfg.stateFile}"
          action_timeout="${toString cfg.actionTimeoutSeconds}"
          reconnect_seconds="${toString cfg.reconnectSeconds}"
          ws_supported=1

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
          auth_cfg="$(mktemp)"
          trap 'rm -f "$auth_cfg"' EXIT
          printf 'header = "Authorization: Bearer %s"\n' "$token" > "$auth_cfg"

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

            curl -fsS \
              --config "$auth_cfg" \
              --max-time 20 \
              --header "Content-Type: application/json" \
              --request POST \
              --data "$(jq -cn --arg node "$node_name" --arg status "$status" --arg phase "$phase" --arg message "$message" --argjson payload "$payload_json" '{node:$node,status:$status,phase:$phase,message:$message,payload:$payload}')" \
              "$api_base/api/deploy-jobs/$job_id/events" >/dev/null
          }

          fetch_next_job() {
            local tmp
            local code
            tmp="$(mktemp)"
            code="$(curl -sS --config "$auth_cfg" --max-time 20 --output "$tmp" --write-out '%{http_code}' "$api_base/api/deploy-jobs/next?node=$node_name" || true)"
            case "$code" in
              200)
                cat "$tmp"
                rm -f "$tmp"
                return 0
                ;;
              404)
                rm -f "$tmp"
                return 1
                ;;
              *)
                rm -f "$tmp"
                return 2
                ;;
            esac
          }

          fetch_ws_job() {
            local ws_url
            local tmp
            local payload
            local auth_message
            if [ "$ws_supported" -ne 1 ]; then
              return 1
            fi
            if ! command -v websocat >/dev/null 2>&1; then
              ws_supported=0
              return 1
            fi
            if ! ws_url="$(ws_url_for_api)"; then
              ws_supported=0
              return 1
            fi
            tmp="$(mktemp)"
            auth_message="$(jq -cn --arg node "$node_name" --arg token "$token" '{type:"auth",node:$node,token:$token}')"
            timeout "$reconnect_seconds" \
              env HOSTENV_WS_AUTH_MESSAGE="$auth_message" sh -c 'printf "%s\n" "$HOSTENV_WS_AUTH_MESSAGE" | websocat "$1"' _ "$ws_url" >"$tmp" 2>/dev/null || true
            payload="$(jq -cs 'map(select(type=="object" and .type=="deploy_job" and (.jobId // "") != "" and .intent != null)) | if length == 0 then empty else (.[-1] | {jobId,commitSha,node,intent,actions}) end' "$tmp" 2>/dev/null || true)"
            rm -f "$tmp"
            if [ -n "$payload" ]; then
              printf '%s\n' "$payload"
              return 0
            fi
            return 1
          }

          newer_job_id() {
            local current_job_id="$1"
            local next_json
            local fetch_rc=0
            if ! next_json="$(fetch_next_job)"; then
              fetch_rc=$?
              if [ "$fetch_rc" -eq 1 ]; then
                printf '__JOB_GONE__\n'
                return 0
              fi
              return 1
            fi
            local next_id
            next_id="$(jq -r '.jobId // empty' <<<"$next_json")"
            if [ -n "$next_id" ] && [ "$next_id" != "$current_job_id" ]; then
              printf '%s\n' "$next_id"
              return 0
            fi
            return 1
          }

          run_cmd_cancelable() {
            local job_id="$1"
            local description="$2"
            shift 2
            local child
            local checks=0
            local interval="${toString cfg.reconnectSeconds}"

            setsid "$@" &
            child=$!

            while kill -0 "$child" >/dev/null 2>&1; do
              sleep 1
              checks=$((checks + 1))
              if [ "$checks" -ge "$interval" ]; then
                checks=0
                if superseding="$(newer_job_id "$job_id" 2>/dev/null || true)" && [ -n "$superseding" ]; then
                  kill -TERM -- "-$child" >/dev/null 2>&1 || true
                  wait "$child" >/dev/null 2>&1 || true
                  if [ "$superseding" = "__JOB_GONE__" ]; then
                    return 98
                  fi
                  return 99
                fi
              fi
            done

            wait "$child"
          }

          run_profile_action() {
            local job_id="$1"
            local op="$2"
            local user_name="$3"
            local store_path="$4"
            local source_node="$5"
            local migrations_csv="$6"
            local phase="$op"
            local profile="/nix/var/nix/profiles/per-user/$user_name/profile"
            local exec_path=""

            if ! is_safe_atom "$user_name"; then
              return 3
            fi
            if [ -n "$source_node" ] && ! is_safe_atom "$source_node"; then
              return 3
            fi

            if [ -n "$store_path" ]; then
              if ! run_cmd_cancelable "$job_id" "realise-$op-$user_name" nix store realise "$store_path"; then
                return $?
              fi
              if ! run_cmd_cancelable "$job_id" "set-profile-$user_name" nix-env -p "$profile" --set "$store_path"; then
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

            if [ "$op" = "restore" ] && [ -n "$source_node" ]; then
              local snapshot_tmp
              local snapshot_payload_file
              snapshot_tmp="$(mktemp)"
              local snapshot_code
              snapshot_code="$(curl -sS --config "$auth_cfg" --max-time 20 --output "$snapshot_tmp" --write-out '%{http_code}' "$api_base/api/deploy-jobs/$job_id/backup-snapshot?node=$node_name&source=$source_node&user=$user_name" || true)"
              if [ "$snapshot_code" = "200" ]; then
                snapshot_payload_file="$(mktemp)"
                jq -c '.payload // {}' "$snapshot_tmp" > "$snapshot_payload_file"
                if ! run_cmd_cancelable "$job_id" "$op-$user_name" timeout "$action_timeout" runuser -u "$user_name" -- env HOSTENV_RESTORE_SNAPSHOT_FILE="$snapshot_payload_file" HOSTENV_MIGRATIONS="$migrations_csv" "$exec_path"; then
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

            if ! run_cmd_cancelable "$job_id" "$op-$user_name" timeout "$action_timeout" runuser -u "$user_name" -- "$exec_path"; then
              return $?
            fi
          }

          handle_job() {
            local job_json="$1"
            local job_id
            local commit_sha
            local system_path

            job_id="$(jq -r '.jobId // empty' <<<"$job_json")"
            commit_sha="$(jq -r '.commitSha // empty' <<<"$job_json")"
            system_path="$(jq -r '.intent.systemPath // .intent.systemToplevel // empty' <<<"$job_json")"

            [ -n "$job_id" ] || return 1

            post_event "$job_id" "running" "intent" "Deploy job started" "{}" || true

            if [ -n "$system_path" ]; then
              post_event "$job_id" "running" "system" "Switching system profile" "{}" || true
              if ! run_cmd_cancelable "$job_id" "system-realise" nix store realise "$system_path"; then
                rc=$?
                if [ "$rc" -eq 99 ] || [ "$rc" -eq 98 ]; then
                  post_event "$job_id" "failed" "intent" "Superseded by newer job" '{"reason":"superseded"}' || true
                else
                  post_event "$job_id" "failed" "system" "Failed to realise system path" "{}" || true
                fi
                return "$rc"
              fi
              if ! run_cmd_cancelable "$job_id" "system-set" nix-env -p /nix/var/nix/profiles/system --set "$system_path"; then
                rc=$?
                if [ "$rc" -eq 99 ] || [ "$rc" -eq 98 ]; then
                  post_event "$job_id" "failed" "intent" "Superseded by newer job" '{"reason":"superseded"}' || true
                else
                  post_event "$job_id" "failed" "system" "Failed to set system profile" "{}" || true
                fi
                return "$rc"
              fi
              if ! run_cmd_cancelable "$job_id" "system-switch" "$system_path/bin/switch-to-configuration" switch; then
                rc=$?
                if [ "$rc" -eq 99 ] || [ "$rc" -eq 98 ]; then
                  post_event "$job_id" "failed" "intent" "Superseded by newer job" '{"reason":"superseded"}' || true
                else
                  post_event "$job_id" "failed" "system" "System switch failed" "{}" || true
                fi
                return "$rc"
              fi
              post_event "$job_id" "success" "system" "System switch complete" "{}" || true
            fi

            local action_count
            action_count="$(jq '.intent.actions | length' <<<"$job_json")"
            local idx=0
            while [ "$idx" -lt "$action_count" ]; do
              local action
              local op
              local user_name
              local store_path
              local source_node
              local migrations_csv

              action="$(jq -c ".intent.actions[$idx]" <<<"$job_json")"
              op="$(jq -r '.op // empty' <<<"$action")"
              user_name="$(jq -r '.user // empty' <<<"$action")"
              store_path="$(jq -r '.storePath // .envStorePath // .path // empty' <<<"$action")"
              source_node="$(jq -r '.fromNode // empty' <<<"$action")"
              migrations_csv="$(jq -r '(.migrations // []) | map(tostring) | join(",")' <<<"$action")"

              if [ -z "$op" ] || [ -z "$user_name" ]; then
                post_event "$job_id" "failed" "intent" "Malformed action payload" "{}" || true
                return 1
              fi

              post_event "$job_id" "running" "$op" "Running action $op for $user_name" "{}" || true

              if ! run_profile_action "$job_id" "$op" "$user_name" "$store_path" "$source_node" "$migrations_csv"; then
                rc=$?
                if [ "$rc" -eq 99 ] || [ "$rc" -eq 98 ]; then
                  post_event "$job_id" "failed" "intent" "Superseded by newer job" '{"reason":"superseded"}' || true
                elif [ "$rc" -eq 124 ]; then
                  post_event "$job_id" "timed_out" "$op" "Action timed out" "{}" || true
                elif [ "$rc" -eq 4 ]; then
                  post_event "$job_id" "failed" "$op" "Restore snapshot unavailable" "{}" || true
                elif [ "$rc" -eq 3 ]; then
                  post_event "$job_id" "failed" "$op" "Action executable not found" "{}" || true
                else
                  post_event "$job_id" "failed" "$op" "Action failed" "{}" || true
                fi
                return "$rc"
              fi

              if [ "$op" = "backup" ]; then
                post_event "$job_id" "success" "$op" "Action complete" "$(jq -cn --arg user "$user_name" '{user:$user}')" || true
              else
                post_event "$job_id" "success" "$op" "Action complete" "{}" || true
              fi
              idx=$((idx + 1))
            done

            post_event "$job_id" "success" "intent" "Deploy job complete" "{}" || true
            jq -cn --arg jobId "$job_id" --arg commitSha "$commit_sha" --arg updatedAt "$(date -u +%Y-%m-%dT%H:%M:%SZ)" '{lastAppliedJobId:$jobId,lastCommitSha:$commitSha,updatedAt:$updatedAt}' > "$state_file"
          }

          while true; do
            next_job_json="$(fetch_ws_job || true)"
            if [ -z "$next_job_json" ]; then
              next_job_json="$(fetch_next_job || true)"
            fi
            if [ -n "$next_job_json" ]; then
              current_job_id="$(jq -r '.jobId // empty' <<<"$next_job_json")"
              last_job_id=""
              if [ -s "$state_file" ]; then
                last_job_id="$(jq -r '.lastAppliedJobId // empty' "$state_file" 2>/dev/null || true)"
              fi
              if [ -n "$current_job_id" ] && [ "$current_job_id" != "$last_job_id" ]; then
                handle_job "$next_job_json" || true
              fi
            fi
            sleep "$reconnect_seconds"
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
