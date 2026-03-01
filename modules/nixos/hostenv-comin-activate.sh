# shellcheck shell=bash
set -Eeuo pipefail

current_op=""
current_user=""
trap 'rc=$?; echo "hostenv-comin-activate: unhandled_error rc=$rc line=$LINENO op=${current_op:-<none>} user=${current_user:-<none>} cmd=${BASH_COMMAND:-<none>}" >&2' ERR

sha="${COMIN_GIT_SHA:-}"
node="${HOSTENV_COMIN_NODE_NAME:-}"
api_base="${HOSTENV_COMIN_API_BASE_URL:-}"
token_file="${HOSTENV_COMIN_TOKEN_FILE:-}"
action_timeout="${HOSTENV_COMIN_ACTION_TIMEOUT:-}"
bash_bin="$(command -v bash || true)"

if [ -z "$sha" ]; then
  exit 0
fi

if [ -z "$node" ] || [ -z "$api_base" ] || [ -z "$token_file" ] || [ -z "$action_timeout" ]; then
  echo "hostenv-comin-activate: missing required environment configuration" >&2
  exit 1
fi

echo "hostenv-comin-activate: node=$node sha=$sha action_timeout=$action_timeout" >&2

if [ -z "$bash_bin" ]; then
  bash_bin="/run/current-system/sw/bin/bash"
fi

auth_header=()
token=""
token_wait_deadline=$((SECONDS + 30))
while true; do
  if [ -r "$token_file" ]; then
    token="$(tr -d '\n' < "$token_file")"
    if [ -n "$token" ]; then
      break
    fi
  fi

  if [ "$SECONDS" -ge "$token_wait_deadline" ]; then
    break
  fi

  sleep 1
done

if [ -z "$token" ]; then
  echo "hostenv-comin-activate: token file is missing or empty: $token_file" >&2
  exit 1
fi

auth_header=(-H "Authorization: Bearer $token")

query_url="$api_base/api/deploy-intents/by-sha?sha=$sha&node=$node"
max_wait_seconds=30
retry_interval_seconds=2
intent_status=""
intent_json=""
retry_deadline=$((SECONDS + max_wait_seconds))
while true; do
  if ! intent_resp="$(curl -sS "${auth_header[@]}" -w $'\n%{http_code}' "$query_url")"; then
    echo "hostenv-comin-activate: failed to fetch deploy intent for node $node (sha=$sha)" >&2
    exit 1
  fi
  intent_status="${intent_resp##*$'\n'}"
  intent_json="${intent_resp%$'\n'*}"
  if [ "$intent_status" = "200" ]; then
    break
  fi
  if [ "$intent_status" = "404" ]; then
    if [ "$SECONDS" -ge "$retry_deadline" ]; then
      exit 0
    fi
    sleep "$retry_interval_seconds"
    continue
  fi
  echo "hostenv-comin-activate: unexpected deploy intent status $intent_status for node $node (sha=$sha)" >&2
  exit 1
done

job_id="$(jq -r '.jobId // empty' <<<"$intent_json")"
if [ -z "$job_id" ]; then
  echo "hostenv-comin-activate: deploy intent response missing jobId for node $node" >&2
  exit 1
fi

user_systemctl() {
  local target_user="$1"
  shift

  local output=""
  local machine_output=""
  local rc=0
  local machine_rc=0

  output="$(run_as_user "$target_user" systemctl --user "$@" 2>&1)" || rc="$?"
  if [ "$rc" -eq 0 ]; then
    if [ -n "$output" ]; then
      printf '%s\n' "$output"
    fi
    return 0
  fi

  machine_output="$(systemctl --machine="${target_user}@.host" --user "$@" 2>&1)" || machine_rc="$?"
  if [ "$machine_rc" -eq 0 ]; then
    if [ -n "$machine_output" ]; then
      printf '%s\n' "$machine_output"
    fi
    return 0
  fi

  if [ -n "$output" ]; then
    printf '%s\n' "$output" >&2
  fi
  if [ -n "$machine_output" ]; then
    printf '%s\n' "$machine_output" >&2
  fi

  if [ "$rc" -ne 0 ]; then
    return "$rc"
  fi
  return "$machine_rc"
}

resolve_user_home() {
  local target_user="$1"
  local home_dir=""
  while IFS=: read -r name _ _ _ _ home _; do
    if [ "$name" = "$target_user" ]; then
      home_dir="$home"
      break
    fi
  done < /etc/passwd

  if [ -z "$home_dir" ]; then
    if [ -n "${HOME:-}" ]; then
      home_dir="$HOME"
    else
      echo "hostenv-comin-activate: could not resolve home directory for $target_user" >&2
      return 1
    fi
  fi

  printf '%s' "$home_dir"
}

run_as_user() {
  local target_user="$1"
  shift

  local uid
  local home_dir
  uid="$(id -u "$target_user")"
  home_dir="$(resolve_user_home "$target_user")"
  runuser -u "$target_user" -- env \
    HOME="$home_dir" \
    USER="$target_user" \
    LOGNAME="$target_user" \
    XDG_CONFIG_HOME="$home_dir/.config" \
    XDG_STATE_HOME="$home_dir/.local/state" \
    XDG_DATA_HOME="$home_dir/.local/share" \
    XDG_CACHE_HOME="$home_dir/.cache" \
    XDG_RUNTIME_DIR="/run/user/$uid" \
    DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$uid/bus" \
    "$@"
}

summarize_error() {
  local output="$1"
  local fallback="$2"
  local max_len="${3:-220}"
  local one_line

  one_line="${output//$'\n'/; }"
  one_line="${one_line//$'\r'/ }"
  if [ -z "$one_line" ]; then
    printf '%s' "$fallback"
    return
  fi

  if [ "${#one_line}" -gt "$max_len" ]; then
    one_line="${one_line:0:max_len}..."
  fi
  printf '%s' "$one_line"
}

truncate_for_payload() {
  local output="$1"
  local max_chars="${2:-4000}"

  if [ "${#output}" -le "$max_chars" ]; then
    printf '%s' "$output"
    return
  fi

  printf '%s...<truncated>' "${output:0:max_chars}"
}

post_event() {
  local status="$1"
  local phase="$2"
  local message="$3"
  local event_payload_json="${4:-}"
  if [ -z "$job_id" ]; then
    return 0
  fi
  local payload
  local event_url
  local event_tmp
  local event_code
  payload="$(jq -nc \
    --arg node "$node" \
    --arg status "$status" \
    --arg phase "$phase" \
    --arg message "$message" \
    '{node:$node,status:$status,phase:($phase|select(length>0)),message:($message|select(length>0))}')"
  if [ -n "$event_payload_json" ] && [ "$event_payload_json" != "{}" ]; then
    payload="$(jq -nc \
      --argjson base "$payload" \
      --argjson eventPayload "$event_payload_json" \
      '$base + {payload:$eventPayload}')"
  fi

  event_url="$api_base/api/deploy-jobs/$job_id/events"
  event_tmp="$(mktemp)"
  if ! event_code="$(curl -sS "${auth_header[@]}" -H "Content-Type: application/json" \
    -d "$payload" -o "$event_tmp" -w '%{http_code}' "$event_url")"; then
    event_body="$(cat "$event_tmp" 2>/dev/null || true)"
    event_summary="$(summarize_error "$event_body" "event post request failed")"
    rm -f "$event_tmp"
    echo "hostenv-comin-activate: failed to post event request node=$node job_id=$job_id phase=$phase status=$status body=$event_summary" >&2
    return 1
  fi
  if [ "$event_code" -eq 409 ]; then
    event_body="$(cat "$event_tmp" 2>/dev/null || true)"
    event_summary="$(summarize_error "$event_body" "empty response body")"
    rm -f "$event_tmp"
    echo "hostenv-comin-activate: event already recorded node=$node job_id=$job_id phase=$phase status=$status http_code=$event_code body=$event_summary" >&2
    return 0
  fi
  if [ "$event_code" -lt 200 ] || [ "$event_code" -ge 300 ]; then
    event_body="$(cat "$event_tmp" 2>/dev/null || true)"
    event_summary="$(summarize_error "$event_body" "empty response body")"
    rm -f "$event_tmp"
    echo "hostenv-comin-activate: failed to post event node=$node job_id=$job_id phase=$phase status=$status http_code=$event_code body=$event_summary" >&2
    return 1
  fi
  rm -f "$event_tmp"
}

run_activate() {
  local activate_user="$1"
  local activate_uid
  local activate_home
  activate_uid="$(id -u "$activate_user")"
  activate_home="$(resolve_user_home "$activate_user")"
  # shellcheck disable=SC2016
  timeout "${action_timeout}s" runuser -u "$activate_user" -- env \
    HOME="$activate_home" \
    USER="$activate_user" \
    LOGNAME="$activate_user" \
    XDG_CONFIG_HOME="$activate_home/.config" \
    XDG_STATE_HOME="$activate_home/.local/state" \
    XDG_DATA_HOME="$activate_home/.local/share" \
    XDG_CACHE_HOME="$activate_home/.cache" \
    XDG_RUNTIME_DIR="/run/user/$activate_uid" \
    DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$activate_uid/bus" \
    "$bash_bin" -c '\
    set -euo pipefail; \
    cd "$HOME"; \
    profile_activate="/etc/profiles/per-user/$USER/bin/activate"; \
    user_profile_activate="/nix/var/nix/profiles/per-user/$USER/profile/bin/activate"; \
    if command -v activate >/dev/null 2>&1; then \
      exec activate; \
    elif [ -x "$profile_activate" ]; then \
      exec "$profile_activate"; \
    elif [ -x "$user_profile_activate" ]; then \
      exec "$user_profile_activate"; \
    elif [ -x "$HOME/.nix-profile/bin/activate" ]; then \
      exec "$HOME/.nix-profile/bin/activate"; \
    else \
      echo "activate script not found for user $USER" >&2; \
      exit 1; \
    fi'
}

run_deactivate() {
  local deactivate_user="$1"
  local deactivate_uid
  local deactivate_home
  deactivate_uid="$(id -u "$deactivate_user")"
  deactivate_home="$(resolve_user_home "$deactivate_user")"
  # shellcheck disable=SC2016
  timeout "${action_timeout}s" runuser -u "$deactivate_user" -- env \
    HOME="$deactivate_home" \
    USER="$deactivate_user" \
    LOGNAME="$deactivate_user" \
    XDG_CONFIG_HOME="$deactivate_home/.config" \
    XDG_STATE_HOME="$deactivate_home/.local/state" \
    XDG_DATA_HOME="$deactivate_home/.local/share" \
    XDG_CACHE_HOME="$deactivate_home/.cache" \
    XDG_RUNTIME_DIR="/run/user/$deactivate_uid" \
    DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$deactivate_uid/bus" \
    "$bash_bin" -c '\
    set -euo pipefail; \
    cd "$HOME"; \
    systemctl --user stop hostenv.target 2>/dev/null || true; \
    systemctl --user daemon-reload 2>/dev/null || true'
}

if ! jq -e '.intent.schemaVersion == 1 and (.intent.actions | type == "array")' >/dev/null 2>&1 <<<"$intent_json"; then
  post_event "failed" "intent" "Invalid deploy intent payload for node $node"
  echo "hostenv-comin-activate: invalid intent response for node $node" >&2
  exit 1
fi

actions_file="$(mktemp)"
trap 'rm -f "$actions_file"' EXIT
jq -c '.intent.actions[]?' <<<"$intent_json" > "$actions_file"

if [ ! -s "$actions_file" ]; then
  post_event "success" "intent" "No user actions for node $node"
  exit 0
fi

while IFS= read -r action; do
  user="$(jq -r '.user // empty' <<<"$action")"
  op="$(jq -r '.op // empty' <<<"$action")"
  current_user="$user"
  current_op="$op"
  from_node="$(jq -r '.fromNode // empty' <<<"$action")"
  to_node="$(jq -r '.toNode // empty' <<<"$action")"
  if [ -z "$user" ] || [ -z "$op" ]; then
    continue
  fi
  echo "hostenv-comin-activate: action_start node=$node op=$op user=$user from=${from_node:-<none>} to=${to_node:-<none>} timeout=$action_timeout" >&2
  post_event "running" "$op" "Applying action for $user"
  case "$op" in
    activate|reload)
      activate_output=""
      rc=0
      activate_output="$(run_activate "$user" 2>&1)" || rc="$?"
      if [ "$rc" -ne 0 ]; then
        if [ "$rc" -eq 124 ] || [ "$rc" -eq 137 ]; then
          post_event "timed_out" "$op" "Activation timed out for $user after ${action_timeout}s"
        else
          error_summary="$(summarize_error "$activate_output" "Activation command failed")"
          post_event "failed" "$op" "Activation failed for $user: $error_summary"
        fi
        exit 1
      fi
      post_event "success" "$op" "Activation succeeded for $user"
      ;;
    backup)
      migration_count="$(jq -r '(.migrations // []) | length' <<<"$action")"
      if [ "$migration_count" -eq 0 ]; then
        post_event "success" "$op" "No migration backups required for $user"
        continue
      fi

      snapshots='{}'
      while IFS= read -r migration_key; do
        [ -z "$migration_key" ] && continue
        unit_name="restic-backups-${migration_key}.service"
        wrapper_key="$migration_key"
        load_state="$(user_systemctl "$user" show -p LoadState "$unit_name" 2>&1 || true)"
        if [ "$load_state" != "LoadState=loaded" ]; then
          fallback_unit="restic-backups-${user}.service"
          fallback_state="$(user_systemctl "$user" show -p LoadState "$fallback_unit" 2>&1 || true)"
          if [ "$fallback_state" = "LoadState=loaded" ]; then
            unit_name="$fallback_unit"
            load_state="$fallback_state"
          fi
        fi
        if [ "$load_state" != "LoadState=loaded" ]; then
          load_state_summary="$(summarize_error "$load_state" "LoadState check failed")"
          available_units="$(user_systemctl "$user" list-unit-files 'restic-backups-*.service' --no-legend 2>/dev/null | awk '{print $1":"$2}' | paste -sd, - || true)"
          available_wrappers="$(run_as_user "$user" "$bash_bin" -lc "for f in \"\$HOME\"/.local/bin/restic-*; do [ -x \"\$f\" ] || continue; basename \"\$f\"; done" 2>/dev/null | paste -sd, - || true)"
          post_event "failed" "$op" "Migration backup unit is not loadable: user=$user migration_key=$migration_key unit_name=$unit_name ($load_state_summary) units=${available_units:-<none>} wrappers=${available_wrappers:-<none>}"
          exit 1
        fi
        rc=0
        start_output=""
        start_output="$(user_systemctl "$user" start --wait "$unit_name" 2>&1)" || rc="$?"
        unit_show_output="$(user_systemctl "$user" show "$unit_name" -p ActiveState -p SubState -p Result -p ExecMainCode -p ExecMainStatus -p StatusText 2>&1 || true)"
        if [ "$rc" -ne 0 ]; then
          if printf '%s\n' "$unit_show_output" | grep -q '^Result=success$'; then
            rc=0
          fi
        fi
        if [ "$rc" -ne 0 ]; then
          start_summary="$(summarize_error "$start_output" "Backup unit start failed")"
          status_output="$(user_systemctl "$user" status "$unit_name" --no-pager --full 2>&1 || true)"
          status_summary="$(summarize_error "$status_output" "status output unavailable" 1200)"
          unit_show_summary="$(summarize_error "$unit_show_output" "unit show unavailable" 1200)"
          error_payload="$(jq -cn \
            --arg user "$user" \
            --arg migrationKey "$migration_key" \
            --arg unitName "$unit_name" \
            --arg exitCode "$rc" \
            --arg startOutput "$(truncate_for_payload "$start_output" 4000)" \
            --arg unitStatus "$(truncate_for_payload "$status_output" 4000)" \
            --arg unitShow "$(truncate_for_payload "$unit_show_output" 4000)" \
            '{user:$user,migrationKey:$migrationKey,unitName:$unitName,exitCode:$exitCode,startOutput:$startOutput,unitStatus:$unitStatus,unitShow:$unitShow}')"
          if [ "$rc" -eq 124 ] || [ "$rc" -eq 137 ]; then
            post_event "timed_out" "$op" "Migration backup timed out: user=$user migration_key=$migration_key unit_name=$unit_name after ${action_timeout}s ($start_summary) status=($status_summary) show=($unit_show_summary)" "$error_payload"
          else
            post_event "failed" "$op" "Migration backup failed to start unit: user=$user migration_key=$migration_key unit_name=$unit_name ($start_summary) status=($status_summary) show=($unit_show_summary)" "$error_payload"
          fi
          exit 1
        fi
        rc=0
        snapshot_id="$(run_as_user "$user" env MIGRATION_KEY="$wrapper_key" "$bash_bin" -lc "set -euo pipefail; restic_wrapper=\"\$HOME/.local/bin/restic-\$MIGRATION_KEY\"; [ -x \"\$restic_wrapper\" ] || exit 2; \"\$restic_wrapper\" snapshots --tag \"\$MIGRATION_KEY\" --json | jq -r 'sort_by(.time) | last | .id // empty'" 2>/dev/null)" || rc="$?"
        if [ "$rc" -ne 0 ]; then
          post_event "failed" "$op" "Migration backup snapshot lookup failed: user=$user migration_key=$migration_key wrapper_key=$wrapper_key unit_name=$unit_name"
          exit 1
        fi
        if [ -z "$snapshot_id" ]; then
          post_event "failed" "$op" "Migration backup snapshot id missing: user=$user migration_key=$migration_key unit_name=$unit_name"
          exit 1
        fi
        snapshots="$(jq -c --arg key "$migration_key" --arg snapshot "$snapshot_id" '. + {($key):$snapshot}' <<<"$snapshots")"
      done < <(jq -r '.migrations[]? // empty' <<<"$action")

      backup_payload="$(jq -cn \
        --arg user "$user" \
        --arg sourceNode "$node" \
        --argjson snapshots "$snapshots" \
        '{user:$user,sourceNode:$sourceNode,snapshots:$snapshots}')"
      post_event "success" "$op" "Migration backups completed for $user" "$backup_payload"
      ;;
    restore)
      migration_count="$(jq -r '(.migrations // []) | length' <<<"$action")"
      snapshots='{}'
      if [ "$migration_count" -gt 0 ] && [ -n "$from_node" ] && [ "$from_node" != "$node" ]; then
        post_event "waiting" "$op" "Waiting for migration backups on $from_node before restoring $user"
        wait_start="$(date +%s)"
        while true; do
          status_url="$api_base/api/deploy-jobs/$job_id/statuses"
          snapshot_url="$api_base/api/deploy-jobs/$job_id/backup-snapshot?node=$node&source=$from_node&user=$user"
          if status_json="$(curl -fsS "${auth_header[@]}" "$status_url")"; then
            if jq -e --arg source "$from_node" '.statuses[]? | select(.node == $source and .phase == "backup" and (.status == "failed" or .status == "timed_out"))' >/dev/null 2>&1 <<<"$status_json"; then
              post_event "failed" "$op" "Source node $from_node reported backup failure while restoring $user"
              exit 1
            fi
          fi
          if snapshot_json="$(curl -fsS "${auth_header[@]}" "$snapshot_url")"; then
            snapshots="$(jq -c '.payload.snapshots // {}' <<<"$snapshot_json")"
          fi
          snapshot_missing=0
          while IFS= read -r migration_key; do
            [ -z "$migration_key" ] && continue
            if ! jq -e --arg key "$migration_key" '(.[$key] // empty) | type == "string" and length > 0' >/dev/null 2>&1 <<<"$snapshots"; then
              snapshot_missing=1
              break
            fi
          done < <(jq -r '.migrations[]? // empty' <<<"$action")
          if [ "$snapshot_missing" -eq 0 ]; then
            break
          fi
          now="$(date +%s)"
          elapsed=$((now - wait_start))
          if [ "$elapsed" -ge "$action_timeout" ]; then
            post_event "timed_out" "$op" "Timed out waiting for backups on $from_node before restoring $user"
            exit 1
          fi
          sleep 5
        done
      fi

      restore_plan="/run/hostenv/user/$user/restore/plan.json"
      if [ "$migration_count" -gt 0 ]; then
        if [ -z "$from_node" ]; then
          post_event "failed" "$op" "Missing source node for restore action ($user)"
          exit 1
        fi
        if [ "$snapshots" = "{}" ]; then
          post_event "failed" "$op" "Missing backup snapshots for restore action ($user)"
          exit 1
        fi
        restore_dir="$(dirname "$restore_plan")"
        mkdir -p "$restore_dir"
        chmod 0700 "$restore_dir"
        if ! chown "$user:$user" "$restore_dir" 2>/dev/null; then
          chown "$user:users" "$restore_dir"
        fi
        restore_payload="$(jq -cn --arg sourceNode "$from_node" --argjson snapshots "$snapshots" '{sourceNode:$sourceNode,snapshots:$snapshots}')"
        printf '%s\n' "$restore_payload" > "$restore_plan"
        chmod 0600 "$restore_plan"
        if ! chown "$user:$user" "$restore_plan" 2>/dev/null; then
          chown "$user:users" "$restore_plan"
        fi
      fi
      if [ "$migration_count" -gt 0 ]; then
        if ! jq -e '.snapshots | type == "object"' "$restore_plan" >/dev/null 2>&1; then
          post_event "failed" "$op" "Restore plan is missing snapshots object for $user"
          exit 1
        fi
        while IFS= read -r migration_key; do
          [ -z "$migration_key" ] && continue
          if ! jq -e --arg key "$migration_key" '(.snapshots[$key] // empty) | type == "string" and length > 0' "$restore_plan" >/dev/null 2>&1; then
            post_event "failed" "$op" "Restore plan missing snapshot id for migration key $migration_key ($user)"
            exit 1
          fi
        done < <(jq -r '.migrations[]? // empty' <<<"$action")
      fi
      rc=0
      restore_output="$(run_activate "$user" 2>&1)" || rc="$?"
      if [ "$rc" -ne 0 ]; then
        restore_summary="$(summarize_error "$restore_output" "Restore activation command failed")"
        echo "hostenv-comin-activate: restore_failed node=$node user=$user rc=$rc detail=$restore_summary" >&2
        echo "hostenv-comin-activate: restore_output_begin" >&2
        printf '%s\n' "$restore_output" >&2
        echo "hostenv-comin-activate: restore_output_end" >&2
        restore_payload="$(jq -cn \
          --arg user "$user" \
          --arg exitCode "$rc" \
          --arg detail "$(truncate_for_payload "$restore_output" 6000)" \
          '{user:$user,exitCode:$exitCode,detail:$detail}')"
        if [ "$rc" -eq 124 ] || [ "$rc" -eq 137 ]; then
          post_event "timed_out" "$op" "Restore activation timed out for $user after ${action_timeout}s" "$restore_payload"
        else
          post_event "failed" "$op" "Restore activation failed for $user: $restore_summary" "$restore_payload"
        fi
        exit 1
      fi
      post_event "success" "$op" "Restore activation succeeded for $user"
      ;;
    deactivate)
      if [ -n "$to_node" ] && [ "$to_node" != "$node" ]; then
        post_event "waiting" "$op" "Waiting for activation on $to_node before deactivating $user"
        wait_start="$(date +%s)"
        target_observed=0
        target_observe_timeout="${HOSTENV_COMIN_TARGET_OBSERVE_TIMEOUT:-120}"
        while true; do
          status_url="$api_base/api/deploy-jobs/$job_id/statuses"
          if status_json="$(curl -fsS "${auth_header[@]}" "$status_url")"; then
            if jq -e --arg target "$to_node" '.statuses[]? | select(.node == $target)' >/dev/null 2>&1 <<<"$status_json"; then
              target_observed=1
            fi
            if jq -e --arg target "$to_node" '.statuses[]? | select(.node == $target and (.phase == "activate" or .phase == "restore") and (.status == "failed" or .status == "timed_out"))' >/dev/null 2>&1 <<<"$status_json"; then
              post_event "failed" "$op" "Target node $to_node reported failure while deactivating $user"
              exit 1
            fi
            if jq -e --arg target "$to_node" '.statuses[]? | select(.node == $target and .phase == "activate" and .status == "success")' >/dev/null 2>&1 <<<"$status_json"; then
              break
            fi
          fi
          now="$(date +%s)"
          elapsed=$((now - wait_start))
          if [ "$target_observed" -eq 0 ] && [ "$elapsed" -ge "$target_observe_timeout" ]; then
            post_event "running" "$op" "Target node $to_node activation status not visible to node $node; proceeding with deactivation for $user"
            break
          fi
          if [ "$elapsed" -ge "$action_timeout" ]; then
            post_event "timed_out" "$op" "Timed out waiting for activation on $to_node before deactivating $user"
            exit 1
          fi
          sleep 5
        done
      fi
      rc=0
      run_deactivate "$user" || rc="$?"
      if [ "$rc" -ne 0 ]; then
        if [ "$rc" -eq 124 ] || [ "$rc" -eq 137 ]; then
          post_event "timed_out" "$op" "Deactivation timed out for $user after ${action_timeout}s"
        else
          post_event "failed" "$op" "Deactivation failed for $user"
        fi
        exit 1
      fi
      post_event "success" "$op" "Deactivation applied for $user"
      ;;
    *)
      post_event "failed" "$op" "Unsupported action: $op"
      exit 1
      ;;
  esac
done < "$actions_file"

post_event "success" "intent" "Completed all deploy actions for node $node"
