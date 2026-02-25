set -euo pipefail

sha="${COMIN_GIT_SHA:-}"
node="${HOSTENV_COMIN_NODE_NAME:-}"
api_base="${HOSTENV_COMIN_API_BASE_URL:-}"
token_file="${HOSTENV_COMIN_TOKEN_FILE:-}"
action_timeout="${HOSTENV_COMIN_ACTION_TIMEOUT:-}"

if [ -z "$sha" ]; then
  exit 0
fi

if [ -z "$node" ] || [ -z "$api_base" ] || [ -z "$token_file" ] || [ -z "$action_timeout" ]; then
  echo "hostenv-comin-activate: missing required environment configuration" >&2
  exit 1
fi

auth_header=()
if [ -r "$token_file" ]; then
  token="$(tr -d '\n' < "$token_file")"
  if [ -n "$token" ]; then
    auth_header=(-H "Authorization: Bearer $token")
  fi
fi

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

  local uid
  uid="$(id -u "$target_user")"
  runuser -u "$target_user" -- env XDG_RUNTIME_DIR="/run/user/$uid" systemctl --user "$@"
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
  curl -fsS "${auth_header[@]}" -H "Content-Type: application/json" \
    -d "$payload" "$api_base/api/deploy-jobs/$job_id/events" >/dev/null
}

run_activate() {
  local activate_user="$1"
  # shellcheck disable=SC2016
  timeout "${action_timeout}s" runuser -u "$activate_user" -- bash -lc '\
    set -euo pipefail; \
    export XDG_RUNTIME_DIR="/run/user/$(id -u)"; \
    if command -v activate >/dev/null 2>&1; then \
      exec activate; \
    elif [ -x "$HOME/.nix-profile/bin/activate" ]; then \
      exec "$HOME/.nix-profile/bin/activate"; \
    else \
      echo "activate script not found for user $USER" >&2; \
      exit 1; \
    fi'
}

run_deactivate() {
  local deactivate_user="$1"
  # shellcheck disable=SC2016
  timeout "${action_timeout}s" runuser -u "$deactivate_user" -- bash -lc '\
    set -euo pipefail; \
    export XDG_RUNTIME_DIR="/run/user/$(id -u)"; \
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
  from_node="$(jq -r '.fromNode // empty' <<<"$action")"
  to_node="$(jq -r '.toNode // empty' <<<"$action")"
  if [ -z "$user" ] || [ -z "$op" ]; then
    continue
  fi
  post_event "running" "$op" "Applying action for $user"
  case "$op" in
    activate|reload)
      rc=0
      run_activate "$user" || rc="$?"
      if [ "$rc" -ne 0 ]; then
        if [ "$rc" -eq 124 ] || [ "$rc" -eq 137 ]; then
          post_event "timed_out" "$op" "Activation timed out for $user after ${action_timeout}s"
        else
          post_event "failed" "$op" "Activation failed for $user"
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
        load_state="$(user_systemctl "$user" show -p LoadState "$unit_name" 2>/dev/null || true)"
        if [ "$load_state" != "LoadState=loaded" ]; then
          post_event "failed" "$op" "Migration backup unit is not loadable: user=$user migration_key=$migration_key unit_name=$unit_name"
          exit 1
        fi
        rc=0
        uid="$(id -u "$user")"
        timeout "${action_timeout}s" runuser -u "$user" -- env XDG_RUNTIME_DIR="/run/user/$uid" systemctl --user start --wait "$unit_name" || rc="$?"
        if [ "$rc" -ne 0 ]; then
          if [ "$rc" -eq 124 ] || [ "$rc" -eq 137 ]; then
            post_event "timed_out" "$op" "Migration backup timed out: user=$user migration_key=$migration_key unit_name=$unit_name after ${action_timeout}s"
          else
            post_event "failed" "$op" "Migration backup failed to start unit: user=$user migration_key=$migration_key unit_name=$unit_name"
          fi
          exit 1
        fi
        rc=0
        snapshot_id="$(runuser -u "$user" -- env MIGRATION_KEY="$migration_key" bash -lc '\
          set -euo pipefail; \
          restic_wrapper="$HOME/.local/bin/restic-${MIGRATION_KEY}"; \
          [ -x "$restic_wrapper" ] || exit 2; \
          "$restic_wrapper" snapshots --tag "$MIGRATION_KEY" --json | jq -r '\''sort_by(.time) | last | .id // empty'\''' 2>/dev/null)" || rc="$?"
        if [ "$rc" -ne 0 ]; then
          post_event "failed" "$op" "Migration backup snapshot lookup failed: user=$user migration_key=$migration_key unit_name=$unit_name"
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
          status_url="$api_base/api/deploy-jobs/$job_id/statuses?node=$node"
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
      run_activate "$user" || rc="$?"
      if [ "$rc" -ne 0 ]; then
        if [ "$rc" -eq 124 ] || [ "$rc" -eq 137 ]; then
          post_event "timed_out" "$op" "Restore activation timed out for $user after ${action_timeout}s"
        else
          post_event "failed" "$op" "Restore activation failed for $user"
        fi
        exit 1
      fi
      post_event "success" "$op" "Restore activation succeeded for $user"
      ;;
    deactivate)
      if [ -n "$to_node" ] && [ "$to_node" != "$node" ]; then
        post_event "waiting" "$op" "Waiting for activation on $to_node before deactivating $user"
        wait_start="$(date +%s)"
        while true; do
          status_url="$api_base/api/deploy-jobs/$job_id/statuses?node=$node"
          if status_json="$(curl -fsS "${auth_header[@]}" "$status_url")"; then
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
