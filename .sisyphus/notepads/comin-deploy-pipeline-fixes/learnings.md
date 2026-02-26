# Learnings

- Restic user units are `restic-backups-${name}.service` (no username prefix).
- `modules/nixos/hostenv-comin-activate.sh` currently relies on `/run/hostenv/user/${user}/backup/summary.json`, but no writer exists; snapshot ids should come from `restic snapshots --json`.
- Webhook pipeline can push before deploy intents/actions are persisted; nodes may fetch SHA before `/api/deploy-intents/by-sha` exists.
- Backup action must validate `LoadState=loaded` for `restic-backups-${migration_key}.service` before start, and derive snapshot id from `$HOME/.local/bin/restic-${migration_key} snapshots --tag "${migration_key}" --json`.
- In the unit harness, `timeout ... user_systemctl ...` needs a `user_systemctl` executable stub on `PATH`; GNU `timeout` cannot invoke shell functions directly.
- Production script backup start now uses `timeout` with `runuser ... systemctl --user` directly (with explicit `XDG_RUNTIME_DIR`) so no shell-function execution is attempted.
- The comin backup unit harness intentionally does not provide a `user_systemctl` executable, so any regression to invoking it as a binary fails the check.
