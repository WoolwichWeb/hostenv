## Task 12 - migration backup/restore integration

- Updated `examples/local-provider-migration/run-demo.sh` to preserve the node-a `generated/plan.json` snapshot before switching provider placement to node-b, then restore that snapshot immediately before the node-b webhook trigger. This ensures deploy intent derivation sees node-a as migration source and emits `backup` on node-a + `restore` on node-b.
- Added Task 12 runtime verification helpers in `run-demo.sh`:
  - `run_task12_backup_restore_verification()` validates provider job statuses include `backup` success on `node-a` and `restore` success on `node-b`, and verifies backup snapshot payload is present via `/api/deploy-jobs/<jobId>/backup-snapshot`.
  - `run_task12_migration_marker_verification()` validates node-b HTTP response contains `from-node-a` marker.
- Added Task 12 evidence paths used by the script:
  - `.sisyphus/evidence/task-12-backup.log`
  - `.sisyphus/evidence/task-12-migration-verify.log`

### QA status in this workspace

- Attempted automated demo run with required tools in nix shell.
- Execution blocked before node-a deploy by existing provider lock/input error:
  - `error: input 'deploy-rs' follows a non-existent input 'hostenv/deploy-rs'`
- Wrote blocked-run evidence to:
  - `.sisyphus/evidence/task-12-backup.log`
  - `.sisyphus/evidence/task-12-migration-verify.log`
