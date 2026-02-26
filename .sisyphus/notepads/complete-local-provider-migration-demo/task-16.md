# Task 16: Edge cases and cleanup hardening

## Summary
- Hardened `examples/local-provider-migration/run-demo.sh` for startup/boot edge cases and partial-failure cleanup.
- Cleanup now removes all demo resources on any non-zero exit (not just explicit `--cleanup`/abort).

## Changes made
- Added failure state tracking in `fail` and `fail_stage` (`DEMO_FAILED=1`).
- Updated `wait_for_ssh` to support optional PID monitoring and return early when VM process dies.
- Updated `wait_for_unix_socket` to support optional PID monitoring and return early when provider-service process dies.
- Enhanced `ensure_provider_service_running()`:
  - Validates an existing provider socket with `/health` before reusing it.
  - Removes stale socket before restart.
  - Fails with explicit message when provider-service exits before socket creation.
- Enhanced `start_vm()` boot handling:
  - Distinguishes VM boot process exit from plain SSH timeout.
  - Reports node log path in both cases.
- Updated top-level `cleanup()` to always remove hostctl profile and remove workdir on any non-zero exit code.

## QA evidence
- Cleanup scenario evidence: `.sisyphus/evidence/task-16-cleanup.log`
- Interrupt run log: `.sisyphus/evidence/task-16-run.log`
- Syntax check: `bash -n examples/local-provider-migration/run-demo.sh` (pass)
- LSP diagnostics: clean for `examples/local-provider-migration/run-demo.sh`

## QA notes
- In this workspace, full demo startup remains blocked by upstream provider flake input issue:
  - `error: input 'deploy-rs' follows a non-existent input 'hostenv/deploy-rs'`
- Despite that blocker, the interrupt cleanup verification records:
  - no leftover `qemu.*hostenv-demo` processes,
  - no `hostenv-local-demo` hostctl profile,
  - no remaining demo workdir.
