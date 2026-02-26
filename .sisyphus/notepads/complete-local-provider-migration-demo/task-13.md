## Task 13 - end-to-end automated test

- Added `tests/integration/local-provider-migration.nix` as a comprehensive automated bash test wrapper (`local-provider-migration-test`).
- The test executes `examples/local-provider-migration/run-demo.sh --automated --cleanup --no-color` and asserts:
  - setup/deploy/migrate stages are present in logs,
  - provider-service and deploy status evidence files exist,
  - node-a and node-b deploy jobs report `final_state=success`,
  - migration backup/restore and marker checks report success.
- Registered the test in `tests/integration/default.nix` as `local-provider-migration-test`, enabling:
  - `nix build .#checks.x86_64-linux.local-provider-migration-test`

### QA status in this workspace

- `nix build .#checks.x86_64-linux.local-provider-migration-test` passes.
- Running `./result/bin/local-provider-migration-test` is currently blocked by an existing provider flake input issue in demo flow:
  - `error: input 'deploy-rs' follows a non-existent input 'hostenv/deploy-rs'`
- Blocked-run evidence written to:
  - `.sisyphus/evidence/task-13-e2e-test.log`
