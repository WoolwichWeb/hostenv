# Comin Deploy Pipeline Fixes

## TL;DR
> **Summary**: Fix migrations backup correctness on comin nodes, eliminate deploy-intent race by persisting before push, and hard-enforce consistent comin configuration between provider and provider-service.
> **Deliverables**: migration backup snapshot-id derivation, reordered webhook pipeline, stronger assertions, tests covering the new behavior, repo hygiene.
> **Effort**: Medium
> **Parallel**: YES - 2 waves
> **Critical Path**: Fix comin node backup/restore → Fix webhook save-before-push ordering → Add assertions + tests → `nix flake check`

## Context
### Original Request
- Summarize the code-review tool output and continue.

### Interview Summary
- Backward compatibility is explicitly NOT required.

### Metis Review (gaps addressed)
- Ensure snapshot IDs come from machine-readable restic output (not ad-hoc files).
- Persist deploy intents/actions durably before any push that triggers comin nodes.
- Enforce a single canonical comin enablement (no silent divergence).

## Work Objectives
### Core Objective
- Comin-driven migrations (backup/restore) succeed reliably and webhook-driven deploy jobs do not flake due to intent persistence races.

### Deliverables
- Correct restic migration unit naming + snapshot id extraction in `modules/nixos/hostenv-comin-activate.sh`.
- Webhook pipeline saves deploy intents/actions/events before pushing commits that comin nodes poll.
- Strict config assertions preventing `services.hostenv-provider.comin.enable` from being enabled when `provider.comin.enable` is disabled.
- Automated tests covering snapshot derivation logic, push ordering, and assertions.

### Definition of Done (verifiable)
- `nix flake check` passes.
- Provider-service Haskell tests pass (as invoked by flake checks) and include coverage for save-before-push ordering.
- Added unit/integration tests prove:
  - Migration backup uses `restic-backups-${migration_key}.service` (no username prefix) and derives a snapshot id from `restic snapshots --json`.
  - Webhook never performs `git push` before `saveDeployIntents/saveDeployActions` when job enters waiting.
  - Enabling provider-service comin without provider comin fails evaluation with a clear assertion message.

### Must Have
- Remove all fallback logic for old restic unit naming; fail fast on missing unit/wrapper.
- Snapshot id comes from restic repository metadata, filtered by the migration key tag.
- Persist deploy intents/actions/events before any push.

### Must NOT Have
- No backward-compat fallbacks (old unit names, old file locations, old plan schema paths).
- No silent success on missing migration prerequisites (unit missing, wrapper missing, snapshot missing).

## Verification Strategy
> ZERO HUMAN INTERVENTION — all verification is agent-executed.
- Tests-after: use existing `nix flake check` + add targeted tests (Nix unit + Haskell unit).
- Evidence files:
  - `.sisyphus/evidence/task-1-comin-backup-test.txt`
  - `.sisyphus/evidence/task-2-webhook-order-test.txt`
  - `.sisyphus/evidence/task-3-assertions-test.txt`

## Execution Strategy
### Parallel Execution Waves
Wave 1:
- Task 1 (comin node migration backup correctness)
- Task 2 (webhook save-before-push ordering)
- Task 3 (strict comin config assertions)

Wave 2:
- Task 4 (tests: comin backup snapshot derivation)
- Task 5 (tests: webhook ordering)
- Task 6 (repo hygiene: ignore build artifacts)

### Dependency Matrix (full)
- Task 4 depends on Task 1
- Task 5 depends on Task 2
- Task 6 independent

### Agent Dispatch Summary
- Wave 1: 3 tasks (unspecified-high + quick)
- Wave 2: 3 tasks (unspecified-high + quick)

## TODOs

- [ ] 1. Fix comin migration backup unit + snapshot id derivation

  **What to do**:
  - Edit `modules/nixos/hostenv-comin-activate.sh` backup action (`case "$op" in backup)`):
    - Replace the current unit discovery logic that builds `effective_name="$user-$migration_key"` and searches for `restic-backups-$effective_name.service`.
    - Set `unit_name="restic-backups-${migration_key}.service"`.
    - Validate the unit exists and is loadable before starting:
      - Use `user_systemctl "$user" show -p LoadState "$unit_name"` and require `LoadState=loaded`.
    - Start the unit with `user_systemctl "$user" start --wait "$unit_name"` (wrap in `timeout "${action_timeout}s" ...`).
      - Note: `user_systemctl` is a bash function defined earlier in `modules/nixos/hostenv-comin-activate.sh` (not a standalone command).
    - Remove snapshot-id reading from `/run/hostenv/user/${user}/backup/summary.json`.
    - Derive `snapshot_id` by querying restic directly using the per-backup wrapper script (same env vars as the unit):
      - Canonical wrapper path: `$HOME/.local/bin/restic-${migration_key}` (restic module activation installs symlinks there).
      - If not executable, fall back to `command -v "restic-${migration_key}"` inside a `runuser -u "$user" -- bash -lc ...` context.
      - If still not found, try `$HOME/.nix-profile/bin/restic-${migration_key}`.
      - Fail if the wrapper cannot be found/executed.
      - Run:
        - `"$restic_cmd" snapshots --tag "${migration_key}" --json | jq -r 'sort_by(.time) | last | .id // empty'`
      - Fail if the resulting snapshot id is empty.
    - Keep emitting the backup event payload `{user,sourceNode,snapshots}` so `backup-snapshot` API continues to work.
  - Ensure failure paths post an event with `status=failed` (or `timed_out`) and a message that includes `user`, `migration_key`, and `unit_name`.

  **Must NOT do**:
  - Do not try `restic-backups-$user-$migration_key.service` or strip `-migrate` suffixes.
  - Do not rely on `summary.json` or any other implicitly-created file.

  **Recommended Agent Profile**:
  - Category: `unspecified-high` — Bash correctness + production semantics.
  - Skills: []

  **Parallelization**: Can Parallel: YES | Wave 1 | Blocks: Task 4 | Blocked By: none

  **References**:
  - Node script: `modules/nixos/hostenv-comin-activate.sh`
  - Restic unit naming (user-level): `modules/features/restic.nix`
  - Migration key source (plan-time): `modules/entrypoints/provider/plan.nix` (derives `migrations` from restic backup keys ending with `-migrate`)
  - Migration key propagation (deploy intent): `modules/services/hostenv-provider-service/Hostenv/Provider/Service.hs` (reads `migrations` from plan JSON and emits backup/restore actions)
  - Migration restore expects restore plan: `modules/features/drupal.nix`, `modules/features/php-app.nix`

  **Acceptance Criteria**:
  - [ ] `nix flake check` passes.
  - [ ] Backup action fails fast if `restic-backups-${migration_key}.service` is not `LoadState=loaded`.
  - [ ] Backup action derives a non-empty snapshot id from restic JSON and includes it in the posted backup payload.

  **QA Scenarios**:
  ```
  Scenario: Migration backup derives snapshot id
    Tool: Bash
    Steps:
      1) Run the new unit/integration test added in Task 4.
    Expected: Test asserts `restic snapshots --json` parsing returns expected snapshot id.
    Evidence: .sisyphus/evidence/task-1-comin-backup-test.txt

  Scenario: Missing restic unit fails with clear message
    Tool: Bash
    Steps:
      1) Run the test variant that simulates `LoadState=not-found`.
    Expected: Non-zero exit and failure event message includes unit name and migration key.
    Evidence: .sisyphus/evidence/task-1-comin-backup-test-missing-unit.txt
  ```

  **Commit**: YES | Message: `fix(comin): derive migration snapshots via restic JSON` | Files: `modules/nixos/hostenv-comin-activate.sh`

- [ ] 2. Reorder webhook pipeline: persist intents/actions before push

  **What to do**:
  - Ensure *no* `git push` occurs before deploy intents/actions are persisted when the job is entering `waiting`.
  - Keep deploy-token revoke behavior unchanged (it is for the hosted project flake fetch, not provider repo push).
  - Refactor push so it happens outside `runWebhookWith`:
    - In `modules/services/hostenv-provider-service/Hostenv/Provider/Service.hs`:
      - Update `finalizeRepoUpdate` to:
        1) stage files, check `git status --porcelain` for changes
        2) if changes exist: `git commit -m ...`
        3) return `commitSha` (via `git rev-parse HEAD`) and a `committed=True` flag
        4) DO NOT run `git push` in this function
      - Rename `WebhookUpdateStatus` to reflect the new meaning:
        - Replace `WebhookUpdatePushed` with `WebhookUpdateCommitted`.
        - Update JSON encoding string from `"pushed"` to `"committed"`.
        - Update `shouldWaitForCallbacks` to use `WebhookUpdateCommitted`.
        - Update `modules/services/hostenv-provider-service/Test.hs` expectations accordingly (`testCommandSequence`, `testShouldWaitForCallbacks`).
    - In `modules/services/hostenv-provider-service/Hostenv/Provider/Webhook.hs`:
      - After `runWebhookWith` returns `WebhookResult`:
        - If a new commit was created (`updateStatus == WebhookUpdateCommitted`):
          - If `shouldWaitForCallbacks` is true:
            - Call `saveDeployIntents`, `saveDeployActions`, and append initial queued `deploy_node_events`.
            - Then execute `git push`.
            - Then return `JobWaiting ...`.
          - Else (not waiting):
            - Execute `git push`.
            - Return `JobComplete ...prepared and pushed...`.
        - If no new commit (`WebhookUpdateNoop`):
          - Return `JobComplete ...noop...`.
      - Implement the “waiting branch” persistence+push via a small helper in this module (so tests can enforce ordering):
        - Helper must accept its side effects as parameters (saveIntents/saveActions/appendQueued/push) and execute them strictly in that order.
        - Export the helper so `modules/services/hostenv-provider-service/Test.hs` can call it with stubs.
  - Keep `/api/deploy-intents/by-sha` semantics unchanged.

  **Must NOT do**:
  - Do not increase node retry windows as the primary fix.
  - Do not push before DB writes.

  **Recommended Agent Profile**:
  - Category: `unspecified-high` — Haskell refactor affecting job semantics.
  - Skills: []

  **Parallelization**: Can Parallel: YES | Wave 1 | Blocks: Task 5 | Blocked By: none

  **References**:
  - Webhook job orchestration: `modules/services/hostenv-provider-service/Hostenv/Provider/Webhook.hs`
  - Pipeline runner + finalizeRepoUpdate: `modules/services/hostenv-provider-service/Hostenv/Provider/Service.hs`
  - Node intent fetch: `modules/nixos/hostenv-comin-activate.sh`

  **Acceptance Criteria**:
  - [ ] `modules/services/hostenv-provider-service/Test.hs:testCommandSequence` proves `runWebhookWith` no longer issues `git push`.
  - [ ] `nix flake check` passes.

  **QA Scenarios**:
  ```
  Scenario: Save-before-push ordering
    Tool: Bash
    Steps:
      1) Run the new provider-service test added in Task 5.
    Expected: Test fails if push occurs before DB save; passes with new ordering.
    Evidence: .sisyphus/evidence/task-2-webhook-order-test.txt

  ```

  **Commit**: YES | Message: `fix(provider): persist deploy intents before pushing` | Files: `modules/services/hostenv-provider-service/Hostenv/Provider/{Webhook,Service}.hs`

- [ ] 3. Enforce single-source comin enablement (no mismatch)

  **What to do**:
  - In `modules/features/hostenv-provider-service.nix`, add a hard assertion:
    - If `services.hostenv-provider.comin.enable == true` then `(((config.provider or {}).comin or {}).enable or false) == true`.
  - Ensure the assertion message instructs how to fix (enable provider.comin, generate tokens, ensure secret exists).
  - Ensure provider-service starts only on the selected `provider.service` environment (keep existing assertions).

  **Must NOT do**:
  - Do not allow provider-service comin APIs to start without a node token map file.

  **Recommended Agent Profile**:
  - Category: `quick` — Nix assertion change.
  - Skills: []

  **Parallelization**: Can Parallel: YES | Wave 1 | Blocks: Task 3 verification + Task 6 (optional) | Blocked By: none

  **References**:
  - Provider-service module: `modules/features/hostenv-provider-service.nix`
  - Provider comin node module: `modules/nixos/provider-common.nix`

  **Acceptance Criteria**:
  - [ ] Update `tests/integration/provider-nixos-system.nix` to add a `builtins.tryEval` case where:
    - `config.nodes.${nodeName}.provider.comin.enable = false`
    - `config.environments.${envName}.services.hostenv-provider.enable = true`
    - `config.environments.${envName}.services.hostenv-provider.comin.enable = true`
    - Expect evaluation failure with assertion text mentioning both option paths.
  - [ ] `nix flake check` passes.

  **QA Scenarios**:
  ```
  Scenario: Mismatched comin enablement fails eval
    Tool: Bash
    Steps:
      1) Run the updated NixOS system evaluation test (Task 5 or new one) with services.hostenv-provider.comin.enable=true and provider.comin.enable=false.
    Expected: Nix eval fails with assertion mentioning both option names.
    Evidence: .sisyphus/evidence/task-3-assertions-test.txt
  ```

  **Commit**: YES | Message: `fix(provider): assert provider comin enabled for provider-service comin` | Files: `modules/features/hostenv-provider-service.nix`, `tests/integration/provider-nixos-system.nix`

- [ ] 4. Add Nix unit test for comin migration backup snapshot derivation

  **What to do**:
  - Add `tests/unit/hostenv-comin-activate.nix` that:
    - Builds a test harness derivation that runs `modules/nixos/hostenv-comin-activate.sh` with:
      - stub `curl` that returns a fixed deploy intent with a `backup` action containing `migrations=["drupal-migrate"]`
      - stub `runuser` that ignores `-u <user>` and runs the requested command as the current user (so the script can run in a sandbox)
      - stub `id` so `id -u <user>` returns a stable integer (e.g. `1000`)
      - stub `systemctl` that reports `LoadState=loaded` for `restic-backups-drupal-migrate.service` and exits 0 for `start --wait`
      - stub `restic-drupal-migrate` placed on `PATH` that prints a valid `restic snapshots --json` array with known `.time` and `.id`
      - `jq` from nixpkgs to parse
    - Asserts the script exits 0 and that the posted backup event payload includes the expected snapshot id.
  - Add the new unit test to `tests/unit/default.nix` so it is included in `checks`.

  **Must NOT do**:
  - Do not require a real restic repo or network.

  **Recommended Agent Profile**:
  - Category: `unspecified-high` — Nix test harness + bash stubs.
  - Skills: []

  **Parallelization**: Can Parallel: NO | Wave 2 | Blocks: none | Blocked By: Task 1

  **References**:
  - Existing unit test style: `tests/unit/restic.nix`
  - Script under test: `modules/nixos/hostenv-comin-activate.sh`

  **Acceptance Criteria**:
  - [ ] `nix flake check` runs the new unit test.

  **QA Scenarios**:
  ```
  Scenario: Unit test harness validates snapshot id
    Tool: Bash
    Steps:
      1) Run `nix flake check`.
    Expected: New unit test passes.
    Evidence: .sisyphus/evidence/task-4-comin-test.txt
  ```

  **Commit**: YES | Message: `test(comin): add harness for migration snapshot derivation` | Files: `tests/unit/hostenv-comin-activate.nix`, `tests/unit/default.nix`

- [ ] 5. Add provider-service test for save-before-push ordering

  **What to do**:
  - Add/extend tests in `modules/services/hostenv-provider-service/Test.hs`:
    - Update `testCommandSequence` expected commands:
      - Remove `CommandSpec "git" ["push"] ...` from the expected list.
      - Update the expected update status constructor (`WebhookUpdateCommitted`).
    - Add a new unit test that validates the waiting-branch ordering without a DB:
      - Use the exported helper added in Task 2.
      - In the test, use a single intent (one node) and an `IORef [Text]` to record calls; assert the call list is exactly:
        - `saveDeployIntents` → `saveDeployActions` → `appendDeployEvent(queued)` → `git push`.

  **Must NOT do**:
  - Do not rely on timing-based sleeps for correctness.

  **Recommended Agent Profile**:
  - Category: `unspecified-high` — Haskell testing + injection.
  - Skills: []

  **Parallelization**: Can Parallel: NO | Wave 2 | Blocks: none | Blocked By: Task 2

  **References**:
  - Pipeline runner: `modules/services/hostenv-provider-service/Hostenv/Provider/Service.hs`
  - Webhook job: `modules/services/hostenv-provider-service/Hostenv/Provider/Webhook.hs`
  - Existing tests: `modules/services/hostenv-provider-service/Test.hs`

  **Acceptance Criteria**:
  - [ ] `nix flake check` passes and includes this test.

  **QA Scenarios**:
  ```
  Scenario: Ordering test passes
    Tool: Bash
    Steps:
      1) Run `nix flake check`.
    Expected: Test passes and would fail if push occurs too early.
    Evidence: .sisyphus/evidence/task-5-webhook-order-test.txt
  ```

  **Commit**: YES | Message: `test(provider): assert deploy intent saved before push` | Files: `modules/services/hostenv-provider-service/Test.hs`

- [ ] 6. Repo hygiene: ignore local build artifacts

  **What to do**:
  - Add/update `.gitignore` to ignore:
    - `.tmp/`
    - `result-*`
    - `plan.txt`

  **Recommended Agent Profile**:
  - Category: `quick`
  - Skills: []

  **Parallelization**: Can Parallel: YES | Wave 2 | Blocks: none | Blocked By: none

  **Acceptance Criteria**:
  - [ ] `git status --short` no longer shows those paths when present.

  **QA Scenarios**:
  ```
  Scenario: Ignore rules work
    Tool: Bash
    Steps:
      1) Create dummy files under `.tmp/` and `result-123`.
      2) Run `git status --short`.
    Expected: Paths are ignored.
    Evidence: .sisyphus/evidence/task-6-gitignore.txt
  ```

  **Commit**: YES | Message: `chore: ignore local nix build artifacts` | Files: `.gitignore`

## Final Verification Wave (4 parallel agents, ALL must APPROVE)
- [ ] F1. Plan Compliance Audit — oracle
- [ ] F2. Code Quality Review — unspecified-high
- [ ] F3. Real Manual QA (script + webhook) — unspecified-high
- [ ] F4. Scope Fidelity Check — deep

## Commit Strategy
- Prefer 4 commits (Tasks 1-3 grouped by component; Tasks 4-6 for tests/hygiene).

## Success Criteria
- Migrations (backup/restore) can complete without missing-unit or missing-snapshot failures.
- Webhook-driven deploy jobs do not time out due to missing deploy intents after a push.
- Misconfigured comin enablement fails early with explicit assertion messaging.
