# Task 15: Manual wizard mode verification

## Result
- Status: BLOCKED (dependency Task 13 unresolved in this workspace)
- Wizard mode cannot reach interactive stage prompts because `prepare_node_a_baseline` fails before `run_wizard_flow` starts.

## Manual test execution

### Attempt 1: direct wizard launch (recorded)
- Command: `./examples/local-provider-migration/run-demo.sh` (wrapped with `script` in tmux)
- Result: early failure due missing `sops` in auto-reexec tool set.

### Attempt 2: full tool shell + wizard launch (recorded)
- Command run in tmux recording:
  - `nix shell nixpkgs#gum nixpkgs#sops nixpkgs#age nixpkgs#openssh nixpkgs#curl nixpkgs#jq nixpkgs#pv nixpkgs#gzip nixpkgs#git nixpkgs#qemu_kvm -c bash -lc 'hostctl(){ return 0; }; sudo(){ shift; $@; }; export -f hostctl sudo; ./examples/local-provider-migration/run-demo.sh'`
- Result: deterministic failure during provider plan generation:
  - `error: input 'deploy-rs' follows a non-existent input 'hostenv/deploy-rs'`
- This occurs in `Preparing node-a`, before intro wizard instructions and before Step 1/2/3 pauses.

## Required checks vs observed state
- Interactive wizard starts without errors: FAIL (blocked by flake input error)
- Each stage pauses correctly: NOT REACHED (stages 1-3 never entered)
- Prompts/instructions accuracy: NOT REACHED in runtime (only static script text review possible)
- Abort/cleanup at final stage: NOT REACHED (final stage never entered)

## Evidence
- `.sisyphus/evidence/task-15-wizard-session.rec`
- `.sisyphus/evidence/task-15-abort-session.rec`

## Notes
- The hostctl sudo prompt also blocks true local-host profile mutation in this environment; for runtime continuation attempts, `hostctl`/`sudo` were shell-overridden only to isolate the Task 13 blocker.
- Workdirs from failed attempts were retained by script failure path:
  - `/tmp/hostenv-local-vm-demo-oNWfq5`
  - `/tmp/hostenv-local-vm-demo-X2ZYVs`
