# Task 8: Verify post-deployment activation script

- Verified activation script wiring in `modules/nixos/provider-common.nix`: comin uses `postDeploymentCommand = "${cominActivateWrapper}"` and wrapper exports `HOSTENV_COMIN_NODE_NAME`, `HOSTENV_COMIN_API_BASE_URL`, `HOSTENV_COMIN_TOKEN_FILE`, `HOSTENV_COMIN_ACTION_TIMEOUT`.
- Verified script behavior in `modules/nixos/hostenv-comin-activate.sh`: token file is read when present, deploy intent is queried from provider-service API, and status events are posted back to `/api/deploy-jobs/$job_id/events`.
- Updated `examples/local-provider-migration/run-demo.sh` to execute Task 8 runtime verification on `node-a` after node deployment (wizard and automated modes), including:
  - activation script presence + env wrapper checks,
  - provider API query check,
  - manual comin trigger (`systemctl restart comin.service`) and manual activation invocation with synthetic SHA.
- Runtime QA execution is currently blocked in this workspace by provider flake input resolution (`hostenv/deploy-rs` follow target missing) before node deployment starts.

Evidence:
- `.sisyphus/evidence/task-8-activation-script.log`
- `.sisyphus/evidence/task-8-provider-api.log`
- `.sisyphus/evidence/task-8-manual-comin-trigger.log`
