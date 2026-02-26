## Task 5 - comin configuration on node VMs

- Updated `write_provider_flake` in `examples/local-provider-migration/run-demo.sh` to enable `provider.comin`.
- Added `provider.comin.remoteUrl = "file:///mnt/hostenv-shared/provider-repo"` and linked `$SHARED_DIR/provider-repo -> $PROVIDER_DIR` so node VMs can read the provider repo through the shared mount.
- Added `provider.comin.providerApiBaseUrl = "http://10.0.2.2:${NODE_HTTP_PORT}"` so callbacks target the QEMU host gateway.
- Added `provider.comin.nodeAuthTokenFile = "/run/secrets/hostenv-provider/comin_node_token"`.
- Added required `provider.service.{organisation,project,environmentName}` for comin validation (`demo/drupal/main`).
- Verified generated plan data contains node-specific comin node names (`node-a`, `node-b`) in QA workspace.
- Evidence: `.sisyphus/evidence/task-5-comin-config.json`.
