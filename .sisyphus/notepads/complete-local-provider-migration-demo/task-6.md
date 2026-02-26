## Task 6 - node-to-provider-service connectivity

- Updated `examples/local-provider-migration/run-demo.sh` to route comin callbacks to the host-visible provider API endpoint:
  - `providerApiBaseUrl = "http://${PROVIDER_API_VM_GATEWAY}:${PROVIDER_HTTP_PORT}"`
  - `PROVIDER_API_VM_GATEWAY="10.0.2.2"` (QEMU user networking host gateway)
  - `PROVIDER_HTTP_PORT=8080`
- Ensured provider-service dev wrapper exposes HTTP from Unix socket via `socat` with explicit bind control:
  - `HOSTENV_PROVIDER_HTTP_PORT=${PROVIDER_HTTP_PORT}`
  - `HOSTENV_PROVIDER_HTTP_BIND=127.0.0.1`
  - listener now uses `bind="$http_bind"`.
- QA scenario executed (`ssh node-a "curl -s http://10.0.2.2:8080/health"`) but failed due missing running VM in this workspace.
- Evidence files:
  - `.sisyphus/evidence/task-6-connectivity.log`
  - `.sisyphus/evidence/task-6-provider-api-config.log`
