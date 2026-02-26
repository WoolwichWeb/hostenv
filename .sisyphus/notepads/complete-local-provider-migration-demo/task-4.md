## Task 4 - provider-service API verification harness

- Added `examples/local-provider-migration/test-service.sh`.
- Harness creates an isolated runtime, starts PostgreSQL + `hostenv-provider-service`, and verifies Unix-socket API routes.
- `/api/deploy-intents/by-sha?sha=abc123&node=node-a` tested with bearer auth token from `comin.nodeAuthTokensFile`; expected `404` observed with empty intents.
- Webhook tested without signature headers (`/webhook/demohash`), expected `202` accepted response observed.
- `plan.json` lookup verified by hash behavior: known hash `demohash` returns `202`, unknown hash returns `404`.
- Evidence logs written to:
  - `.sisyphus/evidence/task-4-health-check.log`
  - `.sisyphus/evidence/task-4-api-test.log`
  - `.sisyphus/evidence/task-4-webhook-test.log`
  - `.sisyphus/evidence/task-4-plan-read.log`
  - `.sisyphus/evidence/task-4-service.log`
