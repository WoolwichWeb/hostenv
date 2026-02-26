# Task 10: Deploy intent creation flow

## Summary
- Wired demo flow to call webhook-trigger verification for deploy intents (`run-demo.sh` now invokes Task 10 verification after Task 8 in wizard and automated modes).
- Expanded `trigger-webhook.sh` into a full deploy-intent verifier:
  - resolves runtime layouts (`<workdir>/runtime` and `<workdir>`),
  - reads `plan.json` hash robustly (supports scalar/array hash values),
  - triggers webhook,
  - polls `/api/deploy-intents/by-sha?sha=...&node=...`,
  - validates intent envelope/fields (`schemaVersion`, `actions`, action keys),
  - queries PostgreSQL (`deploy_intents`, `jobs`) when a local socket is available,
  - writes evidence to `task-10-*.log`.

## Files Modified
- `examples/local-provider-migration/trigger-webhook.sh`
- `examples/local-provider-migration/run-demo.sh`

## QA Evidence
- `.sisyphus/evidence/task-10-webhook-trigger.log`
- `.sisyphus/evidence/task-10-deploy-intent.log`
- `.sisyphus/evidence/task-10-harness-workdir.log`

## QA Outcome
- Webhook trigger path is confirmed (`202` + `accepted=true` + `jobId`).
- `plan.json` hash resolution is confirmed with actual runtime plan lookup.
- In the isolated harness used here, deploy-intent retrieval stayed `404` and no `deploy_intents` row was persisted because webhook jobs require a project OAuth deploy credential in DB (`load_project_credential` stage); the harness has no seeded project credential.

## Notes
- Script-side verification and evidence capture are now in place for SHA/node/job correlation and action payload checks.
- Full end-to-end intent persistence in this workspace remains dependent on provider-service project OAuth credential setup, not script wiring.
