# Decisions

- Backward compatibility: NOT required; prefer fail-fast canonical behavior.
- Snapshot id source for migrations: derive via `restic snapshots --json` filtered by migration key tag.
- Webhook ordering: persist deploy intents/actions/events before pushing commits that comin nodes poll.
- Refactored webhook deploy flow so `runWebhookWith` only stages/commits and reports `WebhookUpdateCommitted`; `runWebhookDeployJob` now performs `git push` exactly once after deploy intent/action/event persistence when entering `JobWaiting` to guarantee `/api/deploy-intents/by-sha` exists before nodes poll.
