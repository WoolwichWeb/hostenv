# Local Provider Migration Demo

This example is a local, end-to-end Hostenv walkthrough. It demonstrates the provider-service architecture with deployments, webhooks, projects, and migrations.

## What This Demo Shows

1. **Bootstrapping a Hostenv provider locally** using VMs as nodes.
2. **Provider-service architecture** with webhook-driven deployments.
3. **Provider-deploy integration** for node deployment execution.
4. **Deploying a Drupal environment** to `node-a` via webhook trigger.
5. **Importing a Drupal seed database** via project dev shell tooling.
6. **Migrating the same environment** to `node-b` using backup/restore.
7. **Verifying migrated data** on `node-b`.

## Architecture Overview

This demo uses the modern provider-service architecture:

```
┌─────────────────┐     webhook      ┌──────────────────┐
│  run-demo.sh    │ ───────────────> │ provider-service │
│  (orchestrator) │                  │   (Unix socket)  │
└─────────────────┘                  └────────┬─────────┘
                                              │
                                              │ deploy intent
                                              ▼
┌─────────────────┐ hostenv-deploy-agent ┌──────────────────┐
│     node-a      │ <────────────── │  plan.json       │
│  (NixOS VM)     │   actions       │  (environment    │
└─────────────────┘                 │   placement)     │
                                    └──────────────────┘
```

### Provider-Service Flow

The modern deployment flow works like this:

1. **Plan Generation**: The provider evaluates project flakes and generates `generated/plan.json` with environment placements.
2. **Webhook Trigger**: A POST to `/webhook/<projectHash>` starts the deployment pipeline.
3. **Deploy Intent Creation**: The provider-service creates a deploy intent record with actions for each affected node.
4. **Node Actions**: Each node executes hostenv-deploy-agent actions and reports status.
5. **Status Reporting**: The activation script reports job status back to the provider-service API.

### Webhook Mechanism

Deployments are triggered via webhook calls to the provider-service:

```bash
POST /webhook/<projectNameHash>
Content-Type: application/octet-stream

{"checkout_sha": "abc123", "ref": "refs/heads/main"}
```

The webhook handler:

- Validates the project hash against `plan.json`
- Creates a deployment job with unique ID
- Generates deploy intents for affected nodes
- Returns `202 Accepted` with `{"accepted": true, "jobId": "..."}`

### Provider-deploy Integration

Nodes use hostenv-deploy-agent for action-based deployment:

- Polls the provider Git repository for changes
- Runs the activation script on new commits
- Queries deploy intents from provider-service API
- Reports deployment status via API calls

## Prerequisites

- Linux host with `/dev/kvm`
- Nix with flakes enabled

It deploys Drupal to `node-a`, imports seed data, then migrates to `node-b`.

## What the Script Does

1. Creates a temporary local provider workspace.
2. Boots `node-a` and starts provider-service.
3. Triggers a webhook deployment to `node-a`.
4. Imports `seed/seed.sql.gz` into Drupal on `node-a`.
5. Switches placement to `node-b` and triggers migration.
6. Verifies migrated marker data on `node-b`.

## Usage

From repo root:

```bash
./examples/local-provider-migration/run-demo.sh
```

Automated mode:

```bash
./examples/local-provider-migration/run-demo.sh --automated --cleanup
```

Automated mode is mainly for validation and testing purposes (but feel free to experiment!)

Supported flags are:

- `--automated`
- `--cleanup`

## Interactive Pauses

In interactive mode, the script pauses at key moments to let you explore:

- Before provider/node-a setup
- Before node-a deployment
- Before seed import
- Before node-b migration
- At final completion

Each pause includes “things to explore” such as provider UI URLs and curl probes.

## Useful URLs and Checks

Provider UI:

```text
http://localhost:18080/dashboard
```

Drupal probe pattern used by the script:

```bash
curl -H 'Host: <vhost-from-plan>' http://127.0.0.2:8080/
curl -H 'Host: <vhost-from-plan>' http://127.0.0.3:8080/
```

## Output and Artifacts

- A `hostenv-demo` symlink is created in your current directory, pointing to the active temp workspace.
- VM and provider-service logs are in `<workdir>/logs/`.
- On failure, the workdir is preserved for debugging.
- On success, workdir is removed only when `--cleanup` is passed.

## File Structure

```text
examples/local-provider-migration/
├── README.md               # This file
├── run-demo.sh             # Main script
├── trigger-webhook.sh      # Webhook trigger utility that triggers deploys
├── seed/
│   └── seed.sql.gz         # Drupal database for seeding a demo site
└── demo-project/           # Demo hostenv project with a Drupal site
```
