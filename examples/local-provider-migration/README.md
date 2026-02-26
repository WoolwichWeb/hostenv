# Local Provider Migration Demo

This example is a local, end-to-end Hostenv walkthrough that runs on Linux with KVM. It demonstrates the modern provider-service architecture with comin-driven deployments and webhook-based automation.

## What This Demo Shows

1. **Bootstrapping a Hostenv provider locally** using VMs as nodes.
2. **Provider-service architecture** with webhook-driven deployments.
3. **Comin integration** for Git-based continuous deployment.
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
┌─────────────────┐     comin       ┌──────────────────┐
│     node-a      │ <────────────── │  plan.json       │
│  (NixOS VM)     │   activation    │  (environment    │
└─────────────────┘                 │   placement)     │
                                    └──────────────────┘
```

### Provider-Service Flow

The modern deployment flow works like this:

1. **Plan Generation**: The provider evaluates project flakes and generates `generated/plan.json` with environment placements.
2. **Webhook Trigger**: A POST to `/webhook/<projectHash>` starts the deployment pipeline.
3. **Deploy Intent Creation**: The provider-service creates a deploy intent record with actions for each affected node.
4. **Comin Activation**: Each node runs comin to pull changes and activate deployments.
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

### Comin Integration

Nodes use [comin](https://github.com/nlewo/comin) for Git-based deployment:

- Polls the provider Git repository for changes
- Runs the activation script on new commits
- Queries deploy intents from provider-service API
- Reports deployment status via API calls

## Prerequisites

- Linux host with `/dev/kvm`
- Nix with flakes enabled
- Commands: `nix`, `git`, `jq`, `ssh`, `ssh-keygen`, `sops`, `age-keygen`, `curl`, `pv`, `gunzip`, `hostctl`
- Wizard UI uses `gum`; if `gum` or `hostctl` are missing, the script re-runs itself via `nix shell` to provide them.
- `hostctl` needs permission to edit `/etc/hosts` (root or `sudo`).

## Quickstart

### Wizard Mode (Interactive)

From repo root:

```bash
./examples/local-provider-migration/run-demo.sh
```

The wizard:
- Starts local VMs (`node-a` and `node-b`).
- Launches the provider-service on a Unix socket.
- Configures a temporary `hostctl` profile for local demo hostnames.
- Creates `./hostenv-demo` symlink to its working directory.
- Generates `./hostenv-demo/flake.nix` so you can enter the demo environment with `cd ./hostenv-demo && nix develop`.
- Triggers deployments via webhook calls.
- Polls deployment status via provider-service API.
- Pauses between stages and gives copy/paste commands.
- Lets you press `<Enter>` to continue or press `a` to abort and clean up.

### Automated Mode (CI/Regression)

```bash
./examples/local-provider-migration/run-demo.sh --automated --cleanup
```

This runs the full pipeline non-interactively, suitable for regression testing.

### Manual Webhook Trigger

To trigger a deployment manually:

```bash
./examples/local-provider-migration/trigger-webhook.sh \
  --workdir /tmp/hostenv-demo-XXXXXX \
  --commit-sha abc123
```

## Teardown

If a prior run crashed or was interrupted:

```bash
./examples/local-provider-migration/run-demo.sh --teardown
```

To tear down one specific workspace:

```bash
./examples/local-provider-migration/run-demo.sh --teardown --workdir /tmp/hostenv-local-vm-demo-XXXXXX
```

## How It Works

### VM and Network Setup

The demo creates two NixOS VMs:
- `node-a`: Initial deployment target
- `node-b`: Migration target

Each VM gets:
- SSH access with generated keys
- Comin configuration for Git-based deployment
- Hostenv provider-common modules

### Provider-Service Startup

The demo starts `hostenv-provider-service-dev` with:
- Unix socket for local API access
- Comin node tokens for VM authentication
- Runtime and data directories in the workdir
- Poll interval of 5 seconds for faster demo feedback

### Deployment Flow

1. **Node A Deployment**:
   ```bash
   trigger-webhook.sh --workdir <dir> --commit-sha <sha>
   ```
   This creates a deploy intent and triggers comin activation.

2. **Database Seeding**:
   After node-a is ready, the demo imports `seed/seed.sql.gz` via the project's dev shell.

3. **Migration to Node B**:
   The demo preserves the pre-migration plan snapshot, switches environment placement to `node-b`, then triggers another webhook. This emits `backup` on node-a and `restore` on node-b.

4. **Verification**:
   The demo polls job statuses and verifies the migration marker is present in HTTP responses.

## File Structure

```
examples/local-provider-migration/
├── README.md                 # This file
├── run-demo.sh              # Main orchestration script
├── trigger-webhook.sh       # Webhook trigger utility
├── teardown.sh              # Cleanup utility
├── seed/
│   └── seed.sql.gz         # Demo database seed
└── nodes/
    ├── node-a/             # VM configuration
    └── node-b/
```

## Troubleshooting

### Check VM Logs

If deployment fails, inspect VM logs:

```bash
cat /tmp/hostenv-local-vm-demo-XXXXXX/logs/node-a.log
cat /tmp/hostenv-local-vm-demo-XXXXXX/logs/node-b.log
```

### Provider-Service Issues

Check the provider-service logs:

```bash
cat /tmp/hostenv-local-vm-demo-XXXXXX/logs/provider-service.log
```

### Webhook Failures

Verify the webhook trigger manually:

```bash
curl -X POST \
  --unix-socket /tmp/hostenv-local-vm-demo-XXXXXX/runtime/hostenv-provider.sock \
  -H "Content-Type: application/octet-stream" \
  -d '{"checkout_sha": "test"}' \
  http://localhost/webhook/<projectHash>
```

### Deploy Intent Not Found

Deploy intents are created asynchronously. If verification shows 404, the webhook job may still be processing. Check the provider-service logs for job status.

### Flake Input Errors

If you see errors about `deploy-rs` inputs, ensure your flake.lock is up to date:

```bash
nix flake update
```

### General Debugging

1. Run with `--automated` to see full output
2. Check evidence files in `.sisyphus/evidence/`
3. Open a GitLab issue with the logs if needed

## Notes

- The wizard demonstrates real provider commands (`plan`, `dns-gate`) while keeping setup details in the script.
- Migration uses an explicit source override (`--migration-source <env>=node-a`) so the flow does not depend on committing `generated/state.json` timing.
- DB seed source file is `examples/local-provider-migration/seed/seed.sql.gz`.
- Teardown/abort removes the temporary `hostctl` profile alongside VMs and demo workspace files.
- The provider-service runs on a Unix socket, not TCP, for local security.
