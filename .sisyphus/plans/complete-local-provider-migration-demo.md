# Plan: Complete local-provider-migration Demo (Provider-Service + Comin)

## TL;DR

> Complete the half-built `examples/local-provider-migration` demo by implementing **Option B: Provider-Service + Comin Integration**. The demo will showcase hostenv's modern pull-based deployment architecture with webhook-triggered deploy intents, comin-based node reconciliation, and migration support.
>
> **Deliverables:**
> - Updated `run-demo.sh` with provider-service integration
> - Provider-service configuration in demo environment
> - Comin-enabled node configurations
> - Webhook trigger mechanism (simulated or manual)
> - End-to-end test verifying full deployment flow
> - Updated documentation
>
> **Estimated Effort:** Large (1-2 weeks)
> **Parallel Execution:** YES - 4 waves
> **Critical Path:** Provider-service setup → Comin config → Webhook flow → Migration test

---

## Context

### Original Request
User applied a git stash to restore a half-built example/demo that should provide:
1. An end-to-end provider-service and provider test
2. A showcase for potential users learning hostenv and/or Nix

### Current State
The demo at `examples/local-provider-migration/` has:
- Working VM orchestration (node-a, node-b via QEMU/KVM)
- Provider workspace setup from template
- Sample Drupal project with database seed
- **MISSING:** Provider-service integration and comin-based deployment

The demo script expects `nix run .#hostenv-provider -- deploy` which doesn't exist. Instead, we'll implement the modern architecture using provider-service + comin.

### Metis Review Findings
- **Option B selected:** Provider-service + comin integration (modern architecture)
- **Complexity:** High - requires provider-service runtime, comin config, webhook simulation
- **Effort:** 1-2 weeks
- **Key challenge:** Multi-component coordination (service, comin, webhook, migration)

---

## Work Objectives

### Core Objective
Transform the local-provider-migration demo from a broken legacy deploy script into a working showcase of hostenv's modern provider-service + comin architecture, demonstrating webhook-triggered deployments and cross-node migrations.

### Concrete Deliverables
1. **Provider-service runtime** in demo environment (PostgreSQL + service binary)
2. **Comin configuration** on node-a and node-b VMs
3. **Webhook trigger mechanism** (curl-based or simple UI)
4. **Updated run-demo.sh** orchestrating the full flow
5. **End-to-end verification** test
6. **Updated README** documenting the modern flow

### Definition of Done
```bash
# Demo runs end-to-end without manual intervention
./examples/local-provider-migration/run-demo.sh --automated --cleanup
# Exit code: 0

# Provider-service responds to deploy intent API
curl -s http://localhost:8080/api/deploy-intents/by-sha?sha=abc&node=node-a
# Returns: 200 with valid deploy intent JSON

# Migration verification
curl -s http://demo-project.main.demo.hostenv.test:8080/ | grep "from-node-a"
# Returns: HTTP 200 with database marker from migration
```

### Must Have
- [ ] Provider-service runs in demo environment with PostgreSQL
- [ ] Comin enabled on both node VMs
- [ ] Nodes can reach provider-service API
- [ ] Webhook trigger creates deploy intent
- [ ] Comin pulls changes and activates environment
- [ ] Migration from node-a to node-b works
- [ ] Automated mode completes without prompts

### Must NOT Have (Guardrails)
- **NO** production-grade OAuth/GitLab integration (use dev mode)
- **NO** changes to VM infrastructure (KVM setup is fine)
- **NO** cloud deployment variants
- **NO** support for non-Linux platforms
- **NO** backward compatibility with legacy deploy approach

---

## Verification Strategy

### Test Decision
- **Infrastructure exists:** YES - NixOS VM tests via `pkgs.testers.runNixOSTest`
- **Automated tests:** Tests-after (integration test added after implementation)
- **Framework:** NixOS VM test for full E2E, bash scripts for manual demo
- **Agent-Executed QA:** YES - Every task includes executable verification

### QA Policy
Every task MUST include agent-executed QA scenarios. The executing agent will directly verify each deliverable:

- **Nix builds:** `nix build .#<attr> --print-build-logs` 
- **VM tests:** `nix build .#checks.x86_64-linux.<test-name>`
- **Service endpoints:** `curl` commands with specific URLs and expected JSON
- **File assertions:** `test -f <path>` with specific paths
- **Process verification:** `pgrep` or `systemctl is-active`

---

## Execution Strategy

### Parallel Execution Waves

```
Wave 1 (Foundation - can start immediately):
├── Task 1: Provider-service dev runner integration [quick]
├── Task 2: Demo environment provider-service config [quick]  
├── Task 3: PostgreSQL setup for demo [quick]
└── Task 4: Provider-service API verification harness [unspecified-high]

Wave 2 (Node Configuration - after Wave 1):
├── Task 5: Comin configuration for node VMs [unspecified-high]
├── Task 6: Node-to-provider-service connectivity [unspecified-high]
├── Task 7: Comin token generation and distribution [quick]
└── Task 8: Post-deployment activation script verification [unspecified-high]

Wave 3 (Integration - after Wave 2):
├── Task 9: Webhook trigger mechanism (curl wrapper) [quick]
├── Task 10: Deploy intent creation flow [deep]
├── Task 11: Update run-demo.sh orchestration [deep]
└── Task 12: Migration backup/restore integration [deep]

Wave 4 (Testing & Documentation - after Wave 3):
├── Task 13: End-to-end automated test [deep]
├── Task 14: README updates [writing]
├── Task 15: Manual wizard mode verification [unspecified-high]
└── Task 16: Demo cleanup and edge case handling [unspecified-high]

Wave FINAL (Review - after ALL):
├── Task F1: Plan compliance audit (oracle)
├── Task F2: Code quality review (unspecified-high)
├── Task F3: Full demo run verification (unspecified-high)
└── Task F4: Documentation review (writing)

Critical Path: Task 1 → Task 5 → Task 9 → Task 13 → F1-F4
Parallel Speedup: ~60% faster than sequential
Max Concurrent: 4 (Wave 1 & 2)
```

### Dependency Matrix

| Task | Depends On | Blocks |
|------|-----------|--------|
| 1 | - | 2, 4 |
| 2 | 1 | 3 |
| 3 | 2 | 4 |
| 4 | 1, 3 | 5 |
| 5 | 4 | 6, 7, 8 |
| 6 | 5 | 9 |
| 7 | 5 | 9 |
| 8 | 5 | 9 |
| 9 | 6, 7, 8 | 10 |
| 10 | 9 | 11 |
| 11 | 10 | 12, 13 |
| 12 | 11 | 13 |
| 13 | 11, 12 | 14, 15, 16 |
| 14 | 13 | F1-F4 |
| 15 | 13 | F1-F4 |
| 16 | 13 | F1-F4 |

### Agent Dispatch Summary

- **Wave 1:** 4 tasks → `quick` (T1, T3), `unspecified-high` (T2, T4)
- **Wave 2:** 4 tasks → `unspecified-high` (T5, T6, T8), `quick` (T7)
- **Wave 3:** 4 tasks → `quick` (T9), `deep` (T10, T11, T12)
- **Wave 4:** 4 tasks → `deep` (T13), `writing` (T14), `unspecified-high` (T15, T16)
- **FINAL:** 4 tasks → `oracle`, `unspecified-high`, `unspecified-high`, `writing`

---

## TODOs

- [x] 1. Integrate provider-service-dev runner into demo

  **What to do:**
  - Add `hostenv-provider-service-dev` to demo shell packages
  - Create wrapper script that sets up environment variables
  - Configure PostgreSQL data directory in workdir
  - Set up dummy GitLab OAuth secrets for dev mode

  **Must NOT do:**
  - Don't implement real GitLab OAuth (use dev mode with dummy secrets)
  - Don't change the service source code

  **Recommended Agent Profile:**
  - **Category:** `quick`
  - **Skills:** []
  - **Reason:** Package integration and wrapper script creation

  **Parallelization:**
  - **Can Run In Parallel:** YES
  - **Parallel Group:** Wave 1
  - **Blocks:** Task 2, Task 4
  - **Blocked By:** None

  **References:**
  - Pattern: `modules/features/provider-service-build.nix` lines 31-107 (servicePkgDev)
  - Pattern: `examples/local-provider-migration/run-demo.sh` lines 97-126 (tool bootstrapping)
  - API: Environment variables in `modules/services/hostenv-provider-service/Hostenv/Provider/Config.hs`

  **Acceptance Criteria:**
  - [ ] `nix develop` in demo workdir includes `hostenv-provider-service-dev` command
  - [ ] Running `hostenv-provider-service-dev` starts PostgreSQL and service
  - [ ] Service listens on configured Unix socket

  **QA Scenarios:**
  ```
  Scenario: Service starts successfully
    Tool: Bash
    Preconditions: Demo workdir initialized
    Steps:
      1. cd $WORKDIR && nix develop
      2. hostenv-provider-service-dev &
      3. sleep 5
      4. test -S $WORKDIR/hostenv-provider.sock
    Expected Result: Unix socket exists at $WORKDIR/hostenv-provider.sock
    Evidence: .sisyphus/evidence/task-1-service-start.log

  Scenario: PostgreSQL initialized
    Tool: Bash
    Preconditions: Service started
    Steps:
      1. test -f $WORKDIR/pgdata/PG_VERSION
      2. psql -h $WORKDIR -l | grep hostenv-provider
    Expected Result: Database 'hostenv-provider' exists
    Evidence: .sisyphus/evidence/task-1-postgres.log
  ```

  **Commit:** YES
  - Message: `feat(demo): add provider-service-dev runner integration`
  - Files: `examples/local-provider-migration/run-demo.sh`, `examples/local-provider-migration/demo-project/.hostenv/flake.nix`

---

- [x] 2. Configure provider-service in demo environment

  **What to do:**
  - Add `services.hostenv-provider.enable = true` to demo environment config
  - Configure PostgreSQL database for provider-service
  - Set up nginx proxy for provider-service socket
  - Configure webhook host and UI base URL

  **Must NOT do:**
  - Don't enable GitLab OAuth (keep it disabled for demo simplicity)
  - Don't use production secret paths (use workdir-relative paths)

  **Recommended Agent Profile:**
  - **Category:** `quick`
  - **Skills:** []
  - **Reason:** Nix module configuration

  **Parallelization:**
  - **Can Run In Parallel:** YES (with Task 1)
  - **Parallel Group:** Wave 1
  - **Blocks:** Task 3
  - **Blocked By:** Task 1

  **References:**
  - Pattern: `docs/provider-quickstart.md` lines 91-168 (provider-service config)
  - Pattern: `modules/features/hostenv-provider-service.nix` lines 156-260 (options)
  - Pattern: `examples/local-provider-migration/demo-project/.hostenv/hostenv.nix` (current config)

  **Acceptance Criteria:**
  - [ ] Environment config includes `services.hostenv-provider.enable = true`
  - [ ] PostgreSQL configuration points to workdir
  - [ ] Webhook host set to `demo.hostenv.test`

  **QA Scenarios:**
  ```
  Scenario: Provider-service module enabled
    Tool: Bash
    Preconditions: Demo workdir initialized
    Steps:
      1. grep -q "services.hostenv-provider.enable = true" $PROJECT_DIR/.hostenv/hostenv.nix
    Expected Result: Configuration includes enabled provider service
    Evidence: .sisyphus/evidence/task-2-config-check.txt
  ```

  **Commit:** YES
  - Message: `feat(demo): configure provider-service in demo environment`
  - Files: `examples/local-provider-migration/demo-project/.hostenv/hostenv.nix`

---

- [x] 3. Set up PostgreSQL for demo

  **What to do:**
  - Configure PostgreSQL service in demo environment
  - Create `hostenv-provider` database
  - Set up proper permissions for environment user
  - Ensure PostgreSQL data directory is in workdir (ephemeral)

  **Must NOT do:**
  - Don't use system PostgreSQL (must be self-contained in demo)
  - Don't persist data outside workdir

  **Recommended Agent Profile:**
  - **Category:** `quick`
  - **Skills:** []
  - **Reason:** Standard NixOS PostgreSQL configuration

  **Parallelization:**
  - **Can Run In Parallel:** YES (with Task 1, 2)
  - **Parallel Group:** Wave 1
  - **Blocks:** Task 4
  - **Blocked By:** Task 2

  **References:**
  - Pattern: `docs/provider-quickstart.md` lines 143-157 (PostgreSQL config)
  - Pattern: `modules/features/hostenv-provider-service.nix` lines 232-236 (dbUri)

  **Acceptance Criteria:**
  - [ ] PostgreSQL service enabled in environment
  - [ ] `hostenv-provider` database created automatically
  - [ ] Environment user has ALL PRIVILEGES on database

  **QA Scenarios:**
  ```
  Scenario: PostgreSQL database accessible
    Tool: Bash
    Preconditions: Environment activated
    Steps:
      1. cd $PROJECT_DIR/.hostenv && nix develop
      2. psql -h $WORKDIR/pgdata -l | grep hostenv-provider
    Expected Result: Database exists and is accessible
    Evidence: .sisyphus/evidence/task-3-postgres-setup.log
  ```

  **Commit:** YES
  - Message: `feat(demo): add PostgreSQL configuration for provider-service`
  - Files: `examples/local-provider-migration/demo-project/.hostenv/hostenv.nix`

---

- [x] 4. Create provider-service API verification harness

  **What to do:**
  - Create test script that verifies provider-service endpoints
  - Test `/api/deploy-intents/by-sha` endpoint
  - Test webhook endpoint (without signature verification)
  - Verify service can read plan.json

  **Must NOT do:**
  - Don't test GitLab OAuth flows (not enabled)
  - Don't test admin UI (out of scope for demo)

  **Recommended Agent Profile:**
  - **Category:** `unspecified-high`
  - **Skills:** []
  - **Reason:** API testing and verification logic

  **Parallelization:**
  - **Can Run In Parallel:** YES (with Wave 1 tasks)
  - **Parallel Group:** Wave 1
  - **Blocks:** Task 5
  - **Blocked By:** Task 1, Task 3

  **References:**
  - API: `modules/services/hostenv-provider-service/Hostenv/Provider/Server.hs` lines 27-86 (API types)
  - API: `modules/services/hostenv-provider-service/Hostenv/Provider/DeployApi.hs` (endpoint handlers)
  - Test pattern: `tests/integration/provider-service-module.nix`

  **Acceptance Criteria:**
  - [ ] Harness script can start service and verify it's running
  - [ ] Can query `/api/deploy-intents/by-sha` (returns 404 when no intent exists)
  - [ ] Can POST to webhook endpoint

  **QA Scenarios:**
  ```
  Scenario: Service health check
    Tool: Bash
    Preconditions: Service running
    Steps:
      1. curl -s --unix-socket $WORKDIR/hostenv-provider.sock http://localhost/health
    Expected Result: Returns 200 or valid response
    Evidence: .sisyphus/evidence/task-4-health-check.log

  Scenario: Deploy intent API accessible
    Tool: Bash
    Preconditions: Service running with plan.json
    Steps:
      1. curl -s --unix-socket $WORKDIR/hostenv-provider.sock \
         "http://localhost/api/deploy-intents/by-sha?sha=abc123&node=node-a"
    Expected Result: Returns JSON (404 if no intent, 200 with intent if exists)
    Evidence: .sisyphus/evidence/task-4-api-test.log
  ```

  **Commit:** YES
  - Message: `feat(demo): add provider-service API verification harness`
  - Files: `examples/local-provider-migration/test-service.sh` (new)

---

- [x] 5. Configure comin on node VMs

  **What to do:**
  - Enable `provider.comin.enable = true` in provider flake
  - Configure `remoteUrl` pointing to provider git repo
  - Configure `providerApiBaseUrl` pointing to provider-service
  - Set up `nodeAuthTokenFile` for callback authentication
  - Configure `nodeName` for each VM

  **Must NOT do:**
  - Don't change node VM hardware configuration
  - Don't modify SSH or firewall settings (already correct)

  **Recommended Agent Profile:**
  - **Category:** `unspecified-high`
  - **Skills:** []
  - **Reason:** NixOS module configuration for comin

  **Parallelization:**
  - **Can Run In Parallel:** NO (must wait for service harness)
  - **Parallel Group:** Wave 2
  - **Blocks:** Task 6, Task 7, Task 8
  - **Blocked By:** Task 4

  **References:**
  - Pattern: `template/provider/flake.nix` lines 52-58 (comin config template)
  - Pattern: `modules/nixos/provider-common.nix` lines 58-95 (comin options)
  - Assertions: `modules/nixos/provider-common.nix` lines 138-172 (validation)

  **Acceptance Criteria:**
  - [ ] Provider flake has comin enabled with all required fields
  - [ ] Each node has unique nodeName (node-a, node-b)
  - [ ] remoteUrl points to provider repo accessible by VMs

  **QA Scenarios:**
  ```
  Scenario: Comin configuration valid
    Tool: Bash
    Preconditions: Provider flake generated
    Steps:
      1. nix run .#hostenv-provider -- plan
      2. jq -e '.comin.enable == true' generated/plan.json
      3. jq -e '.comin.remoteUrl != null' generated/plan.json
    Expected Result: Comin enabled with valid configuration
    Evidence: .sisyphus/evidence/task-5-comin-config.json
  ```

  **Commit:** YES
  - Message: `feat(demo): enable comin on node VMs`
  - Files: `examples/local-provider-migration/run-demo.sh` (write_provider_flake function)

---

- [x] 6. Configure node-to-provider-service connectivity

  **What to do:**
  - Ensure VMs can reach provider-service API
  - Configure `providerApiBaseUrl` to point to host (10.0.2.2 for QEMU VMs)
  - Set up port forwarding or shared socket if needed
  - Test connectivity from within VMs

  **Must NOT do:**
  - Don't require manual network configuration
  - Don't use hardcoded IPs that might conflict

  **Recommended Agent Profile:**
  - **Category:** `unspecified-high`
  - **Skills:** []
  - **Reason:** Network configuration and VM connectivity

  **Parallelization:**
  - **Can Run In Parallel:** YES (with Task 5)
  - **Parallel Group:** Wave 2
  - **Blocks:** Task 9
  - **Blocked By:** Task 5

  **References:**
  - Pattern: QEMU user networking gateway is 10.0.2.2
  - Pattern: `modules/nixos/provider-common.nix` lines 80-84 (providerApiBaseUrl)

  **Acceptance Criteria:**
  - [ ] VMs can curl provider-service API endpoint
  - [ ] providerApiBaseUrl configured to reach host

  **QA Scenarios:**
  ```
  Scenario: VM can reach provider service
    Tool: Bash (inside VM)
    Preconditions: VM running, service on host
    Steps:
      1. ssh node-a "curl -s http://10.0.2.2:8080/health"
    Expected Result: Returns service health response
    Evidence: .sisyphus/evidence/task-6-connectivity.log
  ```

  **Commit:** YES
  - Message: `feat(demo): configure node-to-provider-service connectivity`
  - Files: `examples/local-provider-migration/run-demo.sh`

---

- [x] 7. Generate and distribute comin node tokens

  **What to do:**
  - Generate unique tokens for node-a and node-b
  - Store tokens in provider secrets (sops file)
  - Configure `nodeAuthTokenFile` path on nodes
  - Ensure tokens are available to provider-service

  **Must NOT do:**
  - Don't use same token for all nodes (security issue)
  - Don't store tokens in git (use sops)

  **Recommended Agent Profile:**
  - **Category:** `quick`
  - **Skills:** []
  - **Reason:** Token generation and secret management

  **Parallelization:**
  - **Can Run In Parallel:** YES (with Task 5, 6)
  - **Parallel Group:** Wave 2
  - **Blocks:** Task 9
  - **Blocked By:** Task 5

  **References:**
  - Pattern: `modules/nixos/provider-common.nix` lines 174-182 (sops secrets)
  - Command: `nix run .#hostenv-provider -- comin-tokens` (generates tokens)

  **Acceptance Criteria:**
  - [ ] Tokens generated for both nodes
  - [ ] Tokens stored in `secrets/provider.yaml`
  - [ ] Provider-service config includes token map

  **QA Scenarios:**
  ```
  Scenario: Node tokens exist
    Tool: Bash
    Preconditions: Provider initialized
    Steps:
      1. sops -d secrets/provider.yaml | grep -q "comin_node_tokens"
      2. sops -d secrets/provider.yaml | grep -q "node-a:"
      3. sops -d secrets/provider.yaml | grep -q "node-b:"
    Expected Result: Both node tokens present in secrets
    Evidence: .sisyphus/evidence/task-7-tokens.txt
  ```

  **Commit:** YES
  - Message: `feat(demo): generate comin node tokens`
  - Files: `examples/local-provider-migration/run-demo.sh`

---

- [x] 8. Verify post-deployment activation script

  **What to do:**
  - Verify `hostenv-comin-activate.sh` is called by comin
  - Ensure script can query provider-service API
  - Verify script posts events back to provider-service
  - Test with manual comin trigger

  **Must NOT do:**
  - Don't modify the activation script logic (already implemented)
  - Don't change comin postDeploymentCommand

  **Recommended Agent Profile:**
  - **Category:** `unspecified-high`
  - **Skills:** []
  - **Reason:** Integration testing of activation flow

  **Parallelization:**
  - **Can Run In Parallel:** YES (with Wave 2 tasks)
  - **Parallel Group:** Wave 2
  - **Blocks:** Task 9
  - **Blocked By:** Task 5

  **References:**
  - Script: `modules/nixos/hostenv-comin-activate.sh`
  - Config: `modules/nixos/provider-common.nix` lines 184-196 (comin service)

  **Acceptance Criteria:**
  - [ ] Activation script present on nodes
  - [ ] Script has correct environment variables
  - [ ] Script can call provider-service API

  **QA Scenarios:**
  ```
  Scenario: Activation script exists
    Tool: Bash (inside VM)
    Preconditions: VM running
    Steps:
      1. ssh node-a "test -f /run/current-system/sw/bin/hostenv-comin-activate"
      2. ssh node-a "grep -q HOSTENV_COMIN_API_BASE_URL /etc/systemd/system/comin.service"
    Expected Result: Script exists and env vars configured
    Evidence: .sisyphus/evidence/task-8-activation-script.log
  ```

  **Commit:** YES
  - Message: `feat(demo): verify post-deployment activation`
  - Files: `examples/local-provider-migration/run-demo.sh`

---

- [x] 9. Create webhook trigger mechanism

  **What to do:**
  - Create script to simulate webhook trigger
  - Calculate project hash for webhook URL
  - POST to `/webhook/<hash>` endpoint
  - Handle response and verify deploy intent created

  **Must NOT do:**
  - Don't implement full GitLab webhook verification
  - Don't require external GitLab access

  **Recommended Agent Profile:**
  - **Category:** `quick`
  - **Skills:** []
  - **Reason:** Simple curl wrapper script

  **Parallelization:**
  - **Can Run In Parallel:** NO (must wait for connectivity)
  - **Parallel Group:** Wave 3
  - **Blocks:** Task 10
  - **Blocked By:** Task 6, Task 7, Task 8

  **References:**
  - API: `modules/services/hostenv-provider-service/Hostenv/Provider/Webhook.hs`
  - URL pattern: `docs/provider-quickstart.md` line 111 (webhook URL format)

  **Acceptance Criteria:**
  - [ ] Script can trigger webhook
  - [ ] Webhook creates deploy intent in database
  - [ ] Intent includes correct commit SHA and node mappings

  **QA Scenarios:**
  ```
  Scenario: Webhook trigger creates deploy intent
    Tool: Bash
    Preconditions: Service running, plan generated
    Steps:
      1. ./trigger-webhook.sh
      2. curl -s --unix-socket $WORKDIR/hostenv-provider.sock \
         "http://localhost/api/deploy-intents/by-sha?sha=<commit>&node=node-a"
    Expected Result: Returns deploy intent JSON with status "pending"
    Evidence: .sisyphus/evidence/task-9-webhook-trigger.log
  ```

  **Commit:** YES
  - Message: `feat(demo): add webhook trigger mechanism`
  - Files: `examples/local-provider-migration/trigger-webhook.sh` (new)

---

- [x] 10. Implement deploy intent creation flow

  **What to do:**
  - Connect webhook trigger to deploy intent creation
  - Ensure provider-service reads plan.json correctly
  - Verify intent includes all required fields (actions, backup config)
  - Test intent retrieval by comin activation script

  **Must NOT do:**
  - Don't modify provider-service Haskell code
  - Don't change intent schema

  **Recommended Agent Profile:**
  - **Category:** `deep`
  - **Skills:** []
  - **Reason:** Complex integration between multiple components

  **Parallelization:**
  - **Can Run In Parallel:** NO (depends on webhook)
  - **Parallel Group:** Wave 3
  - **Blocks:** Task 11
  - **Blocked By:** Task 9

  **References:**
  - Flow: `docs/provider-quickstart.md` lines 91-134 (webhook flow)
  - DB: `modules/services/hostenv-provider-service/Hostenv/Provider/DB.hs`

  **Acceptance Criteria:**
  - [ ] Webhook creates deploy intent in PostgreSQL
  - [ ] Intent can be queried by SHA and node
  - [ ] Intent includes correct environment configuration

  **QA Scenarios:**
  ```
  Scenario: Deploy intent queryable
    Tool: Bash
    Preconditions: Webhook triggered
    Steps:
      1. psql -h $WORKDIR -c "SELECT * FROM deploy_intents WHERE job_id='...'"
      2. curl -s ".../api/deploy-intents/by-sha?sha=...&node=node-a" | jq -e '.intent'
    Expected Result: Intent exists and is queryable
    Evidence: .sisyphus/evidence/task-10-deploy-intent.log
  ```

  **Commit:** YES
  - Message: `feat(demo): implement deploy intent creation flow`
  - Files: `examples/local-provider-migration/run-demo.sh`, `trigger-webhook.sh`

---

- [x] 11. Update run-demo.sh orchestration

  **What to do:**
  - Replace legacy deploy commands with provider-service flow
  - Add service startup to demo preparation
  - Add webhook trigger step
  - Update polling logic to check provider-service events
  - Keep wizard and automated modes working

  **Must NOT do:**
  - Don't break existing VM setup
  - Don't remove hostctl integration
  - Don't change seed import flow

  **Recommended Agent Profile:**
  - **Category:** `deep`
  - **Skills:** []
  - **Reason:** Complex bash scripting, orchestration logic

  **Parallelization:**
  - **Can Run In Parallel:** NO (core integration task)
  - **Parallel Group:** Wave 3
  - **Blocks:** Task 12, Task 13
  - **Blocked By:** Task 10

  **References:**
  - Current: `examples/local-provider-migration/run-demo.sh` lines 910-1058 (wizard/automated flows)
  - Pattern: Lines 838-868 (prepare_node functions)

  **Acceptance Criteria:**
  - [ ] Demo script starts provider-service
  - [ ] Demo triggers deployment via webhook
  - [ ] Demo polls for deployment completion via provider-service API
  - [ ] Both wizard and automated modes work

  **QA Scenarios:**
  ```
  Scenario: Automated flow completes
    Tool: Bash
    Preconditions: Clean workdir
    Steps:
      1. ./run-demo.sh --automated --cleanup
    Expected Result: Exit code 0, all stages complete
    Evidence: .sisyphus/evidence/task-11-automated-run.log

  Scenario: Wizard flow reaches each stage
    Tool: Bash (manual verification)
    Preconditions: Clean workdir
    Steps:
      1. ./run-demo.sh (interactive)
      2. Follow prompts through all 3 stages
    Expected Result: All stages complete successfully
    Evidence: .sisyphus/evidence/task-11-wizard-run.log
  ```

  **Commit:** YES
  - Message: `feat(demo): update run-demo.sh for provider-service flow`
  - Files: `examples/local-provider-migration/run-demo.sh`

---

- [ ] 12. Integrate migration backup/restore

  **What to do:**
  - Configure migration source in deploy intent
  - Ensure backup action runs on source node
  - Ensure restore action runs on target node
  - Verify database marker present after migration

  **Must NOT do:**
  - Don't change backup/restore logic in activation script
  - Don't modify restic configuration

  **Recommended Agent Profile:**
  - **Category:** `deep`
  - **Skills:** []
  - **Reason:** Complex multi-node coordination

  **Parallelization:**
  - **Can Run In Parallel:** NO (depends on orchestration)
  - **Parallel Group:** Wave 3
  - **Blocks:** Task 13
  - **Blocked By:** Task 11

  **References:**
  - Script: `modules/nixos/hostenv-comin-activate.sh` (backup/restore logic)
  - API: `modules/services/hostenv-provider-service/Hostenv/Provider/DeployApi.hs` lines 162-179 (backupSnapshotHandler)

  **Acceptance Criteria:**
  - [ ] Migration creates backup on source node
  - [ ] Backup snapshot accessible to target node
  - [ ] Restore completes successfully on target
  - [ ] Database marker shows migration source

  **QA Scenarios:**
  ```
  Scenario: Migration backup created
    Tool: Bash
    Preconditions: Migration triggered
    Steps:
      1. Check provider-service events for backup action
      2. Verify backup snapshot exists in restic repo
    Expected Result: Backup action succeeded
    Evidence: .sisyphus/evidence/task-12-backup.log

  Scenario: Migration restore completed
    Tool: Bash
    Preconditions: Migration to node-b complete
    Steps:
      1. curl -s http://demo-project.main.demo.hostenv.test:8080/ | grep "from-node-a"
    Expected Result: Page contains database marker
    Evidence: .sisyphus/evidence/task-12-migration-verify.log
  ```

  **Commit:** YES
  - Message: `feat(demo): integrate migration backup/restore`
  - Files: `examples/local-provider-migration/run-demo.sh`

---

- [ ] 13. Create end-to-end automated test

  **What to do:**
  - Create NixOS VM test or comprehensive bash test
  - Test full flow: setup → deploy → verify → migrate → verify
  - Assert all components work together
  - Run in CI if possible (may be manual-only due to KVM)

  **Must NOT do:**
  - Don't require manual intervention
  - Don't skip verification steps

  **Recommended Agent Profile:**
  - **Category:** `deep`
  - **Skills:** []
  - **Reason:** Complex integration testing

  **Parallelization:**
  - **Can Run In Parallel:** NO (final integration)
  - **Parallel Group:** Wave 4
  - **Blocks:** Task 14, Task 15, Task 16
  - **Blocked By:** Task 11, Task 12

  **References:**
  - Pattern: `tests/integration/provider-secrets-runtime.nix` (NixOS VM test)
  - Pattern: `tests/integration/default.nix` (test registration)

  **Acceptance Criteria:**
  - [ ] Test runs full demo flow
  - [ ] Test verifies provider-service API responses
  - [ ] Test verifies deployment on both nodes
  - [ ] Test verifies migration success

  **QA Scenarios:**
  ```
  Scenario: Full E2E test passes
    Tool: Bash
    Preconditions: Clean environment
    Steps:
      1. nix build .#checks.x86_64-linux.local-provider-migration-test
      OR
      1. ./run-demo.sh --automated --cleanup
      2. Verify exit code 0
    Expected Result: All assertions pass
    Evidence: .sisyphus/evidence/task-13-e2e-test.log
  ```

  **Commit:** YES
  - Message: `test(demo): add end-to-end automated test`
  - Files: `tests/integration/local-provider-migration.nix` (new)

---

- [ ] 14. Update README documentation

  **What to do:**
  - Document the modern provider-service + comin flow
  - Explain webhook mechanism
  - Update architecture diagram/description
  - Document prerequisites and setup
  - Keep troubleshooting section

  **Must NOT do:**
  - Don't document legacy deploy approach
  - Don't remove existing VM/network docs

  **Recommended Agent Profile:**
  - **Category:** `writing`
  - **Skills:** []
  - **Reason:** Documentation writing

  **Parallelization:**
  - **Can Run In Parallel:** YES (with Wave 4 tasks)
  - **Parallel Group:** Wave 4
  - **Blocks:** FINAL review
  - **Blocked By:** Task 13

  **References:**
  - Current: `examples/local-provider-migration/README.md`
  - Pattern: `docs/provider-quickstart.md`

  **Acceptance Criteria:**
  - [ ] README explains provider-service architecture
  - [ ] README documents webhook flow
  - [ ] README has updated quickstart
  - [ ] Troubleshooting section current

  **QA Scenarios:**
  ```
  Scenario: Documentation complete
    Tool: Manual review
    Preconditions: README updated
    Steps:
      1. grep -q "provider-service" README.md
      2. grep -q "webhook" README.md
      3. grep -q "comin" README.md
    Expected Result: All key concepts documented
    Evidence: .sisyphus/evidence/task-14-readme-review.txt
  ```

  **Commit:** YES
  - Message: `docs(demo): update README for provider-service flow`
  - Files: `examples/local-provider-migration/README.md`

---

- [ ] 15. Verify manual wizard mode

  **What to do:**
  - Test interactive wizard mode manually
  - Verify each stage pauses correctly
  - Verify prompts and instructions are accurate
  - Test abort/cleanup functionality

  **Must NOT do:**
  - Don't automate wizard mode (it's meant to be interactive)

  **Recommended Agent Profile:**
  - **Category:** `unspecified-high`
  - **Skills:** []
  - **Reason:** Manual testing and verification

  **Parallelization:**
  - **Can Run In Parallel:** YES (with Wave 4 tasks)
  - **Parallel Group:** Wave 4
  - **Blocks:** FINAL review
  - **Blocked By:** Task 13

  **References:**
  - Current: `examples/local-provider-migration/run-demo.sh` lines 979-1020 (wizard flow)

  **Acceptance Criteria:**
  - [ ] Wizard mode runs without errors
  - [ ] Each stage shows correct instructions
  - [ ] Continue/abort prompts work
  - [ ] Cleanup works on abort

  **QA Scenarios:**
  ```
  Scenario: Wizard mode interactive test
    Tool: Manual (tmux for recording)
    Preconditions: Clean workdir
    Steps:
      1. ./run-demo.sh (no flags)
      2. Follow wizard through all stages
      3. Abort at final stage, verify cleanup
    Expected Result: Smooth experience, accurate instructions
    Evidence: .sisyphus/evidence/task-15-wizard-session.rec
  ```

  **Commit:** NO (manual verification only)

---

- [ ] 16. Handle edge cases and cleanup

  **What to do:**
  - Handle service startup failures gracefully
  - Handle VM boot failures
  - Ensure cleanup removes all resources
  - Handle partial failures (resume or cleanup)

  **Must NOT do:**
  - Don't leave resources behind on failure
  - Don't require manual cleanup

  **Recommended Agent Profile:**
  - **Category:** `unspecified-high`
  - **Skills:** []
  - **Reason:** Error handling and robustness

  **Parallelization:**
  - **Can Run In Parallel:** YES (with Wave 4 tasks)
  - **Parallel Group:** Wave 4
  - **Blocks:** FINAL review
  - **Blocked By:** Task 13

  **References:**
  - Current: `examples/local-provider-migration/run-demo.sh` lines 1178-1213 (cleanup function)

  **Acceptance Criteria:**
  - [ ] Cleanup removes VMs, workdir, hostctl profile
  - [ ] Service processes terminated
  - [ ] No leftover processes or files

  **QA Scenarios:**
  ```
  Scenario: Cleanup after failure
    Tool: Bash
    Preconditions: Demo interrupted
    Steps:
      1. Start demo, interrupt with Ctrl+C
      2. Verify no qemu processes: ! pgrep -f qemu.*hostenv-demo
      3. Verify no hostctl profile: ! hostctl list | grep hostenv-local-demo
      4. Verify workdir removed: ! test -d $WORKDIR
    Expected Result: All resources cleaned up
    Evidence: .sisyphus/evidence/task-16-cleanup.log
  ```

  **Commit:** YES
  - Message: `fix(demo): improve edge case handling and cleanup`
  - Files: `examples/local-provider-migration/run-demo.sh`

---

## Final Verification Wave

- [ ] F1. **Plan Compliance Audit** — `oracle`
  Read the plan end-to-end. For each "Must Have": verify implementation exists. For each "Must NOT Have": search codebase for forbidden patterns. Check evidence files exist in `.sisyphus/evidence/`.
  Output: `Must Have [N/N] | Must NOT Have [N/N] | Tasks [N/N] | VERDICT: APPROVE/REJECT`

- [ ] F2. **Code Quality Review** — `unspecified-high`
  Run `nix flake check`. Review all changed files for: bash best practices, Nix formatting, no hardcoded paths, proper error handling.
  Output: `Build [PASS/FAIL] | Lint [PASS/FAIL] | Files [N clean/N issues] | VERDICT`

- [ ] F3. **Full Demo Run Verification** — `unspecified-high`
  Run `./examples/local-provider-migration/run-demo.sh --automated --cleanup` on clean Linux with KVM. Verify all stages complete, all assertions pass.
  Output: `Demo [PASS/FAIL] | Stages [N/N] | Duration [MM:SS] | VERDICT`

- [ ] F4. **Documentation Review** — `writing`
  Review README for accuracy, completeness, clarity. Verify all commands work as documented.
  Output: `README [APPROVE/NEEDS_WORK] | Issues [list] | VERDICT`

---

## Commit Strategy

- **Task 1-4:** `feat(demo): provider-service foundation` — service integration, config, postgres, harness
- **Task 5-8:** `feat(demo): comin node configuration` — comin setup, connectivity, tokens, activation
- **Task 9-12:** `feat(demo): webhook and orchestration` — trigger, deploy intent, run-demo.sh updates, migration
- **Task 13:** `test(demo): end-to-end automated test`
- **Task 14:** `docs(demo): update README for provider-service flow`
- **Task 16:** `fix(demo): improve edge case handling and cleanup`

---

## Success Criteria

### Verification Commands
```bash
# Full demo runs successfully
./examples/local-provider-migration/run-demo.sh --automated --cleanup
# Expected: Exit code 0

# Provider-service API accessible
curl -s --unix-socket $WORKDIR/hostenv-provider.sock \
  http://localhost/api/deploy-intents/by-sha?sha=<sha>&node=node-a
# Expected: Valid JSON response

# Deployment verified on node-a
curl -s http://demo-project.main.demo.hostenv.test:8080/ | grep -q "Drupal"
# Expected: HTTP 200, contains "Drupal"

# Migration verified on node-b
curl -s http://demo-project.main.demo.hostenv.test:8080/ | grep -q "from-node-a"
# Expected: HTTP 200, contains "from-node-a" marker

# Nix checks pass
nix flake check --print-build-logs
# Expected: All checks pass
```

### Final Checklist
- [ ] All 16 tasks completed
- [ ] All 4 final verification tasks approved
- [ ] Demo runs end-to-end without manual intervention
- [ ] README documentation accurate and complete
- [ ] No regressions in existing functionality
