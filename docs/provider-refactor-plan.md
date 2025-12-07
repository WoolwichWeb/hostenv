# Hostenv Provider Refactor Plan

Prepared: 2025-12-07  
Audience: hostenv core/devops maintainers

## Goals
- Split end-user CLI (`hostenv`) from provider/ops tooling.
- Move backend logic from `for_refactoring` into first-class, maintained code under `src/provider`.
- Adopt flake-parts + dendritic module layout to reduce custom glue/templating.
- Deprecate and ultimately remove `hostenv deploy` from the end-user CLI.

## Scope (what changes)
- New provider-facing CLI: `hostenv-provider` (or `hostenv admin`) packaged in the main flake.
- Provider modules for plan generation, deploy-rs profiles, DNS/ACME gating, Cloudflare upserts, state (UID/vhost) management, and NixOS node assembly.
- Replace template-based generated flake with flake-parts composition.
- Keep project/user modules and CLI behaviour unchanged aside from warning on `hostenv deploy`.

## Non-goals (v1)
- No change to project-facing module surface (`services.*`, `environments.*`, etc.).
- No redesign of backup strategy or resource planning (just carry current limits forward).

## Target Deliverables
1) `src/provider/flake-parts-module.nix` exporting per-system:
   - `packages.hostenv-provider` (CLI binary)
   - `apps.plan`, `apps.deploy`, `apps.dns-gate`
   - `packages.nodes.<node>` (NixOS systems)
   - `packages.environments.<user>` (env closures; mirrors current generated flake)
2) Provider CLI with subcommands:
   - `plan`: emit `generated/plan.json`, `generated/state.json`, and refreshed flake outputs (no deploy).
   - `dns-gate [--node]`: ACME/DNS checks; disable ACME/forceSSL when unsafe; optional Cloudflare upsert.
   - `deploy [--node]`: drive deploy-rs using the generated outputs.
   - `state list|prune`: inspect/prune UID/vhost reservations.
   - `nodes build <node>`: build a node system closure (for CI/preflight).
3) Migration of logic from `for_refactoring`:
   - `generate-infra.nix` → `src/provider/plan.nix` (pure attrset function, no string templating).
   - `templates/generated-flake.nix` → deleted; replaced by flake-parts wiring.
   - `makeNixosSystem.nix` + `nodes/common.nix` → `src/provider/nixos-system.nix` + `src/provider/common.nix`.
   - `scripts/postgen.hs` → CLI subcommands (`dns-gate`, `deploy`).
4) Deprecation message in end-user CLI when `hostenv deploy` is invoked; later removal toggle.

## Architecture Notes
- **Dendritic layout (flake-parts):**
  - Trunk: provider common module (SOPS, deploy user, tmpfiles, nginx defaults, slices/quotas).
  - Branches: per-node module files (provider repo keeps hardware/network specifics).
  - Leaves: per-project hostenv inputs (already exported by projects) merged automatically.
- **State management:** keep `generated/state.json` committed; reserve UIDs and vhosts there; honour existing schema (avoid churn).
- **ACME/DNS gating:** preserve current behaviour—disable ACME/forceSSL for vhosts not pointing at the node; optional Cloudflare CNAME upsert.
- **Security:** provider CLI handles secrets only via sops; no secret paths in project CLI.

## File/Repo Moves
- New: `src/provider/plan.nix`, `src/provider/nixos-system.nix`, `src/provider/common.nix`, `src/provider/cli/` (CLI sources), `src/provider/flake-parts-module.nix`.
- New generated outputs folder (in provider repo): `generated/plan.json`, `generated/state.json`, `generated/flake.lock` (if needed), symlink `generated/flake.nix` produced by flake-parts, not templated.
- Legacy `for_refactoring/` kept temporarily for reference; scheduled for removal after parity achieved.

## Migration Steps (execution order)
1. **Scaffold provider module**
   - Add `src/provider/flake-parts-module.nix` exporting stubs for `plan`, `deploy`, `dns-gate`.
   - Wire into top-level `flake.nix` under a guarded import (e.g. enable when `inputs ? provider`).
2. **Port infra generator**
   - Reimplement `generate-infra.nix` as `plan.nix` returning `{ planJson, stateJson, environments, nodes }`.
   - Expose `apps.plan` (calls `plan.nix`; writes `generated/*`; git-add helper optional).
3. **Port NixOS node assembly**
   - Move `makeNixosSystem.nix` logic into `nixos-system.nix`; keep hardware/network overlay points.
   - Move common base to `common.nix`; parameterise hostenv hostname, slices, quotas.
   - Export `packages.nodes.<node>` and `deploy.nodes` via flake-parts (no templates).
4. **Port DNS/ACME + deploy flow**
   - Rewrite `postgen.hs` as CLI subcommands: `dns-gate` (ACME flip + optional Cloudflare upsert) and `deploy` (deploy-rs system-first, then envs).
   - Keep behaviour identical; add `--node` filter.
5. **Deprecate end-user deploy**
   - In existing hostenv CLI, emit warning when `deploy` is called; forward to `hostenv-provider deploy` if installed; add feature flag to disable entirely later.
6. **Docs & onboarding**
   - Write provider README: required inputs (hostenv projects), secrets (sops), Cloudflare vars, common commands.
   - Add quickstart for migrating from `for_refactoring`.
7. **Clean-up**
   - Once parity verified on one node, delete `for_refactoring/` and old templates.

## Open Questions
- Naming: `hostenv-provider` vs `hostenv admin` (choose before publishing).
- Distribution: ship provider CLI as flake app only, or also a binary cache artifact?
- Should plan/state live inside provider repo root or under `.hostenv-provider/` to keep git clean?

## Risks / Mitigations
- **Behaviour drift**: keep golden outputs from `for_refactoring/generated-infra` and diff against new `plan.nix` outputs in tests.
- **Secrets handling**: ensure CLI never writes secrets; only references sops paths.
- **Breaking deploy flow**: stage deprecation; keep shim in `hostenv deploy` until providers migrate.

## Definition of Done (v1)
- `nix run .#hostenv-provider plan` produces same plan/state as current generator (within stable ordering).
- `nix run .#hostenv-provider deploy --node <node>` successfully deploys a node + envs equivalent to current workflow.
- End-user `hostenv` CLI works unchanged except for a deprecation warning on `deploy`.
- `for_refactoring/` removable without lost capability.

