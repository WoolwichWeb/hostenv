# Implementation Plan

## 1) Goals & Architecture Overview

### 1.1 Desired Outcome

* A provider-service hostenv environment serves:

  1. The **provider-service** itself, which acts as orchestrator + builder + gc-root creator.
  2. The **provider-deploy** service, which runs on each node and deploys changes.
  3. An **authenticated Nix binary cache** served by Harmonia behind nginx at a path prefix (e.g. `/cache`).

* All non-provider nodes:

  * Trust the cache signing public key.
  * Use the provider cache as the **highest-priority substituter**.
  * Have `provider-deploy` that uses `nix store realise <store-path>` (or `nix copy`) to pull from cache, switch to those already-built store paths, and activate.

* Provider-service node:

  * Runs Harmonia via a UNIX socket.
  * Ensures the provider-service user can query the system nix store / nix-daemon.
  * Builds outputs.
  * Notifies `provider-deploy` with commit sha, output store paths, actions.

---

## 2) Provider-Service: Add a Hostenv Feature Module for Cache

### 2.1 New Hostenv Feature Module

Create `modules/features/harmonia.nix` (hostenv feature module) with options + config for running a Harmonia server in a Hostenv project.

**Activation condition:**
Enabled automatically when `services.hostenv-provider.enable = true` in the environment. This matches existing patterns: provider-service already uses unix socket at `services.hostenv-provider.socketPath` and configures nginx locations in the same hostenv module.

### 2.2 Harmonia Service (Unix Socket)

* Run Harmonia as a hostenv-managed systemd user service (consistent with provider-service service style).
* Bind Harmonia to a unix socket, not a TCP port.

**Harmonia config:**

* Harmonia binds to `unix:${config.hostenv.runtimeDir}/harmonia.sock` (or similar).
* The provider-service environment nginx vhost adds a location:

  * `/cache/` → `proxyPass = "http://unix:${config.hostenv.runtimeDir}/harmonia.sock";`
  * Rewrite paths if needed so harmonia sees `/nix-cache-info`, `/*.narinfo`, etc. at its root
* Harmonia ↔ Nix daemon access:

  * Keep Harmonia as a **hostenv user service**.
  * Add a small NixOS-level tweak on the **provider-service node only**:

    * ensure the provider-service environment user is in `nix.settings.allowed-users` (or equivalent),
    * and ensure socket perms allow that user/group to connect.
* Use a signing key file (from sops secret materialized on disk)

### 2.3 Authentication for `/cache`

Authentication must be compatible with nix HTTP fetches to substituter endpoints

Solution: `extraConfig` for `auth_basic` referencing `/run/secrets/${user}/…` paths (don’t use `basicAuth` because that stores passwords in the Nix store—hostenv's nginx library warns about that).

**Secrets placement for htpasswd:**

* Store htpasswd line(s) in SOPS secret.
* Materialize as a file readable by the nginx instance in the provider-service environment (owner = provider-service environment user, mode `0400`).

---

## 3) Provider: Update Strategy

### 3.1 Repository Location

Within provider-service environment data directory (already in `hostenv-provider-service.nix`):

* `${dataDir}/git/provider.git` (bare repo)
* Optionally `${dataDir}/git/<org__project>.git` for mirroring project inputs later; only implement if necessary

### 3.2 provider-service Workflow Change (Webhook Pipeline)

Update provider-service webhook pipeline (Haskell) so that it:

1. Pulls upstream repo changes (current behavior).
2. Runs plan generation (current behavior).
3. Builds and signs required outputs (new step).
4. Commits & pushes provider repo (if build successful).
5. Adds GC-roots (new step).
6. Notifies relevant node(s) via provider-deploy websocket (new step).

### 3.3 Build (to populate cache) and Notify

After plan generation, provider-service builds outputs in the following way.

#### On customer project webhook

1. node system closures: `./generated#node-${nodeName}`
2. environment packages as needed: `./generated#env-${envUserName}`
3. **Build artifacts**

   * Build system toplevel(s) for impacted nodes *only if the node toplevel changed*.
   * Build env artifacts for environments impacted by the project update.
4. **GC-root the built store paths** on the provider-service machine.

   * Ensures cache won’t 404 after GC.
   * `${dataDir}/gc-roots/system/<nodeName>` → symlink to `node-<node>` toplevel.
   * `${dataDir}/gc-roots/env/<userName>` → symlink to `env-<userName>` package.
   * timestamped history + prune to last ~5 store paths.
5. Only after successful build+root:

   * mark this job as “successful build available”
   * notify each connected node with the store paths/actions relevant to that node

#### On provider repo webhook

Same flow, but typically build for **all nodes whose toplevel changed** (or at least all nodes whose toplevel changes).

#### Only notify latest successful build

Implement this at provider-service by tracking, per node and per env:

* latest successful artifact job id
* store path(s)
* commit sha

When multiple webhooks arrive:

* cancel any in-progress builds superseded by a newer ref
* only send the newest successful one

#### Move cross-node coordination into provider-service

`hostenv-comin-activate.sh` currently has a “wait for activation on target node before deactivating” hack implemented by polling job statuses.

With websockets:

* For migrations, provider-service should *sequence*:

  1. backup on source node
  2. restore on destination node
  3. deactivate on source node
* That means `provider-deploy` no longer needs to poll “statuses” at all.

#### Cancellation semantics

Design and implement the following:

* a per-node/per-env “current build job id”
* a way to kill the currently running `nix build` process (process group kill)
* and a rule for what happens if:

  * env build is canceled but system build completes
  * multiple envs overlap, etc.

---

## 4) Cache Client Configuration: Keep It in NixOS Provider Modules

### 4.1 Nix Substituter Configuration

In `provider-common.nix`:

* Set `nix.settings.substituters` with provider cache first:

  * `https://<provider-service-host>/cache`
  * then `https://cache.nixos.org`
* Ensure `nix.settings.trusted-public-keys` includes provider cache public key

  * Reuse existing `provider.nixSigning.trustedPublicKeys` and append the cache key.
* Set `nix.settings.require-signed-binaries = true`.

### 4.2 Provide HTTP Auth to Nix (Netrc)

Use SOPS and `nix.settings.netrc-file = /run/secrets/.../netrc`.

---

## 5) Provider-Service User Must Talk to nix-daemon

On the provider-service machine only:

* Add the provider-service user to nix trusted users:

  * `nix.settings.allowed-users = lib.mkAfter [ "<providerServiceUser>" ]`
* If needed:

  * Ensure membership in the group that can access the nix daemon socket (varies by NixOS configuration).

**How to detect provider-service node in NixOS modules:**

* In `provider-common.nix`, use `config.hostenv.environments` to determine if the provider-service environment exists on this node. If yes, apply the allowed-user settings.

---

## 6) Comin: Remove

Remove comin inputs, configuration, secrets, and any other references, or refactor to provide provider-deploy equivalents.

This includes:

* deleting `modules/nixos/hostenv-comin-activate.sh`
* deleting unit tests `tests/unit/hostenv-comin-activate.nix`
* rewriting integration tests that assert `services.comin.*`
* removing `provider.comin` options and all config propagation
* removing `comin` from generated flake inputs in plan generation

Right now, `modules/entrypoints/provider/options.nix` only adds `providerServiceSecrets` when `provider.comin.enable` is true (it’s how `comin_node_tokens.yaml` gets delivered to the provider-service environment user).

Refactor that logic so:

* the provider-service env still receives:

  * node token map (for websocket auth)
  * cache signing key
  * cache auth secret(s)
* it keys off `provider.service != null` (or “provider-deploy enabled”), not off comin.

---

## 7) Secrets & Auto-Initialization

### 7.1 Signing Key Pair

A binary cache requires signing keys:

* Generate via `nix-store --generate-binary-cache-key <name> privkey pubkey`
* Store private key in SOPS secrets file.
* Store public key in a plaintext repo file (e.g. `generated/cache-public-key.txt`), committed.

### 7.2 Basic Auth Credentials

Add SOPS secrets for:

* Store a single `cacheAuthPassword` secret (and fixed username like `cache`)
* At deploy time, generate:
  * htpasswd file for nginx
  * netrc file for nix

### 7.3 Tooling automation

Extend the provider devshell auto-init (currently generates comin tokens in `modules/entrypoints/provider/tooling.nix`) to also:

* Ensure cache signing keys exist and public key file is created
* Ensure auth secrets exist
* Do not require users to manually edit provider config strings

---

## 8) provider-deploy: the Node Agent

* Maintains one websocket connection to provider-service.
* Authenticates with a per-node token (you can keep the same “node token map” you already generate today for comin).
* Receives “desired state” messages that include:

  * provider repo commit sha (for bookkeeping)
  * **system toplevel store path** for this node (optional; only when it changed)
  * **per-environment store paths** for the users on this node that changed
  * the ordered actions to run (`activate`, `reload`, `backup`, `restore`, `deactivate`, etc.)
  * a monotonic sequence number / job id (so “latest wins” is easy)

### 8.1 NixOS module

* add `services.provider-deploy` NixOS module
* systemd service runs as root
* reads node token from `/run/secrets/...`
* knows websocket URL (derived from provider-service environment hostname + path)
* has a restart/backoff strategy

Update `provider-common.nix` to:

* add the sops secret for node token (replacing the current comin node-token secret)
* enable and configure the provider-deploy service

### 8.2 System switch (root)

When told to deploy a system toplevel path:

1. `nix store realise <toplevel>` (fast substitute if already cached)
2. `nix-env -p /nix/var/nix/profiles/system --set <toplevel>`
3. `<toplevel>/bin/switch-to-configuration switch`
4. record “current system commit/store path” locally

No git, no flake eval.

### 8.3 Environment switch (unprivileged)

When told to deploy an env store path for `user`:

1. `nix store realise <env-store-path>`
2. install it into the user’s profile (as that user):

   * either `nix profile install --profile /nix/var/nix/profiles/per-user/<user>/profile <env-store-path>`
   * or `nix-env -p /nix/var/nix/profiles/per-user/<user>/profile --set <env-store-path>`
3. run the action:

   * `activate`: run `…/profile/bin/activate` (or directly `<env-store-path>/bin/activate`)
   * migrations: run restic backup/restore, then activate/reload as appropriate
4. record “current env commit/store path” per user locally

### 8.4 Reporting back

Over websocket, send structured events:

* job started / finished
* per action: started / success / failed / timed_out
* include stderr summaries and exit codes (but avoid huge payloads—store full logs locally and upload if you ever add that)

### 8.5 Cancellation / “latest wins”

On the agent:

* When a new job arrives with a higher sequence/job id:

  * cancel the current in-progress work (kill the build/activate process group)
  * drop any queued older actions
  * move to the newest job

---

## 9) Decoupling: Env Packages Should be Separate From the System Closure

Right now (in the current hostenv provider implementation) environment packages are being pulled into the node system closure via `users.users.<name>.packages = [ envPackage ]`. That means **any env package change forces a node system rebuild/switch**, which defeats unprivileged build+switch per user.

To remedy this:

* Remove env packages from the node’s NixOS closure.
* `provider-deploy` will manage user profiles instead.

System changes should be limited to:

* nginx vhosts / routing changes
* user account creation, linger, UIDs
* secrets materialization and system wiring

Env “code/package” changes should be:

* user profile updates + activation/reload

This will make the build graph and rollout much saner.

---

## 10) Tests & Verification

### 10.1 Update existing integration tests

* Add tests for:

  * `nix.settings.substituters` includes provider cache first
  * trusted public keys include cache public key
  * provider-service node sets nix trusted-users for provider-service user

### 10.2 Add a provider-service environment test

Create an integration test that asserts:

* nginx locations include `/cache`
* harmonia and fcgiwrap services are present and bound to unix sockets
