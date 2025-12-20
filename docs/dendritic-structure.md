# Dendritic structure for hostenv

This repo now exposes both project-facing and provider-facing pieces as flake-parts “trunk/branch/leaf” modules, with `hostenv.environments` as the single trunk.

## Provider options (trunk)

Set under `perSystem.config.provider` when importing `hostenv.providerModule` (the public `hostenv` flake stays provider-neutral; each project sets `hostenvHostname` in its `.hostenv/flake.nix`, typically via a `hostenvHostname` variable passed into the modules list):

```nix
perSystem = { config, ... }: {
  provider = {
    hostenvHostname = "hostenv.example.invalid";
    letsEncrypt = {
      adminEmail = "ops@example.com";
      acceptTerms = true;
    };
    deployPublicKey = "ssh-ed25519 AAAA... deployment";
    nodeFor = {
      production = "backend04";
      testing = "backend02";
      development = "backend02";
      default = "backend02";
    };
    nodesPath = ./nodes;             # per-node configuration modules
    secretsPath = ./secrets/secrets.yaml; # sops-encrypted secrets
    statePath = ./generated/state.json;   # required persisted state for UIDs/host reservations
    planPath = ./generated/plan.json;     # optional persisted plan for auditability
    planSource = "disk";                  # or "eval" to consume generated plan directly
    goldenPlanPath = null;                # optional path for regression diff
  };
};
```

## Branches

- `modules/environments.nix` defines the canonical environments schema; it is bridged into `hostenv.environments` for feature modules.
- `nodesPath` is a directory of per-node configs (`configuration.nix` + hardware config).
- Secrets are supplied via `secretsPath` (sops). Provide your own example or copy from provider secrets when bootstrapping.
- The provider template includes starter node stubs in `nodes/sample/`; copy and adapt for a new provider setup. Secrets are created with `sops` at `secrets/secrets.yaml`.
- `nodeSystems` maps node name → system (e.g. `x86_64-linux`); required if nodes are heterogeneous.
- Client project inputs should point at the `.hostenv` flake (e.g. `dir=.hostenv`).

## Leaves

- Hostenv project inputs (end-user flakes) provide environments; plan generation discovers them automatically from flake inputs that expose `hostenv`.
- Feature modules (branches) consume `config.hostenv.environments`:
  - `modules/nixos/users-slices.nix` — Unix users + slices per env
  - `modules/nixos/nginx-hostenv.nix` — reverse proxy to per-env socket
  - `modules/nixos/backups-hostenv.nix` — restic jobs keyed off env extras
- `modules/nixos/monitoring-hostenv.nix` — monitoring labels (placeholder)

## Split of concerns (system vs user level)

- **System level (provider)**: runs NixOS, creates the Unix users/slices, exposes reverse proxies, backups, monitoring. It pre-creates runtime directories the user-level services will use:
  - `/run/hostenv/nginx/<env>/` (2770, owner `<env>`, group `nginx`) — where system nginx expects to find the *user-level nginx* upstream socket `in.sock`.
  - `/run/hostenv/user/<env>/` (2700, owner `<env>`) — where app daemons (e.g. PHP-FPM pools) place their own sockets.
- **User level (hostenv activation package)**: runs under the environment’s Unix user via `systemd --user`. Pattern:
  - user-level nginx listens on `${config.hostenv.upstreamRuntimeDir}/in.sock` (group `nginx`, mode `660`) so the system nginx can proxy into it.
  - PHP-FPM (or other app backend) listens on `${config.hostenv.runtimeDir}/<pool>.sock` (owned by the env user); user-level nginx fastcgi_proxy/uwsgi_proxy to that.
- User nginx sets `UMask=0007` so its socket inherits group `nginx` (from the setgid upstream dir) and is group-writable.
- This separation keeps privileged bits (system nginx, restic, node exporter, user creation) on the system side, while application daemons stay unprivileged in the user slice.

## Provider workflow

1) Generate plan/state/deploy flake artefacts:

   ```
   nix run .#hostenv-provider plan
   ```

   Outputs go to `${HOSTENV_PROVIDER_OUT:-generated}`.
   Why emit a new flake? Flake inputs are static, but client repos expose many environments (often one per branch/tag). Plan generation materialises a flake whose inputs enumerate *every environment* (repo × env), so deploy-rs can build exactly the closures in the plan without re-evaluating dynamic inputs. The bundle `plan.json` + `state.json` + `flake.nix` is the auditable deploy snapshot.
2) Safety gate + optional Cloudflare CNAME upsert:

   ```
   CF_API_TOKEN=... CF_ZONE_ID=... nix run .#hostenv-provider dns-gate [-n node]
   ```

3) Deploy (system then env profiles):

   ```
   nix run .#hostenv-provider deploy [-n node]
   ```

## Live deploy outputs in flake-parts

When `generated/plan.json` is present, the provider module exposes:

- `packages.deploy-nodes`  — NixOS systems per node (for deploy-rs).
- `packages.deploy-envs`   — environment closures per user/env.
- `deploy.nodes`           — deploy-rs node spec matching the plan.

If `generated/plan.json` is absent, these remain empty (safe default).

## Golden check

If you provide a golden plan (recommended path: `tests/golden/plan.json`, or set `provider.goldenPlanPath`), `checks.plan-golden` diffs it against the current plan to catch regressions. If none is present, the check is skipped.
