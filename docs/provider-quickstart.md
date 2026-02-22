# Provider Quickstart (hostenv)

1) Add hostenv as an input:

```nix
{
  inputs.hostenv = {
    url = "gitlab:woolwichweb/hostenv";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.flake-parts.follows = "flake-parts";
    inputs.phps.follows = "phps";
  };
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.phps.url = "gitlab:woolwichweb/nix-phps-lts";
  outputs = inputs@{ self, flake-parts, hostenv, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
    imports = [
      hostenv.flakeModules.provider
    ];
      systems = [ "x86_64-linux" "aarch64-linux" ];
      provider = {
        hostenvHostname = "hostenv.sh";
        nodeSystems = { backend01 = "aarch64-linux"; backend02 = "aarch64-linux"; backend03 = "x86_64-linux"; backend04 = "aarch64-linux"; };
        planSource = "eval"; # or "disk"
      };
    };
}
```

Optional provider knobs:

- `provider.nodeModules = [ "nodes/common.nix" ];` (paths are relative to the provider root).
- `provider.generatedFlake.inputs = { ... };` to inject extra inputs into `generated/flake.nix`.
- `provider.generatedFlake.envInputs.follows = { ... };` to override per-environment `inputs.*.follows`.
- `provider.generatedFlake.envInputs.extra = env: { ... };` to merge extra attrs into each environment input.

**Important:** each client project flake must export `outputs.lib.hostenv.<system>.environments`
so the provider can discover environments. The shipped project template already does this.
Client inputs should point at the `.hostenv` flake (e.g. `dir=.hostenv`) so `hostenv.nix` is at the flake root.

2) Populate nodes and secrets:
   - Copy `nodes/sample` to `nodes/<node>/` and edit `configuration.nix`/`hardware-configuration.nix`.
   - Create `secrets/secrets.yaml` with sops.
   - Create `generated/state.json` (can be `{}` initially).

3) Generate plan/state (optional if using planSource=eval):

```
nix run .#hostenv-provider -- plan
```

Why the generated `flake.nix` exists: flake inputs are static, but your client
repos can have many environments (often one per branch/tag). Plan generation
materialises a new flake whose inputs enumerate each environment (repo × env),
so deploy-rs can build the exact activation packages and NixOS systems without
re-evaluating the dynamic hostenv graph. The bundle `generated/{plan.json,
state.json,flake.nix}` is the deployable, auditable snapshot.

4) DNS/ACME safety + Cloudflare (optional):

```
CF_API_TOKEN=... CF_ZONE_ID=... nix run .#hostenv-provider -- dns-gate [--with-dns-update] [-n node]
```

5) Deploy:

```
nix run .#hostenv-provider -- deploy [-n node]
```

End-to-end local VM demo (interactive wizard):

```
./examples/local-provider-migration/run-demo.sh
```

Automated end-to-end demo:

```
./examples/local-provider-migration/run-demo.sh --automated --cleanup
```

The local demo uses `hostctl` to install temporary hostname mappings for the demo VMs and removes them during teardown/abort.

Outputs:

- `packages.deploy-nodes` / `packages.deploy-envs` per system when a plan exists.
- Deploy specs live at `lib.hostenv.deploySpec` (per flake output). Example:
  `nix eval .#lib.hostenv.deploySpec --json | jq` (planSource=eval recommended).
- Add Haskell dev shell deps via `provider.haskellDevPackages` (appended to `hostenv.haskell.devPackages`).

Optional per-environment settings:

- `environments.<name>.hostenv.backupsRepoHost`, `backupsEnvFile`, `backupsSecretFile` for restic repo + secrets
- `environments.<name>.virtualHosts.<host>.allowIndexing` to control search engine indexing
- `environments.<name>.virtualHosts.<host>.security` for CSP and common headers (`csp`, `cspMode`, `cspReportTo`, `reportTo`, `referrerPolicy`, etc.)
- `hostenv.monitoring.enable` to turn on basic exporters/labels

## Provider webhook service (optional)

Hostenv ships an optional webhook service that can listen for GitHub/GitLab webhooks and run the
`plan → dns-gate → deploy` workflow automatically. It runs inside a hostenv environment and is
proxied through that environment’s nginx.

Example (in a provider hostenv environment config):

```nix
{
  services.hostenv-provider = {
    enable = true;
    # webhookHost defaults to hostenv.hostname
  };
}
```

The webhook URL is:

```
https://<webhookHost>/webhook/<webhookHash>
```

`<webhookHash>` should be the `environments.<default>.hostenv.projectNameHash` value from the
client project (usually the default environment hash).

Notes:

- On first boot the service copies the provider repo into `$XDG_DATA_HOME/hostenv-provider` if
  `flake.nix` or `generated/state.json` are missing, then runs a broad `nix flake update`.
- The service resolves hashes from `generated/plan.json`, so ensure a plan exists
  (run `nix run .#hostenv-provider plan` at least once).
- Webhook requests resolve the project by matching `<hash>` to
  `hostenv.projectNameHash` in `generated/plan.json`, then run
  `nix flake update <org>__<project>`, generate a new plan, run dns-gate, and deploy
  every node hosting environments for that org/project.
- If a webhook signature header is present, the service verifies it against the
  configured secret (GitHub `X-Hub-Signature-256` or GitLab `X-Gitlab-Token`).
- **Warning:** encrypted secrets (e.g., sops files) from the provider repo will still be stored in
  the Nix store on the deploy machine. They remain encrypted, but they will be present.
- If a webhook secrets directory is configured, the service looks for secret files named either
  `<hash>` or `<org>__<project>` to verify signatures for each project.

## Provider admin UI (GitLab SSO) (optional)

The provider service exposes a minimal admin UI for adding GitLab projects and
managing the generated flake inputs. It uses GitLab OAuth and a local PostgreSQL
database for sessions and project metadata.

Example (in a provider hostenv environment config):

```nix
{
  services.postgresql = {
    enable = true;
    user = config.hostenv.userName;
    dataDir = "${config.hostenv.dataDir}/postgresql";
    runtimeDir = config.hostenv.runtimeDir;
    ensureDatabases = [ "hostenv-provider" ];
    ensureUsers = [
      {
        name = config.hostenv.userName;
        ensurePermissions = { "hostenv-provider" = "ALL PRIVILEGES"; };
      }
    ];
  };

  services.hostenv-provider = {
    enable = true;
    gitlab = {
      enable = true;
      # hosts = [ "gitlab.com" ];
      # deployTokenTtlMinutes = 60;
    };
    # uiHost defaults to webhookHost; uiBasePath defaults to /dashboard
  };
}
```

The secrets file is a simple `key=value` file, for example:

```
client_id=...
client_secret=...
```

Notes:

- The UI is available at `https://<uiHost>/dashboard` by default.
- OAuth scopes requested: `api`, `read_repository`.
- `gitlab.hosts` defaults to `["gitlab.com"]`; set it to allow additional GitLab hosts.
- `gitlab.deployTokenTtlMinutes` defaults to `60`; deploy runs still revoke per-run tokens immediately on completion/failure.
- `gitlab.oAuthSecretsFile` defaults to `/run/secrets/<hostenv.userName>/gitlab_oauth`.
- `gitlab.tokenEncryptionKeyFile` defaults to `/run/secrets/<hostenv.userName>/gitlab_token_key`.
- Provider service secret path options are read-only; hostenv controls where they are mounted on server.
- `hostenv secrets` scaffolding auto-generates `gitlab_token_key` (after confirmation) when GitLab OAuth is enabled and the key is missing from both project/current-environment secrets.
- Add your admin user by setting their `users.role` in the database (defaults to `user`).
- The UI regenerates `flake.nix` from a template (`flake.template.nix`) using
  projects stored in PostgreSQL. Keep structural edits in `flake.template.nix`,
  not `flake.nix`.
- Deploy/webhook runs mint a short-lived, project-scoped GitLab project access
  token and inject it via per-run `NIX_CONFIG` (`access-tokens = ...`).
- The template must include the `{{HOSTENV_PROJECT_INPUTS}}` marker.
- Persisted OAuth tokens are encrypted at rest in PostgreSQL using
  `gitlab.tokenEncryptionKeyFile`.
