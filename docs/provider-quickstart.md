# Provider Quickstart (hostenv)

1) Add hostenv as an input:

```nix
{
  inputs.hostenv.url = "gitlab:woolwichweb/hostenv";
  inputs.hostenv-platform = {
    url = "gitlab:woolwichweb/hostenv?dir=platform";
    inputs.nixpkgs.follows = "hostenv/nixpkgs";
    inputs.flake-parts.follows = "hostenv/flake-parts";
  };
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
 outputs = inputs@{ self, flake-parts, hostenv, ... }:
   flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ hostenv.flakeModules.provider ];
      systems = [ "x86_64-linux" "aarch64-linux" ];
      perSystem = { config, ... }: {
        provider = {
          hostenvHostname = "hostenv.sh";
          nodeSystems = { backend01 = "aarch64-linux"; backend02 = "aarch64-linux"; backend03 = "x86_64-linux"; backend04 = "aarch64-linux"; };
          planSource = "eval"; # or "disk"
        };
      };
    };
}
```

**Important:** each client project flake must export `outputs.hostenv.<system>.environments`
so the provider can discover environments. The shipped project template already does this.
Client inputs should point at the `.hostenv` flake (e.g. `dir=.hostenv`) so `hostenv.nix` is at the flake root.

2) Populate nodes and secrets:
   - Copy `nodes/sample` to `nodes/<node>/` and edit `configuration.nix`/`hardware-configuration.nix`.
   - Create `secrets/secrets.yaml` with sops.
   - Create `generated/state.json` (can be `{}` initially).

3) Generate plan/state (optional if using planSource=eval):

```
nix run .#hostenv-provider plan
```

Why the generated `flake.nix` exists: flake inputs are static, but your client
repos can have many environments (often one per branch/tag). Plan generation
materialises a new flake whose inputs enumerate each environment (repo × env),
so deploy-rs can build the exact activation packages and NixOS systems without
re-evaluating the dynamic hostenv graph. The bundle `generated/{plan.json,
state.json,flake.nix}` is the deployable, auditable snapshot.

4) DNS/ACME safety + Cloudflare (optional):

```
CF_API_TOKEN=... CF_ZONE_ID=... nix run .#hostenv-provider dns-gate [-n node]
```

5) Deploy:

```
nix run .#hostenv-provider deploy [-n node]
```

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
    webhookSecretFile = "/run/secrets/hostenv/webhook_token"; # optional global secret
    webhookSecretsDir = "/run/secrets/hostenv/webhooks"; # optional per-project secrets
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
- If `webhookSecretsDir` is set, the service looks for secret files named either
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
    gitlabOAuthSecretsFile = "/run/secrets/hostenv/gitlab_oauth";
    webhookSecretsDir = "/run/secrets/hostenv/webhooks";
    # uiHost defaults to webhookHost; uiBasePath defaults to /ui
  };
}
```

The secrets file is a simple `key=value` file, for example:

```
client_id=...
client_secret=...
```

Notes:

- The UI is available at `https://<uiHost>/ui` by default.
- OAuth scopes requested: `api`, `read_repository`.
- `gitlabHosts` defaults to `["gitlab.com"]`; set it to allow additional GitLab hosts.
- Add your admin user by setting their `users.role` in the database (defaults to `user`).
- The UI regenerates `flake.nix` from a template (`flake.template.nix`) using
  projects stored in PostgreSQL, and writes a git credential store file so
  `nix flake update <org>__<project>` can access private GitLab repositories.
  Keep structural edits in `flake.template.nix`, not `flake.nix`.
- The template must include the `{{HOSTENV_PROJECT_INPUTS}}` marker.
- Tokens are stored unencrypted at rest in the database and git credential file.
