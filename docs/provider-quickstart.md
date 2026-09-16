# Provider Quickstart (hostenv)

## 1. Initialize the provider

Start with the provider template so all required flake inputs are declared:

```bash
nix flake init -t gitlab:woolwichweb/hostenv#provider
```

In the template's `provider` configuration, set the hostname, deployment SSH
keys, and node mappings. For example:

```nix
provider = {
  hostenvHostname = "hosting.example.com";
  deployPublicKeys = [ "ssh-ed25519 AAAA..." ]; # replace with your key
  nodeSystems.node-a = "x86_64-linux";
  nodeFor.default = "node-a";
};
```

`nodeSystems` is keyed by actual node names; `default` is a fallback in
`nodeFor`, not in `nodeSystems`. Add `nodeFor.production`, `nodeFor.testing`,
or `nodeFor.development` only when those types need a different node.

Optional provider knobs:

- `provider.nodeModules = [ "nodes/common.nix" ];` (paths are relative to the provider root).
- `provider.generatedFlake.inputs = { ... };` to inject extra inputs into `generated/flake.nix`.
- `provider.generatedFlake.envInputs.follows = { ... };` to override per-environment `inputs.*.follows`.
- `provider.generatedFlake.envInputs.extra = env: { ... };` to merge extra attrs into each environment input.

**Important:** add each client project as an `organisation__project` input.
Each client project flake must export `outputs.lib.hostenv.<system>.environments`
so the provider can discover environments. The shipped project template already does this.
Client inputs should point at the `.hostenv` flake (e.g. `dir=.hostenv`) so `hostenv.nix` is at the flake root.

## 2. Populate nodes and secrets

Copy `nodes/sample` to `nodes/<node>/` and edit `configuration.nix` and
`hardware-configuration.nix`. Create `secrets/secrets.yaml` with `sops` before
planning. The CLI initializes `generated/state.json` on its first non-dry-run
plan when the file is missing.

**Keep existing state when upgrading.** It preserves UIDs and hostname
reservations. Move any custom state file to `generated/state.json` and track it
in Git before removing the old `provider.statePath` setting. See the
[provider template's upgrade notes](../template/provider/README.md#upgrading-from-configurable-plan-sources)
for the removed options. Do not replace existing state with `{}`.

Project services may request provider-managed secret files by name through
`environments.<name>.requiredSecretFiles`. Names are restricted to letters,
numbers, and underscores. The provider resolves each requested name from the
environment, then the project, then the organisation and writes it to
`/run/secrets/<environment-user>/<name>` with that environment user as owner
and group. Projects cannot choose a path, mode, owner, or arbitrary NixOS
configuration.

For example, Laravel automatically requests `laravel_env`. A provider SOPS file
can supply it at environment scope:

```yaml
myproject-main-a1b2c3d:
  backups_secret: ENC[AES256_GCM,...]
  backups_env: ENC[AES256_GCM,...]
  laravel_env: |
    APP_KEY=base64:replace-with-the-generated-application-key
    MAIL_USERNAME=optional-external-service-user
    MAIL_PASSWORD=optional-external-service-password
```

The same key may instead be placed below `<organisation>_<project>` or
`<organisation>` to share it with narrower scopes that do not override it. The
decrypted `laravel_env` value must use the shell/systemd-compatible `KEY=value`
subset. Hostenv requires the file but intentionally does not inspect it for an
`APP_KEY` or restrict the environment variable names a trusted provider sets.
Provider evaluation fails with the three checked SOPS paths when a requested
key is absent.

## 3. Generate the plan

```bash
nix run .#hostenv-provider -- plan
```

Planning always evaluates the project inputs. It writes `generated/plan.json`,
`generated/state.json`, and `generated/flake.nix`, and updates the generated
flake's lock file. Commit the generated state so subsequent plans reuse it.

Why the generated `flake.nix` exists: flake inputs are static, but your client
repos can have many environments (often one per branch/tag). Plan generation
materialises a new flake whose inputs enumerate each environment (repo × env),
so deploy-rs can build the activation packages and NixOS systems from that
snapshot. Deployment still reads `generated/plan.json`; removing disk-mode
planning does not remove the snapshot or regenerate it during deployment.

## 4. DNS/ACME safety and Cloudflare (optional)

```bash
CF_API_TOKEN=... CF_ZONE_ID=... nix run .#hostenv-provider -- dns-gate [--with-dns-update] [-n node]
```

## 5. Deploy

```bash
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

## Outputs

The provider root exposes `packages.<system>.hostenv-provider`,
`apps.<system>.hostenv-provider`, and `lib.provider.planPaths.<system>`.
The latter contains the generated plan, state, and flake store paths used by
the CLI. For example, `nix build --no-link --print-out-paths
.#lib.provider.planPaths.x86_64-linux.plan` builds a plan without installing it
into `generated/`.

The generated flake, not the provider root, exposes `deploy.nodes`,
`nixosConfigurations.<node>`, `packages.<system>.node-<node>`, and
`packages.<system>.env-<environment-user>`.

Add Haskell development dependencies through
`perSystem.provider.haskellDevPackages`; these are included in
`hostenv.haskell.devPackages` for the same system.

Optional per-environment settings:

- `environments.<name>.hostenv.backupsRepoHost`, `backupsEnvFile`, `backupsSecretFile` for restic repo + secrets
- `environments.<name>.requiredSecretFiles` for validated provider-managed runtime secret names
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
  (run `nix run .#hostenv-provider -- plan` at least once).
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
    # gitlabHosts = [ "gitlab.com" ];
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
