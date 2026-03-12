# Provider Quickstart (hostenv)

## Prerequisites

- Nix with flakes enabled
- Access to the servers you'll be configuring as nodes
- Git repository for your provider configuration

## Setup

### Step 1: Initialize the provider template

```bash
nix flake init -t gitlab:woolwichweb/hostenv#provider
```

This creates the basic provider structure including:

- `flake.nix` with hostenv provider module
- `nodes/sample/` directory as a template for node configurations
- `.gitignore` for generated files

### Step 2: Configure your nodes

1. Copy the sample node configuration for each server:

   ```bash
   cp -r nodes/sample nodes/<node-name>
   ```

2. Edit `nodes/<node-name>/configuration.nix`:
   - Set the hostname
   - Set `system.stateVersion` (use your NixOS version, e.g., `"24.05"`)
   - Add any node-specific configuration

3. Edit `nodes/<node-name>/hardware-configuration.nix` with your server's hardware settings

4. Update `flake.nix`:
   - Set `provider.hostenvHostname` to your provider's domain
   - Add your nodes to `provider.nodeSystems` with their architectures

### Step 3: Enter the development shell

```bash
direnv allow  # or: nix develop
```

The devshell automatically handles what used to be manual setup:

- **Secrets**: Creates `secrets/provider.yaml` with an ephemeral age key if missing
- **State**: Creates `generated/state.json` (empty `{}`) if missing
- **Provider node tokens**: If `provider.deploy.enable = true`, prompts to generate node authentication tokens
- **Cache secrets**: Prompts to generate `cache_signing_key`, `cache_auth_password`, and `generated/cache-public-key.txt` when missing

You'll see interactive prompts for any missing components. Confirm to auto-generate, or cancel to set up manually later.

### Step 4: Generate plan and deploy

Generate the deployment plan:

```bash
nix run .#hostenv-provider -- plan
```

This creates `generated/{flake.nix,plan.json,state.json}` with all your environments enumerated.

Optional: Run DNS/ACME safety check:

```bash
CF_API_TOKEN=... CF_ZONE_ID=... nix run .#hostenv-provider -- dns-gate [--with-dns-update] [-n node]
```

Deploy to your nodes using your chosen deployment method (see Architecture section below).

## What Gets Auto-Generated

### Secrets file (`secrets/provider.yaml`)

Created automatically on first devshell entry with:

- An ephemeral age key for encryption
- Empty structure ready for your secrets

**Important**: The ephemeral key is for bootstrapping only. Rotate to proper recipients before production use.

### State file (`generated/state.json`)

Created as `{}` if missing. This tracks deployment state across runs.

### Provider node tokens

If using provider-deploy for deployment, the devshell prompts to generate node authentication tokens. This replaces manual token setup.

## Architecture

Hostenv uses a layered architecture:

1. **Project config** (in client repos): Defines services, environments, and hostnames
2. **Project flake** (in client repos): Evaluates environments and exposes activation packages
3. **Provider flake** (this repo): Defines nodes, maps environments to nodes, generates deployment plans
4. **Node systems**: NixOS machines that activate environment packages

The generated `flake.nix` exists because flake inputs are static, but client repos can have many environments (often one per branch). Plan generation materializes a new flake whose inputs enumerate each environment (repo × env), so the provider pipeline can build exact activation packages without re-evaluating the dynamic hostenv graph.

The bundle `generated/{plan.json,state.json,flake.nix}` is the auditable snapshot of what will be deployed.

## Provider configuration options

### Required

```nix
{
  provider = {
    hostenvHostname = "hostenv.example.com";
    nodeSystems = {
      backend01 = "aarch64-linux";
      backend02 = "x86_64-linux";
    };
  };
}
```

### Optional

```nix
{
  provider = {
    # Additional modules applied to all nodes
    nodeModules = [ "nodes/common.nix" ];

    # Extra inputs injected into generated/flake.nix
    generatedFlake.inputs = { /* ... */ };

    # Override per-environment inputs
    generatedFlake.envInputs.follows = { /* ... */ };

    # Provider-deploy configuration (for node agent deployment)
    deploy = {
      enable = true;
      # providerApiBaseUrl and nodeAuthTokenFile are required when provider-service
      # is not enabled. When provider.serviceResolution points to an environment with
      # services.hostenv-provider.enable = true, these defaults are derived
      # automatically from the service configuration.
      providerApiBaseUrl = "https://hostenv.example.com";
      nodeAuthTokenFile = "/run/secrets/hostenv/provider_node_token";
    };
  };
}
```

**Important**: Each client project flake must export `outputs.lib.hostenv.<system>.environments` so the provider can discover environments. The shipped project template already does this.

Client inputs should point at the `.hostenv` flake (e.g., `dir=.hostenv`) so `hostenv.nix` is at the flake root.

## End-to-end demos

Interactive wizard with local VMs:

```bash
./examples/local-provider-migration/run-demo.sh
```

Automated demo (no interaction, cleans up after):

```bash
./examples/local-provider-migration/run-demo.sh --automated --cleanup
```

The local demo uses `hostctl` to install temporary hostname mappings for the demo VMs and removes them during teardown.

## Provider webhook service (optional)

Hostenv ships an optional webhook service that can listen for GitHub/GitLab webhooks and run the `plan → dns-gate → deploy-intent` workflow automatically. It runs inside a hostenv environment and is proxied through that environment's nginx.

Example configuration:

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

`<webhookHash>` should be the `environments.<default>.hostenv.projectNameHash` value from the client project (usually the default environment hash).

**Notes**:

- If `$XDG_DATA_HOME/hostenv-provider` is missing, the service starts in onboarding mode. An admin logs into the dashboard and bootstraps the provider repository by selecting a GitLab repository to clone.
- The service resolves hashes from `generated/plan.json`, so ensure a plan exists (run `nix run .#hostenv-provider plan` at least once).
- Webhook requests resolve the project by matching `<hash>` to `hostenv.projectNameHash` in `generated/plan.json`, then run `nix flake update <org>__<project>`, generate a new plan, run dns-gate, and deploy every node hosting environments for that org/project.
- If a webhook signature header is present, the service verifies it against the configured secret (GitHub `X-Hub-Signature-256` or GitLab `X-Gitlab-Token`).
- **Warning**: Encrypted secrets (e.g., sops files) from the provider repo will still be stored in the Nix store on the deploy machine. They remain encrypted, but they will be present.
- If a webhook secrets directory is configured, the service looks for secret files named either `<hash>` or `<org>__<project>` to verify signatures for each project.

## Provider admin UI (GitLab SSO) (optional)

The provider service exposes a minimal admin UI for adding GitLab projects and managing the generated flake inputs. It uses GitLab OAuth and a local PostgreSQL database for sessions and project metadata.

Example configuration:

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

The secrets file is a simple `key=value` file:

```
client_id=...
client_secret=...
```

**Notes**:

- The UI is available at `https://<uiHost>/dashboard` by default.
- OAuth scopes requested: `api`, `read_repository`.
- `gitlab.hosts` defaults to `["gitlab.com"]`; set it to allow additional GitLab hosts.
- `gitlab.deployTokenTtlMinutes` defaults to `60`; deploy runs still revoke per-run tokens immediately on completion/failure.
- `gitlab.oAuthSecretsFile` defaults to `/run/secrets/<hostenv.userName>/gitlab_oauth`.
- `gitlab.tokenEncryptionKeyFile` defaults to `/run/secrets/<hostenv.userName>/gitlab_token_key`.
- Provider service secret path options are read-only; hostenv controls where they are mounted on server.
- `hostenv secrets` scaffolding auto-generates `gitlab_token_key` (after confirmation) when GitLab OAuth is enabled and the key is missing from both project/current-environment secrets.
- Add your admin user by setting their `users.role` in the database (defaults to `user`).
- The UI regenerates `flake.nix` from a template (`flake.template.nix`) using projects stored in PostgreSQL. Keep structural edits in `flake.template.nix`, not `flake.nix`.
- Deploy/webhook runs mint a short-lived, project-scoped GitLab project access token and inject it via per-run `NIX_CONFIG` (`access-tokens = ...`).
- The template must include the `{{HOSTENV_PROJECT_INPUTS}}` marker.
- Persisted OAuth tokens are encrypted at rest in PostgreSQL using `gitlab.tokenEncryptionKeyFile`.

## Optional per-environment settings

- `environments.<name>.hostenv.backupsRepoHost`, `backupsEnvFile`, `backupsSecretFile` for restic repo + secrets
- `environments.<name>.virtualHosts.<host>.allowIndexing` to control search engine indexing
- `environments.<name>.virtualHosts.<host>.security` for CSP and common headers (`csp`, `cspMode`, `cspReportTo`, `reportTo`, `referrerPolicy`, etc.)
- `hostenv.monitoring.enable` to turn on basic exporters/labels

## Troubleshooting

### Manual provider node token generation

If you skipped the devshell prompt or need to regenerate tokens:

```bash
nix run .#hostenv-provider -- node-tokens
```

### Manual cache secret generation

If you skipped the devshell prompt or need to regenerate cache signing/auth material:

```bash
nix run .#hostenv-provider -- cache-secrets
```

### Plan generation fails

Ensure `generated/state.json` exists (even if just `{}`) and `secrets/provider.yaml` is properly encrypted with sops.

### Node not found

Verify your node configuration exists at `nodes/<name>/configuration.nix` and the node name matches entries in `provider.nodeSystems`.

## Outputs

- Add Haskell dev shell deps via `provider.haskellDevPackages` (appended to `hostenv.haskell.devPackages`).
