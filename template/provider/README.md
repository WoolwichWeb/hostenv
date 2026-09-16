# Hostenv Provider Template

This template consumes hostenv projects and generates the plan, persistent state,
and flake used to deploy them.

## Quick start

1. Initialize the provider repository with `nix flake init -t gitlab:woolwichweb/hostenv#provider`.
2. Set `provider.hostenvHostname`, `provider.deployPublicKeys`, and the node mappings in `flake.nix`. Add each client project as an `organisation__project` input.
3. Copy `nodes/sample/` to `nodes/<node>/` and edit the configuration files. Each node needs `configuration.nix`, including `system.stateVersion` and its machine-specific configuration.
4. Enter the development shell with `direnv allow` or `nix develop`.
5. Create `secrets/secrets.yaml` with `sops` before generating the plan.
6. Run `nix run .#hostenv-provider -- plan` to write `generated/{flake.nix,plan.json,state.json}`. On a first run, the CLI creates and stages an empty `generated/state.json` if it is missing.
7. Run `nix run .#hostenv-provider -- dns-gate` for DNS/ACME checks, then `nix run .#hostenv-provider -- deploy`.

Planning always evaluates the project inputs. Deployment consumes the generated
snapshot; removing disk-mode planning does not remove `generated/plan.json` or
make the deploy command regenerate it.

**Preserve and commit `generated/state.json`.** It retains environment UIDs,
node locations, and hostname reservations, including entries for retired
environments. An empty state is appropriate only for a new provider, not as an
upgrade or troubleshooting step.

Framework services may request named runtime files. Laravel requests
`laravel_env`, so add a shell/systemd-compatible multiline value at environment,
project, or organisation scope:

```yaml
project-main-a1b2c3d:
  backups_secret: ENC[AES256_GCM,...]
  backups_env: ENC[AES256_GCM,...]
  laravel_env: |
    APP_KEY=base64:replace-with-the-generated-application-key
    MAIL_PASSWORD=optional-external-service-password
```

The provider projects it to `/run/secrets/<environment-user>/laravel_env` and
owns it as that user. Projects can request restrictive names only; they cannot
control secret paths, modes, owners, or NixOS configuration.

## Upgrading from configurable plan sources

Remove `provider.planSource`, `provider.planPath`, `provider.statePath`, and
`provider.plan.autoInit` from the provider configuration. They are no longer
options, and stale settings are evaluation errors rather than ignored values.

Before removing a custom `provider.statePath`, move the existing state to
`generated/state.json` and add it to version control. Do not replace that state
with `{}`. A hand-maintained disk plan must instead be represented by project
inputs and provider configuration before running the planner.

The lower-level `lib.provider.plan` function still accepts `statePath` and
`lockPath` arguments for explicit callers and tests. These are not provider
module options. `lib.provider.planPaths.<system>` remains the CLI's output API.

## Admin UI template

When the provider UI is enabled (GitLab SSO), it regenerates `flake.nix` from
`flake.template.nix` by injecting project inputs stored in the database.
Edit `flake.template.nix` to make structural changes while keeping the marker:

```
{{HOSTENV_PROJECT_INPUTS}}
```

`flake.nix` is treated as generated output in this flow.

## Outputs and customisation

`packages.<system>.hostenv-provider` and `apps.<system>.hostenv-provider` provide
the CLI. `lib.provider.planPaths.<system>` exposes the generated plan, state,
and flake store paths.

Client inputs must point at the `.hostenv` flake (for example, `dir=.hostenv`)
and export `lib.hostenv.<system>.environments`. Use `provider.nodeModules` for
shared node configuration and `provider.generatedFlake` to customise generated
flake inputs. Additional Haskell development dependencies belong in
`perSystem.provider.haskellDevPackages`.
