# Hostenv Provider Template

This template boots a provider flake that consumes hostenv projects and generates plan/state for deployment.

## Quick start

1. Initialize the provider flake:
   ```bash
   nix flake init -t gitlab:woolwichweb/hostenv#provider
   ```

2. Configure your nodes:
   - Copy `nodes/sample/` to `nodes/<node>/` for each server
   - Edit `nodes/<node>/configuration.nix` (set hostname, `system.stateVersion`)
   - Set `provider.hostenvHostname` in `flake.nix`

3. Enter the devshell to auto-generate files:
   ```bash
   direnv allow  # or: nix develop
   ```
   This creates `secrets/provider.yaml`, `generated/state.json`, and generates comin tokens if comin is enabled.

4. Generate the deployment plan:
   ```bash
   nix run .#hostenv-provider -- plan
   ```

That's it. The provider now tracks state and secrets automatically. Files that don't exist are created on first run. Comin tokens generate when you enter the devshell if `provider.comin.enable = true`.


## Auto-init details

The provider devshell includes an auto-initialization hook that runs when you enter the shell:

- **`secrets/provider.yaml`** - Created automatically with a generated age key if missing. You can rotate to proper sops recipients later.
- **`generated/state.json`** - Created as an empty JSON object if missing.
- **Comin tokens** - Generated automatically when `provider.comin.enable = true` and tokens don't exist.

To disable auto-init (if you prefer manual setup):
```nix
provider.plan.autoInit = false;
```


## Admin UI template

When the provider UI is enabled (GitLab SSO), it regenerates `flake.nix` from
`flake.template.nix` by injecting project inputs stored in the database.
Edit `flake.template.nix` to make structural changes while keeping the marker:

```
{{HOSTENV_PROJECT_INPUTS}}
```

`flake.nix` is treated as generated output in this flow.

## Inputs

- `hostenv` (pinned): provides the provider module and platform library.
- `nixpkgs`, `flake-parts`: follow `hostenv` pins to stay in sync.

## Outputs

- `packages.hostenv-provider`: CLI wrapper.

## Customisation tips

- Ensure client project inputs point at the `.hostenv` flake (e.g. `dir=.hostenv`) and export `outputs.lib.hostenv`.
- Use `planSource = "disk"` if you want to reuse an existing plan.json without re-evaluating inputs.
- Add extra Haskell deps for the dev shell via `provider.haskellDevPackages` (appended to `hostenv.haskell.devPackages`).
- Add provider-specific modules under `modules/` in your repo (e.g. `modules/nixos/<aspect>.nix`) and import them alongside `hostenv.flakeModules.provider` using your preferred module loader.
