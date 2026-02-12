# Hostenv Provider Template

This template boots a provider flake that consumes hostenv projects and generates plan/state for deployment.

## Quick start

1. Copy this template (or `nix flake init -t gitlab:woolwichweb/hostenv#provider` once exported).
2. If you use direnv, run `direnv allow` to load the dev shell from `.envrc`.
3. Set `provider.hostenvHostname`, `provider.deployPublicKeys`, and your node mappings in `flake.nix`.
4. The template ships starter node stubs:
   - `nodes/sample/` (copy to `nodes/<node>/` and edit).
5. Create `secrets/secrets.yaml` with `sops` (provider uses this at deploy time).
6. Create `generated/state.json` (can be `{}` initially).
7. Add NixOS node configs under `nodes/<node>/configuration.nix` (with `system.stateVersion`).
8. Run `nix run .#hostenv-provider -- plan` to write `generated/{flake.nix,plan.json,state.json}`.
9. Deploy using your tool of choice (e.g. deploy-rs) pointing at `generated/flake.nix`.

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
