# Hostenv Provider Template

This template boots a provider flake that consumes hostenv projects and generates plan/state for deployment.

## Quick start

1. Copy this template (or `nix flake init -t gitlab:woolwichweb/hostenv#provider` once exported).
2. Set `provider.hostenvHostname`, `provider.deployPublicKey`, and your node mappings in `flake.nix`.
3. The template ships starter stubs:
   - `nodes/sample/` (copy to `nodes/<node>/` and edit).
   - `secrets/secrets.yaml.example` (copy to `secrets/secrets.yaml` and encrypt with sops).
4. Create `generated/state.json` (can be `{}` initially).
5. Add NixOS node configs under `nodes/<node>/configuration.nix` (with `system.stateVersion`).
6. Run `nix run .#hostenv-provider-plan` to write `generated/{flake.nix,plan.json,state.json}`.
7. Deploy using your tool of choice (e.g. deploy-rs) pointing at `generated/flake.nix`.

## Inputs

- `hostenv` (pinned): provides the provider module and hostenv modules.
- `nixpkgs`, `flake-parts`: follow `hostenv` pins to stay in sync.

## Outputs

- `apps.hostenv-provider-plan`: generate plan/state/flake into `generated/` (or `$HOSTENV_PROVIDER_OUT`).
- `packages.hostenv-provider`: CLI wrapper.

## Customisation tips

- Ensure client project inputs point at the `.hostenv` flake (e.g. `dir=.hostenv`).
- Override `nodesPath`, `secretsPath`, or `planSource = "disk"` if you keep generated artifacts elsewhere.
- Add provider-specific modules under `modules/nixos` in your repo and import them via flake-parts alongside `providerModule`.
