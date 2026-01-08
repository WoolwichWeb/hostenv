# Dendritic Refactor Plan + Mapping

This plan aligns the repo with the Dendritic pattern described at https://dendrix.oeiuwq.com/Dendritic.html:
- Flake-parts modules under `modules/` are auto-imported via `import-tree`. Non-module helpers live under `modules/_impl` (ignored by import-tree) and are imported only where needed.
- Aspects are published under `flake.modules.<class>.<aspect>`.
- Minimal `flake.nix`; enable `flake.modules.*` via a flake-parts module (`modules/infra/flake-parts.nix`).
- Avoid `specialArgs` for cross-class wiring where feasible.
- Helpers become modules exporting via `config.flake.lib.*` (Option 1).

## Plan (detailed)

1) **Inventory current module surface and outputs**
   - List platform/hostenv-modules, platform/nixos-modules, platform/services, provider/flake-modules.
   - Record current flake outputs consumed by templates/tests.

2) **Add dendritic spine**
   - Add `modules/` tree at repo root.
   - Add `import-tree` input.
   - Update root `flake.nix` to import `inputs.import-tree ./modules`.
   - Enable `flake.modules.*` via `modules/infra/flake-parts.nix`.

3) **Convert helpers into flake-parts modules (Option 1)**
   - Replace helper-only files with modules under `modules/lib/*.nix` that export functions via `config.flake.lib.*`.
   - Downstream providers consume helpers via `config.flake.lib` (no restriction on usage).

4) **Migrate hostenv trunk**
   - Inline core hostenv modules into dendritic flake-parts modules under `modules/hostenv/`.
   - Consolidate hostenv glue into `modules/hostenv.nix`, exporting:
     - `flake.modules.hostenv.*` aspects (core/base/environments/tools)
     - `config.flake.lib.hostenv.*` helpers
     - `flake.makeHostenv` (eval wiring)
   - Replace `full-env.nix` with the eval wiring inside `modules/hostenv.nix` that assembles module lists from `config.flake.modules.hostenv`.

5) **Migrate project tools**
   - Move `platform/hostenv-modules/project-tools/*` into `modules/hostenv/tools/*.nix`.
   - Expose tools as first-class hostenv aspects (e.g. `flake.modules.hostenv.tools-*`) from those files.
   - Ensure CLI/devshell/apps wiring still happens under `perSystem` in the appropriate module(s).

6) **Migrate env-level services**
   - Move `platform/services/*` into flake-parts modules under `modules/features/`.
   - Export nginx helper modules via `modules/lib/nginx.nix` (under `flake.lib.hostenv.nginx`).
   - Keep perSystem tooling (e.g. CLI or provider service build) in dedicated modules.

7) **Migrate host-level NixOS branches**
   - Move `platform/nixos-modules/*` into flake-parts modules under `modules/nixos/`.

8) **Refactor provider entrypoints**
   - Move `provider/flake-module.nix` and `provider/flake-modules/tooling.nix` into `modules/entrypoints/provider/`.
   - Define `provider.*` options and provider outputs in a single entrypoint aggregator.
   - Gate with `provider.enable`.

9) **Refactor project entrypoints**
   - Merge `platform/flake-modules/environment-registry.nix` + `project-outputs.nix` into `modules/entrypoints/project.nix`.
   - Gate with `project.enable`.

10) **Replace `make-hostenv` wiring**
   - Move logic from `platform/flake-modules/make-hostenv.nix` into `modules/hostenv.nix`.
   - Use `config.flake.modules.hostenv` and avoid `specialArgs` where possible.
   - Expose `flake.makeHostenv` only if still needed.

11) **Remove `specialArgs` for cross-class communication**
   - Refactor provider system builder (now `flake.lib.provider.nixosSystem`), provider plan helper (now `flake.lib.provider.plan`), and any hostenv submodule arg usage.
   - Replace with flake-parts options, let-bindings, or explicit `_module.args` modules in lists.
   - Keep any `specialArgs` usage confined to tests harnesses (optional to refactor).

12) **Update outputs + exports**
   - Create `modules/exports.nix` (optional) to expose `flakeModules.project` / `flakeModules.provider` as thin wrappers.
   - Ensure `flake.modules.*` classes are the primary mechanism.

13) **Update templates**
   - `template/project/.hostenv/flake.nix` imports `(inputs.import-tree (hostenv + "/modules"))` and `hostenv.flakeModules.project`.
   - `template/provider/flake.nix` imports `(inputs.import-tree (hostenv + "/modules"))` and `hostenv.flakeModules.provider`.

14) **Delete sub-flakes**
   - Delete `platform/flake.nix`, `platform/flake-modules/*`, `provider/flake.nix`, `provider/flake-modules/*` once replacements are in place.
   - Update any internal references.

15) **Update tests and docs**
   - Adjust paths/output names in `tests/`, `docs/`, and README(s) to match new structure.
   - Update this plan if needed.

16) **Validate**
   - Run `nix flake check` (or relevant checks).
   - Ensure project and provider templates evaluate.
   - Ensure provider plan/tooling still builds.

## Mapping (current → proposed)

### Root / infra
- `flake-parts/docs.nix` → `modules/infra/docs.nix`
- `flake-parts/tests.nix` → `modules/infra/tests.nix`
- `flake-parts/devshells.nix` → `modules/infra/devshells.nix`
- `flake-parts/templates.nix` → `modules/infra/templates.nix`

### Platform trunk + env modules
- `platform/hostenv-modules/*` → inline into `modules/hostenv/*.nix` (core aspects).
- `modules/hostenv.nix` exports:
  - `flake.modules.hostenv.core`
  - `flake.modules.hostenv.base`
  - `flake.modules.hostenv.environments`
  - `flake.modules.hostenv.tools-*` (project tools aspects)
  - `flake.makeHostenv`
  - `config.flake.lib.hostenv.module` / `environmentModule`
- `platform/hostenv-modules/public-environments.nix` → `modules/lib/public-environments.nix`

### Platform services (env-level features)
- `platform/services/*` → `modules/features/*.nix` (flake-parts modules exporting `flake.modules.hostenv.*`)
- Nginx helpers are exported via `modules/lib/nginx.nix` under `flake.lib.hostenv.nginx`.

### Platform NixOS modules (host-level branches)
- `platform/nixos-modules/*` → `modules/nixos/*.nix` (flake-parts modules exporting `flake.modules.nixos.*`).

### Platform flake-modules (entrypoint wiring)
- `platform/flake-modules/environment-registry.nix` → `modules/entrypoints/project.nix`
- `platform/flake-modules/project-outputs.nix` → `modules/entrypoints/project.nix`
- `platform/flake-modules/make-hostenv.nix` → `modules/hostenv.nix`
- `platform/flake-modules/cli.nix` → `modules/features/cli.nix`
- `platform/flake-modules/hostenv-provider-service.nix` → `modules/features/provider-service-build.nix`
- `platform/flake-modules/exports.nix` → `modules/exports.nix` + `flake-modules/{project,provider}.nix` entrypoints

### Provider
- `provider/flake-module.nix` → `modules/entrypoints/provider/options.nix`
- `provider/flake-modules/tooling.nix` → `modules/entrypoints/provider/tooling.nix`
- `provider/flake-modules/exports.nix` → delete
- `provider/plan.nix` → `modules/entrypoints/provider/plan.nix` (exported as `flake.lib.provider.plan`)
- `provider/nixos-system.nix` → `modules/entrypoints/provider/options.nix` (exported as `flake.lib.provider.nixosSystem`)
- `provider/deploy-outputs.nix` → `modules/entrypoints/provider/options.nix` (exported as `flake.lib.provider.deployOutputs`)
- `provider/cli.hs` / `provider/*.cabal` → keep in `provider/`
- `provider/read-yaml.nix` → `modules/lib/read-yaml.nix`

### Templates
- `template/project/.hostenv/flake.nix` → import-tree hostenv modules + `hostenv.flakeModules.project`
- `template/provider/flake.nix` → import-tree hostenv modules + `hostenv.flakeModules.provider`

## Known anti-dendritic spots to refactor
- Test harness helpers still accept `specialArgs` for convenience (optional cleanup).

## Notes
- Use `inputs.flake-parts.flakeModules.modules` to enable `flake.modules.*`.
- The “hostenv” class is a custom class akin to `nixos`/`homeManager` in dendritic terms.
- Helpers are exported via `config.flake.lib.*` so downstream providers can consume them flexibly.
