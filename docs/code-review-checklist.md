# Hostenv flake-parts dendritic pattern code-review checklist

## A. Overall layout and naming

- [ ] Top-level layout is clear and small:
  - `flake.nix` is minimal.
- Main logic lives in `modules/`.
- [ ] There is a clear separation between:
  - platform / shared modules (e.g. hostenv environments, services),
  - host-specific or deployment bits (e.g. NixOS host roles, hardware config).
- [ ] Each module file has a single, coherent concern (e.g. `phpfpm.nix`, `nginx.nix`, `backups.nix`), rather than “kitchen sink” files.
- [ ] Filenames and directory names are descriptive enough that a new developer can guess what they do.
- [ ] Optional: there is a brief top-level `README.md` describing the structure of the repo.

## B. `flake.nix` and flake-parts glue

- [ ] `flake.nix` uses `flake-parts.lib.mkFlake { … }` (or equivalent) for outputs, rather than hand-rolling everything.
- [ ] Inputs (`nixpkgs`, `flake-parts`, `home-manager`, `phps`, etc.) are declared in one place and wired via `inputs`, not re-imported ad-hoc.
- [ ] The `outputs` function in `flake.nix` is small and mostly delegates to flake-parts modules.
- [ ] Flake-parts `imports` is tidy:
  - either a small explicit list of modules, **or**
  - an auto-import mechanism with clear rules (e.g. “import all modules under `./modules` except `*_test.nix`”).
- [ ] There is no recursive import of `flake.nix` from inside modules.

## C. Hostenv configuration model (the “trunk”)

*(This is the central data model other modules branch from, e.g. `config.hostenv.*` and `config.environments.*`.)*

- [ ] There is a dedicated module (or small cluster of modules) that defines the core hostenv options, e.g.:
  - `hostenv.organisation`, `hostenv.project`, `hostenv.environmentName`, etc.
  - `environments.<name>` for per-environment configuration.
  - Optional `hostenv.hosts.<name>` for NixOS hosts / roles, if applicable.
- [ ] Core options use proper `mkOption` types (e.g. `types.str`, `types.enum`, `types.attrsOf (submodule …)`) rather than `anything`.
- [ ] The “shape” of environment data is consistent and documented (e.g. how `virtualHosts`, users, PHP versions, databases, etc. are represented).
- [ ] One module is clearly the canonical place for the environment schema (no duplicate or diverging definitions scattered around).
- [ ] Global defaults (`allEnvironments`, default users, default environment type, etc.) are expressed in one place and reused.

## D. Dendritic structure: features branching from the trunk

*(Each feature module reads from the central model and adds its own concern.)*

For each feature module (e.g. `phpfpm.nix`, `nginx.nix`, `restic.nix`, `monitoring.nix`, `hostenv-cli.nix`, etc.):

- [ ] The module reads from shared state (e.g. `config.hostenv`, `config.environments`) rather than maintaining a parallel hand-crafted list of environments.
- [ ] The module does **not** redefine its own mini-schema for environments; it uses the central schema wherever possible.
- [ ] Cross-cutting behaviour lives in small, focused modules, not in one mega-module.
- [ ] There is minimal copy-paste between feature modules; shared patterns are factored into helper functions or common submodules if needed.

## E. `perSystem` and outputs

- [ ] A `perSystem = { system, pkgs, ... }: { … }` block is used to define:
  - tools / CLIs (e.g. `hostenv`),
  - development shells (`devShells`),
  - utility packages and scripts.
- [ ] `perSystem` is used consistently for system-dependent stuff; the same logic is not duplicated elsewhere.
- [ ] Flake outputs (`packages`, `apps`, `devShells`, `nixosConfigurations`, possibly `homeConfigurations`) are constructed through flake-parts / `perSystem` rather than ad-hoc logic.
- [ ] Default output(s) are sensible (e.g. `packages.default` is a useful default such as docs or CLI).

## F. NixOS integration for hostenv (if present)

*(Only applicable if this flake builds NixOS systems as well as hostenv environments.)*

- [ ] There is a base NixOS module for hostenv machines (e.g. `nixos/hostenv-base.nix`) that:
  - consumes `config.hostenv` / `config.environments`,
  - sets up users / systemd user slices,
  - configures `/run/hostenv/user/...` (or equivalent runtime layout),
  - configures logging / monitoring hooks that every hostenv machine should have.
- [ ] Per-host modules (e.g. `nixos/hosts/<name>.nix`) are limited to:
  - hardware specifics,
  - IPs / networking,
  - host-specific toggles,  
  and do **not** duplicate platform logic.
- [ ] NixOS configurations are exposed via `flake.nixosConfigurations.<host>` with a clear pattern for adding new hosts.

## G. Separation of concerns and layering

- [ ] There is a clear directional dependency:
  - core model (`hostenv`, `environments`) → feature modules → host / role modules → final outputs,
  not circular references.
- [ ] Hardware / host-specific modules do not know implementation details of individual features (e.g. they don’t manually configure PHP-FPM pools).
- [ ] Feature modules do not hard-code hardware details (IP addresses, block devices, etc.).
- [ ] Where extra inputs (like `inputs.nixpkgs`, `inputs.home-manager`) are needed, they are passed via the module system (`_module.args`) rather than re-importing `flake.nix` or using global hacks.

## H. Extensibility and ergonomics

- [ ] Adding a new environment is a single, obvious action (e.g. adding an entry under `environments.<name>`), and all relevant features (PHP, Nginx, backups, monitoring, etc.) pick it up automatically.
- [ ] Adding a new feature is straightforward:
  - create a new module file,
  - import it in the main flake-parts configuration,
  - read from the central `hostenv` / `environments` model without extra plumbing.
- [ ] Special cases for particular clients or environments are expressed via flags / configuration on the environment model, not scattered `if name == "foo-prod"` checks throughout the codebase.
- [ ] Common workflows (e.g. `nix develop`, `hostenv` CLI, `nix flake check`) are documented and easy to run.

## I. Safety, invariants and validation

- [ ] Invariants are enforced using:
  - Nix assertions (e.g. forbidden configuration combinations),
  - validation logic in `apply` functions when necessary.
- [ ] Error messages for failed assertions are clear and point to what needs changing.
- [ ] Potentially dangerous behaviour (e.g. destructive operations, forced restarts, `rm -rf`) is explicit and documented.
- [ ] Secrets handling (if present) is consistent (e.g. via `sops-nix` or similar) and not sprinkled plain-text credentials.
- [ ] There is a clear and documented way to disable or “retire” an environment (maintenance mode, DNS cut, or `enable = false` that modules respect).

## J. Documentation and discoverability

- [ ] There is at least a minimal `README` covering:
  - what the flake is for,
  - where environment definitions live,
  - how to add or remove environments,
  - how to add or remove hosts (if applicable),
  - how to add a new feature module.
- [ ] Non-obvious design decisions are documented either:
  - in `docs/` or
  - near the relevant code as concise comments.
- [ ] Complex options or modules have short inline descriptions explaining how they interact (especially cross-cutting features like backups, monitoring, auth, etc.).
- [ ] If a documentation site, search index, or `nix flake show` output exists, it is wired up and mentioned in the docs.

## K. Testing and deployment hooks

- [ ] `nix flake check` is defined and does something useful (e.g. module tests, VM tests, static checks).
- [ ] If NixOS VM or container tests exist, they reuse the same modules and environment definitions, not a parallel test-only configuration.
- [ ] Any deployment tooling (e.g. `deploy-rs`, `colmena`, bespoke scripts) reads from flake outputs, not from separate, unsynchronised definitions.
- [ ] There is a documented deployment flow (even if informal) that links:
  - configuration changes,
  - tests,
  - deployment commands.
