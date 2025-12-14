# Tests layout (dendritic style)

The tests under `tests/` exercise *branches* of the module tree in isolation,
mirroring the dendritic structure:

- `plan-bridge.nix` – feeds provider-shaped environments into plan-bridge + host-level nginx/backups and asserts on the resulting configs.
- `users-slices.nix` – evaluates only the users/slices host modules to ensure per-env slices and users are shaped correctly.
- `restic.nix` – exercises the restic env module’s assertions and generated units.

Shared helpers live in `tests/support/`:

- `stubs/` – minimal root option stubs so we can call `lib.evalModules` without pulling a full NixOS config.
- `provider-view.nix` – builds the “provider view” of environments (the same shape plan-bridge expects) from fixture envs.
- `sample-envs.nix` – tiny constructors for provider-style env attrsets used by multiple suites.
- `assert.nix` – small assertion helpers; tests prefer Nix-side assertions (`assertTrue`) to reduce shell/JQ boilerplate.

Guidelines for adding tests:
- Reuse fixtures from `tests/environments.nix` where possible.
- Use the helpers above instead of re-declaring option stubs or reshaping envs by hand.
- Evaluate only the modules you need for the branch under test (keep imports minimal).
- Assert on resulting config values, not implementation details.
