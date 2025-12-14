# Hostenv

*Wow, you found hostenv before it was cool.*

hostenv isn't quite ready for general use yet, but we're getting close! In the meantime, feel free to explore the codebase, browse the Nix modules, or open issues and merge requests. Contributions and curiosity are both welcome.

---

## What is hostenv?

**hostenv** is a Platform as a Service (PaaS) that belongs to all of us.

It lets you define your hosting environment declaratively, using a JSON-like configuration language (Nix). Instead of writing deployment scripts, you describe what you want and hostenv builds the environment for you.

Here's an example hosting environment for the [Drupal](https://www.drupal.org) CMS:

```nix
# hostenv.nix
{ pkgs, config, ... }: {

  services.drupal.enable = true;
  services.drupal.phpVersion = "8.3";

  # Run cron every five minutes.
  services.drupal.cron.timerConfig.OnCalendar = "*:0/5";

  environments.main = {
    enable = true;
    type = "production";
    virtualHosts = {
      "example.com" = {
        globalRedirect = "www.example.com";
      };
      "www.example.com" = {
        locations."/old-url" = {
          redirect = "/new-url";
        };
      };
    };
  };

  environments.test = {
    enable = true;
    type = "testing";
  };
}
```

---

## Repository Map (dendritic layout)

- `modules/` – the trunk and feature branches:
  - `core/` – canonical schemas (`hostenv.nix`, `environments.nix`) and the user environment assembly (`full-env.nix`).
  - `env/` – env-level runtime modules (user services: nginx, php-fpm, drupal, restic).
  - `nixos/` – host-level, provider‑neutral modules (top-level runtime dirs, nginx front-door, users/slices, backups, monitoring, plan-bridge, nginx tuning).
  - `providers/` – reserved for provider-specific modules if you need them.
- `src/provider/` – provider-facing tooling: plan/state generator, CLI, node flake wiring. It now consumes the dendritic modules instead of carrying host glue.
- `template/.hostenv/` – project template used by `nix flake init --template`.
- `tests/` – flake checks and fixtures (provider plan regressions, Drupal).
- `docs/` – design notes, dendritic structure, provider quickstart, review checklists.

## Key Workflows

- **Add an environment**: edit your project’s `.hostenv/hostenv.nix`, add an entry under `environments.<name> { enable = true; type = ...; virtualHosts = { ... }; }`. Run `nix flake check` to ensure feature modules (nginx, php-fpm, backups) pick it up. Only one environment may be `type = "production"` (enforced).
- **Remove an environment**: delete or set `enable = false` in `.hostenv/hostenv.nix`; regenerate plan/state (`hostenv-provider plan`) and deploy.
- **Add a host (provider)**: in the provider repo, add a node file under `nodes/` and map it in `provider.nodeSystems`; regenerate plan/state (`hostenv-provider plan`) and deploy via deploy-rs.
- **Add a feature module**: create `modules/nixos/<name>.nix` (system-level, provider-neutral) or `modules/env/<name>.nix` (user-level); put provider-specific modules under `modules/providers/<name>.nix` if needed. Feature modules read `config.hostenv.environments` (bridged from `config.environments` by `modules/nixos/plan-bridge.nix`). Import new host-level modules in the provider system wiring (`src/provider/nixos-system.nix`) if they’re host-only; import env-level modules in `modules/core/full-env.nix`. Add a test in `tests/`.
- **Run the hostenv CLI**: from a project’s `.hostenv/` directory run `nix run .#hostenv` to use the project-aware CLI (environments come from your `hostenv.nix`). From this repo you can run `nix run .#hostenv` to get a bundled CLI for demos/tests.
- **Dev shell**: `nix develop` (repo root) drops you into a shell with provider + CLI tooling; inside a project’s `.hostenv/` you can also use `nix develop` for project-scoped tools.
- **Docs preview**: `nix run .#serve-docs` serves the generated docs locally (uses the flake app defined in `src/flake-modules/root.nix`).

## Getting Started (projects)

1) Install Nix.  
2) `nix flake init --template gitlab:woolwichweb/hostenv`.  
3) Install/configure direnv, run `direnv allow` inside `.hostenv/`.  
4) Configure environments in `.hostenv/hostenv.nix`.  
5) `nix flake check` (or `nix run .#hostenv-provider -- plan` in provider context).
   - Run project CLI: `cd .hostenv && nix run .#hostenv`  
   - Enter project dev shell: `cd .hostenv && nix develop`  
6) Deploy via provider flow once ready.

## Provider Quickstart (use hostenv as an input)

- Add hostenv to your provider flake inputs and reuse its pins:
  ```nix
  inputs.hostenv.url = "git+https://gitlab.com/woolwichweb/hostenv";
  inputs.nixpkgs.follows = "hostenv/nixpkgs";
  inputs.flake-parts.follows = "hostenv/flake-parts";
  outputs = inputs@{ flake-parts, hostenv, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" ];
      imports = [ hostenv.lib.hostenv.providerModule ];
      provider = {
        hostenvHostname = "hosting.example.com";
        deployPublicKey = "ssh-ed25519 AAAA...";
        nodeSystems = { default = "x86_64-linux"; };
        nodeFor = { default = "edge-01"; };
        nodesPath = ./nodes;
        secretsPath = ./secrets/secrets.yaml;
        statePath = ./generated/state.json;
        planPath = ./generated/plan.json;
        planSource = "eval";
      };
    };
  ```
- Create NixOS node configs under `nodes/<node>/configuration.nix` (plus hardware config); set `system.stateVersion`.
- Generate plan/state/flake: `nix run .#hostenv-provider-plan` (writes to `generated/` or `$HOSTENV_PROVIDER_OUT`).
- Deploy using your preferred tool (e.g. deploy-rs) against the generated flake.
- If client flakes live somewhere other than `.hostenv/`, set `provider.hostenvProjectDir` accordingly.

## Contributing

PRs, issues, and questions welcome. Focus areas that help most: tests for new feature modules, provider UX, and documentation clarity. Thank you!
