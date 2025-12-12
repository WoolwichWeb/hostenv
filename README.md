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

## Repository Map (for contributors)

- `modules/` – canonical hostenv modules. The trunk is `config.environments`; feature modules also read `config.hostenv.environments` (bridged automatically).  
  - `nixos/` – host-level, provider-neutral building blocks (nginx-hostenv, backups-hostenv, users-slices, etc.).  
  - `env/` – env-level runtime modules (user services).  
  - `providers/` – optional provider-specific modules (add your own if needed).  
- `modules/core/full-env.nix` – assembles a user-level hostenv environment.  
  - `hostenv.nix` – core hostenv options (paths, hashes, naming).  
  - `environments.nix` – canonical environment schema and defaults.
- `src/provider/` – provider-side planning/CLI (plan/state generation, deploy flake, dns-gate scaffolding).
- `template/.hostenv/` – project template consumed by `flake init`.
- `tests/` – flake checks; includes provider plan regressions and Drupal fixtures.
- `docs/` – design notes (dendritic structure, provider quickstart, etc.).

## Key Workflows

- **Add an environment**: edit your project’s `.hostenv/hostenv.nix`, add an entry under `environments.<name> { enable = true; type = ...; virtualHosts = { ... }; }`. Run `nix flake check` to ensure feature modules (nginx, php-fpm, backups) pick it up. Only one environment may be `type = "production"` (enforced).
- **Remove an environment**: delete or set `enable = false` in `.hostenv/hostenv.nix`; regenerate plan/state (`hostenv-provider plan`) and deploy.
- **Add a host (provider)**: in the provider repo, add a node file under `nodes/` and map it in `provider.nodeSystems`; regenerate plan/state (`hostenv-provider plan`) and deploy via deploy-rs.
- **Add a feature module**: create `modules/nixos/<name>.nix` (system-level, provider-neutral) or `modules/env/<name>.nix` (user-level), or place provider-specific modules under `modules/providers/<name>.nix`; consume `config.hostenv.environments` (already bridged from `config.environments`); import it in `core/full-env.nix` (or provider-level if system-only). Add a test in `tests/`.

## Getting Started (projects)

1) Install Nix.  
2) `nix flake init --template gitlab:woolwichweb/hostenv`.  
3) Install/configure direnv, run `direnv allow` inside `.hostenv/`.  
4) Configure environments in `.hostenv/hostenv.nix`.  
5) `nix flake check` (or `nix run .#hostenv-provider -- plan` in provider context).  
6) Deploy via provider flow once ready.

## Contributing

PRs, issues, and questions welcome. Focus areas that help most: tests for new feature modules, provider UX, and documentation clarity. Thank you!
