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

And here's the same idea for a tiny PHP app (no Drupal) using the built‑in `php-app` module:

```nix
{ pkgs, config, ... }: {
  services.php-app.enable = true;
  services.php-app.codebase.name = "hello-php";

  environments.main = {
    enable = true;
    type = "production";
    virtualHosts."hello.example.com" = { };
  };
}
```

---

## Repository Map

- `modules/` – the trunk and feature branches:
  - `core/` – canonical schemas (`hostenv.nix`, `environments.nix`) and the user environment assembly (`full-env.nix`).
  - `env/` – env-level runtime modules (user services: nginx, php-fpm, drupal, restic).
  - `nixos/` – host-level, provider‑neutral modules (top-level runtime dirs, nginx front-door, users/slices, backups, monitoring, nginx tuning).
  - `providers/` – reserved for provider-specific modules if you need them.
- `flake-modules/` – flake-parts wiring shared by this repo and downstream flakes (per-system outputs, templates).
- `provider/` – provider-facing tooling: plan/state generator, CLI, node flake wiring. It now consumes the dendritic modules instead of carrying host glue.
- `template/project/.hostenv/` – project template used by `nix flake init --template gitlab:woolwichweb/hostenv`.
- `template/provider/` – provider template for building a hostenv hosting flake.
- `tests/` – flake checks and fixtures (provider plan regressions, Drupal).
- `docs/` – design notes, dendritic structure, provider quickstart, review checklists.

## Key Workflows

- **Add an environment**: edit your project’s `.hostenv/hostenv.nix`, add an entry under `environments.<name> { enable = true; type = ...; virtualHosts = { ... }; }`. Run `nix flake check` to ensure feature modules (nginx, php-fpm, backups) pick it up. Only one environment may be `type = "production"` (enforced).
- **Remove an environment**: delete or set `enable = false` in `.hostenv/hostenv.nix`; regenerate plan/state (`hostenv-provider plan`) and deploy.
- **Add a host (provider)**:
  - Create `nodes/<name>/hardware-configuration.nix` (copy from the machine) and a minimal `nodes/<name>/configuration.nix`.
  - Map it in `provider.nodeSystems` and, if needed, in `provider.nodeFor` to steer env types to that node.
  - Regenerate plan/state/flake: `nix run .#hostenv-provider-plan` (writes to `generated/`).
  - Deploy using your tool (e.g. deploy-rs) against `generated/flake.nix`, targeting that node.
  - Update secrets: on the host `ssh-keygen -y -f /etc/ssh/ssh_host_ed25519_key | ssh-to-age`, add the key to `.sops.yaml`/`secrets/secrets.yaml`, then `sops updatekeys secrets/secrets.yaml` locally.
- **Add a feature module**: create `modules/nixos/<name>.nix` (system-level, provider-neutral) or `modules/env/<name>.nix` (user-level); put provider-specific modules under `modules/providers/<name>.nix` if needed. Feature modules read `config.hostenv.environments` (wired by the provider system). Import new host-level modules in the provider system wiring (`provider/nixos-system.nix`) if they’re host-only; import env-level modules in `modules/core/full-env.nix`. Add a test in `tests/`.
- **Run the hostenv CLI**: from a project’s `.hostenv/` directory run `nix run .#hostenv` to use the project-aware CLI (environments come from your `hostenv.nix`). From this repo you can run `nix run .#hostenv` to get a bundled CLI for demos/tests.
- **Dev shell**: `nix develop` (repo root) drops you into a shell with provider + CLI tooling; inside a project’s `.hostenv/` you can also use `nix develop` for project-scoped tools.
- **Docs preview**: `nix run .#serve-docs` serves the generated docs locally (uses the flake app defined in `flake-modules/root.nix`).

### Default environment selection

- If you do **not** set `defaultEnvironment`, hostenv will pick the first **enabled** environment whose `type = "production"`. Only one production env is allowed (asserted).
- If no production environment is enabled, it falls back to `"main"`.
- To avoid surprises, set `defaultEnvironment = "<env-name>";` explicitly in your project’s `.hostenv/hostenv.nix` when you want a different default (e.g. `dev`, `staging`).

## How environments surface on NixOS

- Every enabled environment becomes a Unix user and slice, named by `hostenv.userName` (derived from organisation/project/environment). The provider assigns a numeric UID stored as `environments.<name>.uid` in the plan/state JSON.
- Host‑level glue (`modules/nixos/*.nix`) creates runtime dirs under `/run/hostenv/<env>/`, tmpfiles rules, and system nginx vhosts that proxy to the per‑env user‑level nginx/fastcgi sockets.
- User‑level services run in that user’s systemd session (packaged by `config.hostenv.activatePackage`) and expose sockets in `/run/hostenv/nginx/<env>/` for the host nginx front‑door.
- Feature modules such as backups, monitoring, and php-fpm read the same `config.hostenv.environments` data and inject per‑env units or credentials where required.

## Getting Started (projects)

1. Install Nix.  
2. In your project root, apply the **project** template (creates a `.hostenv/` directory):  
   `nix flake init -t gitlab:woolwichweb/hostenv`  
3. Install/configure direnv, run `direnv allow` inside `.hostenv/`.  
4. Configure environments in `.hostenv/hostenv.nix`.  
5. Ensure your project flake exports `outputs.lib.hostenv` (the template now does). The provider relies on `lib.hostenv.<system>.environments` to discover environments; if it is missing, plan generation will fail fast with a clear error.

## Getting Started (provider)

- Start from the **provider** template (in a fresh repo or dir):  
  `nix flake init -t gitlab:woolwichweb/hostenv#provider`

- Create NixOS node configs under `nodes/<node>/configuration.nix` (plus hardware config); set `system.stateVersion`.
- Generate plan/state/flake: `nix run .#hostenv-provider-plan` (writes to `generated/`).
- Deploy using your preferred tool (e.g. deploy-rs) against the generated flake.
- Client project inputs should point at the `.hostenv` flake (e.g. `dir=.hostenv`), so `hostenv.nix` is at the flake root.

## Contributing

PRs, issues, and questions welcome. Focus areas that help most: tests for new feature modules, provider UX, and documentation clarity. Thank you!
