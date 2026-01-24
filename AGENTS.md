# Hostenv - what you need to know

## One-liner

hostenv is “the PaaS you control”: a Nix module library and standard flake layout that lets each project describe its hosting environments declaratively, and lets the hosting platform turn those descriptions into real services on a NixOS box. Think Home Manager for web apps.

## Mental model

There are four layers:

1. **Project config** (in the client repo)  
    This is where the project says “I am a Drupal site, with these modules and cron jobs, and I have these environments: main, user-testing, feature/foo, etc.”

2. **Project flake `template/project/.hostenv/flake.nix`** (also in the client repo)  
    This glues the project config to the hostenv module library and exposes build artefacts like “activate this environment”.

3. **Provider/system flake** (lives with the hosting provider)  
    This is the NixOS config for the actual server(s). It creates Unix users for each environment and uses deploy-rs to push and activate the per-environment packages produced by the project flake.

4. **Project/environment library flake** (lives in a public repo on gitlab/github)
    This is the hostenv module library. It takes project config and exposes build artefacts, an environment list, and environment details based on that config.

If you already speak NixOS module system, the key idea is: **each environment is an evaluated module tree** that produces a single `activatePackage`, and the machine flake just runs that package as the relevant Unix user.

## 1. Project config: `.hostenv/hostenv.nix`

In each client repo you have a module file, e.g.:

```nix
{ pkgs, config, ... }: {
  # Service-level configuration (Drupal here, could be others).
  services.drupal.enable = true;
  services.drupal.backups.enable = true;
  services.drupal.phpVersion = "8.3";
  # Per-project environment definitions.
  environments.main = {
    enable = true;
    type = "production";
    hosts."example.ca".globalRedirect = "www.example.ca";
  };
  environments."user-testing" = { enable = true; type = "testing"; };
  # etc.
}
```

A few points for an experienced Nix dev:

* `hostenv.nix` is a **normal NixOS-style module**:

  * Takes `{ pkgs, config, ... }`.
  * Declares options like `services.drupal.*` and `environments.*`.

* The **`services.*`** subtree is hostenv’s “application layer”: Drupal, PHP-FPM, backups, cron, etc.
* The **`environments` attrset** is the place you define environments for the project:

  * Keys are usually git branches or tags (e.g. `main`, `user-testing`, `feature/foo`).
  * Values are small pieces of data: type (production, testing, etc.), hostnames, switches like `enable`.

There is no machine-specific stuff here. It is purely “what this project looks like and which environments exist”.

### 2. Project flake: `.hostenv/flake.nix`

This file is boilerplate that gets templated into each project, and it does two jobs:

1. **Evaluate the project’s environments as metadata.**
2. **Provide a package per environment that can be deployed and activated.**

Under the dendritic and flake-parts based layout this repo follows,

## Constraints and security

1. Environment and project configuration should be separated from system configuration where possible.
2. Data crossing the boundary from project/environment config to system config should be validated and/or kept to a restrictive type (think: nothing that can break system configuration).
3. Environments are not allowed to bind to numeric ports, only sockets inside the hostenv runtime directory. The workaround for this is if the service is inside a systemd-nspawn container. There a service may bind to a port, that port bound to a Unix socket in the hostenv runtime directory, which is itself bind mounted to the host OS so other services can see the socket and communicate with the service through it.
4. Inter machine communication between services is facilitated by tunneling sockets through SSH connections.
