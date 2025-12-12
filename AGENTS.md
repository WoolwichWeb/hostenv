# Hostenv - what you need to know

## One-liner

hostenv is “home-manager for web apps”: a Nix module library and standard flake layout that lets each project describe its hosting environments declaratively, and lets the hosting platform turn those descriptions into real services on a NixOS box.

## Mental model

There are three layers:

1. **Project config** (in the client repo)  
    This is where the project says “I am a Drupal site, with these modules and cron jobs, and I have these environments: main, user-testing, feature/foo, etc.”

2. **Project flake `.hostenv/flake.nix`** (also in the client repo)  
    This glues the project config to the hostenv module library and exposes build artefacts like “activate this environment”.

3. **Provider/system flake** (lives with the hosting provider)  
    This is the NixOS config for the actual server(s). It creates Unix users for each environment and uses deploy-rs to push and activate the per-environment packages produced by the project flake.

If you already speak NixOS module system, the key idea is: **each environment is an evaluated module tree** that produces a single `activatePackage`, and the machine flake just runs that package as the relevant Unix user.

## 1. Project config: `hostenv.nix`

In each client repo you have a module file, e.g.:

nix

Copy code

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
    hosts."georgina.ca".globalRedirect = "www.georgina.ca";
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
* The **`environments` attrset** is the **only place** you define environments for the project:

  * Keys are usually git branches or tags (e.g. `main`, `user-testing`, `feature/foo`).
  * Values are small pieces of data: type (production, testing, etc.), hostnames, switches like `enable`.

There is no machine-specific stuff here. It is purely “what this project looks like and which environments exist”.

### 2. Project flake: `.hostenv/flake.nix`

This file is boilerplate that gets templated into each project, and it does two jobs:

1. **Evaluate the project’s environments as metadata.**
2. **Provide a package per environment that can be deployed and activated.**

Conceptually:

```nix
{
  inputs = { nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable"; flake-utils.url = "github:numtide/flake-utils"; phps.url = "github:fossar/nix-phps"; hostenv.url = "gitlab:woolwichweb/hostenv?dir=modules"; };
  outputs = { self, nixpkgs, flake-utils, ... }@inputs: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      organisation = "giantgoat";
      project = "georgina";
      projectEnv = pkgs.callPackage ../hostenv.nix { };
      # First eval: work out “what environments exist” and the default.
      environments = pkgs.lib.evalModules {
        specialArgs = inputs // { inherit inputs pkgs system; };
        modules = [
          (inputs.hostenv.modules + /environments.nix)
          { environments = projectEnv.environments; }
          # plus a submodule that exposes `hostenv.{organisation,project,...}`
        ];
      };
      # Second eval: given an environment name, fully realise its config.
      makeHostenv = environmentName: pkgs.lib.evalModules {
        specialArgs = inputs // { inherit inputs pkgs system; };
        modules = [
          (inputs.hostenv.modules + /top-level.nix)
          { hostenv.organisation = organisation; hostenv.project = project; hostenv.environmentName = environmentName; hostenv.root = ../.; buildReference = self.rev or null; }
          # Nixpkgs systemd plumbing, tzdata, etc.
          ../hostenv.nix
        ];
      };
    in
    {
      # Metadata for tooling.
      envs = environments;
      # One package per enabled environment.
      packages = let enabled = pkgs.lib.filterAttrs (_: v: v.enable) environments.config.environments; in pkgs.lib.mapAttrs (envName: _envConfig: let env = makeHostenv envName; in env.config.activatePackage) enabled // { default = let defaultEnv = makeHostenv environments.config.defaultEnvironment; in defaultEnv.config.activatePackage; };
    });
}
```
