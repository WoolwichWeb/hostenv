# Hostenv

Wow, you found hostenv before it was cool.

Hostenv isn't ready for general use, but feel free to explore the modules.

## Getting started - hostenv project

1. Install [Nix](https://nixos.org/download/#download-nix)
2. `cd` into your project directory, and run:

   ```bash
   nix --extra-experimental-features "nix-command flakes" flake init --template gitlab:woolwichweb/hostenv
   ```

3. [Install direnv](https://direnv.net/docs/installation.html) and setup the hook for your shell (or skip this step if you already have `direnv`).

   For example, to make direnv work with **Bash** run:

   ```bash
   echo 'eval "$(direnv hook bash)"' >> ~/.bashrc
   ```

   Consult the [direnv hook docs](https://direnv.net/docs/hook.html) for other shells.
4. Open up `.hostenv/hostenv.nix` and configure hostenv for your project.
5. From your project directory run `cd .hostenv` then `direnv allow`.
6. Wait while hostenv builds bespoke tooling, just for your project.
7. Run `hostenv` for hostenv specific commands.
8. Once your project is deployed, commands like `drush status` or `mysql` will JustWork™ with the remote environment, so there's no need to faff with SSH yourself or setup tunnels.
