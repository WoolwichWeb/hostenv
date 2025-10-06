# Hostenv

*Wow, you found hostenv before it was cool.* hostenv isn't ready for general use yet, but we're working on it! In the meantime, feel free to look around and check out the Nix modules, use the code for learning, or report bugs and create merge requests. Contributions are welcome.

hostenv is a Platform as a Service (PaaS) that belongs to all of us.

Use a JSON-like language to simply setup your hosting by toggling basic options:

```nix
# hostenv.nix
{ pkgs, config ... }: {

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

## Getting started - project

*Audience: hostenv user setting up a project's hosting*

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
5. Nix only sees files added to git. So allow Nix to see your hostenv configuration by running `git add .hostenv`.
6. From your project directory run `cd .hostenv` then `direnv allow`.
7. Wait while hostenv builds bespoke tooling, just for your project.
8. Run `hostenv` for hostenv specific commands.
9. Once your project is deployed, commands like `drush status` or `mysql` will JustWork™ with the remote environment, so there's no need to faff with SSH yourself or setup tunnels.
