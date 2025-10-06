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

## Getting Started (for projects)

**Audience:** developers setting up hosting for a project.

1. **Install Nix**  
   Follow the [Nix installation guide](https://nixos.org/download/#download-nix).

2. **Initialise your hostenv project**  
   Inside your project directory, run:

   ```bash
   nix --extra-experimental-features "nix-command flakes" flake init --template gitlab:woolwichweb/hostenv
   ```

3. **Install and configure direnv**  
   [Install direnv](https://direnv.net/docs/installation.html) and set up the shell hook.  
   For **Bash**, run:

   ```bash
   echo 'eval "$(direnv hook bash)"' >> ~/.bashrc
   ```

   (For other shells, see the [direnv hook documentation](https://direnv.net/docs/hook.html).)

4. **Configure hostenv**  
   Edit `.hostenv/hostenv.nix` and set options for your project.

5. **Make hostenv visible to Nix**  
   Since Nix only includes tracked files, run:

   ```bash
   git add .hostenv
   ```

6. **Allow direnv to build your environment**

   ```bash
   cd .hostenv
   direnv allow
   ```

7. **Wait for hostenv to build bespoke tooling**  
   It will automatically prepare a project-specific environment and CLI.

8. **Run hostenv commands**  
   From your project directory, run:

   ```bash
   hostenv
   ```

   to see available commands.

9. **Enjoy automatic integration**  
   Once deployed, tools such as `drush` and `mysql` will *JustWork™* with the remote environment; no need to manage SSH or tunnels manually, just `cd .hostenv` then `drush`.

---

## Contributing

We don't have formal contribution guidelines yet, but we welcome all kinds of help. Whether it's a merge request, bug report, documentation improvement, or a question about how something works.

If you see something that could be better, feel free to open an issue or submit an MR (merge request). We'll work things out together as the project grows.

---

*hostenv: declarative hosting belonging to all of us.*
