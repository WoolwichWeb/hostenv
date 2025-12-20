{ ... }:
{
  flake = {
    flakeModules.provider = ./../provider/flake-module.nix;
    templates.default = {
      path = ./../template/project;
      description = "Hostenv project template";
      welcomeText = ''
        ## Thank you for using Hostenv

        What's next:

         - Configure your project in `.hostenv/hostenv.nix`.
         - Check the project README.md for more instructions:
           https://gitlab.com/woolwichweb/hostenv/-/blob/main/README.md?ref_type=heads
      '';
    };
    templates.provider = {
      path = ./../template/provider;
      description = "Hostenv provider template";
      welcomeText = ''
        ## Hostenv provider flake

        - Set `provider.*` options in flake.nix (hostname, deploy key, nodes).
        - Add node configs under nodes/<name>/configuration.nix.
        - Generate plan/state: nix run .#hostenv-provider-plan
        - Deploy using generated/flake.nix (e.g. via deploy-rs).
      '';
    };
  };
}
