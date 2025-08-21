{
  description = "Hostenv - Your PaaS";
  outputs =
    { ... }:
    {
      templates = {
        default = {
          path = ./template;
          welcomeText = ''
            ## Thank you for using Hostenv

            What's next:

             - If you haven't already, install Nix:
               https://nixos.org/download/#download-nix
             - Configure your project in `.hostenv/hostenv.nix`.
             - Check the project README.md for more instructions:
               https://gitlab.com/woolwichweb/hostenv/-/blob/main/README.md?ref_type=heads
          '';
        };
      };
    };
}
