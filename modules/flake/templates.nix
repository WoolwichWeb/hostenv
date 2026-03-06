{ inputs, lib, ... }:

{
  config = lib.mkIf (!(inputs ? hostenv)) {
    flake.templates = {
      default = {
        path = ../../template/project;
        description = "Hostenv project template";
        welcomeText = ''
          ## Thank you for using Hostenv

          What's next:

           - Configure your project in `.hostenv/hostenv.nix`.
           - Check the project README.md for more instructions:
             https://gitlab.com/woolwichweb/hostenv/-/blob/main/README.md?ref_type=heads
        '';
      };
      provider = {
        path = ../../template/provider;
        description = "Hostenv provider template";
        welcomeText = ''
          ## Hostenv Provider Setup

          Welcome! You've initialized a Hostenv provider flake.

          Next steps:

          1. Edit flake.nix to set:
             - provider.hostenvHostname = "your-hostname.com";
             - provider.nodeFor mappings for your environments

          2. Copy nodes/sample to nodes/<your-node> and edit configuration.nix
             - Add your server's hardware configuration
             - Set system.stateVersion

          3. Run 'direnv allow' or 'nix develop' to enter the devshell
             - The devshell will auto-generate necessary files

          4. Generate deployment files:
             nix run .#hostenv-provider -- plan

          5. Deploy to your nodes using provider-deploy or your preferred deployment method

          For more information, see:
          https://gitlab.com/woolwichweb/hostenv/-/blob/main/README.md
        '';
      };
    };
  };
}
