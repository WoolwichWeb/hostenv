{ ... }:
{
  config.flake = {
    modules = ./.;
    lib = {
      cliModule = ./cli-flake-module.nix;
    };
  };
}
