{ ... }:
{
  config.flake = {
    modules = ../.;
    # Primary exports for consumers: functions under flake.*
    cliModule = ../cli-flake-module.nix;

    # Back-compat: still expose under flake.lib for callers expecting the old path.
    lib = {
      cliModule = ../cli-flake-module.nix;
    };
  };
}
