{ lib, ... }:
let
  types = lib.types;
in
{
  options.flake = {
    lib = lib.mkOption {
      type = types.submodule {
        freeformType = types.attrsOf types.unspecified;
        options = { };
      };
      default = { };
      description = "Library outputs merged under flake.lib.";
    };

    flakeModules = lib.mkOption {
      type = types.attrsOf types.path;
      default = { };
      description = "Exported flake-parts modules (merged across modules).";
    };
  };

  config.flake.flakeModules = {
    exports = ./exports.nix;
    project = ../flake-modules/project.nix;
    provider = ../flake-modules/provider.nix;
  };

  config.flake.modules = lib.mkDefault { };
}
