{ lib, inputs, ... }:
let
  types = lib.types;
  # Used by downstream projects and providers.
  # Allows downstream to import hostenv modules with either project or
  # provider enabled.
  mkFlakeModule = cfg: { ... }:
    {
      imports = [
        (inputs.import-tree ./.)
      ];
      _file = builtins.toString ./exports.nix;

    } // cfg;
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
      type = types.attrsOf types.deferredModule;
      default = { };
      description = "Exported flake-parts modules (merged across modules).";
    };
  };

  config.flake.flakeModules = lib.mkIf (inputs ? import-tree) {
    exports = ./exports.nix;
    project = mkFlakeModule { project.enable = true; provider.enable = false; };
    provider = mkFlakeModule { project.enable = false; provider.enable = true; };
  };

  config.flake.modules = lib.mkDefault { };
}
