{ inputs, lib, ... }:
let
  fp = inputs.flake-parts.lib;
in
{
  options.perSystem = fp.mkPerSystemOption ({ config, ... }: {
    options.documentation = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Whether documentation is enabled by default for Hostenv flake outputs and development tooling.";
      };

      checks.enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Whether flake checks include documentation outputs. Disabled by default to keep CI closures small.";
      };

      nix.enable = lib.mkOption {
        type = lib.types.bool;
        default = config.documentation.enable;
        defaultText = lib.literalExpression "config.documentation.enable";
        description = "Whether Hostenv Nix documentation outputs are enabled.";
      };

      haskell = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = config.documentation.enable;
          defaultText = lib.literalExpression "config.documentation.enable";
          description = "Whether Haskell documentation is enabled by default.";
        };

        dependencies.enable = lib.mkOption {
          type = lib.types.bool;
          default = config.documentation.haskell.enable;
          defaultText = lib.literalExpression "config.documentation.haskell.enable";
          description = "Whether GHC development environments include documentation outputs for GHC and Haskell dependencies.";
        };

        haddock.enable = lib.mkOption {
          type = lib.types.bool;
          default = config.documentation.haskell.enable;
          defaultText = lib.literalExpression "config.documentation.haskell.enable";
          description = "Whether Hostenv local Haskell packages build Haddock documentation.";
        };
      };
    };
  });
}
