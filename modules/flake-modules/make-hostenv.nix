{ inputs, lib, config, ... }:
let
  systems = config.systems;

  mkMakeHostenv = system:
    let
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [ inputs.pog.overlays.${system}.default ];
      };
    in
    modules: environmentName:
      pkgs.lib.evalModules {
        specialArgs = { inherit inputs pkgs; };
        modules = [
          config.flake.hostenvModules.full-env
          ({ config, ... }: {
            # Use provided environmentName, or fall back to the default for discovery/CLI.
            hostenv.environmentName =
              if environmentName == null
              then config.defaultEnvironment
              else environmentName;
          })
          {
            # Restart services when tzdata updates.
            systemd.globalEnvironment.TZDIR = "${pkgs.tzdata}/share/zoneinfo";
          }
        ] ++ modules;
      };
in
{
  options.flake.makeHostenv = lib.mkOption {
    type = lib.types.attrsOf (lib.types.functionTo (lib.types.functionTo lib.types.unspecified));
    readOnly = true;
    description = "Per-system function: modules -> environmentName -> evalModules result.";
  };

  config.flake.makeHostenv = lib.genAttrs systems mkMakeHostenv;
}
