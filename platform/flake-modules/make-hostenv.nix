{ inputs, lib, config, ... }:
let
  systems = config.systems;

  mkMakeHostenv = system:
    let
      pogOverlays =
        if inputs ? pog && inputs.pog ? overlays
        then inputs.pog.overlays
        else { };
      hasPogOverlay =
        pogOverlays ? ${system}
        && pogOverlays.${system} ? default;
      pogOverlay =
        if hasPogOverlay
        then pogOverlays.${system}.default
        else (_: _: { });
      supportedSystems = builtins.attrNames pogOverlays;
      supportedSystemsMsg =
        if supportedSystems == [ ] then
          "unknown (pog input missing)"
        else
          lib.concatStringsSep ", " supportedSystems;
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [ pogOverlay ];
      };
    in
    modules: environmentName:
      # Provide helpful error message if the user is on a system not supported
      # by Pog. 
      if (!hasPogOverlay) then
        builtins.throw ''
          The hostenv CLI requires the Pog library but is not available for ${system}.

          Supported systems: ${supportedSystemsMsg}

          Suggested fixes:
          - Use a supported system (e.g. a Linux host or VM).
          - If you're on macOS, run a Linux dev shell via a remote builder or `nix develop --system x86_64-linux`.
        ''
      else
        pkgs.lib.evalModules {
          specialArgs = { inherit inputs pkgs; };
          modules = [
            config.flake.lib.hostenvModules.fullEnv
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
