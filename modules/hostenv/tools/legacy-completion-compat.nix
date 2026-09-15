{ ... }:
{
  flake.modules.hostenv.tools-legacy-completion-compat =
    { config, lib, ... }:
    let
      visibleRootCommands = builtins.attrNames (
        lib.filterAttrs (_: command: !command.hidden) config.hostenv.cli.commands
      );
    in
    {
      # Older Pog-generated shell completions call this hidden command to get
      # the current list of top-level Hostenv commands. Keep it available as a
      # compatibility shim while new shells use Pog's Carapace completion.
      config.hostenv.cli.commands.__complete-subcommands = lib.mkDefault {
        script = ''
          cat <<'EOF'
          ${builtins.concatStringsSep "\n" visibleRootCommands}
          EOF
        '';
        hidden = true;
        description = "List top-level commands for legacy Hostenv shell completion.";
      };
    };
}
