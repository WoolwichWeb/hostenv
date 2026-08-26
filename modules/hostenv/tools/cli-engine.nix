# Hostenv projects CLI engine.
# Built on top of the excellent Pog:
# https://github.com/jpetrucciani/pog
{ ... }:
{
  flake.modules.hostenv.tools-cli-engine =
    {
      lib,
      config,
      pkgs,
      ...
    }:
    let
      # Pog builds the command parser, help text, and shell completions. Hostenv's
      # work below is limited to adding environment context and package wrappers.
      pog = pkgs.pog.pog;
      cliProgramName = "hostenv";
      commands = config.hostenv.cli.commands;
      pogVariableName = builtins.replaceStrings [ "-" ] [ "_" ];
      shellSafeCommandToken = token: builtins.match "[A-Za-z0-9._+-]+" token != null;

      rootPersistentFlags = [
        {
          name = "env";
          short = "e";
          description = "Target environment (defaults to the current branch or '${config.defaultEnvironment}')";
          argument = "ENV";
          completion = environmentCandidates;
        }
        {
          name = "force";
          short = "f";
          description = "Skip confirmations";
          bool = true;
        }
        {
          name = "tty-mode";
          short = "";
          description = "TTY mode for remote commands";
          argument = "MODE";
          default = "auto";
          completion = ttyModeCandidates;
        }
      ];

      # Pog receives one root runtime environment, while Hostenv lets each command
      # declare its own dependencies. This single tree walk gathers those packages
      # and records commands that also need a standalone executable. It also
      # rejects command tokens and visible flags that Pog cannot safely render.
      collectCommandMetadata =
        parentPath: inheritedPersistentFlags: commandSet:
        lib.foldlAttrs
          (
            result: name: command:
            let
              rawCommandPath = parentPath ++ [ name ];
              unsafeTokens = builtins.filter (token: !shellSafeCommandToken token) ([ name ] ++ command.aliases);
              visibleFlags = inheritedPersistentFlags ++ command.persistentFlags ++ command.flags;
              flagsByPogVariable = lib.groupBy (flag: pogVariableName flag.name) visibleFlags;
              flagCollisions = lib.filterAttrs (_: flags: builtins.length flags > 1) flagsByPogVariable;
              checkedCommand =
                if unsafeTokens != [ ] then
                  throw ''
                    hostenv: `${cliProgramName} ${builtins.concatStringsSep " " rawCommandPath}` uses shell-unsafe command names or aliases: ${builtins.concatStringsSep ", " (map builtins.toJSON unsafeTokens)}

                    Command names and aliases may contain only letters, numbers, dots, underscores, plus signs, and hyphens.
                  ''
                else if flagCollisions != { } then
                  throw ''
                    hostenv: flags collide in `${cliProgramName} ${builtins.concatStringsSep " " rawCommandPath}` after Pog converts their names to Bash variables:
                    ${lib.concatStringsSep "\n" (lib.mapAttrsToList (
                      variable: flags:
                      "  - ${builtins.concatStringsSep " and " (map (flag: "`--${flag.name}`") flags)} both use `${variable}`"
                    ) flagCollisions)}

                    Pog replaces hyphens with underscores when it creates Bash variable names.
                    Rename one flag from each line above.
                  ''
                else
                  command;
              commandPath = builtins.seq checkedCommand rawCommandPath;
              children = collectCommandMetadata
                commandPath
                (inheritedPersistentFlags ++ checkedCommand.persistentFlags)
                checkedCommand.commands;
            in
            {
              runtimeInputs = result.runtimeInputs ++ checkedCommand.runtimeInputs ++ children.runtimeInputs;
              commandPaths = result.commandPaths ++ [ commandPath ] ++ children.commandPaths;
              wrappers =
                result.wrappers
                ++ lib.optional (checkedCommand.executable != null) {
                  name = checkedCommand.executable;
                  inherit commandPath;
                }
                ++ children.wrappers;
            }
          )
          {
            runtimeInputs = [ ];
            commandPaths = [ ];
            wrappers = [ ];
          }
          commandSet;

      # Pog turns hyphens into underscores when it creates Bash function names.
      # It joins command-path segments with two underscores as well. Reject any
      # command paths that would therefore make Pog generate the same function.
      commandMetadata =
        let
          collected = collectCommandMetadata [ ] rootPersistentFlags commands;
          pogFunctionName =
            path: builtins.concatStringsSep "__" (map pogVariableName path);
          pathsByPogFunction = lib.groupBy pogFunctionName collected.commandPaths;
          collisions = lib.filterAttrs (_: paths: builtins.length paths > 1) pathsByPogFunction;
          showPath = path: "`${cliProgramName} ${builtins.concatStringsSep " " path}`";
        in
        if collisions == { } then
          collected
        else
          throw ''
            hostenv: command paths collide after Pog converts them to Bash function names:
            ${lib.concatMapStringsSep "\n" (
              paths: "  - ${builtins.concatStringsSep " and " (map showPath paths)}"
            ) (builtins.attrValues collisions)}

            Pog replaces hyphens with underscores and uses two underscores between path segments.
            Rename one command from each line above.
          '';

      # Wrapper names must be unique and cannot replace either program installed
      # by Pog for the main CLI.
      standaloneWrappers =
        let
          wrappers = commandMetadata.wrappers;
          names = map (wrapper: wrapper.name) wrappers;
          reservedNames = [
            cliProgramName
            "_${cliProgramName}_complete"
          ];
          collisions = builtins.filter (name: builtins.elem name reservedNames) names;
        in
        if builtins.length names != builtins.length (lib.unique names) then
          throw "hostenv: command executable names must be unique"
        else if collisions != [ ] then
          throw "hostenv: command executable names are reserved by the CLI: ${builtins.concatStringsSep ", " collisions}"
        else
          wrappers;

      # Environment metadata is embedded in the generated script. Reject values
      # such as functions or derivations here, where the configuration error is
      # easier to understand, instead of failing later while building the script.
      environmentsJson =
        let
          result = builtins.tryEval (builtins.toJSON config.exportedEnvironments);
        in
        if result.success then
          result.value
        else
          throw ''
            hostenv: config.exportedEnvironments must be JSON-serializable.

            Non-JSON data should be stored elsewhere (e.g. config.hostenv.*).
          '';

      # These are static Carapace completion records. No Hostenv process or jq
      # query needs to run when the user asks their shell for completions.
      environmentCandidates =
        let
          styleByType = {
            production = "red";
            testing = "yellow";
            development = "green";
          };
        in
        lib.mapAttrsToList (name: environment: {
          value = name;
          description = "${environment.type} environment on ${environment.hostenv.hostname}";
          style = styleByType.${environment.type} or "blue";
          tag = "hostenv environments";
        }) config.exportedEnvironments;

      # TTY modes are fixed, but descriptions make the completion menu useful.
      ttyModeCandidates = [
        {
          value = "auto";
          description = "Allocate a TTY when standard input is a terminal";
          tag = "TTY modes";
        }
        {
          value = "on";
          description = "Always allocate a TTY";
          tag = "TTY modes";
        }
        {
          value = "off";
          description = "Never allocate a TTY";
          tag = "TTY modes";
        }
      ];

      # Most commands need to know which environment they target. This shell
      # preamble validates that environment, exposes its SSH details, selects the
      # TTY policy, and defines the banner used by the development shell.
      environmentPreamble =
        helpers: with helpers; ''
          set -o pipefail

          # Select the environment from --env, the current branch, or the project default.
          hostenv_env_name="''${env:-$(
            git symbolic-ref -q --short HEAD 2>/dev/null || true
          )}"
          if ${var.empty "hostenv_env_name"}; then hostenv_env_name="${config.defaultEnvironment}"; fi

          hostenv_environment_or_null() { jq -c --arg e "$1" '.[$e] // null' <<< ${lib.escapeShellArg environmentsJson}; }
          hostenv_environment="$(hostenv_environment_or_null "$hostenv_env_name")"
          if ${var.empty "hostenv_environment"} || [ "$hostenv_environment" = "null" ]; then
            die "Unknown environment: $hostenv_env_name" 2
          fi

          hostenv_user="$(jq -r '.hostenv.userName' <<< "$hostenv_environment")"
          hostenv_host="$(jq -r '.hostenv.hostname' <<< "$hostenv_environment")"
          hostenv_type="$(jq -r '.type' <<< "$hostenv_environment")"
          hostenv_emoji="$(
            case "$hostenv_type" in
              production)  echo "🚨" ;;
              testing)     echo "🧪" ;;
              development) echo "🛠️" ;;
              *)           echo "📦" ;;
            esac
          )"

          case "$tty_mode" in
            auto|"")
              if [ -t 0 ]; then hostenv_ssh_tty="-tt"; else hostenv_ssh_tty="-T"; fi
              ;;
            on)
              hostenv_ssh_tty="-tt"
              ;;
            off)
              hostenv_ssh_tty="-T"
              ;;
            *)
              die "invalid --tty-mode value: '$tty_mode' (use: auto|on|off)" 2
              ;;
          esac
          debug "tty_mode=$tty_mode ssh_flag=$hostenv_ssh_tty stdin_is_tty=$([ -t 0 ] && echo yes || echo no)"

          # Create a quiet direnv configuration once, without overwriting user settings.
          hostenv_direnv_config="''${XDG_CONFIG_HOME:-$HOME/.config}/direnv/direnv.toml"
          if [ ! -f "$hostenv_direnv_config" ]; then
            mkdir -p "$(dirname "$hostenv_direnv_config")"
            cat >"$hostenv_direnv_config" <<'EOF'
          [global]
          hide_env_diff = true
          EOF
            bold "Note: Created a direnv config file at: '$hostenv_direnv_config'"
            green "You may change the settings there and hostenv will not overwrite them"
            echo
          fi

          banner() {
            echo
            cat <<BANNER | boxes -d whirly
          $hostenv_emoji  Working in hostenv environment: "$hostenv_env_name" ($hostenv_type)

          Commands: ${
            builtins.concatStringsSep ", " (map (wrapper: wrapper.name) standaloneWrappers ++ [ "hostenv" ])
          }
          BANNER
            echo
          }
        '';

      # The module type uses null for optional values. Pog uses a missing field
      # for the same purpose, so omit nulls before handing records to Pog.
      removeNulls = lib.filterAttrs (_: value: value != null);
      normaliseArgument = argument: if builtins.isAttrs argument then removeNulls argument else argument;

      # Project configuration uses readable strings; Pog's parser expects these
      # constructor values.
      pogParsingModes = {
        interspersed = pog.parsing.interspersed;
        non-interspersed = pog.parsing.nonInterspersed;
        passthrough = pog.parsing.passthrough;
        disabled = pog.parsing.disabled;
      };

      # Convert the keyed, typed Hostenv command tree to Pog's list-based tree.
      # Hostenv-only fields are consumed above and never passed through to Pog.
      toPogCommand =
        parentPath: name: command:
        let
          commandPath = parentPath ++ [ name ];
          script = command.script;
          scriptIsFunction = builtins.isFunction script || (builtins.isAttrs script && script ? __functor);
          hasScript = scriptIsFunction || script != "";
          needsEnvironment = commandPath != [ "environments" ];
          scriptWithEnvironment =
            helpers:
            lib.optionalString needsEnvironment (environmentPreamble helpers)
            + (if scriptIsFunction then script helpers else script);
        in
        removeAttrs command [
          "argumentCompletion"
          "commands"
          "executable"
          "flags"
          "parsing"
          "persistentFlags"
          "runtimeInputs"
          "script"
        ]
        // {
          inherit name;
          flags = map removeNulls command.flags;
          persistentFlags = map removeNulls command.persistentFlags;
          arguments = map normaliseArgument command.arguments;
          commands = lib.mapAttrsToList (toPogCommand commandPath) command.commands;
        }
        // lib.optionalAttrs (command.parsing != null) {
          parsing = pogParsingModes.${command.parsing};
        }
        // lib.optionalAttrs (command.argumentCompletion != null) {
          inherit (command) argumentCompletion;
        }
        // lib.optionalAttrs hasScript {
          script = scriptWithEnvironment;
        };

      # Pog generates the parser, help, and completion files from this tree.
      hostenvCli = pog {
        name = cliProgramName;
        description = "Interact with your hosting environments.";
        version = "0.2.0";
        commands = lib.mapAttrsToList (toPogCommand [ ]) commands;
        persistentFlags = rootPersistentFlags;
        runtimeInputs =
          with pkgs;
          [
            jq
            openssh
            rsync
            boxes
            coreutils
            git
            gum
          ]
          ++ commandMetadata.runtimeInputs;
        bashBible = true;
        strict = true;
      };

      # Each requested executable is a thin argv-preserving shortcut into the
      # same native Pog command tree (for example, `mysql` -> `hostenv mysql`).
      standaloneWrapperPackages = map (
        wrapper:
        pkgs.writeShellScriptBin wrapper.name ''
          exec ${hostenvCli}/bin/${cliProgramName} ${lib.escapeShellArgs wrapper.commandPath} -- "$@"
        ''
      ) standaloneWrappers;

      # Consumers get one package containing hostenv, its generated completions,
      # and every optional standalone wrapper.
      cliPackage = pkgs.symlinkJoin {
        name = "hostenv-cli";
        paths = [ hostenvCli ] ++ standaloneWrapperPackages;
        passthru.pog = hostenvCli.pog;
        meta = hostenvCli.meta // {
          mainProgram = cliProgramName;
        };
      };
    in
    {
      config.hostenv.cli.commands.banner = lib.mkDefault {
        script = "banner";
        hidden = true;
        description = "Print the current Hostenv environment banner.";
      };

      config.hostenv.cliPackage = lib.mkDefault cliPackage;

      # Provide the main CLI app
      config.hostenv.apps.hostenv = lib.mkDefault {
        type = "app";
        program = "${cliPackage}/bin/${cliProgramName}";
      };
    };
}
