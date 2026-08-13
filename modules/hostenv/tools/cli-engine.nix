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
      commands = config.hostenv.cli.commands;

      # Pog receives one root runtime environment, while Hostenv lets each command
      # declare its own dependencies. This single tree walk gathers those packages
      # and records commands that also need a standalone executable.
      collectCommandMetadata =
        parentPath: commandSet:
        lib.foldlAttrs
          (
            result: name: command:
            let
              commandPath = parentPath ++ [ name ];
              children = collectCommandMetadata commandPath command.commands;
            in
            {
              runtimeInputs = result.runtimeInputs ++ command.runtimeInputs ++ children.runtimeInputs;
              wrappers =
                result.wrappers
                ++ lib.optional (command.executable != null) {
                  name = command.executable;
                  inherit commandPath;
                }
                ++ children.wrappers;
            }
          )
          {
            runtimeInputs = [ ];
            wrappers = [ ];
          }
          commandSet;

      commandMetadata = collectCommandMetadata [ ] commands;

      # Two commands cannot install programs with the same name into cliPackage.
      standaloneWrappers =
        let
          wrappers = commandMetadata.wrappers;
          names = map (wrapper: wrapper.name) wrappers;
        in
        if builtins.length names != builtins.length (lib.unique names) then
          throw "hostenv: command executable names must be unique"
        else
          wrappers;
      standaloneWrapperNames = map (wrapper: wrapper.name) standaloneWrappers;

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
          env_name="''${env:-$(
            git symbolic-ref -q --short HEAD 2>/dev/null || true
          )}"
          if ${var.empty "env_name"}; then env_name="${config.defaultEnvironment}"; fi

          env_or_null() { jq -c --arg e "$1" '.[$e] // null' <<< ${lib.escapeShellArg environmentsJson}; }
          env_cfg="$(env_or_null "$env_name")"
          if ${var.empty "env_cfg"} || [ "$env_cfg" = "null" ]; then
            die "Unknown environment: $env_name" 2
          fi

          user="$(jq -r '.hostenv.userName' <<< "$env_cfg")"
          host="$(jq -r '.hostenv.hostname' <<< "$env_cfg")"
          typ="$(jq -r '.type' <<< "$env_cfg")"
          emoji="$(
            case "$typ" in
              production)  echo "🚨" ;;
              testing)     echo "🧪" ;;
              development) echo "🛠️" ;;
              *)           echo "📦" ;;
            esac
          )"

          case "$tty_mode" in
            auto|"")
              if [ -t 0 ]; then SSH_TTY="-tt"; else SSH_TTY="-T"; fi
              ;;
            on)
              SSH_TTY="-tt"
              ;;
            off)
              SSH_TTY="-T"
              ;;
            *)
              die "invalid --tty-mode value: '$tty_mode' (use: auto|on|off)" 2
              ;;
          esac
          debug "tty_mode=$tty_mode ssh_flag=$SSH_TTY stdin_is_tty=$([ -t 0 ] && echo yes || echo no)"

          # Create a quiet direnv configuration once, without overwriting user settings.
          cfg="''${XDG_CONFIG_HOME:-$HOME/.config}/direnv/direnv.toml"
          if [ ! -f "$cfg" ]; then
            mkdir -p "$(dirname "$cfg")"
            cat >"$cfg" <<'EOF'
          [global]
          hide_env_diff = true
          EOF
            bold "Note: Created a direnv config file at: '$cfg'"
            green "You may change the settings there and hostenv will not overwrite them"
            echo
          fi

          banner() {
            echo
            cat <<BANNER | boxes -d whirly
          $emoji  Working in hostenv environment: "$env_name" ($typ)

          Commands: ${builtins.concatStringsSep ", " (standaloneWrapperNames ++ [ "hostenv" ])}
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

      # This is Pog's generated hostenv program, before standalone wrappers are
      # joined into the final package.
      hostenvCli = pog {
        name = "hostenv";
        description = "Interact with your hosting environments.";
        version = "0.2.0";
        commands = lib.mapAttrsToList (toPogCommand [ ]) commands;
        persistentFlags = [
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
        runtimeInputs =
          with pkgs;
          [
            jq
            openssh
            rsync
            boxes
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
          exec ${hostenvCli}/bin/hostenv ${lib.escapeShellArgs wrapper.commandPath} -- "$@"
        ''
      ) standaloneWrappers;

      # Consumers get one package containing hostenv, its generated completions,
      # and every optional standalone wrapper.
      cliPackage = pkgs.symlinkJoin {
        name = "hostenv-cli";
        paths = [ hostenvCli ] ++ standaloneWrapperPackages;
        passthru.pog = hostenvCli.pog;
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
        program = "${cliPackage}/bin/hostenv";
      };
    };
}
