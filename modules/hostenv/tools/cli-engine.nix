{ ... }:
{
  flake.modules.hostenv."tools-cli-engine" =
    { lib, config, pkgs, ... }:
    let
      inherit (pkgs) pog;

      envJsonEval = builtins.tryEval (builtins.toJSON config.hostenv.publicEnvironments);
      envJson =
        assert envJsonEval.success
          || builtins.throw ''
          hostenv: config.hostenv.publicEnvironments must be JSON-serializable.

          Non-JSON data should be stored elsewhere (e.g. config.hostenv.*).
        '';
        envJsonEval.value;

      defaultEnvName = config.defaultEnvironment or "";

      subCommandList = lib.attrsToList config.hostenv.subCommands;
      userFacingSubCommands = builtins.filter (cmd: !cmd.value.internal) subCommandList;

      # User-facing list of names (internal filtered out)
      subCommandNames = builtins.map (cmd: cmd.name) userFacingSubCommands;

      # Subcommands that request top-level wrapper scripts
      scripts = builtins.filter (cmd: cmd.value.makeScript) subCommandList;

      # Base commands are defaults; services can override if they really want to.
      baseSubCommands = {
    help = {
      exec = helpers: ''
        help
      '';
      description = "Print help for the hostenv command.";
    };

    banner = {
      exec = helpers: ''
        banner
      '';
      internal = true;
    };

    list = {
      exec = helpers: ''
        ${builtins.concatStringsSep "\n" (builtins.map (cmd: ''
          echo
          bold "${cmd.name}"
          ${lib.optionalString (cmd.value.description != "") ''
            cat <<'EOF'
          ${cmd.value.description}
          EOF
          ''}
        '') userFacingSubCommands)}
      '';
      description = "List available hostenv sub-commands. Then run one like 'hostenv SUBCOMMAND'.";
    };

    __complete-subcommands = {
      exec = helpers: ''
        cat <<'EOF'
        ${builtins.concatStringsSep "\n" subCommandNames}
        EOF
      '';
      internal = true;
      description = "List internal commands for Bash completion.";
    };
  };

  # Main CLI package (rename local var to avoid confusion with removed flake-parts hostenvCli option)
  cliPkg = pog.pog {
    name = "hostenv";
    description = ''Interact with your hosting environments.

  SUBCOMMAND may be one of: ${builtins.concatStringsSep " " subCommandNames}

  Use 'hostenv list' to see a list of sub-commands and their descriptions
    '';
    version = "0.1.0";

    flags = [
      {
        name = "env";
        short = "e";
        description = "Target environment (defaults to current branch or '${defaultEnvName}')";
        completion = ''"$(command -v hostenv)" environments | jq -r 'keys[]' '';
        argument = "ENV";
      }
      { name = "force"; short = "f"; description = "Skip confirmations"; bool = true; }
      {
        name = "tty_mode";
        description = "TTY mode for remote commands: auto|on|off";
        argument = "MODE";
        default = "auto";
        completion = ''printf "%s\n" auto on off'';
      }
    ];

    arguments = [{ name = "subcommand"; }];
    argumentCompletion = ''"$(command -v hostenv)" __complete-subcommands'';

    runtimeInputs =
      with pkgs;
      [ jq openssh rsync boxes git gum ]
      ++ builtins.concatLists (builtins.map (v: v.value.runtimeInputs) subCommandList);

    bashBible = true;
    strict = true;

    script = helpers: with helpers; ''
      set -o pipefail

      # Derives the env name in this order:
      # 1) --env, 2) current branch, 3) defaultEnvironment
      env_name="''${env:-$(
        git symbolic-ref -q --short HEAD 2>/dev/null || true
      )}"
      if ${var.empty "env_name"}; then env_name="${defaultEnvName}"; fi

      # jq helpers over embedded JSON
      env_or_null() { jq -c --arg e "$1" '.[$e] // null' <<< '${envJson}'; }
      env_cfg="$(env_or_null "$env_name")"
      if ${var.empty "env_cfg"} || [ "$env_cfg" == "null" ]; then
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

      # If the user hasn't already setup a direnv configuration file
      # make one for them with sensible defaults.
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
        cat <<RS | boxes -d whirly
      $emoji  Working in hostenv environment: "$env_name" ($typ)

      Commands: ${builtins.concatStringsSep ", " (builtins.map (cmd: cmd.name) scripts ++ [ "hostenv" ])}
      RS
        echo
      }

      sub="''${1:-help}"; shift || true

      case "$sub" in

        ${builtins.concatStringsSep "\n" (builtins.map (cmd: ''
          ${cmd.name})
            ${cmd.value.exec helpers}
            ;;
        '') subCommandList)}

        "")
          help
          ;;

        *)
          die "Unknown subcommand: $sub" 2
          ;;
      esac
    '';
      };
    in
    {
      # Provide base commands as defaults so services can extend freely.
      config.hostenv.subCommands =
        lib.mapAttrs (_: v: lib.mkDefault v) baseSubCommands;

      config.hostenv.cliPackage = lib.mkDefault cliPkg;

      # Provide the main CLI app
      config.hostenv.apps.hostenv = lib.mkDefault {
        type = "app";
        program = "${cliPkg}/bin/hostenv";
      };
    };
}
