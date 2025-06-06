# Liberally cribs from:
# https://github.com/cachix/devenv/blob/e17a6a604e478bab6ba88fb049c5bdbe494fb0cf/src/modules/top-level.nix
{ config, pkgs, lib, system, ... }:
let

  types = lib.types;

  failedAssertions = builtins.map (x: x.message) (builtins.filter (x: !x.assertion) config.assertions);

  performAssertions =
    let
      formatAssertionMessage = message:
        let
          lines = lib.splitString "\n" message;
        in
        "- ${lib.concatStringsSep "\n  " lines}";
    in
    if failedAssertions != [ ]
    then
      throw ''
        Failed assertions:
        ${lib.concatStringsSep "\n" (builtins.map formatAssertionMessage failedAssertions)}
      ''
    else lib.trivial.showWarnings config.warnings;
in
{

  options = {

    programs.ssh.package = lib.mkPackageOption pkgs "openssh" { };

    assertions = lib.mkOption {
      type = types.listOf types.unspecified;
      internal = true;
      default = [ ];
      example = [{ assertion = false; message = "you can't enable this for that reason"; }];
      description = ''
        This option allows modules to express conditions that must
        hold for the evaluation of the configuration to succeed,
        along with associated error messages for the user.
      '';
    };

    warnings = lib.mkOption {
      type = types.listOf types.str;
      internal = true;
      default = [ ];
      example = [ "you should fix this or that" ];
      description = ''
        This option allows modules to express warnings about the
        configuration. For example, `lib.mkRenamedOptionModule` uses this to
        display a warning message when a renamed option is used.
      '';
    };

    hostenv = {
      organisation = lib.mkOption {
        type = types.str;
        description = "Business account name or organisation name of the project. Should be short, lowercase, and with no special characters.";
        example = "fooinc";
      };
      project = lib.mkOption {
        type = types.str;
        description = "Name of the project. Should be short, lowercase, and contain no special characters.";
        example = "coolproject";
      };
      environmentName = lib.mkOption {
        type = types.str;
        description = "Name of the current environment. Usually corresponds to a git branch, but can be something else, e.g. an MR slug or number. Should be short, lowercase, and with no special characters.";
      };
      safeEnvironmentName = lib.mkOption {
        type = types.str;
        description = "Name of the current environment, shortened and with special characters removed.";
      };
      userName = lib.mkOption {
        type = types.str;
        description = "UNIX username (on server) of this project.";
      };
      root = lib.mkOption {
        type = types.oneOf [ types.str types.path ];
        description = "The application's root directory.";
      };
      runtimeDir = lib.mkOption {
        type = types.str;
        description = "Path (on server) where hostenv sockets and pid files may be found.";
        example = lib.literalExpression "/run/hostenv/user/\${config.hostenv.userName}";
      };
      upstreamRuntimeDir = lib.mkOption {
        type = types.str;
        description = "Path (on server) where upstream reverse proxy socket may be found.";
        example = lib.literalExpression "/run/hostenv/nginx/\${config.hostenv.userName}";
      };
      dataDir = lib.mkOption {
        type = types.str;
        description = "Path (on server) where data may be found at runtime.";
        example = lib.literalExpression "/home/\${config.hostenv.userName}/.local/share";
      };
      stateDir = lib.mkOption {
        type = types.str;
        description = "Path (on server) where state data may be found at runtime.";
        example = lib.literalExpression "/home/\${config.hostenv.userName}/.local/state";
      };
      cacheDir = lib.mkOption {
        type = types.str;
        description = "Path (on server) where cache data may be found at runtime.";
        example = lib.literalExpression "/home/\${config.hostenv.userName}/.cache";
      };
      backupsSecret = lib.mkOption {
        type = types.str;
        description = "Path (on server) where secret access key for accessing backups may be found.";
      };
      projectNameHash = lib.mkOption {
        type = types.str;
        description = "Hash of organisation, project, and environment names.";
        internal = true;
      };
    };

    activate = lib.mkOption {
      type = types.lines;
      default = "";
      description = "Bash code to activate a module.";
    };

    #### Internal

    profile = lib.mkOption {
      type = types.listOf types.path;
      default = [ ];
      description = "List of paths to build into a profile, visible to the user under 'result/' after `nix build`.";
      internal = true;
    };

    activatePackage = lib.mkOption {
      type = types.package;
      description = "Profile package containing configuration files and software, plus an activation script.";
      internal = true;
    };
  };

  imports = [
    ./hostenv.nix
    ./environments.nix
    ./systemd.nix
    ./mysql.nix
    ./nginx.nix
    ./phpfpm.nix
    ./drupal.nix
    ./php-app.nix
    ./restic.nix
  ];

  config =
    let
      activateScript = pkgs.writeShellScriptBin "activate" config.activate;
    in
    {
      activate = lib.mkBefore ''
        ## Top level activation script.
      '';

      activatePackage = pkgs.buildEnv {
        name = "hostenv-profile";
        paths = config.profile ++ [ activateScript ];
      };
    };
}
