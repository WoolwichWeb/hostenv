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
        example = lib.literalExpression "/home/\${config.hostenv.userName}/.local/share";
      };
      backupsSecret = lib.mkOption {
        type = types.str;
        description = "Path (on server) where secret access key for accessing backups may be found.";
      };
    };

    activate = lib.mkOption {
      type = types.lines;
      default = "";
      description = "Bash code to activate a module.";
    };

    #### Internal

    activatePackage = lib.mkOption {
      type = types.package;
      internal = true;
    };
  };

  imports = [
    ./systemd.nix
    ./mysql.nix
    ./nginx.nix
    ./phpfpm.nix
    ./drupal.nix
    ./restic.nix
  ];

  config = {
    activate = lib.mkBefore ''
      ## Top level activation script.
    '';

    activatePackage = pkgs.writeShellScriptBin "activate" config.activate;
  };
}
