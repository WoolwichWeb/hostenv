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

    hostenv = lib.mkOption {
      type = types.submodule (import ./hostenv.nix);
      description = "Hostenv configuration for the current environment.";
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
