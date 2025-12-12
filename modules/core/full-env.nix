# Builds a hostenv user environment.
{ config, pkgs, lib, ... }:
let

  types = lib.types;

  failedAssertions = builtins.map (x: x.message) (builtins.filter (x: !x.assertion) config.assertions);

  inherit (lib) splitString concatStringsSep mkOption mkPackageOption mkMerge
    mkBefore mkAfter;
  inherit (lib.trivial) showWarnings;

  # You may be wondering: how is this function used as a wrapper? It doesn't
  # even take any parameters.
  # See below, this really is a function with a single parameter.
  performAssertions =
    let
      formatAssertionMessage = message:
        let
          lines = splitString "\n" message;
        in
        "- ${concatStringsSep "\n  " lines}";
    in
    if failedAssertions != [ ]
    then
      throw ''
        Failed assertions:
        ${concatStringsSep "\n" (builtins.map formatAssertionMessage failedAssertions)}
      ''
    # `showWarnings` is key to how this function works.
    # It takes a list of warnings and some polymorphic value, prints the
    # warnings and returns the polymorphic value. `performAssertions`
    # therefore returns whatever value it is passed, allowing it to wrap
    # any variable.
    # Side-note: throw above also accepts a parameter, but doesn't return.
    else showWarnings config.warnings;
in
{

  options = {

    programs.ssh.package = mkPackageOption pkgs "openssh" { };

    assertions = mkOption {
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

    warnings = mkOption {
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

    hostenv = mkOption {
      type = types.submodule ../core/hostenv.nix;
      description = "Hostenv configuration for the current environment.";
    };

    activate = mkOption {
      type = types.lines;
      default = "";
      description = "Bash code to activate a module.";
    };

    buildReference = mkOption {
      type = types.nullOr types.str;
      description = ''
        Optional. Some way of uniquely identifying the current project build,
        e.g. git commit ref, build number, version tag, etc.
      '';
      default = null;
    };

    #### Internal

    profile = mkOption {
      type = types.listOf types.path;
      default = [ ];
      description = "List of paths to build into a profile, visible to the user under 'result/' after `nix build`.";
      internal = true;
    };

    activatePackage = mkOption {
      type = types.package;
      description = "Profile package containing configuration files and software, plus an activation script.";
      internal = true;
    };
  };

  imports = [
    ../core/environments.nix
    ../core/cli.nix
    ../env/systemd.nix
    ../env/mysql.nix
    ../env/nginx.nix
    ../env/nginx-fastcgi-hostenv.nix
    ../nixos/cloudflare-hostenv.nix
    ../env/phpfpm.nix
    ../env/drupal.nix
    ../env/php-app.nix
    ../env/restic.nix
  ];

  config =
    let
      activateScript = pkgs.writeShellScriptBin "activate" config.activate;
    in
    {
      activate = performAssertions (mkMerge [
        (mkBefore ''
          ## Top level activation script.
        '')
        # Remove hostenv profile from user account. This is safe since either:
        # 1) the user is running 'hostenv deploy' and that will re-add the
        # profile anyway, or
        # 2) the profile is being deployed by serokell/deploy-rs and the
        # .hostenv profile needs to be removed to make way for
        # users.users.<name>.packages (.hostenv profile has higher
        # priority than entries in users.users.<name>.packages).
        (mkAfter ''
          nix profile remove .hostenv >/dev/null 2>&1
        '')
      ]);

      activatePackage = pkgs.buildEnv {
        name = "hostenv-profile";
        paths = config.profile ++ [ activateScript ];
      };
    };
}
