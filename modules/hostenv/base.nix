{ ... }:
{
  flake.modules.hostenv.base =
    { config, pkgs, lib, ... }:
    let
      types = lib.types;
      failedAssertions = map (x: x.message) (builtins.filter (x: !x.assertion) config.assertions);

      inherit (lib) splitString concatStringsSep mkOption mkPackageOption mkMerge
        mkAfter;
      inherit (lib.trivial) showWarnings;

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
            ${concatStringsSep "\n" (map formatAssertionMessage failedAssertions)}
          ''
        else showWarnings config.warnings;
    in
    {
      options = {
        programs.ssh.package = mkPackageOption pkgs "opensshWithKerberos" { };

        packages = mkOption {
          type = types.listOf types.package;
          default = [ ];
          example = lib.literalExpression "[ pkgs.pdftk ]";
          description = ''
            Packages required by the hosted application at runtime. These are
            added to the environment profile and made available on the PATH of
            application services such as PHP-FPM, framework schedulers, and
            queue workers. They are also available while a deployment is being
            activated, before the new environment profile is installed.
          '';
        };

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

      config =
        let
          runtimePackagePath = lib.makeBinPath config.packages;
          activateScript = pkgs.writeShellScriptBin "activate" config.activate;
        in
        {
          activate = performAssertions (mkMerge [
            (lib.mkOrder 0 ''
              ## Top level activation script.
              ${lib.optionalString (config.packages != [ ]) ''
                # Activation runs before the new profile is installed. Make this
                # deployment's runtime packages available immediately instead
                # of relying on the previous profile's PATH.
                export PATH=${lib.escapeShellArg runtimePackagePath}:"$PATH"
              ''}
            '')
            (mkAfter ''
              nix profile remove .hostenv >/dev/null 2>&1 || true
            '')
          ]);

          profile = config.packages;

          activatePackage = pkgs.buildEnv {
            name = "hostenv-profile";
            paths = config.profile ++ [ activateScript ];
          };
        };
    };
}
