# Liberally cribs from:
# https://github.com/NixOS/nixpkgs/blob/nixos-24.11/nixos/modules/services/web-servers/phpfpm/default.nix#L251
{ config, lib, pkgs, inputs, ... }:

let
  cfg = config.services.phpfpm;

  phps = inputs.phps or { };

  filterDefaultExtensions = ext: builtins.length (builtins.filter (inner: inner == ext.extensionName) cfg.disableExtensions) == 0;

  configurePackage = package:
    package.buildEnv {
      extensions = { all, enabled }: with all; (builtins.filter filterDefaultExtensions (enabled ++ lib.attrsets.attrValues (lib.attrsets.getAttrs cfg.extensions package.extensions)));
    };

  toStr = value:
    if true == value then "yes"
    else if false == value then "no"
    else toString value;

  fpmCfgFile = pool: poolOpts: pkgs.writeText "phpfpm-${pool}.conf" ''
    [global]
    ${lib.concatStringsSep "\n" (lib.mapAttrsToList (n: v: "${n} = ${toStr v}") cfg.settings)}

    [${pool}]
    ${lib.concatStringsSep "\n" (lib.mapAttrsToList (n: v: "${n} = ${toStr v}") poolOpts.settings)}
    ${lib.concatStringsSep "\n" (lib.mapAttrsToList (n: v: "env[${n}] = ${toStr v}") poolOpts.phpEnv)}
  '';

  phpIni = poolOpts: pkgs.runCommand "php.ini"
    {
      inherit (poolOpts) phpPackage phpOptions;
      preferLocalBuild = true;
      passAsFile = [ "phpOptions" ];
    } ''
    cat ${poolOpts.phpPackage}/etc/php.ini $phpOptionsPath > $out
  '';

  version = cfg_: builtins.replaceStrings [ "." ] [ "" ] cfg_.phpVersion;
  phpVersionError = cfg_:
    if phps == { }
    then
      ''


        🔎 If your version of PHP is supported by this project:
        https://github.com/fossar/nix-phps
        Add ‘phps.url = "github:fossar/nix-phps";’ to your Nix Flake's inputs and try again.
      ''
    else
      "";
  phpsPackage = cfg_: phps.packages.${pkgs.stdenv.system}."php${version cfg_}" or (throw ("PHP version ${cfg_.phpVersion} is not available" + phpVersionError cfg_));
  nixpkgsPackageExists = cfg_: (builtins.tryEval (toString pkgs."php${version cfg_}")).success;
  customPhpPackage = cfg_: if ((builtins.hasAttr "php${version cfg_}" pkgs) && nixpkgsPackageExists cfg_) then pkgs."php${version cfg_}" else phpsPackage cfg_;

  poolOpts = { name, ... }:
    let
      poolOpts = cfg.pools.${name};
    in
    {
      options = {
        socket = lib.mkOption {
          type = lib.types.str;
          readOnly = true;
          description = ''
            Path to the unix socket file on which to accept FastCGI requests.

            ::: {.note}
            This option is read-only and managed by NixOS.
            :::
          '';
          example = "${config.hostenv.runtimeDir}/<name>.sock";
        };

        phpVersion = lib.mkOption {
          type = lib.types.str;
          default = cfg.phpVersion;
          defaultText = "services.phpfpm.phpVersion";
          description = "The PHP version to use.";
        };

        phpPackage = lib.mkOption {
          type = lib.types.package;
          default = cfg.phpPackage;
          defaultText = lib.literalExpression "services.phpfpm.phpPackage";
          description = ''
            Allows you to [override the default used package](https://nixos.org/manual/nixpkgs/stable/#ssec-php-user-guide)
            to adjust the settings or add more extensions.
          '';
          example = lib.literalExpression ''
            pkgs.php.buildEnv {
              extensions = { all, enabled }: with all; enabled ++ [ apcu ];
              extraConfig = '''
                memory_limit=1G
              ''';
            };
          '';
        };

        phpOptions = lib.mkOption {
          type = lib.types.lines;
          description = ''
            "Options appended to the PHP configuration file {file}`php.ini` used for this PHP-FPM pool."
          '';
        };

        phpEnv = lib.mkOption {
          type = with lib.types; attrsOf str;
          default = { };
          description = ''
            Environment variables used for this PHP-FPM pool.
          '';
          example = lib.literalExpression ''
            {
              HOSTNAME = "$HOSTNAME";
              TMP = "/tmp";
              TMPDIR = "/tmp";
              TEMP = "/tmp";
            }
          '';
        };

        settings = lib.mkOption {
          type = with lib.types; attrsOf (oneOf [ str int bool ]);
          default = { };
          description = ''
            PHP-FPM pool directives. Refer to the "List of pool directives" section of
            <https://www.php.net/manual/en/install.fpm.configuration.php>
            for details. Note that settings names must be enclosed in quotes (e.g.
            `"pm.max_children"` instead of `pm.max_children`).
          '';
          example = lib.literalExpression ''
            {
              "pm" = "dynamic";
              "pm.max_children" = 75;
              "pm.start_servers" = 10;
              "pm.min_spare_servers" = 5;
              "pm.max_spare_servers" = 20;
              "pm.max_requests" = 500;
            }
          '';
        };
      };

      config = {
        phpPackage = lib.mkIf (cfg.phpVersion != "") (lib.mkForce (configurePackage (customPhpPackage cfg)));
        socket = "${config.hostenv.runtimeDir}/${name}.sock";
        phpOptions = lib.mkBefore cfg.phpOptions;

        settings = lib.mapAttrs (name: lib.mkDefault) {
          listen = poolOpts.socket;
        };
      };
    };
in
{
  options = {
    services.phpfpm = {

      phpVersion = lib.mkOption {
        type = lib.types.str;
        default = "";
        description = "The PHP version to use.";
      };

      settings = lib.mkOption {
        type = with lib.types; attrsOf (oneOf [ str int bool ]);
        default = { };
        description = ''
          PHP-FPM global directives. Refer to the "List of global php-fpm.conf directives" section of
          <https://www.php.net/manual/en/install.fpm.configuration.php>
          for details. Note that settings names must be enclosed in quotes (e.g.
          `"pm.max_children"` instead of `pm.max_children`).
          You need not specify the options `error_log` or
          `daemonize` here, since they are generated by hostenv.
        '';
      };

      extensions = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = ''
          PHP extensions to enable.
        '';
      };

      disableExtensions = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = ''
          PHP extensions to disable.
        '';
      };

      phpPackage = lib.mkOption {
        type = lib.types.package;
        default = configurePackage pkgs.php;
        defaultText = lib.literalExpression "pkgs.php";
      };

      phpOptions = lib.mkOption {
        type = lib.types.lines;
        default = "";
        example =
          ''
            date.timezone = "CET"
          '';
        description = ''
          Options appended to the PHP configuration file {file}`php.ini`.
        '';
      };

      pools = lib.mkOption {
        type = lib.types.attrsOf (lib.types.submodule poolOpts);
        default = { };
        example = lib.literalExpression ''
          {
            mypool = {
              phpPackage = pkgs.php;
              settings = {
                "pm" = "dynamic";
                "pm.max_children" = 75;
                "pm.start_servers" = 10;
                "pm.min_spare_servers" = 5;
                "pm.max_spare_servers" = 20;
                "pm.max_requests" = 500;
              };
            }
          }'';
        description = ''
          PHP-FPM pools. If no pools are defined, the PHP-FPM
          service is disabled.
        '';
      };
    };
  };

  config =
    let
      pools = lib.mapAttrs
        (pool: poolOpts:
          poolOpts // {
            phpPackage = lib.mkIf
              (poolOpts.phpVersion != "")
              (lib.mkForce (configurePackage (customPhpPackage poolOpts)));
          }
        )
        cfg.pools;
    in
    # Pools have to be calculated separately, to avoid infinite recursion.
    { cfg.pools = pools; } // lib.mkIf (cfg.pools != { }) {

      services.phpfpm.settings = {
        error_log = "syslog";
        daemonize = false;
      };

      systemd.slices.app-phpfpm = {
        description = "PHP FastCGI Process Manager Slice";
      };

      systemd.targets.phpfpm = {
        description = "PHP FastCGI Process manager pools target";
        wantedBy = [ "default.target" ];
      };

      systemd.services = lib.mapAttrs'
        (pool: poolOpts:
          lib.nameValuePair "phpfpm-${pool}" {
            description = "PHP FastCGI Process Manager service for pool ${pool}";
            after = [ "network.target" ];
            wantedBy = [ "phpfpm.target" ];
            partOf = [ "phpfpm.target" ];
            serviceConfig =
              let
                cfgFile = fpmCfgFile pool poolOpts;
                iniFile = phpIni poolOpts;
              in
              {
                Slice = "app-phpfpm.slice";
                Type = "notify";
                ExecStart = "${poolOpts.phpPackage}/bin/php-fpm -y ${cfgFile} -c ${iniFile}";
                ExecReload = "${pkgs.coreutils}/bin/kill -USR2 $MAINPID";
                Restart = "always";
              };
          }
        )
        cfg.pools;
    };
}
