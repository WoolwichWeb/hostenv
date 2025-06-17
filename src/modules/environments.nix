# Server environments, their routes, and the users that may access them.
{ config, lib, pkgs, ... }:
let
  cfg = config.environments;

  user = with lib.types; {
    options = {
      email = lib.mkOption {
        type = str;
        description = "A valid email address for the user.";
      };
      publicKeys = lib.mkOption {
        type = listOf singleLineStr;
        description = ''
          A list of verbatim OpenSSH public keys that should be added to the
          user's authorized keys.
        '';
      };
    };
  };

  environment = with lib.types; {
    options = {
      enable = lib.mkEnableOption "this environment on hostenv";

      users = lib.mkOption {
        type = attrsOf (submodule user);
        default = { };
      };

      # @todo: enforce the 'only one environment may be production' rule.
      type = lib.mkOption {
        type = enum [ "development" "testing" "production" ];
        default = "development";
        description = ''
          Environment type, setting this to production exposes this environment
          to search engines.
          Only one environment may be production.
        '';
      };

      hosts = lib.mkOption {
        type = attrsOf (submodule {
          options = {

            enableLetsEncrypt = lib.mkOption {
              default = true;
              type = bool;
              description = ''
                Enable Let's Encrypt for this host.
              '';
            };

            globalRedirect = mkOption {
              type = types.nullOr types.str;
              default = null;
              example = "newserver.example.org";
              description = ''
                If set, all requests for this host are redirected (defaults to 301,
                configurable with `redirectCode`) to the given hostname.
              '';
            };

            redirectCode = mkOption {
              type = types.ints.between 300 399;
              default = 301;
              example = 308;
              description = ''
                HTTP status used by `globalRedirect` and `forceSSL`. Possible usecases
                include temporary (302, 307) redirects, keeping the request method and
                body (307, 308), or explicitly resetting the method to GET (303).
                See <https://developer.mozilla.org/en-US/docs/Web/HTTP/Redirections>.
              '';
            };

            basicAuthFile = mkOption {
              type = types.nullOr types.path;
              default = null;
              description = ''
                Basic Auth password file for a vhost.
                Can be created by running {command}`nix-shell --packages apacheHttpd --run 'htpasswd -B -c FILENAME USERNAME'`.
              '';
            };

          };
        });
        default = { };
      };

    };
  };
in
{

  options.allEnvironments = with lib.types; lib.mkOption {
    type = attrsOf (submodule {
      options = {
        users = lib.mkOption {
          type = attrsOf (submodule user);
          default = { };
        };
      };
    });
    default = { };
  };

  options.environments = with lib.types; lib.mkOption {
    type = attrsOf (submodule environment);
    default = { };
  };

  options.defaultEnvironment = with lib.types; lib.mkOption {
    type = str;
    description = "Environment built when the default is not specified by the user.";
    example = "production";
  };

  config.defaultEnvironment = lib.mkDefault (
    let
      productionEnvs = lib.filterAttrs (n: v: v.type == "production" && v.enable) cfg;
      names = builtins.attrNames productionEnvs;
    in
    if names != [ ] then builtins.head names else "main"
  );

}
