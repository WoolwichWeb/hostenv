# Server environments, their routes, and the users that may access them.
{ config, lib, ... }:
let
  cfg = config.environments;
  defaultPriority = 1000;

  # Flag for when the user has set the value of an option to
  # `lib.mkForce null` and post-processing should respect that.
  forceNull = "__HOSTENV_INTERNAL_DO_NOT_CHANGE_SEMAPHORE__";

  # `config.hostenv` is not mandatory, as this module may be evaluated as
  # part of a full hostenv build, or separately.
  # This allows hostenv to make environment config available, without having
  # to build a full hostenv system.
  topLevel = config.hostenv or { };

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

  environment = with lib.types; { config, name, allUsers, options, ... }: {
    options = {
      enable = lib.mkEnableOption "this environment on hostenv";

      users = lib.mkOption {
        type = attrsOf (submodule user);
        default = allUsers;
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

      virtualHosts =
        let envConfig = config;
        in lib.mkOption {
          type = attrsOf
            (submodule ({ options, config, ... }: {
              options = {

                locations = lib.mkOption {
                  type = attrsOf (submodule {
                    options = {
                      return = lib.mkOption {
                        type =
                          with types;
                          nullOr (oneOf [
                            str
                            int
                          ]);
                        default = null;
                        example = "301 http://example.com$request_uri";
                        description = ''
                          Adds a return directive, for e.g. redirections.
                        '';
                      };
                    };
                  });
                  default = { };
                };

                enableLetsEncrypt = lib.mkOption {
                  default = true;
                  type = bool;
                  description = ''
                    Enable Let's Encrypt for this host.
                  '';
                };

                allowIndexing = lib.mkOption {
                  default = if envConfig.type == "production" then true else false;
                  type = bool;
                  description = ''
                    Advise search engines whether they are allowed to crawl
                    this site (or not).

                    Defaults to `true` on production environments, and `false`
                    on all other environment types.
                  '';
                };

                globalRedirect = lib.mkOption {
                  type = nullOr str;
                  default = null;
                  example = "newserver.example.org";
                  description = ''
                    If set, all requests for this host are redirected to the given
                    hostname (the HTTP status code defaults to 301, configurable
                    with `redirectCode`).
                  '';
                  apply =
                    let
                      hostName = envConfig.hostenv.hostname;
                      thisHost = config._module.args.name;
                      thisPrio = options.globalRedirect.highestPrio;
                    in
                    # The user has set the redirect to `null` and wants us to
                      # respect that.
                    v:
                    if thisHost == hostName && thisPrio < defaultPriority && v == null
                    then forceNull
                    else v;
                };

                redirectCode = lib.mkOption {
                  type = ints.between 300 399;
                  default = 301;
                  example = 308;
                  description = ''
                    HTTP status used by `globalRedirect` and `forceSSL`. Possible usecases
                    include temporary (302, 307) redirects, keeping the request method and
                    body (307, 308), or explicitly resetting the method to GET (303).
                    See <https://developer.mozilla.org/en-US/docs/Web/HTTP/Redirections>.
                  '';
                };

                basicAuthFile = lib.mkOption {
                  type = nullOr path;
                  default = null;
                  description = ''
                    Basic Auth password file for a vhost.
                    Can be created by running {command}`nix-shell --packages apacheHttpd --run 'htpasswd -B -c FILENAME USERNAME'`.
                  '';
                };

              };
            }));
          default = { };
          apply = v:
            let
              hostname = config.hostenv.hostname;
              # Virtual hosts that are valid redirection targets for the hostenv
              # assigned *.hostenv.sh hostname.
              # Note about Let's Encrypt: the `enableLetsEncrypt` option may be
              # disabled if the user is still pointing the domain, so we do not
              # consider the hostname a valid target in that case. This will
              # probably have to be revisited later, especially once support for
              # custom certificates is added.
              targetVHosts = lib.filterAttrs
                (n: val: n != hostname && val.globalRedirect == null && val.enableLetsEncrypt == true)
                v;

              setRedirect = v: target:
                v // {
                  ${hostname} = v.${hostname} // { globalRedirect = target; };
                };
            in
            # Tests if there is a valid target to redirect to, and that the user
              # hasn't already set a redirect target.
            if v.${hostname}.globalRedirect == null && builtins.length (builtins.attrValues targetVHosts) > 0
            then setRedirect v (builtins.head (lib.attrsToList targetVHosts)).name
            else if v.${hostname}.globalRedirect == forceNull
            then setRedirect v null
            else v;
        };

      hostenv = lib.mkOption {
        type = with lib.types; submoduleWith {
          modules = [
            ./hostenv.nix
            {
              config.organisation = topLevel.organisation;
              config.project = topLevel.project;
              # Environment name is what changes on a per-environment basis,
              # everything else remains the same.
              config.environmentName = name;
              config.root = topLevel.root;
            }
          ];
        };
        default = { };
        description = ''
          Per-environment Hostenv configuration.
        '';
      };

    };
    config.virtualHosts.${config.hostenv.hostname} = lib.mkDefault { };
  };

in
{

  options.allEnvironments = with lib.types; lib.mkOption {
    type = (submodule {
      options = {
        users = lib.mkOption {
          type = attrsOf (submodule user);
          default = { };
        };
      };
    });
    default = { users = { }; };
    description = "Settings applied across all environments.";
  };

  options.environments = with lib.types; lib.mkOption {
    type = attrsOf (submoduleWith {
      modules = [ environment ];
      specialArgs = {
        allUsers = config.allEnvironments.users;
      };
    });
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
