# Per-environment schema used by the hostenv trunk.
{ lib, config, name, options, allUsers ? { }, topLevel ? { }, forceNull ? "__HOSTENV_INTERNAL_DO_NOT_CHANGE_SEMAPHORE__", ... }:
let
  types = lib.types;
  tl = topLevel;

  user = {
    options = {

      email = lib.mkOption {
        type = types.str;
        description = "A valid email address for the user.";
      };

      publicKeys = lib.mkOption {
        type = types.listOf types.singleLineStr;
        description = ''
          A list of verbatim OpenSSH public keys that should be added to the
          user's authorized keys.
        '';
      };
    };
  };
in
{
  options = {
    enable = lib.mkEnableOption "this environment on hostenv";

    users = lib.mkOption {
      type = types.attrsOf (types.submodule user);
      default = allUsers;
    };

    type = lib.mkOption {
      type = types.enum [ "development" "testing" "production" ];
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
        type = types.attrsOf
          (types.submodule ({ options, config, ... }: {
            options = {

              locations = lib.mkOption {
                type = types.attrsOf (types.submodule {
                  options = {
                    return = lib.mkOption {
                      type = with types; nullOr (oneOf [ str int ]);
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
                type = types.bool;
                description = ''
                  Enable Let's Encrypt for this host.
                '';
              };

              allowIndexing = lib.mkOption {
                default = if envConfig.type == "production" then true else false;
                type = types.bool;
                description = ''
                  Advise search engines whether they are allowed to crawl
                  this site (or not).

                  Defaults to `true` on production environments, and `false`
                  on all other environment types.
                '';
              };

              globalRedirect = lib.mkOption {
                type = types.nullOr types.str;
                default = null;
                example = "newserver.example.org";
                description = ''
                  If set, all requests for this host are redirected to the given
                  hostname (the HTTP status code defaults to 301, configurable
                  with `redirectCode`).
                '';
                apply =
                  let
                    hostName = envConfig.hostenv.hostname or "";
                    thisHost = config._module.args.name;
                    thisPrio = options.globalRedirect.highestPrio;
                  in
                  if thisHost == hostName
                  then value: if value == forceNull then null else value
                  else value: value;
              };

              redirectCode = lib.mkOption {
                default = 301;
                type = types.int;
                description = ''
                  HTTP status code to return when redirecting requests to a
                  different host.
                '';
              };

              hsts = lib.mkOption {
                default = true;
                type = types.bool;
                description = ''
                  Include an HSTS header for this host.
                '';
              };

              security = lib.mkOption {
                type = types.submodule ({ ... }: {
                  options = {
                    csp = lib.mkOption {
                      type = types.nullOr types.singleLineStr;
                      default = null;
                      description = ''
                        Content-Security-Policy value for this host. Set to null to disable.
                      '';
                      apply = value:
                        if value != null && lib.strings.hasInfix "\"" value then
                          builtins.throw "virtualHosts.<host>.security.csp may not contain double quotes"
                        else
                          value;
                    };

                    cspMode = lib.mkOption {
                      type = types.enum [ "enforce" "report-only" ];
                      default = "enforce";
                      description = ''
                        Whether to emit CSP as enforce or report-only.
                      '';
                    };

                    cspReportTo = lib.mkOption {
                      type = types.nullOr (types.strMatching "^[A-Za-z0-9._-]+$");
                      default = null;
                      description = ''
                        Optional CSP report-to group name to append to the policy.
                      '';
                    };

                    reportTo = lib.mkOption {
                      type = types.nullOr types.singleLineStr;
                      default = null;
                      description = ''
                        Optional Report-To header value (JSON) to emit when set.
                      '';
                      apply = value:
                        if value != null && lib.strings.hasInfix "'" value then
                          builtins.throw "virtualHosts.<host>.security.reportTo may not contain single quotes"
                        else
                          value;
                    };

                    referrerPolicy = lib.mkOption {
                      type = types.nullOr (types.enum [
                        "no-referrer"
                        "no-referrer-when-downgrade"
                        "same-origin"
                        "origin"
                        "strict-origin"
                        "origin-when-cross-origin"
                        "strict-origin-when-cross-origin"
                        "unsafe-url"
                      ]);
                      default = "strict-origin-when-cross-origin";
                      description = ''
                        Referrer-Policy header value.
                      '';
                    };

                    xFrameOptions = lib.mkOption {
                      type = types.nullOr (types.enum [ "SAMEORIGIN" "DENY" ]);
                      default = "SAMEORIGIN";
                      description = ''
                        X-Frame-Options header value. Set to null to disable.
                      '';
                    };

                    xContentTypeOptions = lib.mkOption {
                      type = types.bool;
                      default = true;
                      description = ''
                        Whether to emit X-Content-Type-Options: nosniff.
                      '';
                    };

                    hsts = lib.mkOption {
                      type = types.bool;
                      default = true;
                      description = ''
                        Whether to emit HSTS (only on HTTPS/forceSSL hosts).
                      '';
                    };
                  };
                });
                default = { };
                description = ''
                  Security header configuration for this host.
                '';
              };
            };
          }));
        default = { };
        description = ''
          Optional virtual host configuration. Enabling a framework provides a
          sensible default.
        '';
      };

    hostenv = lib.mkOption {
      type = types.submoduleWith {
        modules = [
          ./hostenv.nix
          {
            # Provide only the per-env bits; keep environments empty to avoid
            # embedding the whole tree here.
            config.organisation = lib.mkDefault (tl.organisation or "");
            config.project = lib.mkDefault (tl.project or "");
            config.hostenvHostname = lib.mkDefault (tl.hostenvHostname or "example.invalid");
            config.backupsRepoHost = lib.mkDefault (tl.backupsRepoHost or null);
            config.environmentName = name;
            config.root = lib.mkDefault (tl.root or ".");
            config.environments = { };
          }
        ];
        specialArgs = { isNestedHostenv = true; };
      };
      default = { };
      description = "Per-environment hostenv metadata.";
    };

  };

  config.virtualHosts.${config.hostenv.hostname} = lib.mkDefault { };
}
