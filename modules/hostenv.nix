{ inputs, lib, config, ... }:
let
  fp = inputs.flake-parts.lib;
  types = lib.types;

  systems = config.systems;

  mkSecretsType = lib:
    let
      types = lib.types;
    in
    types.submodule ({ ... }: {
      options = {
        enable = lib.mkEnableOption "secrets management using SOPS";

        file = lib.mkOption {
          type = types.nullOr types.str;
          default = null;
          description = ''
            Path to the SOPS secrets file.

            - Relative paths will be interpreted from the project root.
            - When null hostenv will choose a default path depending on the scope.
          '';
        };

        keys = lib.mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            Secret keys to expose under `/run/secrets/<hostenv.userName>/`.
            Each key maps to `/run/secrets/<hostenv.userName>/<key>`.

            When `enable = true` and this list is empty, hostenv exposes all keys
            found in the scope file.
          '';
        };

        providerPublicKeys = lib.mkOption {
          type = types.listOf types.str;
          default = [ ];
          description = ''
            Age public key recipients for the provider bridge.
            Hostenv scaffolding uses these when generating `.sops.yaml` rules.
          '';
        };
      };
    });

  # Basic Hostenv configuration: paths, project hash.
  hostenvModule = { lib, config, ... }:
    let
      types = lib.types;
      secretsType = mkSecretsType lib;

      # Replace non-alpha characters with a hyphen
      sanitise = str:
        lib.strings.stringAsChars
          (c:
            if builtins.match "[[:alnum:]]" c != null || c == "-" then c else "-"
          )
          str;

      # Collapse multiple hyphens, trim leading/trailing ones
      cleanDashes = str:
        let
          collapsed = builtins.replaceStrings [ "--" "---" "----" "-----" ] [ "-" "-" "-" "-" ] str;
          trimmed = lib.removeSuffix "-" (lib.removePrefix "-" collapsed);
        in
        trimmed;

      slugify = s:
        let
          lastSegment = lib.last (lib.splitString "/" s);
          sanitised = cleanDashes (sanitise lastSegment);
          words = lib.splitString "-" sanitised;

          buildSlug = maxLen:
            let
              go = idx: acc:
                if idx >= builtins.length words then acc
                else
                  let
                    next = builtins.elemAt words idx;
                    candidate = lib.concatStringsSep "-" (acc ++ [ next ]);
                  in
                  if lib.stringLength candidate > maxLen
                  then acc
                  else go (idx + 1) (acc ++ [ next ]);

              resultWords = go 0 [ ];
            in
            if resultWords != [ ]
            then lib.concatStringsSep "-" resultWords
            else lib.substring 0 maxLen sanitised;

          slug = buildSlug 11;
        in
        lib.toLower (
          if slug != "" then slug else lib.concatStringsSep "-" (lib.take 1 words)
        );

    in
    {
      options = {
        hostenvHostname = lib.mkOption {
          type = types.str;
          description = "Top-level hostname for Hostenv.";
          example = "example.com";
          default = "example.invalid";
          internal = true;
        };
        organisation = lib.mkOption {
          type = types.str;
          description = "Business account name or organisation name of the project. Should be short, lowercase, and with no special characters.";
          example = "fooinc";
          # Note on the additional removal of '-' characters:
          # This name is used when generating systemd slice names, which are
          # split by '-' characters, allowing '-'s would introduce an extra layer
          # into the systemd slice hierarchy.
          apply = v: builtins.replaceStrings [ "-" ] [ "" ] (sanitise (lib.strings.toLower v));
        };
        project = lib.mkOption {
          type = types.str;
          description = "Name of the project. Should be short, lowercase, and contain no special characters.";
          example = "coolproject";
          apply = v: builtins.replaceStrings [ "-" ] [ "" ] (sanitise (lib.strings.toLower v));
        };
        environmentName = lib.mkOption {
          type = types.str;
          description = "Name of the current environment. Usually corresponds to a git branch, but can be something else, e.g. an MR slug or number. Should be short, lowercase, and with no special characters.";
        };
        defaultEnvironment = lib.mkOption {
          type = types.str;
          description = "Default environment name for the project.";
          default = "main";
        };
        environments = lib.mkOption {
          type = types.attrs;
          default = { };
          description = "Canonical environment tree (filled by environments.nix).";
          internal = true;
        };
        assertions = lib.mkOption {
          type = types.listOf types.unspecified;
          internal = true;
          default = [ ];
          description = "Hostenv-level assertions accumulated from core invariants.";
        };
        safeEnvironmentName = lib.mkOption {
          type = types.str;
          description = "Name of the current environment, shortened and with special characters removed.";
        };
        gitRef = lib.mkOption {
          type = types.str;
          description = "Git branch or tag of this environment. Defaults to the `environmentName`.";
          example = "main";
        };
        userName = lib.mkOption {
          type = types.str;
          description = "UNIX username (on server) of this project.";
        };
        hostname = lib.mkOption {
          type = types.str;
          description = "Server hostname for this project and environment, as a fully qualified domain name (FQDN).";
          example = "hostenv-main-7c25553.hostenv.sh";
        };
        runtimeRoot = lib.mkOption {
          type = types.str;
          description = "Root directory for hostenv runtime sockets and state.";
          default = "/run/hostenv";
        };
        root = lib.mkOption {
          type = types.oneOf [ types.str types.path ];
          description = "The application's root directory.";
        };
        runtimeDir = lib.mkOption {
          type = types.str;
          description = "Path (on server) where hostenv sockets and pid files may be found.";
          example = lib.literalExpression "${config.runtimeRoot}/user/\${config.hostenv.userName}";
        };
        upstreamRuntimeDir = lib.mkOption {
          type = types.str;
          description = "Path (on server) where upstream reverse proxy socket may be found.";
          example = lib.literalExpression "${config.runtimeRoot}/nginx/\${config.hostenv.userName}";
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
        backupsSecretFile = lib.mkOption {
          type = types.str;
          description = "Path (on server) where secret access key for accessing backups may be found.";
        };
        backupsEnvFile = lib.mkOption {
          type = types.str;
          description = "Path (on server) where an environment file with information related to accessing backups may be found.";
        };
        backupsRepoHost = lib.mkOption {
          type = types.nullOr types.str;
          description = "URL of backups hosting service (without the trailing slash).";
          example = "s3:https://s3.amazonaws.com";
          apply = v: if v == null then null else lib.removeSuffix "/" v;
          default = null;
        };
        projectSecrets = lib.mkOption {
          type = secretsType;
          default = { };
          internal = true;
          description = "Internal: project-level secret scope copied into each environment for provider planning.";
        };
        projectNameHash = lib.mkOption {
          type = types.str;
          description = "Hash of organisation, project, and environment names.";
          internal = true;
        };
      };

      config =
        let
          slugHash = builtins.hashString "sha256" (
            # "\n" is so our result matches
            # `echo "org-project-environment" | sha256sum` in bash.
            # This is for historical reasons: I started by creating environments
            # manually, using hashes generated from the command-line, while forgetting
            # the sublty that echo appends "\n" to every string.
            lib.concatStringsSep "-" [ config.organisation config.project config.environmentName ] + "\n"
          );

          shortName = lib.concatStringsSep "-"
            (
              builtins.map slugify [ config.project config.environmentName ]
            ) + "-" + lib.substring 0 7 slugHash;
        in
        let
          envs = config.environments or { };
          productionEnvs = lib.filterAttrs (_: v: (v.enable or false) && v.type == "production") envs;
          productionNames = builtins.attrNames productionEnvs;
        in
        {
          defaultEnvironment = lib.mkDefault (if productionNames != [ ] then builtins.head productionNames else "main");

          assertions = [
            {
              assertion = (lib.length productionNames) <= 1;
              message = "Only one environment may have type=production (found ${toString (lib.length productionNames)}).";
            }
            {
              assertion = config.userName == shortName;
              message = "hostenv.userName is derived from organisation/project/environment and must not be overridden.";
            }
          ];

          userName = lib.mkForce shortName;
          hostname = lib.mkForce "${shortName}.${config.hostenvHostname}";
          safeEnvironmentName = lib.mkForce (cleanDashes (sanitise config.environmentName));
          # Note: we use environmentName and not safeEnvironmentName as the latter
          # is stripped of some characters that are valid in git branch names,
          # '/' and '--' for example.
          gitRef = lib.mkDefault (config.environmentName or "main");
          runtimeDir = lib.mkForce "${config.runtimeRoot}/user/${config.userName}";
          upstreamRuntimeDir = lib.mkForce "${config.runtimeRoot}/nginx/${config.userName}";
          dataDir = lib.mkForce "/home/${config.userName}/.local/share";
          stateDir = lib.mkForce "/home/${config.userName}/.local/state";
          cacheDir = lib.mkForce "/home/${config.userName}/.cache";
          backupsSecretFile = lib.mkForce "/run/secrets/${config.userName}/backups_secret";
          backupsEnvFile = lib.mkForce "/run/secrets/${config.userName}/backups_env";
          projectNameHash = lib.mkForce slugHash;
        };
    };

  # Per-environment schema used by the hostenv trunk.
  environmentModule =
    { allUsers ? { }, topLevel ? { }, forceNull ? "__HOSTENV_INTERNAL_DO_NOT_CHANGE_SEMAPHORE__", hostenvModule }:
    { lib, config, name, options, ... }:
    let
      types = lib.types;
      secretsType = mkSecretsType lib;

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

          gitlabUsername = lib.mkOption {
            type = types.nullOr types.str;
            default = null;
            description = ''
              GitLab username for the user. Optional.

              Only relevant if `hostenv.provider-service.enable = true` and `hostenv.provider-service.gitlab.enable = true`
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

        deploymentVerification = lib.mkOption {
          type = types.submodule ({ ... }: {
            options = {
              enable = lib.mkOption {
                type = types.bool;
                default = true;
                description = ''
                  Whether deployment verification checks are enabled for this environment.
                '';
              };

              enforce = lib.mkOption {
                type = types.bool;
                default = true;
                description = ''
                  Whether failed deployment verification checks fail the deployment.
                '';
              };

              checks = lib.mkOption {
                type = types.listOf (types.submodule ({ ... }: {
                  options = {
                    name = lib.mkOption {
                      type = types.str;
                      default = "verification-check";
                      description = "Human-readable name for this check.";
                    };

                    type = lib.mkOption {
                      type = types.enum [ "httpHostHeaderCurl" ];
                      default = "httpHostHeaderCurl";
                      description = "Verification check implementation to run.";
                    };

                    request = lib.mkOption {
                      type = types.submodule ({ ... }: {
                        options = {
                          virtualHost = lib.mkOption {
                            type = types.str;
                            description = "Virtual host sent in the HTTP Host header.";
                          };

                          path = lib.mkOption {
                            type = types.str;
                            default = "/";
                            description = "Request path used during verification.";
                          };

                          method = lib.mkOption {
                            type = types.str;
                            default = "GET";
                            description = "HTTP method used during verification.";
                          };

                          targetHostSource = lib.mkOption {
                            type = types.enum [ "nodeConnectionHost" ];
                            default = "nodeConnectionHost";
                            description = "Source of the node host used for direct connection.";
                          };

                          followRedirects = lib.mkOption {
                            type = types.bool;
                            default = false;
                            description = "Whether redirects are followed by curl.";
                          };

                          maxRedirects = lib.mkOption {
                            type = types.int;
                            default = 5;
                            description = "Maximum redirects to follow when followRedirects is enabled.";
                          };

                          timeoutSeconds = lib.mkOption {
                            type = types.int;
                            default = 15;
                            description = "Maximum request duration in seconds.";
                          };

                          tlsMode = lib.mkOption {
                            type = types.enum [ "strict" "insecure" ];
                            default = "strict";
                            description = "TLS verification mode for HTTPS requests.";
                          };
                        };
                      });
                      default = { };
                      description = "Request definition for this check.";
                    };

                    constraints = lib.mkOption {
                      type = types.listOf (types.submodule ({ ... }: {
                        options = {
                          rule = lib.mkOption {
                            type = types.enum [
                              "allowNonZeroExitStatus"
                              "skipStdoutRegexOnRedirect"
                              "stdoutRegexMustMatch"
                              "stderrRegexMustNotMatch"
                              "minHttpStatus"
                              "maxHttpStatus"
                            ];
                            description = "Constraint rule to evaluate for the check.";
                          };

                          value = lib.mkOption {
                            type = types.oneOf [ types.bool types.int types.str ];
                            description = "Constraint value for the selected rule.";
                          };
                        };
                      }));
                      default = [ ];
                      description = "Constraint list evaluated against the check output.";
                    };
                  };
                }));
                default = [ ];
                description = ''
                  Checks run after successful environment deployments.
                '';
              };
            };
          });
          default = { };
          description = ''
            Per-environment deployment verification configuration.
          '';
        };

        secrets = lib.mkOption {
          type = secretsType;
          default = { };
          description = ''
            Per-environment secrets using SOPS.
          '';
        };

        hostenv = lib.mkOption {
          type = types.submoduleWith {
            modules = [
              hostenvModule
              {
                # Provide only the per-env bits; keep environments empty to avoid
                # embedding the whole tree here.
                config.organisation = lib.mkDefault (topLevel.organisation or "");
                config.project = lib.mkDefault (topLevel.project or "");
                config.hostenvHostname = lib.mkDefault (topLevel.hostenvHostname or "example.invalid");
                config.backupsRepoHost = lib.mkDefault (topLevel.backupsRepoHost or null);
                config.environmentName = name;
                config.root = lib.mkDefault (topLevel.root or ".");
                config.environments = { };
                config.projectSecrets = lib.mkDefault (topLevel.secrets or { });
              }
            ];
          };
          default = { };
          description = "Per-environment hostenv metadata.";
        };

      };

      config.virtualHosts.${config.hostenv.hostname} = lib.mkDefault { };
    };

  mkMakeHostenv = system:
    let
      pogOverlays =
        if inputs ? pog && inputs.pog ? overlays
        then inputs.pog.overlays
        else { };
      hasPogOverlay =
        (builtins.hasAttr system pogOverlays)
        && (pogOverlays.${system} ? default);
      pogOverlay =
        if hasPogOverlay
        then pogOverlays.${system}.default
        else (_: _: { });
      supportedSystems = builtins.attrNames pogOverlays;
      supportedSystemsMsg =
        if supportedSystems == [ ] then
          "unknown (pog input missing)"
        else
          lib.concatStringsSep ", " supportedSystems;
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [ pogOverlay ];
      };
    in
    modules: environmentName:
      let
        hostenvModules = lib.attrValues config.flake.modules.hostenv;
      in
      if (!hasPogOverlay) then
        builtins.throw ''
          The hostenv CLI requires the Pog library but it is not available for ${system}.

          Supported systems: ${supportedSystemsMsg}

          Suggested fixes:
          - Use a supported system (e.g. a Linux host or VM).
          - If you're on macOS, run a Linux dev shell via a remote builder or `nix develop --system x86_64-linux`.
        ''
      else
        pkgs.lib.evalModules {
          modules =
            [
              { _module.args = { inherit inputs pkgs; }; }
              ({ config, ... }: {
                hostenv.environmentName =
                  if environmentName == null
                  then config.defaultEnvironment
                  else environmentName;
              })
              {
                systemd.globalEnvironment.TZDIR = "${pkgs.tzdata}/share/zoneinfo";
              }
            ]
            ++ hostenvModules
            ++ modules;
        };

in
{
  options.flake.makeHostenv = lib.mkOption {
    type = lib.types.attrsOf (lib.types.functionTo (lib.types.functionTo lib.types.unspecified));
    readOnly = true;
    description = "Per-system function: modules -> environmentName -> evalModules result.";
  };

  options.perSystem = fp.mkPerSystemOption ({ ... }: {
    options.hostenv.makeHostenv = lib.mkOption {
      type = lib.types.functionTo (lib.types.functionTo lib.types.unspecified);
      readOnly = true;
      description = "Per-system makeHostenv helper (modules -> environmentName -> evalModules).";
    };
  });

  config = {
    flake.lib.hostenv.module = hostenvModule;
    flake.lib.hostenv.environmentModule = environmentModule;

    flake.modules.hostenv.core = { lib, config, ... }: {
      options = {
        secrets = lib.mkOption {
          type = mkSecretsType lib;
          default = { };
          description = ''
            Project-wide secrets using SOPS.
          '';
        };

        hostenv = lib.mkOption {
          type = types.submodule hostenvModule;
          description = "Hostenv configuration for the current environment.";
        };
      };
    };

    flake.makeHostenv = lib.genAttrs systems mkMakeHostenv;

    perSystem = { system, ... }: {
      hostenv.makeHostenv = config.flake.makeHostenv.${system};
    };
  };
}
