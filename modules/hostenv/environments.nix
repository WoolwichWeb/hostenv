{ config, lib, ... }:
let
  libHostenv = config.flake.lib.hostenv;
  # hostenvModule = config.flake.lib.hostenv.module;
  # environmentModule = config.flake.lib.hostenv.environmentModule;
  exportUser = user: {
    inherit (user) email publicKeys;
  };

  exportLocation = location: {
    inherit (location) return;
  };

  exportSecurity = security: {
    inherit (security)
      csp
      cspMode
      cspReportTo
      reportTo
      referrerPolicy
      xFrameOptions
      xContentTypeOptions
      hsts;
  };

  exportVirtualHost = virtualHost: {
    inherit (virtualHost)
      enableLetsEncrypt
      allowIndexing
      globalRedirect
      redirectCode
      hsts;

    locations = lib.mapAttrs (_: location: exportLocation location) virtualHost.locations;
    security = exportSecurity virtualHost.security;
  };

  exportSecrets = secrets: {
    inherit (secrets) enable file keys providerPublicKeys;
  };

  exportHostenv = hostenv: {
    inherit (hostenv)
      organisation
      project
      environmentName
      safeEnvironmentName
      gitRef
      userName
      hostname
      runtimeDir
      upstreamRuntimeDir
      dataDir
      stateDir
      cacheDir;
  };

  exportEnvironment = environment: {
    inherit (environment) enable type deploymentVerification;

    users = lib.mapAttrs (_: user: exportUser user) environment.users;
    virtualHosts = lib.mapAttrs (_: virtualHost: exportVirtualHost virtualHost) environment.virtualHosts;
    # secrets = exportSecrets environment.secrets;
    hostenv = exportHostenv environment.hostenv;
  };

  exportEnvironments = environments:
    lib.mapAttrs (_: environment: exportEnvironment environment) environments;

in
{
  flake.modules.hostenv.environments =
    { config, lib, ... }:
    let
      forceNull = "__HOSTENV_INTERNAL_DO_NOT_CHANGE_SEMAPHORE__";
      topLevel = config.hostenv or { };
      types = lib.types;
      envModule = libHostenv.environmentModule {
        allUsers = config.allEnvironments.users;
        hostenvModule = libHostenv.module;
        inherit topLevel forceNull;
      };
    in
    {
      options.allEnvironments = lib.mkOption {
        type = types.submodule {
          options = {
            users = lib.mkOption {
              type = types.attrsOf (types.submodule {
                options = {
                  email = lib.mkOption { type = types.str; description = "A valid email address for the user."; };
                  publicKeys = lib.mkOption { type = types.listOf types.singleLineStr; description = "OpenSSH public keys for this user."; };
                };
              });
              default = { };
            };
          };
        };
        default = { users = { }; };
        description = "Settings applied across all environments.";
      };

      options.environments = lib.mkOption {
        type = types.attrsOf (types.submodule envModule);
        default = { };
        description = "Project environment definitions.";
      };

      options.defaultEnvironment = lib.mkOption {
        type = types.str;
        description = "Name of the default environment for the project.";
        example = "production";
        default = "main";
      };

      options.exportedEnvironments = lib.mkOption {
        type = types.attrs;
        default = { };
        internal = true;
        description = "Sanitized, user-facing view of environments for JSON output.";
      };

      config =
        let
          productionEnvs = lib.filterAttrs (_: v: (v.enable or false) && v.type == "production") config.environments;
          productionNames = builtins.attrNames productionEnvs;
        in
        {
          assertions = [
            {
              assertion =
                let eval = builtins.tryEval (builtins.toJSON config.exportedEnvironments);
                in eval.success;
              message = ''
                config.exportedEnvironments must be JSON-serializable.
              '';
            }
            {
              assertion = (lib.length productionNames) <= 1;
              message = "Only one environment may have type=production (found ${toString (lib.length productionNames)}).";
            }
          ];
          exportedEnvironments = exportEnvironments config.environments;

          # The below does not work anymore due to an infinite recursion bug.
          # There is a workaround: populate the default environment from a
          # separately evaluated copy of the project's config. However, this
          # would introduce some complexity, so it's defered for now.
          # @todo: revisit whether the additional complexity is worth it to
          # get this working again.
          # defaultEnvironment = lib.mkDefault (if productionNames != [ ] then builtins.head productionNames else "main");
        };
    };
}
